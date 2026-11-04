# Analyse bivariée (fonctions internes)
genre_var <- function(x) {
  u <- length(unique(x[!is.na(x)]))
  if (u < 2) return(NA_character_)
  if (is.numeric(x)) return(if (u <= 5) "cat" else "num")
  if ((is.factor(x) || is.character(x) || is.logical(x)) && u <= 20) return("cat")
  NA_character_
}

est_normal <- function(x) {
  n <- length(x)
  if (n >= 30) return(TRUE)
  if (n < 3 || length(unique(x)) < 3) return(FALSE)
  p <- tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
  isTRUE(p > 0.05)
}

analyser_paire <- function(data, a, b) {
  ga <- genre_var(data[[a]]); gb <- genre_var(data[[b]])
  if (is.na(ga) || is.na(gb)) return(NULL)
  if (ga == "cat" && gb == "num") { t <- a; a <- b; b <- t; t <- ga; ga <- gb; gb <- t }
  ok <- !is.na(data[[a]]) & !is.na(data[[b]])
  x <- data[[a]][ok]; y <- data[[b]][ok]
  if (length(x) < 3) return(NULL)
  sortie <- list(a = a, b = b, n = length(x), n_exclus = sum(!ok))

  if (ga == "num" && gb == "num") {
    pearson <- est_normal(x) && est_normal(y)
    m <- if (pearson) "pearson" else "spearman"
    tst <- tryCatch(suppressWarnings(stats::cor.test(x, y, method = m, exact = FALSE)),
                    error = function(e) NULL)
    if (is.null(tst)) return(NULL)
    r <- unname(tst$estimate)
    return(c(sortie, list(
      kind = "nn", test = if (pearson) "Corrélation de Pearson" else "Corrélation de Spearman",
      stat_name = if (pearson) "r" else "rho", stat = r, p = tst$p.value,
      effect_name = if (pearson) "r" else "rho", effect = r,
      table = data.frame(Coefficient = if (pearson) "r de Pearson" else "rho de Spearman",
                         Valeur = fmt_num(r, 3), `p-value` = sub("^p (= )?", "", fmt_p(tst$p.value)),
                         Effectif = length(x), check.names = FALSE, stringsAsFactors = FALSE))))
  }

  if (ga == "num" && gb == "cat") {
    g <- as.character(y)
    tailles <- table(g)
    garder <- names(tailles)[tailles >= 2]
    groupes_exclus <- setdiff(names(tailles), garder)
    if (length(garder) < 2) return(NULL)
    i <- g %in% garder
    x <- x[i]; g <- factor(g[i])
    if (length(unique(x)) < 2) return(NULL)
    decoupe <- split(x, g)
    normal <- all(vapply(decoupe, est_normal, logical(1)))
    k <- length(decoupe)
    if (normal) {
      tst <- tryCatch(if (k == 2) stats::t.test(x ~ g) else stats::oneway.test(x ~ g),
                      error = function(e) NULL)
      nom <- if (k == 2) "Test t de Welch" else "ANOVA de Welch"
      sn <- if (k == 2) "t" else "F"
    } else {
      tst <- tryCatch(suppressWarnings(if (k == 2) stats::wilcox.test(x ~ g, exact = FALSE)
                                       else stats::kruskal.test(x ~ g)),
                      error = function(e) NULL)
      nom <- if (k == 2) "Test de Wilcoxon-Mann-Whitney" else "Test de Kruskal-Wallis"
      sn <- if (k == 2) "W" else "H"
    }
    if (is.null(tst) || is.na(tst$p.value)) return(NULL)
    sct <- sum((x - mean(x))^2)
    sce <- sum(vapply(decoupe, function(v) length(v) * (mean(v) - mean(x))^2, numeric(1)))
    if (normal) {
      eff <- if (sct > 0) sce / sct else NA_real_; en <- "eta2"
    } else {
      h <- suppressWarnings(stats::kruskal.test(x ~ g)$statistic)
      eff <- unname(h) / (length(x) - 1); en <- "epsilon2"
    }
    tab <- data.frame(
      Groupe = names(decoupe),
      Effectif = vapply(decoupe, length, numeric(1)),
      Moyenne = vapply(decoupe, function(v) fmt_num(mean(v)), character(1)),
      `Écart-type` = vapply(decoupe, function(v) fmt_num(if (length(v) > 1) stats::sd(v) else NA), character(1)),
      Médiane = vapply(decoupe, function(v) fmt_num(stats::median(v)), character(1)),
      check.names = FALSE, stringsAsFactors = FALSE)
    rownames(tab) <- NULL
    return(c(sortie, list(
      kind = "nc", test = nom, stat_name = sn, stat = unname(tst$statistic), p = tst$p.value,
      effect_name = en, effect = eff, normal = normal, table = tab,
      groupes_exclus = groupes_exclus,
      moyennes = vapply(decoupe, mean, numeric(1)),
      medianes = vapply(decoupe, stats::median, numeric(1)))))
  }

  x <- as.character(x); y <- as.character(y)
  tab <- table(x, y)
  if (min(dim(tab)) < 2) return(NULL)
  att <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  v <- sqrt(unname(chi$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
  if (any(att < 5)) {
    if (all(dim(tab) == 2)) {
      f <- stats::fisher.test(tab)
    } else {
      graine <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
      on.exit(if (!is.null(graine)) assign(".Random.seed", graine, envir = globalenv()), add = TRUE)
      set.seed(123)
      f <- stats::fisher.test(tab, simulate.p.value = TRUE, B = 10000)
    }
    nom <- "Test exact de Fisher"; p <- f$p.value; st <- NA_real_
  } else {
    nom <- "Test du Chi-deux"; p <- chi$p.value; st <- unname(chi$statistic)
  }
  lignes <- rownames(tab); cols <- colnames(tab)
  affiche <- data.frame(Modalite = lignes, stringsAsFactors = FALSE)
  names(affiche)[1] <- a
  for (cc in cols) {
    affiche[[cc]] <- paste0(tab[, cc], " (", fmt_num(tab[, cc] / rowSums(tab) * 100, 1), " %)")
  }
  affiche[["Total"]] <- as.numeric(rowSums(tab))
  c(sortie, list(kind = "cc", test = nom, stat_name = "Chi2", stat = st, p = p,
                 effect_name = "V de Cramér", effect = v, table = affiche))
}

compute_bivariate <- function(data, paires) {
  res <- list(); ignorees <- character()
  for (pr in paires) {
    r <- tryCatch(analyser_paire(data, pr[1], pr[2]), error = function(e) NULL)
    if (is.null(r)) ignorees <- c(ignorees, paste(pr, collapse = " x ")) else res[[length(res) + 1]] <- r
  }
  list(resultats = res, ignorees = ignorees)
}
