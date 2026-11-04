# Graphiques en base R, en couleurs
AR_COUL <- c("#2980b9", "#e67e22", "#27ae60", "#8e44ad", "#c0392b", "#16a085", "#f1c40f", "#7f8c8d")

normaliser_graph <- function(graph) {
  if (is.null(graph) || identical(graph, FALSE)) return(character())
  if (identical(graph, TRUE)) return("auto")
  if (!is.character(graph)) stop("'graph' doit valoir TRUE, FALSE ou un choix parmi \"hist\", \"box\", \"bar\", \"scatter\".", call. = FALSE)
  graph <- unique(tolower(graph))
  inconnus <- setdiff(graph, c("hist", "box", "bar", "scatter"))
  if (length(inconnus) > 0) stop("Graphique inconnu : ", paste(inconnus, collapse = ", "), ". Choix possibles : hist, box, bar, scatter.", call. = FALSE)
  graph
}

fig_png <- function(dessin, largeur = 1500, hauteur = 900) {
  f <- tempfile("ar_fig_", fileext = ".png")
  grDevices::png(f, width = largeur, height = hauteur, res = 170)
  ok <- FALSE
  on.exit({ grDevices::dev.off(); if (!ok) unlink(f) }, add = TRUE)
  graphics::par(mar = c(5, 5, 3, 1.5), cex.main = 1, font.main = 1, col.axis = "#2c3e50", col.lab = "#2c3e50", las = 1, bty = "l")
  dessin()
  ok <- TRUE
  f
}

court <- function(x, n = 18) ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "."), x)

g_hist <- function(x, col) fig_png(function() graphics::hist(x, col = AR_COUL[1], border = "white", main = paste("Histogramme de", col), xlab = col, ylab = "Effectif"))
g_box <- function(x, col) fig_png(function() graphics::boxplot(x, horizontal = TRUE, col = AR_COUL[2], main = paste("Boîte à moustaches de", col), xlab = col), hauteur = 650)
g_bar <- function(mods, effs, col) {
  if (length(mods) > 15) { mods <- mods[1:15]; effs <- effs[1:15] }
  titre <- paste("Répartition de", col)
  fig_png(function() {
    graphics::par(mar = c(5, 10, 3, 2))
    b <- graphics::barplot(rev(effs), names.arg = rev(court(mods)), horiz = TRUE, col = AR_COUL[1], border = NA, main = titre, xlab = "Effectif", xlim = c(0, max(effs) * 1.12), cex.names = 0.85)
    graphics::text(rev(effs), b, labels = rev(effs), pos = 4, cex = 0.8, col = "#2c3e50")
  }, hauteur = max(650, 300 + 70 * length(mods)))
}

graphes_uni <- function(x, s, col, choix) {
  g <- list()
  if (length(choix) == 0) return(g)
  auto <- "auto" %in% choix
  if (s$type == "numeric") {
    v <- x[is.finite(x)]
    discret <- s$n_unique <= 10
    if ((auto && !discret) || "hist" %in% choix) g$hist <- g_hist(v, col)
    if ("box" %in% choix) g$box <- g_box(v, col)
    if ((auto && discret) || "bar" %in% choix) {
      if (discret) { t <- table(v); g$bar <- g_bar(names(t), as.numeric(t), col) }
    }
  } else if (auto || "bar" %in% choix) {
    g$bar <- g_bar(s$all_categories$Modalite, s$all_categories$Effectif, col)
  }
  g
}

graphe_bi <- function(data, r, choix) {
  if (length(choix) == 0) return(list())
  auto <- "auto" %in% choix
  ok <- !is.na(data[[r$a]]) & !is.na(data[[r$b]])
  x <- data[[r$a]][ok]; y <- data[[r$b]][ok]
  if (r$kind == "nn" && (auto || "scatter" %in% choix)) {
    return(list(scatter = fig_png(function() {
      graphics::plot(x, y, pch = 19, col = grDevices::adjustcolor(AR_COUL[1], 0.6), xlab = r$a, ylab = r$b, main = paste(r$b, "selon", r$a))
      a <- tryCatch(stats::lm(y ~ x), error = function(e) NULL)
      if (!is.null(a)) graphics::abline(a, col = AR_COUL[2], lwd = 2)
    })))
  }
  if (r$kind == "nc" && (auto || "box" %in% choix)) {
    g <- factor(as.character(y))
    return(list(box = fig_png(function() graphics::boxplot(x ~ g, col = rep_len(AR_COUL, nlevels(g)), xlab = r$b, ylab = r$a, main = paste(r$a, "selon", r$b)))))
  }
  if (r$kind == "cc" && (auto || "bar" %in% choix)) {
    t <- table(as.character(y), as.character(x))
    return(list(bar = fig_png(function() {
      cols <- rep_len(AR_COUL, nrow(t))
      lim <- max(t) * 1.25
      graphics::barplot(t, beside = TRUE, col = cols, border = NA, ylim = c(0, lim), xlab = r$a, ylab = "Effectif", main = paste(r$a, "selon", r$b), names.arg = court(colnames(t)))
      graphics::legend("topright", legend = court(rownames(t)), fill = cols, border = NA, bty = "n", title = r$b, cex = 0.8)
    })))
  }
  list()
}
