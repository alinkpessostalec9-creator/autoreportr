# Interprétations statiques, utilisées sans clé ou si l'IA échoue
interp_numeric <- function(col, s) {
  if (is.na(s$sd)) return(paste0(col, " ne compte qu'une seule valeur exploitable (", fmt_num(s$mean), ")."))
  disp <- if (is.na(s$cv)) "non évaluable (moyenne nulle)" else if (s$cv < 15) "faible" else if (s$cv < 30) "modérée" else "élevée"
  txt <- paste0(col, " a une moyenne de ", fmt_num(s$mean), " et une médiane de ", fmt_num(s$median),
                ". Les valeurs vont de ", fmt_num(s$min), " à ", fmt_num(s$max),
                ". La dispersion est ", disp, " (écart-type = ", fmt_num(s$sd),
                if (!is.na(s$cv)) paste0(", CV = ", fmt_pct(s$cv)), ").")
  if (!is.na(s$skewness)) {
    forme <- if (abs(s$skewness) < 0.5) "plutôt symétrique"
             else if (s$skewness > 0) "étirée vers les grandes valeurs" else "étirée vers les petites valeurs"
    txt <- paste0(txt, " La distribution est ", forme, ".")
  }
  if (s$n_outliers > 0) {
    txt <- paste0(txt, " ", s$n_outliers, " valeur(s) sortent de l'intervalle [", fmt_num(s$lower_fence), " ; ",
                  fmt_num(s$upper_fence), "] et méritent une vérification.")
  }
  txt
}

interp_categorical <- function(col, s) {
  if (s$n_exaequo > 1) {
    mods <- paste(s$modalites_exaequo$Modalite, collapse = ", ")
    return(paste0("Les catégories ", mods, " de ", col, " sont les plus fréquentes, avec ", s$max_effectif,
                  " observations chacune (", fmt_pct(s$modalites_exaequo$Pourcentage[1]), ")."))
  }
  top <- s$all_categories[1, ]
  paste0("La catégorie ", top$Modalite, " est la plus représentée pour ", col, ", avec ", top$Effectif,
         " observations (", fmt_pct(top$Pourcentage), ") sur ", s$n_levels, " catégorie(s).")
}

force_effet <- function(r) {
  e <- abs(r$effect)
  if (is.na(e)) return("non évaluable")
  if (r$kind == "nc") return(if (e < 0.01) "négligeable" else if (e < 0.06) "faible" else if (e < 0.14) "moyenne" else "forte")
  if (e < 0.1) "négligeable" else if (e < 0.3) "faible" else if (e < 0.5) "moyenne" else "forte"
}

interp_bivariee <- function(r) {
  sig <- !is.na(r$p) && r$p < 0.05
  mot <- "significative"
  seuil <- "au seuil de 5 %"
  debut <- paste0(r$test, " (", fmt_p(r$p), ", n = ", r$n, "). ")
  taille <- paste0(r$effect_name, " = ", fmt_num(r$effect, 3))
  if (r$kind == "nn") {
    sens <- if (r$effect > 0) "positive" else "négative"
    fin <- if (sig) paste0("La liaison entre ", r$a, " et ", r$b, " est ", mot, " ", seuil, ", ", sens, " et d'intensité ", force_effet(r), " (", taille, ").")
           else paste0("Aucune liaison significative au seuil de 5 % n'est mise en évidence entre ", r$a, " et ", r$b, " (", taille, ").")
  } else if (r$kind == "nc") {
    centre <- if (isTRUE(r$normal)) r$moyennes else r$medianes
    indic <- if (isTRUE(r$normal)) "moyenne" else "médiane"
    fin <- if (sig) paste0("La différence de ", r$a, " selon ", r$b, " est ", mot, " ", seuil, ". La ", indic, " la plus élevée est observée pour ", names(which.max(centre)), " (", fmt_num(max(centre)),
                           ") et la plus basse pour ", names(which.min(centre)), " (", fmt_num(min(centre)), "). L'intensité de la différence est ", force_effet(r), " (", taille, ").")
           else paste0("Aucune différence significative au seuil de 5 % de ", r$a, " n'est mise en évidence selon ", r$b, " (", taille, ").")
  } else {
    fin <- if (sig) paste0("L'association entre ", r$a, " et ", r$b, " est ", mot, " ", seuil, ", d'intensité ", force_effet(r), " (", taille, ").")
           else paste0("Aucune association significative au seuil de 5 % n'est mise en évidence entre ", r$a, " et ", r$b, " (", taille, ").")
  }
  paste0(debut, fin)
}

interp_manquants <- function(p) {
  if (p > 20) "Le taux de valeurs manquantes est élevé. Il est recommandé d'en rechercher les causes avant de conclure."
  else if (p > 5) "Le taux de valeurs manquantes est modéré. Une attention particulière est recommandée."
  else "Le taux de valeurs manquantes est faible."
}

resume_statique <- function(res) {
  phrases <- paste0("L'ensemble de ", res$n_obs, " observations et ", length(res$vars), " variable(s) présente un taux de complétude de ", fmt_pct(res$completude),
                    if (res$n_dup == 0) ", sans ligne en double." else if (res$n_dup == 1) ", avec une ligne en double." else paste0(", avec ", res$n_dup, " lignes en double."))
  stats_u <- lapply(res$uni, function(u) u$stat)
  num <- Filter(function(s) s$type == "numeric", stats_u)
  cats <- Filter(function(s) s$type == "categorical", stats_u)
  cv <- vapply(num, function(s) if (is.na(s$cv)) -1 else s$cv, numeric(1))
  if (length(num) > 1 && max(cv) >= 0) {
    phrases <- c(phrases, paste0("Parmi les variables numériques, ", names(which.max(cv)), " est la plus dispersée (CV = ", fmt_pct(max(cv)), ")."))
  }
  for (col in utils::head(names(cats), 3)) {
    top <- cats[[col]]$all_categories[1, ]
    phrases <- c(phrases, if (cats[[col]]$n_exaequo > 1) paste0("Les catégories de ", col, " les plus fréquentes sont à égalité (", fmt_pct(top$Pourcentage), " chacune).")
                 else paste0("Pour ", col, ", la catégorie la plus fréquente est ", top$Modalite, " (", fmt_pct(top$Pourcentage), ")."))
  }
  atyp <- names(Filter(function(s) s$n_outliers > 0, num))
  if (length(atyp) > 0) phrases <- c(phrases, paste0("Des valeurs atypiques sont présentes pour ", paste(atyp, collapse = ", "), "."))
  manq <- names(Filter(function(s) s$missing_pct > 20, stats_u))
  if (length(manq) > 0) phrases <- c(phrases, paste0("Plus de 20 % des valeurs manquent pour ", paste(manq, collapse = ", "), "."))
  if (length(res$bi) > 0) {
    sig <- Filter(function(r) !is.na(r$p) && r$p < 0.05, res$bi)
    phrases <- c(phrases, paste0("Au seuil de 5 %, ", length(sig), " croisement(s) sur ", length(res$bi), " présente(nt) un résultat significatif."))
    for (r in utils::head(Filter(function(r) r$kind == "nc", sig), 4)) {
      centre <- if (isTRUE(r$normal)) r$moyennes else r$medianes
      phrases <- c(phrases, paste0("Pour ", r$a, ", la ", if (isTRUE(r$normal)) "moyenne" else "médiane", " va de ", fmt_num(min(centre)), " (", names(which.min(centre)), ") à ", fmt_num(max(centre)), " (", names(which.max(centre)), ")."))
    }
  }
  list(synthese = paste(phrases, collapse = " "))
}
