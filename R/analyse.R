#' Vérifier la structure des données
#'
#' @param data Un data.frame.
#' @return Une liste : `n_obs`, `n_vars`, `var_names`, `n_duplicated`, `has_missing`.
#' @examples
#' check_data(iris)
#' @export
check_data <- function(data) {
  list(
    n_obs        = nrow(data),
    n_vars       = ncol(data),
    var_names    = names(data),
    n_duplicated = sum(duplicated(data)),
    has_missing  = any(is.na(data))
  )
}

#' Repérer les colonnes d'identifiants
#'
#' Une colonne est considérée comme un identifiant si ses valeurs sont toutes
#' différentes et si son nom ressemble à un identifiant (`id`, `code_patient`,
#' `matricule`...) ou si ses valeurs en ont la forme (texte unique, ou suite
#' d'entiers consécutifs à partir de 20 observations). Une variable numérique
#' avec décimales n'est donc pas exclue, sauf si son nom l'indique.
#'
#' @param data Un data.frame.
#' @return Un vecteur de noms de colonnes.
#' @examples
#' detect_id_vars(data.frame(id = 1:5, age = c(20, 31, 25, 40, 33)))
#' @export
detect_id_vars <- function(data) {
  mots <- "^(id|ids|ident|identifiant|identifier|uid|uuid|guid|code|matricule|numero|num|no|nro|cle|key|dossier|enregistrement)$"
  est_id <- vapply(names(data), function(col) {
    x <- data[[col]]
    if (!(is.numeric(x) || is.character(x) || is.factor(x))) return(FALSE)
    x <- x[!is.na(x)]
    n <- length(x)
    if (n < 5 || length(unique(x)) < n) return(FALSE)
    nom <- iconv(tolower(col), to = "ASCII//TRANSLIT")
    if (is.na(nom)) nom <- tolower(col)
    morceaux <- strsplit(gsub("([a-z])([A-Z])", "\\1_\\2", nom), "[^a-z0-9]+")[[1]]
    if (any(grepl(mots, morceaux))) return(TRUE)
    if (is.character(x) || is.factor(x)) return(TRUE)
    if (n >= 20 && all(x == floor(x)) && (max(x) - min(x) + 1) == n) return(TRUE)
    FALSE
  }, logical(1))
  names(data)[est_id]
}

#' Détecter le type des variables
#'
#' @param data Un data.frame.
#' @return Une liste nommée : `identifiant`, `numerique discret`,
#'   `numerique continu`, `categorielle`, `texte`, `logique` ou `autre`.
#' @examples
#' detect_types(iris)
#' @export
detect_types <- function(data) {
  ids <- detect_id_vars(data)
  types <- list()
  for (col in names(data)) {
    var <- data[[col]]
    n_unique <- length(unique(stats::na.omit(var)))
    types[[col]] <- if (col %in% ids) {
      "identifiant"
    } else if (is.numeric(var)) {
      if (n_unique <= 10) "numerique discret" else "numerique continu"
    } else if (is.factor(var) || is.character(var)) {
      if (n_unique <= 20) "categorielle" else "texte"
    } else if (is.logical(var)) {
      "logique"
    } else {
      "autre"
    }
  }
  types
}

#' Analyser les valeurs manquantes
#'
#' @param data Un data.frame.
#' @return Une liste : `summary`, `total_missing`, `variables_with_missing`.
#' @examples
#' analyze_missing(airquality)
#' @export
analyze_missing <- function(data) {
  missing_count <- vapply(data, function(x) sum(is.na(x)), numeric(1))
  missing_pct   <- if (nrow(data) > 0) missing_count / nrow(data) * 100 else missing_count * 0
  missing_df <- data.frame(
    Variable        = names(missing_count),
    Nombre_manquant = as.numeric(missing_count),
    Pourcentage     = round(as.numeric(missing_pct), 2),
    stringsAsFactors = FALSE
  )
  missing_df <- missing_df[missing_df$Nombre_manquant > 0, , drop = FALSE]
  missing_df <- missing_df[order(-missing_df$Nombre_manquant), , drop = FALSE]
  rownames(missing_df) <- NULL
  list(
    summary                = missing_df,
    total_missing          = sum(missing_count),
    variables_with_missing = sum(missing_count > 0)
  )
}

#' Calculer les statistiques descriptives
#'
#' Pour une variable numérique : tendance centrale, dispersion (variance,
#' écart-type, coefficient de variation, étendue, écart interquartile), forme
#' et valeurs atypiques. Pour une variable catégorielle : effectifs,
#' pourcentages et mode.
#'
#' @param data Un data.frame.
#' @return Une liste nommée avec une entrée par variable analysable.
#' @examples
#' compute_descriptive_stats(iris)$Species$all_categories
#' @export
compute_descriptive_stats <- function(data) {
  results <- list()
  for (col in names(data)) {
    var <- data[[col]]
    n <- length(var)
    if (is.numeric(var)) {
      var_clean <- var[is.finite(var)]
      n_complete <- length(var_clean)
      if (n_complete == 0) next
      q <- stats::quantile(var_clean, probs = c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
      moy <- mean(var_clean)
      et <- if (n_complete > 1) stats::sd(var_clean) else NA_real_
      iqr <- q[4] - q[2]
      bornes <- c(q[2] - 1.5 * iqr, q[4] + 1.5 * iqr)
      atypiques <- sum(var_clean < bornes[1] | var_clean > bornes[2])
      a_forme <- n_complete > 2 && !is.na(et) && et > 0
      results[[col]] <- list(
        type = "numeric", n = n, n_complete = n_complete,
        missing = n - n_complete,
        missing_pct = round((n - n_complete) / n * 100, 2),
        mean = moy,
        variance = if (n_complete > 1) stats::var(var_clean) else NA_real_,
        sd = et,
        cv = if (!is.na(et) && moy != 0) et / abs(moy) * 100 else NA_real_,
        min = q[1], q1 = q[2], median = q[3], q3 = q[4], max = q[5],
        range = q[5] - q[1], iqr = iqr,
        skewness = if (a_forme) e1071::skewness(var_clean) else NA_real_,
        kurtosis = if (a_forme) e1071::kurtosis(var_clean) else NA_real_,
        lower_fence = bornes[1], upper_fence = bornes[2],
        n_outliers = atypiques,
        n_unique = length(unique(var_clean))
      )
    } else if (is.factor(var) || is.character(var) || is.logical(var)) {
      var <- as.character(var)
      n_complete <- sum(!is.na(var))
      if (n_complete == 0) next
      freq <- sort(table(var, useNA = "no"), decreasing = TRUE)
      pct <- as.numeric(freq) / n_complete * 100
      all_cats <- data.frame(
        Modalite = names(freq), Effectif = as.numeric(freq),
        Pourcentage = round(pct, 2),
        Pourcentage_cumule = round(cumsum(pct), 2),
        stringsAsFactors = FALSE
      )
      max_eff <- max(all_cats$Effectif)
      ex <- all_cats[all_cats$Effectif == max_eff, , drop = FALSE]
      results[[col]] <- list(
        type = "categorical", n = n, n_complete = n_complete,
        missing = n - n_complete,
        missing_pct = round((n - n_complete) / n * 100, 2),
        n_levels = nrow(all_cats), all_categories = all_cats,
        max_effectif = max_eff, modalites_exaequo = ex, n_exaequo = nrow(ex)
      )
    }
  }
  results
}
