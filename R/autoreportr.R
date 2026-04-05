# ============================================
# AUTOREPORTR - Reporting statistique automatique
# Version 1.0 - Package R
# Auteur : Stalec Alinkpesso
# ============================================

#' Check data structure
#'
#' @param data A data.frame
#' @return A list with data information
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

#' Detect variable types
#'
#' @param data A data.frame
#' @return A named list with variable types
#' @export
detect_types <- function(data) {
  types <- list()
  for (col in names(data)) {
    var <- data[[col]]
    if (is.numeric(var)) {
      n_unique <- length(unique(na.omit(var)))
      types[[col]] <- if (n_unique <= 10) "numerique discret" else "numerique continu"
    } else if (is.factor(var) || is.character(var)) {
      n_unique <- length(unique(na.omit(var)))
      types[[col]] <- if (n_unique <= 20) "categorielle" else "texte"
    } else if (is.logical(var)) {
      types[[col]] <- "logique"
    } else {
      types[[col]] <- "autre"
    }
  }
  return(types)
}

#' Analyze missing values
#'
#' @param data A data.frame
#' @return A list with missing data summary
#' @export
analyze_missing <- function(data) {
  missing_count <- sapply(data, function(x) sum(is.na(x)))
  missing_pct   <- missing_count / nrow(data) * 100
  
  missing_df <- data.frame(
    Variable        = names(missing_count),
    Nombre_manquant = missing_count,
    Pourcentage     = round(missing_pct, 2)
  )
  missing_df <- missing_df[missing_df$Nombre_manquant > 0, ]
  missing_df <- missing_df[order(-missing_df$Nombre_manquant), ]
  rownames(missing_df) <- NULL
  
  list(
    summary                = missing_df,
    total_missing          = sum(missing_count),
    variables_with_missing = sum(missing_count > 0)
  )
}

#' Compute descriptive statistics
#'
#' @param data A data.frame
#' @return A list with statistics for each variable
#' @export
compute_descriptive_stats <- function(data) {
  results <- list()
  
  for (col in names(data)) {
    var <- data[[col]]
    
    if (is.numeric(var)) {
      var_clean  <- var[!is.na(var)]
      n          <- length(var)
      n_complete <- length(var_clean)
      if (n_complete == 0) next
      
      quantiles <- quantile(var_clean, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
      
      results[[col]] <- list(
        type        = "numeric",
        n           = n,
        n_complete  = n_complete,
        missing     = n - n_complete,
        missing_pct = round((n - n_complete) / n * 100, 2),
        mean        = mean(var_clean),
        variance    = var(var_clean),
        sd          = sd(var_clean),
        min         = min(var_clean),
        q1          = quantiles[2],
        median      = quantiles[3],
        q3          = quantiles[4],
        max         = max(var_clean),
        iqr         = IQR(var_clean)
      )
      
    } else if (is.factor(var) || is.character(var)) {
      n          <- length(var)
      n_complete <- sum(!is.na(var))
      if (n_complete == 0) next
      
      freq_table <- sort(table(var, useNA = "no"), decreasing = TRUE)
      prop_table <- round(prop.table(freq_table) * 100, 2)
      n_levels   <- length(freq_table)
      
      all_cats <- data.frame(
        Modalite    = names(freq_table),
        Effectif    = as.numeric(freq_table),
        Pourcentage = as.numeric(prop_table),
        stringsAsFactors = FALSE
      )
      
      all_cats$Pourcentage_cumule <- cumsum(all_cats$Pourcentage)
      
      max_effectif      <- max(all_cats$Effectif)
      modalites_exaequo <- all_cats[all_cats$Effectif == max_effectif, ]
      
      results[[col]] <- list(
        type              = "categorical",
        n                 = n,
        n_complete        = n_complete,
        missing           = n - n_complete,
        missing_pct       = round((n - n_complete) / n * 100, 2),
        n_levels          = n_levels,
        all_categories    = all_cats,
        max_effectif      = max_effectif,
        modalites_exaequo = modalites_exaequo,
        n_exaequo         = nrow(modalites_exaequo)
      )
    }
  }
  
  return(results)
}

#' Generate CSS for report
#'
#' @return Character string with CSS
#' @keywords internal
report_css <- function() {
  "
<style>
  * {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
  }
  
  body {
    font-family: 'Segoe UI', Arial, sans-serif;
    line-height: 1.65;
    max-width: 1200px;
    margin: 0 auto;
    padding: 35px 20px;
    background-color: #f4f6f8;
    color: #2c3e50;
  }
  
  .header {
    background: white;
    border-radius: 8px;
    padding: 30px 35px;
    margin-bottom: 30px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.05);
  }
  
  h1 {
    font-size: 28px;
    font-weight: 700;
    color: #1a252f;
    border-bottom: 3px solid #2980b9;
    padding-bottom: 12px;
    margin-bottom: 8px;
  }
  
  .doc-meta {
    font-size: 13px;
    color: #7f8c8d;
    margin-top: 12px;
  }
  
  h2 {
    font-size: 20px;
    font-weight: 600;
    color: #2980b9;
    border-left: 4px solid #2980b9;
    padding-left: 12px;
    margin-top: 35px;
    margin-bottom: 18px;
  }
  
  h3 {
    font-size: 18px;
    font-weight: 600;
    color: #1a252f;
    background-color: #eef2f7;
    padding: 10px 16px;
    border-radius: 6px;
    margin-top: 25px;
    margin-bottom: 15px;
  }
  
  .card {
    background: white;
    border-radius: 8px;
    padding: 20px 25px;
    margin: 15px 0;
    box-shadow: 0 1px 4px rgba(0,0,0,0.07);
    transition: all 0.3s ease;
  }
  
  .card:hover {
    transform: translateY(-2px);
    box-shadow: 0 6px 16px rgba(0,0,0,0.1);
  }
  
  .card.blue {
    border-left: 4px solid #2980b9;
  }
  
  .stats-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(130px, 1fr));
    gap: 15px;
    margin: 15px 0;
  }
  
  .stat-box {
    background: #f8f9fa;
    border: 1px solid #e8e8e8;
    border-radius: 8px;
    padding: 14px 10px;
    text-align: center;
    transition: all 0.3s ease;
    cursor: pointer;
  }
  
  .stat-box:hover {
    transform: translateY(-3px);
    border-color: #2980b9;
    box-shadow: 0 6px 14px rgba(41,128,185,0.15);
    background: white;
  }
  
  .stat-val {
    font-size: 22px;
    font-weight: 700;
    color: #2980b9;
    line-height: 1.2;
    margin-bottom: 6px;
  }
  
  .stat-lbl {
    font-size: 11px;
    color: #7f8c8d;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }
  
  table {
    border-collapse: collapse;
    width: 100%;
    margin: 15px 0;
    background: white;
    border-radius: 8px;
    overflow: hidden;
    box-shadow: 0 1px 3px rgba(0,0,0,0.07);
    font-size: 13px;
  }
  
  th {
    background-color: #2980b9;
    color: white;
    padding: 12px 16px;
    text-align: left;
    font-weight: 600;
  }
  
  td {
    padding: 10px 16px;
    border-bottom: 1px solid #f0f0f0;
  }
  
  tr:hover {
    background-color: #f8fafc;
  }
  
  .badge {
    display: inline-block;
    font-size: 10px;
    font-weight: 700;
    padding: 3px 10px;
    border-radius: 20px;
    margin-left: 10px;
    vertical-align: middle;
  }
  
  .badge-num {
    background: #2980b9;
    color: white;
  }
  
  .badge-cat {
    background: #e67e22;
    color: white;
  }
  
  .mode-tag {
    display: inline-block;
    background: #2980b9;
    color: white;
    padding: 2px 8px;
    border-radius: 12px;
    font-size: 9px;
    font-weight: 600;
    margin-left: 8px;
  }
  
  .exaequo-tag {
    display: inline-block;
    background: #e67e22;
    color: white;
    padding: 2px 8px;
    border-radius: 12px;
    font-size: 9px;
    font-weight: 600;
    margin-left: 8px;
  }
  
  .interp {
    background: #eef2fa;
    border-left: 4px solid #2980b9;
    padding: 16px 20px;
    border-radius: 6px;
    margin-top: 15px;
    transition: all 0.2s ease;
  }
  
  .interp:hover {
    background: #e8f0f8;
  }
  
  .interp-title {
    font-weight: 700;
    font-size: 11px;
    color: #2980b9;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    margin-bottom: 8px;
  }
  
  .note {
    font-size: 11px;
    color: #95a5a6;
    font-style: italic;
    margin-top: 10px;
    padding: 6px 0;
  }
  
  hr {
    margin: 25px 0;
    border: none;
    height: 1px;
    background: #e8e8e8;
  }
  
  .footer {
    text-align: center;
    font-size: 11px;
    color: #95a5a6;
    margin-top: 50px;
    padding-top: 20px;
    border-top: 1px solid #e8e8e8;
  }
  
  .overflow-x {
    overflow-x: auto;
  }
  
  @media (max-width: 768px) {
    body {
      padding: 20px 15px;
    }
    .stats-grid {
      grid-template-columns: repeat(auto-fit, minmax(100px, 1fr));
      gap: 10px;
    }
    .stat-val {
      font-size: 18px;
    }
    h1 {
      font-size: 22px;
    }
    h2 {
      font-size: 18px;
    }
    h3 {
      font-size: 16px;
    }
  }
</style>
"
}

#' Generate HTML report
#'
#' @param data Data frame
#' @param data_check Result from check_data()
#' @param var_types Result from detect_types()
#' @param missing_analysis Result from analyze_missing()
#' @param descriptive_stats Result from compute_descriptive_stats()
#' @param title Report title
#' @param author Report author
#' @return Filename of generated report
#' @keywords internal
generate_report <- function(data, data_check, var_types, missing_analysis,
                            descriptive_stats, title, author) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename  <- paste0("rapport_analyse_", timestamp, ".html")
  
  creation_date <- format(Sys.Date(), "%d %B %Y")
  
  total_cells <- data_check$n_obs * data_check$n_vars
  missing_percent <- if(missing_analysis$total_missing > 0) {
    round(missing_analysis$total_missing / total_cells * 100, 1)
  } else {
    0
  }
  
  completeness_rate <- 100 - missing_percent
  
  sbox <- function(value, label) {
    paste0("<div class='stat-box'><div class='stat-val'>", value,
           "</div><div class='stat-lbl'>", label, "</div></div>")
  }
  
  html <- c(
    "<!DOCTYPE html>",
    "<html lang='fr'>",
    "<head>",
    "<meta charset='UTF-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1.0'>",
    paste0("<title>", title, "</title>"),
    report_css(),
    "</head>",
    "<body>",
    
    "<div class='header'>",
    "<h1>", title, "</h1>",
    "<div class='doc-meta'>",
    "Auteur : <strong>", author, "</strong> | ",
    "Date : <strong>", creation_date, "</strong> | ",
    "AutoReportR v1.0",
    "</div>",
    "</div>"
  )
  
  # Section 1: Présentation des données
  html <- c(html,
            "<h2>1. Présentation des données</h2>",
            "<div class='card blue'>",
            "<div class='stats-grid'>",
            sbox(data_check$n_obs, "Observations"),
            sbox(data_check$n_vars, "Variables"),
            sbox(paste0(completeness_rate, "%"), "Complétude"),
            sbox(ifelse(data_check$has_missing, "Oui", "Non"), "Valeurs manquantes"),
            "</div>",
            "<div style='margin-top: 10px;'>",
            "<strong>Résumé :</strong> L'analyse porte sur ", data_check$n_obs, " observations et ", 
            data_check$n_vars, " variables. ",
            if(data_check$n_duplicated > 0) {
              paste0(data_check$n_duplicated, " lignes en double détectées. ")
            } else {
              "Aucune ligne en double détectée. "
            },
            if(data_check$has_missing) {
              paste0("Des valeurs manquantes sont présentes (taux de complétude : ", completeness_rate, "%).")
            } else {
              "Les données sont complètes."
            },
            "</div>",
            "<div class='note'>Variables analysées : ", paste(data_check$var_names, collapse = ", "), "</div>",
            "</div>"
  )
  
  # Section 2: Types de variables
  html <- c(html,
            "<h2>2. Types de variables</h2>",
            "<div class='card'>",
            "<div class='overflow-x'>",
            "<table>",
            "<thead>",
            "<tr><th>Type</th><th>Nombre</th><th>Variables</th></tr>",
            "</thead>",
            "<tbody>"
  )
  
  type_summary <- table(unlist(var_types))
  for (type in names(type_summary)) {
    vars_of_type <- names(var_types)[unlist(var_types) == type]
    vars_list <- paste(vars_of_type, collapse = ", ")
    html <- c(html, paste0(
      "<tr>",
      "<td><strong>", type, "</strong></td>",
      "<td>", type_summary[type], "</td>",
      "<td>", vars_list, "</td>",
      "</tr>"
    ))
  }
  
  html <- c(html,
            "</tbody>",
            "</table>",
            "</div>",
            "</div>"
  )
  
  # Section 3: Qualité des données
  if (missing_analysis$total_missing > 0) {
    html <- c(html,
              "<h2>3. Analyse des valeurs manquantes</h2>",
              "<div class='card'>",
              "<div class='stats-grid'>",
              sbox(missing_analysis$total_missing, "Manquantes totales"),
              sbox(missing_analysis$variables_with_missing, "Variables concernées"),
              sbox(paste0(missing_percent, "%"), "Taux de manquantes"),
              "</div>",
              "<div class='overflow-x'>",
              "<table>",
              "<thead><tr><th>Variable</th><th>Nombre manquant</th><th>Pourcentage</th></tr></thead>",
              "<tbody>"
    )
    
    for (i in seq_len(nrow(missing_analysis$summary))) {
      pct <- missing_analysis$summary$Pourcentage[i]
      html <- c(html, paste0(
        "<tr>",
        "<td><strong>", missing_analysis$summary$Variable[i], "</strong></td>",
        "<td>", missing_analysis$summary$Nombre_manquant[i], "</td>",
        "<td>", pct, "%</td>",
        "</tr>"
      ))
    }
    
    html <- c(html,
              "</tbody>",
              "</table>",
              "</div>",
              "<div class='interp'>",
              "<div class='interp-title'>Interprétation</div>",
              "Le taux global de données manquantes est de <strong>", missing_percent, "%</strong>. ",
              if(missing_percent > 20) {
                "Un taux élevé de valeurs manquantes a été détecté. Il est recommandé d'investiguer les causes."
              } else if(missing_percent > 5) {
                "Un taux modéré de valeurs manquantes a été détecté. Une attention particulière est recommandée."
              } else {
                "Le taux de valeurs manquantes est faible et ne devrait pas impacter significativement les analyses."
              },
              "</div>",
              "</div>"
    )
  }
  
  # Section 4: Analyse univariée détaillée
  html <- c(html,
            "<h2>4. Analyse univariée détaillée</h2>"
  )
  
  for (col in names(descriptive_stats)) {
    stat <- descriptive_stats[[col]]
    badge_cl <- if (stat$type == "numeric") "badge-num" else "badge-cat"
    badge_tx <- if (stat$type == "numeric") "NUMERIQUE" else "CATEGORIELLE"
    
    html <- c(html,
              "<h3>", col, " <span class='badge ", badge_cl, "'>", badge_tx, "</span></h3>",
              "<div class='card'>"
    )
    
    if (stat$type == "numeric" && stat$n_complete > 0) {
      html <- c(html,
                "<div class='stats-grid'>",
                sbox(stat$n_complete, "Effectif"),
                sbox(round(stat$mean, 2), "Moyenne"),
                sbox(round(stat$median, 2), "Médiane"),
                sbox(round(stat$sd, 2), "Écart-type"),
                sbox(round(stat$min, 2), "Minimum"),
                sbox(round(stat$max, 2), "Maximum"),
                "</div>"
      )
      
      if (stat$missing > 0) {
        html <- c(html, paste0("<div class='note'>", stat$missing,
                               " valeur(s) manquante(s) exclue(s) (", stat$missing_pct, "%).</div>"))
      }
      
      cv <- stat$sd / abs(stat$mean) * 100
      dispersion <- if (cv < 15) "faible" else if (cv < 30) "modérée" else "élevée"
      
      html <- c(html,
                "<div class='interp'>",
                "<div class='interp-title'>Interprétation</div>",
                "<p><strong>", col, "</strong> présente une valeur moyenne de <strong>", 
                round(stat$mean, 2), "</strong> avec une dispersion <strong>", dispersion, 
                "</strong> (écart-type = ", round(stat$sd, 2), ").</p>",
                "<p>Les valeurs s'étendent de <strong>", round(stat$min, 2), "</strong> à <strong>", 
                round(stat$max, 2), "</strong>, avec une médiane de <strong>", round(stat$median, 2), "</strong>.</p>",
                "</div>"
      )
      
    } else if (stat$type == "categorical" && stat$n_complete > 0) {
      all_cats <- stat$all_categories
      
      html <- c(html,
                "<div class='stats-grid'>",
                sbox(stat$n_complete, "Effectif total"),
                sbox(stat$n_levels, "Catégories"),
                "</div>"
      )
      
      html <- c(html,
                "<div class='overflow-x'>",
                "<table>",
                "<thead><tr><th>Catégorie</th><th>Effectif</th><th>Pourcentage</th><th>% Cumulé</th></tr></thead>",
                "<tbody>"
      )
      
      for (i in seq_len(nrow(all_cats))) {
        is_mode <- all_cats$Effectif[i] == stat$max_effectif
        is_exaequo <- is_mode && stat$n_exaequo > 1
        tag <- if (is_exaequo) "<span class='exaequo-tag'>EX AEQUO</span>"
        else if (is_mode) "<span class='mode-tag'>MODE</span>" else ""
        
        html <- c(html, paste0(
          "<tr>",
          "<td><strong>", all_cats$Modalite[i], "</strong>", tag, "</td>",
          "<td>", all_cats$Effectif[i], "</td>",
          "<td>", all_cats$Pourcentage[i], "%</td>",
          "<td>", all_cats$Pourcentage_cumule[i], "%</td>",
          "</tr>"
        ))
      }
      
      html <- c(html,
                "</tbody>",
                "</table>",
                "</div>"
      )
      
      if (stat$missing > 0) {
        html <- c(html, paste0("<div class='note'>", stat$missing,
                               " valeur(s) manquante(s) exclue(s) (", stat$missing_pct, "%).</div>"))
      }
      
      html <- c(html,
                "<div class='interp'>",
                "<div class='interp-title'>Interprétation</div>"
      )
      
      if (stat$n_exaequo > 1) {
        mods <- paste(stat$modalites_exaequo$Modalite, collapse = " et ")
        html <- c(html, paste0(
          "<p>Les catégories <strong>", mods, "</strong> sont les plus fréquentes avec <strong>",
          stat$max_effectif, "</strong> observations chacune (", 
          stat$modalites_exaequo$Pourcentage[1], "% du total).</p>"
        ))
      } else {
        top <- all_cats[1, ]
        html <- c(html, paste0(
          "<p>La catégorie <strong>", top$Modalite, "</strong> est la plus représentée avec <strong>",
          top$Effectif, " observations</strong> (", top$Pourcentage, "% du total).</p>"
        ))
      }
      
      html <- c(html, "</div>")
    }
    
    html <- c(html, "</div>", "<hr>")
  }
  
  # Footer
  html <- c(html,
            "<div class='footer'>",
            "Rapport généré par AutoReportR v1.0 | ", creation_date,
            "</div>",
            "</body>",
            "</html>"
  )
  
  writeLines(html, filename, useBytes = FALSE)
  return(filename)
}

#' AutoReportR: Generate Automatic Statistical Report
#'
#' This function generates an automatic HTML statistical report with 
#' interactive cards, univariate statistics, missing data analysis, 
#' and data quality indicators.
#'
#' @param data A data.frame containing the data to analyze
#' @param vars Optional character vector of variable names to select
#' @param title Optional title for the report
#' @param author Author name (defaults to system username)
#' @param open_report Logical, whether to open the report after generation
#'
#' @return Invisibly returns the path to the generated HTML file
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' auto_report(mtcars)
#'
#' # With custom title and variable selection
#' auto_report(mtcars, vars = c("mpg", "cyl", "hp"), 
#'             title = "Analyse des performances auto")
#' }
#'
#' @export
auto_report <- function(data,
                        vars        = NULL,
                        title       = NULL,
                        author      = Sys.getenv("USERNAME"),
                        open_report = TRUE) {
  
  if (!is.data.frame(data)) stop("L'argument 'data' doit être un data.frame.")
  
  if (!is.null(vars)) {
    absents <- setdiff(vars, names(data))
    if (length(absents) > 0)
      stop(paste("Variables introuvables :", paste(absents, collapse = ", ")))
    data <- data[, vars, drop = FALSE]
    cat("Sélection de", length(vars), "variable(s).\n")
  }
  
  if (is.null(title)) {
    title <- "Rapport d'analyse statistique"
  }
  
  sep <- paste(rep("=", 60), collapse = "")
  cat("\n", sep, "\n")
  cat("  AutoReportR — Génération du rapport\n")
  cat(sep, "\n\n")
  
  cat("[1/4] Vérification des données...\n")
  data_check <- check_data(data)
  cat("     ->", data_check$n_obs, "observations,", data_check$n_vars, "variables\n")
  
  cat("[2/4] Détection des types...\n")
  var_types    <- detect_types(data)
  type_summary <- table(unlist(var_types))
  for (t in names(type_summary))
    cat("     ->", type_summary[t], "variable(s) de type :", t, "\n")
  
  cat("[3/4] Analyse des valeurs manquantes...\n")
  missing_analysis <- analyze_missing(data)
  if (missing_analysis$total_missing == 0)
    cat("     -> Aucune valeur manquante\n")
  else
    cat("     ->", missing_analysis$total_missing, "valeurs manquantes détectées\n")
  
  cat("[4/4] Calcul des statistiques univariées...\n")
  descriptive_stats <- compute_descriptive_stats(data)
  cat("     ->", length(descriptive_stats), "variable(s) analysée(s)\n")
  
  cat("Génération du rapport...\n")
  report_file <- generate_report(data, data_check, var_types, missing_analysis,
                                 descriptive_stats, title, author)
  
  cat("\n", sep, "\n")
  cat("  Rapport généré :", normalizePath(report_file), "\n")
  
  if (open_report && interactive()) {
    cat("  Ouverture du rapport...\n")
    browseURL(report_file)
  }
  cat(sep, "\n")
  
  return(invisible(report_file))
}