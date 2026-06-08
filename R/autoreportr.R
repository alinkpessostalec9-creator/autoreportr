# ============================================
# AUTOREPORTR - Reporting statistique automatique
# Version 2.0 - Package R
# Auteur : Stalec Alinkpesso
# ============================================

# 1. CHARGEMENT DES PACKAGES
# ============================================
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")

library(httr)
library(jsonlite)

# 2. CONFIGURATION API
# ============================================
api_key <- "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"

# 3. FONCTIONS D'ANALYSE
# ============================================

check_data <- function(data) {
  list(
    n_obs = nrow(data),
    n_vars = ncol(data),
    var_names = names(data),
    n_duplicated = sum(duplicated(data)),
    has_missing = any(is.na(data))
  )
}

detect_types <- function(data) {
  types <- list()
  for (col in names(data)) {
    var <- data[[col]]
    var_clean <- na.omit(var)
    if (length(var_clean) == 0) {
      types[[col]] <- "vide"
      next
    }
    n_unique <- length(unique(var_clean))
    if (is.numeric(var)) {
      types[[col]] <- if (n_unique <= 10) "numerique discret" else "numerique continu"
    } else if (is.factor(var) || is.character(var)) {
      types[[col]] <- "categorielle"
    } else if (is.logical(var)) {
      types[[col]] <- "logique"
    } else {
      types[[col]] <- "autre"
    }
  }
  return(types)
}

analyze_missing <- function(data) {
  missing_count <- sapply(data, function(x) sum(is.na(x)))
  missing_pct <- missing_count / nrow(data) * 100
  missing_df <- data.frame(
    Variable = names(missing_count),
    Nombre_manquant = missing_count,
    Pourcentage = round(missing_pct, 2)
  )
  missing_df <- missing_df[missing_df$Nombre_manquant > 0, ]
  missing_df <- missing_df[order(-missing_df$Nombre_manquant), ]
  rownames(missing_df) <- NULL
  list(
    summary = missing_df,
    total_missing = sum(missing_count),
    variables_with_missing = sum(missing_count > 0)
  )
}

compute_descriptive_stats <- function(data) {
  results <- list()
  for (col in names(data)) {
    var <- data[[col]]
    
    if (is.numeric(var)) {
      var_clean <- var[!is.na(var)]
      n <- length(var)
      n_complete <- length(var_clean)
      if (n_complete == 0) next
      quantiles <- quantile(var_clean, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
      results[[col]] <- list(
        type = "numeric", n = n, n_complete = n_complete,
        missing = n - n_complete, missing_pct = round((n - n_complete) / n * 100, 2),
        mean = mean(var_clean), variance = var(var_clean), sd = sd(var_clean),
        min = min(var_clean), q1 = quantiles[2], median = quantiles[3],
        q3 = quantiles[4], max = max(var_clean), iqr = IQR(var_clean)
      )
    } else if (is.factor(var) || is.character(var)) {
      n <- length(var)
      n_complete <- sum(!is.na(var))
      if (n_complete == 0) next
      
      freq_table <- sort(table(var, useNA = "no"), decreasing = TRUE)
      prop_table <- round(prop.table(freq_table) * 100, 2)
      n_levels <- length(freq_table)
      all_cats <- data.frame(
        Modalite = names(freq_table),
        Effectif = as.numeric(freq_table),
        Pourcentage = as.numeric(prop_table),
        stringsAsFactors = FALSE
      )
      all_cats$Pourcentage_cumule <- cumsum(all_cats$Pourcentage)
      max_effectif <- max(all_cats$Effectif)
      modalites_exaequo <- all_cats[all_cats$Effectif == max_effectif, ]
      
      results[[col]] <- list(
        type = "categorical", n = n, n_complete = n_complete,
        missing = n - n_complete, missing_pct = round((n - n_complete) / n * 100, 2),
        n_levels = n_levels, all_categories = all_cats,
        max_effectif = max_effectif, n_exaequo = nrow(modalites_exaequo),
        modalites_exaequo = modalites_exaequo
      )
    }
  }
  return(results)
}

# 4. FONCTION D'INTERPRETATION
# ============================================

interpret_with_api <- function(var_name, stats) {
  
  if (stats$type == "numeric") {
    prompt <- paste0(
      "Interprete ces resultats pour la variable '", var_name, 
      "' en 2 phrases: Effectif=", stats$n_complete, 
      ", Moyenne=", round(stats$mean, 2), ", Mediane=", round(stats$median, 2), 
      ", Ecart-type=", round(stats$sd, 2), ", Min=", round(stats$min, 2), 
      ", Max=", round(stats$max, 2)
    )
  } else {
    if (stats$n_exaequo > 1) {
      exaequo_liste <- paste(stats$modalites_exaequo$Modalite, collapse = ", ")
      prompt <- paste0(
        "Interprete ces resultats pour la variable categorielle '", var_name, 
        "' en 2 phrases: Effectif total=", stats$n_complete, 
        ", Nombre de categories=", stats$n_levels, 
        ". ATTENTION: Il y a ", stats$n_exaequo, " modes ex aequo: ", exaequo_liste, 
        " (chacun avec ", stats$max_effectif, " observations)."
      )
    } else {
      top <- stats$all_categories[1, ]
      prompt <- paste0(
        "Interprete ces resultats pour la variable categorielle '", var_name, 
        "' en 2 phrases: Categorie dominante='", top$Modalite, 
        "' (", top$Effectif, " observations, ", top$Pourcentage, "%), ",
        "Nombre total de categories=", stats$n_levels
      )
    }
  }
  
  response <- tryCatch({
    POST(
      url = "https://api.groq.com/openai/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type_json(),
      encode = "json",
      body = list(
        model = "llama-3.3-70b-versatile",
        messages = list(
          list(role = "system", content = "Tu es un expert en statistique. Reponds en francais, court et clair."),
          list(role = "user", content = prompt)
        ),
        temperature = 0.7,
        max_tokens = 200
      ),
      simplifyVector = FALSE
    )
  }, error = function(e) return(NULL))
  
  if (is.null(response)) return("")
  if (response$status_code != 200) return("")
  
  result <- fromJSON(rawToChar(response$content), simplifyVector = FALSE)
  if (!is.null(result$choices) && length(result$choices) > 0) {
    return(result$choices[[1]]$message$content)
  }
  
  return("")
}

# 5. FONCTION DE SYNTHESE
# ============================================

generate_synthesis <- function(descriptive_stats) {
  if (length(descriptive_stats) == 0) {
    return("Aucune variable n'a pu être analysée. Vérifiez vos données.")
  }
  
  points <- c()
  
  for (col in names(descriptive_stats)) {
    stat <- descriptive_stats[[col]]
    
    if (stat$type == "numeric") {
      if (stat$missing_pct > 10) {
        points <- c(points, paste0("La variable '", col, "' contient ", stat$missing_pct, "% de valeurs manquantes."))
      }
      cv <- abs(stat$sd / stat$mean)
      if (cv > 0.5 && !is.na(cv) && is.finite(cv)) {
        points <- c(points, paste0("La variable '", col, "' présente une forte dispersion (coefficient de variation = ", round(cv, 2), ")."))
      }
      if (stat$mean > stat$median * 1.2) {
        points <- c(points, paste0("La variable '", col, "' est asymétrique à droite (quelques valeurs élevées)."))
      }
      if (stat$mean < stat$median * 0.8) {
        points <- c(points, paste0("La variable '", col, "' est asymétrique à gauche (quelques valeurs basses)."))
      }
    } else if (stat$type == "categorical") {
      if (stat$missing_pct > 10) {
        points <- c(points, paste0("La variable '", col, "' a ", stat$missing_pct, "% de non-réponses."))
      }
      top_pct <- stat$all_categories$Pourcentage[1]
      if (top_pct > 80) {
        points <- c(points, paste0("La variable '", col, "' est dominée par la catégorie '", 
                                   stat$all_categories$Modalite[1], "' (", top_pct, "% des observations)."))
      }
      if (stat$n_exaequo > 1) {
        points <- c(points, paste0("La variable '", col, "' a ", stat$n_exaequo, 
                                   " catégories ex aequo comme mode."))
      }
      if (stat$n_levels > 15 && top_pct < 20) {
        points <- c(points, paste0("La variable '", col, "' est très fragmentée (", 
                                   stat$n_levels, " catégories différentes)."))
      }
    }
  }
  
  if (length(points) == 0) {
    return("Les données sont de bonne qualité. Les distributions sont régulières et ne présentent pas d'anomalies majeures.")
  }
  
  return(paste(points, collapse = " "))
}

# 6. CSS STYLE
# ============================================

report_css <- function() {
  "
<style>
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body {
    font-family: 'Times New Roman', Times, serif;
    line-height: 1.6;
    max-width: 1200px;
    margin: 0 auto;
    padding: 35px 20px;
    background-color: #f5f5f0;
    color: #2c3e50;
  }
  .header {
    background: white;
    border-radius: 8px;
    padding: 30px 35px;
    margin-bottom: 30px;
    box-shadow: 0 2px 10px rgba(0,0,0,0.08);
    border-bottom: 3px solid #2980b9;
    transition: all 0.3s ease;
  }
  .header:hover {
    box-shadow: 0 4px 20px rgba(0,0,0,0.12);
  }
  h1 {
    font-size: 28px;
    font-weight: 700;
    font-family: 'Times New Roman', Times, serif;
    color: #1a252f;
    border-bottom: 2px solid #2980b9;
    padding-bottom: 12px;
    margin-bottom: 8px;
  }
  .doc-meta {
    font-size: 13px;
    color: #7f8c8d;
    margin-top: 12px;
  }
  .version-badge {
    display: inline-block;
    background: #2980b9;
    color: white;
    font-size: 10px;
    padding: 2px 10px;
    border-radius: 20px;
    margin-left: 10px;
    vertical-align: middle;
  }
  h2 {
    font-size: 20px;
    font-weight: 600;
    font-family: 'Times New Roman', Times, serif;
    color: #2980b9;
    border-left: 4px solid #2980b9;
    padding-left: 12px;
    margin-top: 35px;
    margin-bottom: 18px;
    transition: all 0.3s ease;
  }
  h2:hover {
    padding-left: 20px;
  }
  h3 {
    font-size: 18px;
    font-weight: 600;
    font-family: 'Times New Roman', Times, serif;
    color: #1a252f;
    background-color: #eef2f7;
    padding: 10px 16px;
    border-radius: 6px;
    margin-top: 25px;
    margin-bottom: 15px;
    transition: all 0.3s ease;
  }
  h3:hover {
    background-color: #e0e8f0;
    transform: translateX(5px);
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
    transform: translateY(-3px);
    box-shadow: 0 6px 16px rgba(0,0,0,0.12);
  }
  .card.blue { border-left: 4px solid #2980b9; }
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
    transform: translateY(-5px);
    border-color: #2980b9;
    box-shadow: 0 8px 20px rgba(41,128,185,0.15);
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
    font-size: 13px;
    font-family: 'Times New Roman', Times, serif;
    border: 1px solid #ddd;
    border-radius: 8px;
    overflow: hidden;
  }
  th {
    background-color: #2980b9;
    color: white;
    padding: 12px 15px;
    text-align: left;
    font-weight: 600;
  }
  td {
    padding: 10px 15px;
    border-bottom: 1px solid #eee;
  }
  tr:hover td {
    background-color: #f1f5f9;
  }
  tr:last-child td {
    border-bottom: none;
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
  .badge-num { background: #2980b9; color: white; }
  .badge-cat { background: #e67e22; color: white; }
  .mode-tag, .exaequo-tag {
    display: inline-block;
    font-size: 9px;
    font-weight: 600;
    padding: 2px 8px;
    border-radius: 12px;
    margin-left: 8px;
  }
  .mode-tag { background: #2980b9; color: white; }
  .exaequo-tag { background: #e67e22; color: white; }
  .interp {
    background: #f0f7ff;
    border-left: 4px solid #2980b9;
    padding: 16px 20px;
    border-radius: 6px;
    margin-top: 15px;
    transition: all 0.3s ease;
  }
  .interp:hover {
    background: #e5f0ff;
    transform: translateX(5px);
  }
  .interp-title {
    font-weight: 700;
    font-size: 11px;
    color: #2980b9;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    margin-bottom: 8px;
  }
  .synthesis {
    background: #e8f5e9;
    border-left: 4px solid #27ae60;
    padding: 20px 25px;
    margin: 20px 0;
    border-radius: 8px;
    transition: all 0.3s ease;
  }
  .synthesis:hover {
    background: #dff0e0;
    transform: translateX(5px);
  }
  .synthesis h3 {
    background: none;
    color: #27ae60;
    padding: 0 0 10px 0;
    margin-top: 0;
  }
  .note {
    font-size: 11px;
    color: #95a5a6;
    font-style: italic;
    margin-top: 10px;
  }
  hr {
    margin: 25px 0;
    border: none;
    height: 1px;
    background: #ddd;
  }
  .footer {
    text-align: center;
    font-size: 11px;
    color: #95a5a6;
    margin-top: 50px;
    padding-top: 20px;
    border-top: 1px solid #ddd;
  }
  .overflow-x { overflow-x: auto; }
  @media (max-width: 768px) {
    body { padding: 20px 15px; }
    .stats-grid { grid-template-columns: repeat(auto-fit, minmax(100px, 1fr)); }
    .stat-val { font-size: 18px; }
    h1 { font-size: 22px; }
    h2 { font-size: 18px; }
    h3 { font-size: 16px; }
    th, td { padding: 8px 10px; }
  }
</style>
"
}

# 7. GENERATION DU RAPPORT
# ============================================

generate_report <- function(data, data_check, var_types, missing_analysis,
                            descriptive_stats, title, author) {
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0("rapport_analyse_", timestamp, ".html")
  creation_date <- format(Sys.Date(), "%d %B %Y")
  
  total_cells <- data_check$n_obs * data_check$n_vars
  missing_percent <- if(missing_analysis$total_missing > 0) {
    round(missing_analysis$total_missing / total_cells * 100, 1)
  } else { 0 }
  completeness_rate <- 100 - missing_percent
  
  sbox <- function(value, label) {
    paste0("<div class='stat-box'><div class='stat-val'>", value,
           "</div><div class='stat-lbl'>", label, "</div></div>")
  }
  
  html <- c(
    "<!DOCTYPE html><html lang='fr'><head><meta charset='UTF-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1.0'>",
    paste0("<title>", title, " - AutoReportR v2.0</title>"),
    report_css(), "</head><body>",
    "<div class='header'>",
    "<h1>", title, "<span class='version-badge'>v2.0</span></h1>",
    "<div class='doc-meta'>Auteur : <strong>", author, "</strong> | Date : <strong>",
    creation_date, "</strong> | AutoReportR</div></div>"
  )
  
  # Section 1: Présentation
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
            if(data_check$n_duplicated > 0) paste0(data_check$n_duplicated, " lignes en double détectées. "),
            if(data_check$has_missing) paste0("Taux de complétude : ", completeness_rate, "%."),
            "</div></div>"
  )
  
  # Section 2: Types de variables
  html <- c(html,
            "<h2>2. Types de variables</h2>",
            "<div class='card'>",
            "<div class='overflow-x'>",
            "<table>",
            "<thead><tr><th>Type</th><th>Nombre</th><th>Variables</th></tr></thead><tbody>"
  )
  
  type_summary <- table(unlist(var_types))
  for (type in names(type_summary)) {
    vars_of_type <- names(var_types)[unlist(var_types) == type]
    html <- c(html, paste0(
      "<tr>",
      "<td><strong>", type, "</strong></td>",
      "<td>", type_summary[type], "</td>",
      "<td>", paste(vars_of_type, collapse = ", "), "</td>",
      "</tr>"
    ))
  }
  html <- c(html, "</tbody></table></div></div>")
  
  # Section 3: Analyse des valeurs manquantes
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
              "<thead><tr><th>Variable</th><th>Nombre manquant</th><th>Pourcentage</th></tr></thead><tbody>"
    )
    for (i in seq_len(nrow(missing_analysis$summary))) {
      html <- c(html, paste0(
        "<tr>",
        "<td>", missing_analysis$summary$Variable[i], "</td>",
        "<td>", missing_analysis$summary$Nombre_manquant[i], "</td>",
        "<td>", missing_analysis$summary$Pourcentage[i], "%</td>",
        "</tr>"
      ))
    }
    html <- c(html, "</tbody></table></div></div>")
  }
  
  # Section 4: Analyse détaillée
  html <- c(html, "<h2>4. Analyse détaillée</h2>")
  cat("Génération des interprétations...\n")
  
  for (col in names(descriptive_stats)) {
    stat <- descriptive_stats[[col]]
    
    if (stat$type == "numeric") {
      badge_txt <- "NUMERIQUE"
      cat("  ->", col, "(numérique)\n")
    } else {
      badge_txt <- "CATEGORIELLE"
      cat("  ->", col, "(catégorielle - ", stat$n_levels, " modalités)\n")
    }
    
    interpretation <- interpret_with_api(col, stat)
    Sys.sleep(0.3)
    
    html <- c(html,
              "<h3>", col, " <span class='badge badge-", if(stat$type=="numeric") "num" else "cat", "'>", badge_txt, "</span></h3>",
              "<div class='card'>"
    )
    
    if (stat$type == "numeric") {
      html <- c(html,
                "<div class='stats-grid'>",
                sbox(stat$n_complete, "Effectif"),
                sbox(round(stat$mean,2), "Moyenne"),
                sbox(round(stat$median,2), "Médiane"),
                sbox(round(stat$sd,2), "Ecart-type"),
                sbox(round(stat$min,2), "Minimum"),
                sbox(round(stat$max,2), "Maximum"),
                "</div>"
      )
    } else {
      html <- c(html,
                "<div class='stats-grid'>",
                sbox(stat$n_complete, "Effectif total"),
                sbox(stat$n_levels, "Catégories"),
                sbox(paste0(stat$missing_pct, "%"), "Manquantes"),
                "</div>",
                "<div class='overflow-x'>",
                "<table>",
                "<thead><tr><th>Catégorie</th><th>Effectif</th><th>Pourcentage</th><th>% Cumulé</th></tr></thead><tbody>"
      )
      
      for (i in seq_len(nrow(stat$all_categories))) {
        is_mode <- stat$all_categories$Effectif[i] == stat$max_effectif
        is_exaequo <- is_mode && stat$n_exaequo > 1
        tag <- if(is_exaequo) "<span class='exaequo-tag'>EX AEQUO</span>" else if(is_mode) "<span class='mode-tag'>MODE</span>" else ""
        
        html <- c(html, paste0(
          "<tr>",
          "<td>", stat$all_categories$Modalite[i], " ", tag, "</td>",
          "<td>", stat$all_categories$Effectif[i], "</td>",
          "<td>", stat$all_categories$Pourcentage[i], "%</td>",
          "<td>", stat$all_categories$Pourcentage_cumule[i], "%</td>",
          "<tr>"
        ))
      }
      html <- c(html, "</tbody></table></div>")
    }
    
    if (interpretation != "") {
      html <- c(html,
                "<div class='interp'>",
                "<div class='interp-title'>Interprétation</div>",
                "<p>", interpretation, "</p>",
                "</div>"
      )
    }
    html <- c(html, "</div><hr>")
  }
  
  # Section 5: Synthèse
  synth <- generate_synthesis(descriptive_stats)
  
  html <- c(html,
            "<h2>5. Synthèse et points clés</h2>",
            "<div class='synthesis'>",
            "<h3>Ce qu'il faut retenir</h3>",
            "<p>", synth, "</p>",
            "</div>",
            "<div class='footer'>Rapport généré par AutoReportR v2.0 | ", creation_date, "</div>",
            "</body></html>"
  )
  
  writeLines(html, filename, useBytes = FALSE)
  return(filename)
}

# 8. FONCTION PRINCIPALE
# ============================================

auto_report <- function(data, vars = NULL, title = NULL, 
                        author = Sys.getenv("USERNAME"), 
                        open_report = TRUE) {
  
  if (!is.data.frame(data)) stop("'data' doit être un data.frame")
  
  if (!is.null(vars)) {
    absents <- setdiff(vars, names(data))
    if (length(absents) > 0)
      stop(paste("Variables introuvables :", paste(absents, collapse = ", ")))
    data <- data[, vars, drop = FALSE]
    cat("Sélection de", length(vars), "variable(s)\n")
  }
  
  if (is.null(title)) title <- "Rapport d'analyse statistique"
  
  cat("\n============================================================\n")
  cat("  AutoReportR v2.0 - Génération du rapport\n")
  cat("============================================================\n\n")
  
  cat("[1/4] Vérification des données...\n")
  data_check <- check_data(data)
  cat("     ->", data_check$n_obs, "observations,", data_check$n_vars, "variables\n")
  
  cat("[2/4] Détection des types...\n")
  var_types <- detect_types(data)
  type_summary <- table(unlist(var_types))
  for (t in names(type_summary))
    cat("     ->", type_summary[t], "variable(s) de type :", t, "\n")
  
  cat("[3/4] Analyse des valeurs manquantes...\n")
  missing_analysis <- analyze_missing(data)
  if (missing_analysis$total_missing == 0)
    cat("     -> Aucune valeur manquante\n")
  else
    cat("     ->", missing_analysis$total_missing, "valeurs manquantes\n")
  
  cat("[4/4] Calcul des statistiques...\n")
  descriptive_stats <- compute_descriptive_stats(data)
  cat("     ->", length(descriptive_stats), "variable(s) analysée(s)\n")
  
  cat("Génération du rapport...\n")
  report_file <- generate_report(data, data_check, var_types, missing_analysis,
                                 descriptive_stats, title, author)
  
  cat("\n============================================================\n")
  cat("  Rapport généré :", normalizePath(report_file), "\n")
  
  if (open_report && interactive()) {
    cat("  Ouverture du rapport...\n")
    browseURL(report_file)
  }
  cat("============================================================\n")
  
  return(invisible(report_file))
}

# 9. MESSAGE DE BIENVENUE ET TEST
# ============================================
cat("\n✓ AutoReportR v2.0 chargé avec succès\n")
cat("  Utilisez auto_report(vos_donnees) pour générer votre rapport\n\n")
