# Rendu HTML : conserve les cartes interactives et les couleurs bleues
report_css <- "
<style>
  * {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
  }
  
  body {
    font-family: 'Times New Roman', Times, serif;
    font-size: 16px;
    line-height: 1.65;
    max-width: 1200px;
    margin: 0 auto;
    padding: 35px 20px;
    background-color: #f4f4ef;
    color: #2c3e50;
  }
  
  /* En-tête */
  .header {
    background: white;
    border-radius: 8px;
    padding: 30px 35px;
    margin-bottom: 30px;
    border-bottom: 4px solid #2980b9;
    box-shadow: 0 2px 8px rgba(0,0,0,0.05);
  }
  
  h1 {
    font-size: 32px;
    font-weight: 700;
    color: #1a252f;
    border-bottom: 3px solid #2980b9;
    padding-bottom: 14px;
    margin-bottom: 8px;
  }
  
  .version { display: inline-block; background: #2980b9; color: white; font-size: 13px; font-weight: 700; padding: 3px 12px; border-radius: 20px; margin-left: 14px; vertical-align: middle; }
  .doc-meta {
    font-size: 15px;
    color: #7f8c8d;
    margin-top: 12px;
  }
  
  /* Sections */
  h2 {
    font-size: 24px;
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
  
  /* Cartes */
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
  
  /* Grille de statistiques avec cartes interactives */
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
    font-size: 13px;
    color: #7f8c8d;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }
  
  /* Tableaux */
  table {
    border-collapse: collapse;
    width: 100%;
    margin: 15px 0;
    background: white;
    border-radius: 8px;
    overflow: hidden;
    box-shadow: 0 1px 3px rgba(0,0,0,0.07);
    font-size: 15px;
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
  
  /* Badges */
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
  
  /* Cartes d'interprétation */
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
    font-size: 13px;
    color: #2980b9;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    margin-bottom: 8px;
  }
  
  /* Notes */
  .note {
    font-size: 13px;
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
  
  /* Footer */
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
  
  /* Responsive */
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
  .fig { text-align: center; margin: 15px 0; }
  .fig img { max-width: 100%; height: auto; border-radius: 6px; }
  .id-note { margin-top: 10px; font-size: 13px; }
  @page { size: A4; margin: 15mm; }
  @media print {
    body { background: white; max-width: none; padding: 0; }
    * { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
    .card, .stat-box, table, .interp, .fig { break-inside: avoid; box-shadow: none; }
    h2, h3 { break-after: avoid; }
  }
</style>
"

sbox <- function(value, label) {
  paste0("<div class='stat-box'><div class='stat-val'>", ar_esc(value),
         "</div><div class='stat-lbl'>", ar_esc(label), "</div></div>")
}

html_table <- function(df, gras1 = TRUE) {
  th <- paste0("<th>", ar_esc(names(df)), "</th>", collapse = "")
  lignes <- apply(as.matrix(format(df, trim = TRUE)), 1, function(l) {
    cel <- ar_esc(l)
    if (gras1) cel[1] <- paste0("<strong>", cel[1], "</strong>")
    paste0("<tr>", paste0("<td>", cel, "</td>", collapse = ""), "</tr>")
  })
  paste0("<div class='overflow-x'><table><thead><tr>", th, "</tr></thead><tbody>",
         paste(lignes, collapse = ""), "</tbody></table></div>")
}

html_figs <- function(g) {
  vapply(g, function(f) paste0("<div class='fig'><img alt='Graphique' src='", base64enc::dataURI(file = f, mime = "image/png"), "'></div>"), character(1))
}

html_interp <- function(texte, source) {
  titre <- if (identical(source, "ia")) "Interprétation (IA, à vérifier)" else "Interprétation"
  paste0("<div class='interp'><div class='interp-title'>", titre, "</div><p>", ar_esc(texte), "</p></div>")
}

render_html <- function(res, fichier) {
  sec <- 0
  h2 <- function(t) { sec <<- sec + 1; paste0("<h2>", sec, ". ", ar_esc(t), "</h2>") }
  h <- c("<!DOCTYPE html>", "<html lang='fr'>", "<head>", "<meta charset='UTF-8'>",
         "<meta name='viewport' content='width=device-width, initial-scale=1.0'>",
         paste0("<title>", ar_esc(res$title), " - AutoReportR v", AR_VERSION, "</title>"), report_css, "</head>", "<body>",
         "<div class='header'>", paste0("<h1>", ar_esc(res$title), " <span class='version'>v", AR_VERSION, "</span></h1>"),
         paste0("<div class='doc-meta'>Auteur : <strong>", ar_esc(res$author), "</strong> | Date : <strong>", date_fr(), "</strong> | AutoReportR</div>"),
         "</div>")

  dbl <- if (res$n_dup == 0) "Aucune ligne en double détectée." else if (res$n_dup == 1) "1 ligne en double détectée." else paste0(res$n_dup, " lignes en double détectées.")
  h <- c(h, h2("Présentation des données"), "<div class='card blue'><div class='stats-grid'>",
         sbox(res$n_obs, "Observations"), sbox(length(res$vars), "Variables"),
         sbox(fmt_pct(res$completude), "Complétude"), sbox(if (res$manquants$total_missing > 0) "Oui" else "Non", "Valeurs manquantes"), "</div>",
         paste0("<div><strong>Résumé :</strong> L'analyse porte sur ", res$n_obs, " observations et ", length(res$vars), " variables. ", dbl, "</div>"))
  if (length(res$ids_exclus) > 0) {
    h <- c(h, paste0("<div class='id-note'><strong>Identifiant(s) détecté(s) et exclu(s) de l'analyse :</strong> ", ar_esc(paste(res$ids_exclus, collapse = ", ")),
                     ". Pour en inclure un, indique-le dans <code>vars</code>.</div>"))
  }
  h <- c(h, "</div>")

  h <- c(h, h2("Types de variables"), "<div class='card'>", html_table(res$types_table), "</div>")

  if (res$manquants$total_missing > 0) {
    m <- res$manquants$summary
    tab <- data.frame(Variable = m$Variable, `Nombre manquant` = m$Nombre_manquant, Pourcentage = fmt_pct(m$Pourcentage, 2), check.names = FALSE, stringsAsFactors = FALSE)
    h <- c(h, h2("Analyse des valeurs manquantes"), "<div class='card'>", html_table(tab),
           html_interp(paste0("Le taux global est de ", fmt_pct(res$pct_manquant), ". ", interp_manquants(res$pct_manquant)), "auto"), "</div>")
  }

  if (length(res$uni) > 0) {
    h <- c(h, h2("Analyse univariée"))
    for (col in names(res$uni)) {
      u <- res$uni[[col]]; s <- u$stat
      bc <- if (s$type == "numeric") "badge-num" else "badge-cat"
      bt <- if (s$type == "numeric") "NUMÉRIQUE" else "CATÉGORIELLE"
      h <- c(h, paste0("<h3>", ar_esc(col), " <span class='badge ", bc, "'>", bt, "</span></h3>"), "<div class='card'>")
      if (s$type == "numeric") {
        h <- c(h, "<div class='stats-grid'>", sbox(s$n_complete, "Effectif"), sbox(fmt_num(s$mean), "Moyenne"), sbox(fmt_num(s$median), "Médiane"),
               sbox(fmt_num(s$sd), "Écart-type"), sbox(if (is.na(s$cv)) "-" else fmt_pct(s$cv), "CV"), sbox(fmt_num(s$iqr), "Écart interquartile"),
               sbox(fmt_num(s$min), "Minimum"), sbox(fmt_num(s$max), "Maximum"), "</div>")
      } else {
        cats <- s$all_categories
        tronque <- nrow(cats) > 15
        vue <- utils::head(cats, 15)
        lig <- vapply(seq_len(nrow(vue)), function(i) {
          est_mode <- vue$Effectif[i] == s$max_effectif
          tag <- if (est_mode && s$n_exaequo > 1) "<span class='exaequo-tag'>EX AEQUO</span>" else if (est_mode) "<span class='mode-tag'>MODE</span>" else ""
          paste0("<tr><td><strong>", ar_esc(vue$Modalite[i]), "</strong>", tag, "</td><td>", vue$Effectif[i], "</td><td>", fmt_pct(vue$Pourcentage[i], 2), "</td><td>", fmt_pct(vue$Pourcentage_cumule[i], 2), "</td></tr>")
        }, character(1))
        h <- c(h, "<div class='stats-grid'>", sbox(s$n_complete, "Effectif total"), sbox(s$n_levels, "Catégories"), "</div>",
               "<div class='overflow-x'><table><thead><tr><th>Catégorie</th><th>Effectif</th><th>Pourcentage</th><th>% cumulé</th></tr></thead><tbody>", lig, "</tbody></table></div>")
        if (tronque) h <- c(h, paste0("<div class='note'>Les 15 catégories les plus fréquentes sur ", nrow(cats), " sont affichées.</div>"))
      }
      if (s$missing > 0) h <- c(h, paste0("<div class='note'>", s$missing, " valeur(s) manquante(s) ou non finie(s) exclue(s) (", fmt_pct(s$missing_pct, 2), ").</div>"))
      h <- c(h, html_figs(u$graphes), html_interp(u$interp, u$source), "</div>")
    }
  }

  if (length(res$bi) > 0 || length(res$bi_ignorees) > 0) {
    h <- c(h, h2("Analyse bivariée"))
    for (r in res$bi) {
      h <- c(h, paste0("<h3>", ar_esc(r$a), " x ", ar_esc(r$b), "</h3>"), "<div class='card'>", html_table(r$table), html_figs(r$graphes), html_interp(r$interp, r$source))
      if (r$n_exclus > 0) h <- c(h, paste0("<div class='note'>", r$n_exclus, " observation(s) exclue(s) pour valeur manquante.</div>"))
      h <- c(h, "</div>")
    }
    if (length(res$bi_ignorees) > 0) h <- c(h, paste0("<div class='note'>Croisement(s) non réalisable(s) : ", ar_esc(paste(res$bi_ignorees, collapse = " ; ")), ".</div>"))
  }

  h <- c(h, h2(if (identical(res$resume_source, "ia")) "Résumé (IA, à vérifier)" else "Résumé"),
         "<div class='card blue'>", paste0("<p>", ar_esc(res$resume$synthese), "</p>"), "</div>")

  mention_ia <- if (isTRUE(res$ia$utilisee)) paste0(" | Interprétations IA via Groq (", ar_esc(res$ia$modele), ")") else ""
  h <- c(h, "<div class='footer'>", paste0("Rapport généré par AutoReportR v", AR_VERSION, " | ", date_fr(), mention_ia), "<br>",
         AR_AUTEUR, "<br>", AR_CONTACT, "</div>", "</body>", "</html>")
  ecrire_utf8(h, fichier)
}
