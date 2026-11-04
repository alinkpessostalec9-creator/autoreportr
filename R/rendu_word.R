# Rendu Word : Times New Roman, interligne 1,5, taille 12 (tableaux en taille 10)
W_BLEU <- "#2980b9"
w_txt <- function(gras = FALSE, couleur = "black", taille = 12, ital = FALSE) {
  officer::fp_text(font.family = "Times New Roman", font.size = taille, bold = gras, color = couleur, italic = ital)
}
w_par <- function(doc, texte, gras = FALSE, couleur = "black", taille = 12, ital = FALSE, align = "justify", gauche = 0) {
  officer::body_add_fpar(doc, officer::fpar(officer::ftext(texte, w_txt(gras, couleur, taille, ital)),
                                            fp_p = officer::fp_par(text.align = align, line_spacing = 1.5, padding.bottom = 4, padding.left = gauche)))
}
w_titre <- function(doc, texte, niveau, couleur = W_BLEU) {
  officer::body_add_fpar(doc, officer::fpar(officer::ftext(texte, w_txt(TRUE, couleur, c(16, 14, 12)[niveau])),
                                            fp_p = officer::fp_par(line_spacing = 1.5, padding.top = 8, padding.bottom = 4, keep_with_next = TRUE)),
                         style = paste("heading", niveau))
}
w_table <- function(doc, df) {
  df <- as.data.frame(format(df, trim = TRUE), check.names = FALSE, stringsAsFactors = FALSE)
  noms <- names(df)
  names(df) <- paste0("c", seq_along(noms))
  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(ft, values = stats::setNames(as.list(noms), names(df)))
  ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::line_spacing(ft, space = 1, part = "all")
  ft <- flextable::bg(ft, bg = W_BLEU, part = "header")
  ft <- flextable::color(ft, color = "white", part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bold(ft, j = 1, part = "body")
  ft <- flextable::align(ft, align = "left", part = "all")
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline(ft, border = officer::fp_border(color = "#d5dbe1", width = 0.75), part = "body")
  ft <- flextable::set_table_properties(ft, layout = "autofit", width = 1)
  doc <- flextable::body_add_flextable(doc, ft, align = "center")
  w_par(doc, "", taille = 6)
}
w_figs <- function(doc, g) {
  for (f in g) {
    tl <- png_dims(f)
    l <- 5.6; h <- l * tl[2] / tl[1]
    doc <- officer::body_add_img(doc, src = f, width = l, height = h, style = "centered")
  }
  doc
}
png_dims <- function(f) {
  r <- readBin(f, "raw", 24)
  v <- function(i) sum(as.integer(r[i:(i + 3)]) * 256^(3:0))
  c(v(17), v(21))
}
w_interp <- function(doc, texte, source) {
  titre <- if (identical(source, "ia")) "Interprétation (IA, à vérifier)" else "Interprétation"
  doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(titre, w_txt(TRUE, W_BLEU)), fp_p = officer::fp_par(line_spacing = 1.5, keep_with_next = TRUE)))
  w_par(doc, texte)
}

render_word <- function(res, fichier) {
  doc <- officer::read_docx()
  # Retire la numérotation automatique et le trait des titres du modèle par défaut
  styles <- file.path(doc$package_dir, "word", "styles.xml")
  x <- paste(readLines(styles, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  x <- gsub("(<w:name w:val=\"heading [1-3]\"/>(?:(?!</w:style>).)*?)<w:numPr>.*?</w:numPr>", "\\1", x, perl = TRUE)
  x <- gsub("(<w:name w:val=\"heading 1\"/>(?:(?!</w:style>).)*?)<w:pBdr>.*?</w:pBdr>", "\\1", x, perl = TRUE)
  ecrire_utf8(x, styles)
  pied <- officer::block_list(
    officer::fpar(officer::ftext(paste0("AutoReportR v", AR_VERSION, " | Page "), w_txt(taille = 9, couleur = "#7f8c8d")),
                  officer::run_word_field("PAGE", prop = w_txt(taille = 9, couleur = "#7f8c8d")),
                  fp_p = officer::fp_par(text.align = "center")))
  doc <- officer::body_set_default_section(doc, officer::prop_section(
    page_size = officer::page_size(width = 8.27, height = 11.69, orient = "portrait"),
    page_margins = officer::page_mar(top = 1, bottom = 1, left = 1, right = 1, header = 0.5, footer = 0.5, gutter = 0),
    footer_default = pied))
  doc <- w_titre(doc, res$title, 1, "#1a252f")
  doc <- w_par(doc, paste0("Auteur : ", res$author, " | Date : ", date_fr()), couleur = "#555555", align = "left")
  sec <- 0
  t2 <- function(d, t) { sec <<- sec + 1; w_titre(d, paste0(sec, ". ", t), 2) }

  doc <- t2(doc, "Présentation des données")
  doc <- w_par(doc, paste0("L'analyse porte sur ", res$n_obs, " observations et ", length(res$vars), " variable(s). Le taux de complétude est de ", fmt_pct(res$completude),
                           if (res$n_dup > 0) paste0(" et ", res$n_dup, " ligne(s) en double ont été détectées.") else ", sans ligne en double."))
  doc <- w_par(doc, paste0("Variables analysées : ", paste(res$vars, collapse = ", "), "."))
  if (length(res$ids_exclus) > 0) doc <- w_par(doc, paste0("Identifiant(s) détecté(s) et exclu(s) de l'analyse : ", paste(res$ids_exclus, collapse = ", "), "."))

  doc <- t2(doc, "Types de variables")
  doc <- w_table(doc, res$types_table)

  if (res$manquants$total_missing > 0) {
    m <- res$manquants$summary
    doc <- t2(doc, "Analyse des valeurs manquantes")
    doc <- w_table(doc, data.frame(Variable = m$Variable, `Nombre manquant` = m$Nombre_manquant, Pourcentage = fmt_pct(m$Pourcentage, 2), check.names = FALSE, stringsAsFactors = FALSE))
    doc <- w_interp(doc, paste0("Le taux global est de ", fmt_pct(res$pct_manquant), ". ", interp_manquants(res$pct_manquant)), "auto")
  }

  if (length(res$uni) > 0) {
    doc <- t2(doc, "Analyse univariée")
    for (col in names(res$uni)) {
      u <- res$uni[[col]]; s <- u$stat
      doc <- w_titre(doc, col, 3)
      if (s$type == "numeric") {
        doc <- w_table(doc, data.frame(Effectif = s$n_complete, Moyenne = fmt_num(s$mean), Médiane = fmt_num(s$median), `Écart-type` = fmt_num(s$sd),
                                       CV = if (is.na(s$cv)) "-" else fmt_pct(s$cv), EIQ = fmt_num(s$iqr), Min = fmt_num(s$min), Max = fmt_num(s$max), check.names = FALSE, stringsAsFactors = FALSE))
      } else {
        vue <- utils::head(s$all_categories, 15)
        doc <- w_table(doc, data.frame(Catégorie = vue$Modalite, Effectif = vue$Effectif, Pourcentage = fmt_pct(vue$Pourcentage, 2), `% cumulé` = fmt_pct(vue$Pourcentage_cumule, 2), check.names = FALSE, stringsAsFactors = FALSE))
        if (nrow(s$all_categories) > 15) doc <- w_par(doc, paste0("Les 15 catégories les plus fréquentes sur ", nrow(s$all_categories), " sont affichées."), taille = 10, ital = TRUE)
      }
      if (s$missing > 0) doc <- w_par(doc, paste0(s$missing, " valeur(s) manquante(s) ou non finie(s) exclue(s) (", fmt_pct(s$missing_pct, 2), ")."), taille = 10, ital = TRUE)
      doc <- w_figs(doc, u$graphes)
      doc <- w_interp(doc, u$interp, u$source)
    }
  }

  if (length(res$bi) > 0 || length(res$bi_ignorees) > 0) {
    doc <- t2(doc, "Analyse bivariée")
    for (r in res$bi) {
      doc <- w_titre(doc, paste(r$a, "x", r$b), 3)
      doc <- w_table(doc, r$table)
      doc <- w_figs(doc, r$graphes)
      doc <- w_interp(doc, r$interp, r$source)
      if (r$n_exclus > 0) doc <- w_par(doc, paste0(r$n_exclus, " observation(s) exclue(s) pour valeur manquante."), taille = 10, ital = TRUE)
    }
    if (length(res$bi_ignorees) > 0) doc <- w_par(doc, paste0("Croisement(s) non réalisable(s) : ", paste(res$bi_ignorees, collapse = " ; "), "."), taille = 10, ital = TRUE)
  }

  doc <- t2(doc, if (identical(res$resume_source, "ia")) "Résumé (IA, à vérifier)" else "Résumé")
  doc <- w_par(doc, res$resume$synthese)
  doc <- w_par(doc, "", taille = 6)
  doc <- w_par(doc, paste0("Rapport généré par AutoReportR v", AR_VERSION, if (isTRUE(res$ia$utilisee)) paste0(", interprétations IA via Groq (", res$ia$modele, ")"), ". ", AR_AUTEUR, ". Contact : ", AR_CONTACT, "."), taille = 9, couleur = "#7f8c8d")
  print(doc, target = fichier)
  invisible(fichier)
}
