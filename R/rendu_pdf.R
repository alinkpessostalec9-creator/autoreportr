# Rendu PDF au format du Word, avec Chrome ou Edge déjà installés
trouver_navigateur <- function() {
  env <- Sys.getenv("AUTOREPORTR_BROWSER")
  if (nzchar(env) && file.exists(env)) return(env)
  sys <- Sys.info()[["sysname"]]
  if (sys == "Windows") {
    bases <- Sys.getenv(c("PROGRAMFILES", "PROGRAMFILES(X86)", "LOCALAPPDATA"))
    cand <- c(file.path(bases, "Google/Chrome/Application/chrome.exe"), file.path(bases, "Microsoft/Edge/Application/msedge.exe"))
  } else if (sys == "Darwin") {
    cand <- c("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome", "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge")
  } else {
    cand <- Sys.which(c("google-chrome", "chromium", "chromium-browser", "microsoft-edge"))
  }
  cand <- cand[nzchar(cand) & file.exists(cand)]
  if (length(cand) == 0) NULL else cand[[1]]
}

render_pdf <- function(res, fichier) {
  nav <- trouver_navigateur()
  if (is.null(nav)) stop("PDF impossible : installe Google Chrome ou Microsoft Edge, ou utilise type = \"html\" ou \"word\".", call. = FALSE)
  html <- tempfile("ar_", fileext = ".html")
  profil <- tempfile("ar_profil_")
  dir.create(profil)
  on.exit(unlink(c(html, profil), recursive = TRUE), add = TRUE)
  render_html_pdf(res, html)
  sortie <- tempfile("ar_", fileext = ".pdf")
  url <- paste0("file:///", utils::URLencode(enc2utf8(sub("^/", "", gsub("\\\\", "/", normalizePath(html))))))
  args <- c("--headless=new", "--disable-gpu", "--no-first-run", "--no-default-browser-check", "--disable-extensions",
            "--disable-background-networking", "--disable-component-update", "--disable-sync", "--no-pdf-header-footer",
            if (Sys.info()[["sysname"]] == "Linux") "--no-sandbox",
            paste0("--user-data-dir=", shQuote(profil)), paste0("--print-to-pdf=", shQuote(sortie)), shQuote(url))
  suppressWarnings(system2(nav, args, stdout = FALSE, stderr = FALSE, timeout = 60))
  for (i in 1:20) { if (file.exists(sortie) && file.size(sortie) > 0) break; Sys.sleep(0.5) }
  if (!file.exists(sortie) || file.size(sortie) == 0) stop("Le navigateur n'a pas produit le PDF. Essaie type = \"html\" ou \"word\".", call. = FALSE)
  file.copy(sortie, fichier, overwrite = TRUE)
  unlink(sortie)
  invisible(fichier)
}

PDF_CSS <- "
<style>
  @page { size: A4; margin: 25.4mm; }
  * { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
  body { font-family: 'Times New Roman', Times, serif; font-size: 12pt; line-height: 1.5; color: #000; margin: 0; }
  p { margin: 0 0 4pt; text-align: justify; }
  h1, h2, h3 { font-weight: bold; line-height: 1.5; margin: 8pt 0 4pt; break-after: avoid; }
  h1 { font-size: 16pt; color: #1a252f; }
  h2 { font-size: 14pt; color: #2980b9; }
  h3 { font-size: 12pt; color: #2980b9; }
  .meta { color: #555; text-align: left; }
  table { border-collapse: collapse; width: 100%; font-size: 10pt; line-height: 1.2; margin: 0 0 8pt; break-inside: avoid; }
  th { background: #2980b9; color: #fff; text-align: left; padding: 4pt 5pt; font-weight: bold; }
  td { padding: 4pt 5pt; border-bottom: 0.75pt solid #d5dbe1; text-align: left; }
  td:first-child { font-weight: bold; }
  .fig { text-align: center; margin: 0 0 6pt; break-inside: avoid; }
  .fig img { width: 5.6in; height: auto; }
  .titre-interp { font-weight: bold; color: #2980b9; margin: 0; break-after: avoid; text-align: left; }
  .petit { font-size: 10pt; font-style: italic; }
  .pied { font-size: 9pt; color: #7f8c8d; margin-top: 10pt; }
</style>
"

render_html_pdf <- function(res, fichier) {
  sec <- 0
  h2 <- function(t) { sec <<- sec + 1; paste0("<h2>", sec, ". ", ar_esc(t), "</h2>") }
  p <- function(t) paste0("<p>", ar_esc(t), "</p>")
  petit <- function(t) paste0("<p class='petit'>", ar_esc(t), "</p>")
  table <- function(df) {
    df <- format(df, trim = TRUE)
    lignes <- apply(as.matrix(df), 1, function(l) paste0("<tr>", paste0("<td>", ar_esc(l), "</td>", collapse = ""), "</tr>"))
    paste0("<table><thead><tr>", paste0("<th>", ar_esc(names(df)), "</th>", collapse = ""), "</tr></thead><tbody>", paste(lignes, collapse = ""), "</tbody></table>")
  }
  interp <- function(texte, source) {
    paste0("<p class='titre-interp'>", if (identical(source, "ia")) "Interprétation (IA, à vérifier)" else "Interprétation", "</p>", p(texte))
  }
  h <- c("<!DOCTYPE html>", "<html lang='fr'><head><meta charset='UTF-8'>", paste0("<title>", ar_esc(res$title), "</title>"), PDF_CSS, "</head><body>",
         paste0("<h1>", ar_esc(res$title), "</h1>"), paste0("<p class='meta'>", ar_esc(paste0("Auteur : ", res$author, " | Date : ", date_fr())), "</p>"))
  h <- c(h, h2("Présentation des données"),
         p(paste0("L'analyse porte sur ", res$n_obs, " observations et ", length(res$vars), " variable(s). Le taux de complétude est de ", fmt_pct(res$completude),
                  if (res$n_dup > 0) paste0(" et ", res$n_dup, " ligne(s) en double ont été détectées.") else ", sans ligne en double.")),
         p(paste0("Variables analysées : ", paste(res$vars, collapse = ", "), ".")))
  if (length(res$ids_exclus) > 0) h <- c(h, p(paste0("Identifiant(s) détecté(s) et exclu(s) de l'analyse : ", paste(res$ids_exclus, collapse = ", "), ".")))
  h <- c(h, h2("Types de variables"), table(res$types_table))
  if (res$manquants$total_missing > 0) {
    m <- res$manquants$summary
    h <- c(h, h2("Analyse des valeurs manquantes"),
           table(data.frame(Variable = m$Variable, `Nombre manquant` = m$Nombre_manquant, Pourcentage = fmt_pct(m$Pourcentage, 2), check.names = FALSE, stringsAsFactors = FALSE)),
           interp(paste0("Le taux global est de ", fmt_pct(res$pct_manquant), ". ", interp_manquants(res$pct_manquant)), "auto"))
  }
  if (length(res$uni) > 0) {
    h <- c(h, h2("Analyse univariée"))
    for (col in names(res$uni)) {
      u <- res$uni[[col]]; s <- u$stat
      h <- c(h, paste0("<h3>", ar_esc(col), "</h3>"))
      if (s$type == "numeric") {
        h <- c(h, table(data.frame(Effectif = s$n_complete, Moyenne = fmt_num(s$mean), Médiane = fmt_num(s$median), `Écart-type` = fmt_num(s$sd),
                                   CV = if (is.na(s$cv)) "-" else fmt_pct(s$cv), EIQ = fmt_num(s$iqr), Min = fmt_num(s$min), Max = fmt_num(s$max), check.names = FALSE, stringsAsFactors = FALSE)))
      } else {
        vue <- utils::head(s$all_categories, 15)
        h <- c(h, table(data.frame(Catégorie = vue$Modalite, Effectif = vue$Effectif, Pourcentage = fmt_pct(vue$Pourcentage, 2), `% cumulé` = fmt_pct(vue$Pourcentage_cumule, 2), check.names = FALSE, stringsAsFactors = FALSE)))
        if (nrow(s$all_categories) > 15) h <- c(h, petit(paste0("Les 15 catégories les plus fréquentes sur ", nrow(s$all_categories), " sont affichées.")))
      }
      if (s$missing > 0) h <- c(h, petit(paste0(s$missing, " valeur(s) manquante(s) ou non finie(s) exclue(s) (", fmt_pct(s$missing_pct, 2), ").")))
      h <- c(h, html_figs(u$graphes), interp(u$interp, u$source))
    }
  }
  if (length(res$bi) > 0 || length(res$bi_ignorees) > 0) {
    h <- c(h, h2("Analyse bivariée"))
    for (r in res$bi) {
      h <- c(h, paste0("<h3>", ar_esc(r$a), " x ", ar_esc(r$b), "</h3>"), table(r$table), html_figs(r$graphes), interp(r$interp, r$source))
      if (r$n_exclus > 0) h <- c(h, petit(paste0(r$n_exclus, " observation(s) exclue(s) pour valeur manquante.")))
    }
    if (length(res$bi_ignorees) > 0) h <- c(h, petit(paste0("Croisement(s) non réalisable(s) : ", paste(res$bi_ignorees, collapse = " ; "), ".")))
  }
  h <- c(h, h2(if (identical(res$resume_source, "ia")) "Résumé (IA, à vérifier)" else "Résumé"), p(res$resume$synthese))
  h <- c(h, paste0("<p class='pied'>", ar_esc(paste0("Rapport généré par AutoReportR v", AR_VERSION, if (isTRUE(res$ia$utilisee)) paste0(", interprétations IA via Groq (", res$ia$modele, ")"), ". ", AR_AUTEUR, ". Contact : ", AR_CONTACT, ".")), "</p>"), "</body></html>")
  ecrire_utf8(h, fichier)
}
