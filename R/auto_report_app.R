#' Ouvrir la plateforme interactive
#'
#' Ouvre une application Shiny pour importer une base (CSV, Excel ou RDS),
#' choisir les variables, l'analyse et les graphiques, puis voir le rapport et
#' le télécharger en Word, PDF ou HTML. La clé Groq reste facultative et n'est
#' jamais enregistrée.
#'
#' @param data Un data.frame facultatif, déjà chargé dans R.
#' @param port Port de l'application (choisi automatiquement par défaut).
#' @param launch.browser `TRUE` pour ouvrir le navigateur.
#' @return Rien. Lance l'application jusqu'à sa fermeture.
#' @examples
#' \dontrun{
#' auto_report_app()
#' auto_report_app(iris)
#' }
#' @export
auto_report_app <- function(data = NULL, port = NULL, launch.browser = TRUE) {
  ancien <- options(shiny.maxRequestSize = 200 * 1024^2)
  on.exit(options(ancien), add = TRUE)
  donnee_initiale <- if (is.null(data)) NULL else as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  css <- "
    body { background:#f4f6f8; color:#2c3e50; font-family:'Segoe UI',Arial,sans-serif; }
    .titre-app { background:#fff; border-bottom:3px solid #2980b9; padding:16px 22px; margin:0 -15px 18px; }
    .titre-app h1 { margin:0; font-size:24px; font-weight:700; color:#1a252f; }
    .titre-app p { margin:4px 0 0; color:#7f8c8d; font-size:13px; }
    .well { background:#fff; border:none; box-shadow:0 1px 4px rgba(0,0,0,.07); }
    .btn-primary { background:#2980b9; border-color:#2980b9; font-weight:600; width:100%; }
    .btn-primary:hover { background:#21689a; border-color:#21689a; }
    .telecharger { margin-bottom:12px; }
    .telecharger .btn { margin:0 6px 6px 0; }
    iframe { width:100%; height:78vh; border:1px solid #e3e7eb; border-radius:6px; background:#fff; }
    .aide { font-size:12px; color:#7f8c8d; margin-top:-8px; margin-bottom:12px; }
    .aide a, .pied a { color:#2980b9; }
    .pied { font-size:11px; color:#95a5a6; margin-top:14px; }
    .vide { background:#fff; border-radius:6px; padding:40px; text-align:center; color:#7f8c8d; }
  "
  ui <- shiny::fluidPage(
    title = "AutoReportR",
    shiny::tags$head(shiny::tags$style(shiny::HTML(css))),
    shiny::div(class = "titre-app", shiny::h1(paste("AutoReportR", AR_VERSION)), shiny::p("Du fichier de données au rapport statistique")),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 4,
        if (is.null(donnee_initiale)) shiny::fileInput("fichier", "Base de données (CSV, XLSX, XLS, RDS)", accept = c(".csv", ".xlsx", ".xls", ".rds"), buttonLabel = "Parcourir", placeholder = "Aucun fichier"),
        shiny::textInput("titre", "Titre du rapport", "Rapport d'analyse statistique"),
        shiny::textInput("auteur", "Auteur", ar_auteur_defaut()),
        shiny::passwordInput("cle", "Clé Groq (facultative)"),
        shiny::div(class = "aide", shiny::a("Créer ma clé Groq", href = AR_GROQ_LIEN, target = "_blank"), ". Sans clé, les interprétations sont automatiques."),
        shiny::uiOutput("choix_vars"),
        shiny::radioButtons("analyse", "Analyse", c("Univariée" = "u", "Bivariée" = "b", "Les deux" = "ub"), inline = TRUE),
        shiny::conditionalPanel("input.analyse != 'u'", shiny::uiOutput("choix_ref")),
        shiny::checkboxGroupInput("graph", "Graphiques (aucun par défaut)", c("Histogramme" = "hist", "Boîte à moustaches" = "box", "Barres" = "bar", "Nuage de points" = "scatter"), inline = TRUE),
        shiny::actionButton("generer", "Générer le rapport", class = "btn-primary"),
        shiny::div(class = "pied", AR_AUTEUR)
      ),
      shiny::mainPanel(width = 8, shiny::uiOutput("boutons"), shiny::uiOutput("apercu"))
    )
  )
  server <- function(input, output, session) {
    base <- shiny::reactive({
      if (!is.null(donnee_initiale)) return(donnee_initiale)
      shiny::req(input$fichier)
      chemin <- input$fichier$datapath
      ext <- tolower(tools::file_ext(input$fichier$name))
      tryCatch(switch(ext,
        csv = { pv <- sum(gregexpr(";", readLines(chemin, n = 1, warn = FALSE))[[1]] > 0); virg <- sum(gregexpr(",", readLines(chemin, n = 1, warn = FALSE))[[1]] > 0)
                if (pv > virg) utils::read.csv2(chemin, stringsAsFactors = FALSE, check.names = FALSE) else utils::read.csv(chemin, stringsAsFactors = FALSE, check.names = FALSE) },
        xlsx = , xls = as.data.frame(readxl::read_excel(chemin), check.names = FALSE),
        rds = as.data.frame(readRDS(chemin), check.names = FALSE),
        stop("format non pris en charge")),
        error = function(e) { shiny::showNotification(paste("Lecture impossible :", conditionMessage(e)), type = "error"); NULL })
    })
    output$choix_vars <- shiny::renderUI({
      d <- base(); shiny::req(d)
      ids <- detect_id_vars(d)
      shiny::tagList(
        shiny::selectizeInput("vars", "Variables à analyser", names(d), selected = setdiff(names(d), ids), multiple = TRUE),
        if (length(ids) > 0) shiny::div(class = "aide", paste0("Identifiant(s) détecté(s) et retiré(s) : ", paste(ids, collapse = ", "), ". Tu peux les rajouter."))
      )
    })
    output$choix_ref <- shiny::renderUI({
      d <- base(); shiny::req(d)
      shiny::selectInput("ref", "Variable de croisement", c("Toutes les paires" = "all", names(d)))
    })
    resultat <- shiny::reactiveVal(NULL)
    apercu_html <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$generer, {
      d <- base()
      if (is.null(d)) { shiny::showNotification("Importe d'abord une base.", type = "warning"); return() }
      if (length(input$vars) == 0) { shiny::showNotification("Choisis au moins une variable.", type = "warning"); return() }
      res <- tryCatch(shiny::withProgress(message = "Analyse en cours", value = 0.1, {
        construire_resultats(d, vars = input$vars, title = input$titre, author = input$auteur, key = input$cle,
                             analyse = input$analyse, ref_var = if (input$analyse == "u") NULL else input$ref,
                             graph = if (length(input$graph) == 0) FALSE else input$graph,
                             progres = function(v, m) shiny::setProgress(value = 0.1 + 0.85 * v, message = m))
      }), error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); NULL })
      if (is.null(res)) return()
      if (!is.null(res$ia$alerte)) shiny::showNotification(paste("IA :", res$ia$alerte, "- interprétations automatiques utilisées si besoin."), type = "warning", duration = 10)
      f <- tempfile(fileext = ".html")
      render_html(res, f)
      apercu_html(paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n"))
      unlink(f)
      resultat(res)
    })
    output$boutons <- shiny::renderUI({
      shiny::req(resultat())
      shiny::div(class = "telecharger",
                 shiny::downloadButton("dl_word", "Word"), shiny::downloadButton("dl_pdf", "PDF"), shiny::downloadButton("dl_html", "HTML"))
    })
    output$apercu <- shiny::renderUI({
      if (is.null(apercu_html())) return(shiny::div(class = "vide", "Le rapport s'affichera ici."))
      shiny::tags$iframe(srcdoc = apercu_html())
    })
    telecharger <- function(ext, rendu) shiny::downloadHandler(
      filename = function() paste0("rapport_analyse_", format(Sys.time(), "%Y%m%d_%H%M%S"), ext),
      content = function(file) {
        tmp <- tempfile(fileext = ext)
        rendu(resultat(), tmp)
        file.copy(tmp, file, overwrite = TRUE)
        unlink(tmp)
      })
    output$dl_word <- telecharger(".docx", render_word)
    output$dl_pdf <- telecharger(".pdf", render_pdf)
    output$dl_html <- telecharger(".html", render_html)
  }
  arguments <- list(launch.browser = launch.browser)
  if (!is.null(port)) arguments$port <- port
  do.call(shiny::runApp, c(list(appDir = shiny::shinyApp(ui, server)), arguments))
}
