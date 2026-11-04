construire_resultats <- function(data, vars = NULL, title = NULL, author = ar_auteur_defaut(), key = NULL,
                                 analyse = "u", ref_var = NULL, graph = FALSE, model = NULL,
                                 progres = function(...) NULL) {
  if (!is.data.frame(data)) stop("L'argument 'data' doit être un data.frame.", call. = FALSE)
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(data) == 0 || ncol(data) == 0) stop("La base de données est vide.", call. = FALSE)
  analyse <- tolower(analyse)
  if (length(analyse) != 1 || !analyse %in% c("u", "b", "ub", "bu")) stop("'analyse' doit valoir \"u\" (univariée), \"b\" (bivariée) ou \"ub\" (les deux).", call. = FALSE)
  faire_u <- grepl("u", analyse); faire_b <- grepl("b", analyse)
  choix <- normaliser_graph(graph)
  toutes <- identical(ref_var, "all")
  if (!is.null(vars)) {
    absents <- setdiff(vars, names(data))
    if (length(absents) > 0) stop("Variables introuvables : ", paste(absents, collapse = ", "), call. = FALSE)
  }
  if (!is.null(ref_var) && !toutes && (length(ref_var) != 1 || !ref_var %in% names(data))) stop("'ref_var' est introuvable dans la base.", call. = FALSE)
  if (faire_b && is.null(ref_var)) {
    message("Analyse bivariée ignorée : indique une variable de croisement avec ref_var = \"nom\", ou ref_var = \"all\" pour toutes les paires. Analyse univariée produite.")
    faire_u <- TRUE; faire_b <- FALSE
  }
  ids <- detect_id_vars(data)
  ids_exclus <- character()
  if (is.null(vars)) {
    ids_exclus <- ids
    sel <- setdiff(names(data), ids)
    if (length(sel) == 0) stop("Toutes les colonnes ressemblent à des identifiants. Choisis-les avec 'vars'.", call. = FALSE)
  } else {
    sel <- vars
  }
  ref <- if (!is.null(ref_var) && !toutes && faire_b) ref_var else NULL
  sous <- data[, sel, drop = FALSE]
  res <- list(title = if (is.null(title) || !nzchar(title)) "Rapport d'analyse statistique" else title,
              author = if (is.null(author) || !nzchar(author)) "Non renseigné" else author, vars = sel, n_obs = nrow(data), ids_exclus = ids_exclus,
              n_dup = sum(duplicated(data[, setdiff(names(data), ids), drop = FALSE])), resume_source = "auto",
              ia = list(utilisee = FALSE))
  res$manquants <- analyze_missing(sous)
  res$pct_manquant <- res$manquants$total_missing / (nrow(sous) * ncol(sous)) * 100
  res$completude <- 100 - res$pct_manquant
  types <- unlist(detect_types(data)[sel])
  niveaux <- unique(types)
  res$types_table <- data.frame(Type = niveaux, Nombre = vapply(niveaux, function(t) sum(types == t), numeric(1)),
                                Variables = vapply(niveaux, function(t) paste(names(types)[types == t], collapse = ", "), character(1)),
                                stringsAsFactors = FALSE)
  rownames(res$types_table) <- NULL

  res$uni <- list()
  stats_u <- compute_descriptive_stats(sous)
  for (col in names(stats_u)) {
    s <- stats_u[[col]]
    if (!faire_u) break
    res$uni[[col]] <- list(stat = s, source = "auto",
                           interp = if (s$type == "numeric") interp_numeric(col, s) else interp_categorical(col, s),
                           graphes = graphes_uni(data[[col]], s, col, choix))
  }
  res$bi <- list(); res$bi_ignorees <- character()
  if (faire_b) {
    if (toutes) {
      paires <- utils::combn(sel, 2, simplify = FALSE)
      if (length(paires) > 50) {
        message(length(paires), " paires possibles : seules les 50 premières sont analysées. Réduis la liste avec 'vars'.")
        paires <- paires[1:50]
      }
    } else {
      paires <- lapply(setdiff(sel, ref), function(v) c(v, ref))
    }
    b <- compute_bivariate(data, paires)
    res$bi_ignorees <- b$ignorees
    res$bi <- lapply(b$resultats, function(r) {
      r$interp <- interp_bivariee(r); r$source <- "auto"; r$graphes <- graphe_bi(data, r, choix); r
    })
  }
  res$resume <- resume_statique(res)
  if (is.null(key) || !nzchar(key)) key <- Sys.getenv("GROQ_API_KEY")
  if (nzchar(key)) res <- ia_enrichir(res, key, model, progres)
  res
}

#' Générer un rapport statistique automatique
#'
#' Produit un rapport complet en HTML, Word ou PDF : présentation des données,
#' identifiants, types, valeurs manquantes, analyse univariée et bivariée,
#' graphiques au choix, interprétations (p-value exacte, seuil de décision de 5 %) et résumé final en un paragraphe.
#'
#' @section Clé Groq:
#' Chaque utilisateur crée gratuitement sa clé sur
#' <https://console.groq.com/keys>, puis la met dans `key`. Sans clé, ou si
#' l'IA ne répond pas, le rapport est quand même produit avec des
#' interprétations automatiques. Seuls des résultats agrégés sont envoyés
#' (noms des variables, statistiques, effectifs des modalités), jamais les
#' lignes de la base. Pour éviter d'écrire la clé dans un script partagé, tu
#' peux la placer dans `.Renviron` sous le nom `GROQ_API_KEY`.
#'
#' @section Identifiants:
#' Les colonnes d'identifiants sont détectées par [detect_id_vars()],
#' signalées dans le rapport et exclues, sauf si elles sont citées dans `vars`.
#'
#' @param data Un data.frame.
#' @param vars Variables à analyser (toutes par défaut, sans les identifiants).
#' @param title Titre du rapport.
#' @param author Auteur du rapport.
#' @param open_report `TRUE` pour ouvrir le rapport à la fin (le premier si plusieurs formats sont demandés).
#' @param key Clé Groq personnelle, facultative.
#' @param type Format : `"html"`, `"word"`, `"pdf"`, ou plusieurs avec `c()`.
#'   Le PDF nécessite Google Chrome ou Microsoft Edge.
#' @param analyse `"u"` univariée, `"b"` bivariée, `"ub"` les deux.
#' @param ref_var Variable de croisement pour l'analyse bivariée, ou `"all"`
#'   pour toutes les paires (50 au maximum).
#' @param graph `FALSE` par défaut (aucun graphique). `TRUE` choisit un
#'   graphique adapté à chaque analyse. Sinon, choisis parmi `"hist"`, `"box"`,
#'   `"bar"` et `"scatter"`, par exemple `graph = c("hist", "bar")`.
#' @param model Modèle Groq à utiliser à la place de celui choisi par défaut.
#' @param output_dir Dossier où enregistrer le rapport.
#' @return Le chemin du ou des fichiers créés, de manière invisible.
#' @seealso [auto_report_app()], [detect_id_vars()]
#' @examples
#' dossier <- tempdir()
#' auto_report(iris, title = "Analyse des iris", author = "Stalec",
#'             open_report = FALSE, output_dir = dossier)
#' \dontrun{
#' auto_report(iris, title = "Analyse des iris", author = "Stalec",
#'             key = "gsk_...", type = "word", analyse = "ub",
#'             ref_var = "Species", graph = TRUE)
#' }
#' @export
auto_report <- function(data, vars = NULL, title = NULL, author = ar_auteur_defaut(), open_report = TRUE,
                        key = NULL, type = "html", analyse = "u", ref_var = NULL, graph = FALSE,
                        model = NULL, output_dir = getwd()) {
  type <- unique(tolower(type))
  if (length(setdiff(type, c("html", "word", "pdf"))) > 0 || length(type) == 0) stop("'type' doit contenir \"html\", \"word\" ou \"pdf\".", call. = FALSE)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  cat("AutoReportR v", AR_VERSION, " - Analyse en cours...\n", sep = "")
  res <- construire_resultats(data, vars, title, author, key, analyse, ref_var, graph, model)
  if (length(res$ids_exclus) > 0) cat("  Identifiant(s) exclu(s) :", paste(res$ids_exclus, collapse = ", "), "\n")
  if (isTRUE(res$ia$utilisee)) {
    cat("  Interprétations IA :", res$ia$modele, "\n")
    if (isTRUE(res$ia$partielle) && !is.null(res$ia$alerte)) cat("  Certaines interprétations sont automatiques :", res$ia$alerte, "\n")
  } else if (!is.null(res$ia$alerte)) {
    cat("  IA indisponible (", res$ia$alerte, "). Interprétations automatiques utilisées.\n", sep = "")
  } else {
    cat("  Sans clé Groq : interprétations automatiques. Créer une clé :", AR_GROQ_LIEN, "\n")
  }
  base <- file.path(output_dir, paste0("rapport_analyse_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  if (length(Sys.glob(paste0(base, ".*"))) > 0) base <- paste0(base, "_", format(Sys.time(), "%OS3"))
  chemins <- character()
  for (t in type) {
    ext <- c(html = ".html", word = ".docx", pdf = ".pdf")[[t]]
    f <- paste0(base, ext)
    ok <- tryCatch({ switch(t, html = render_html(res, f), word = render_word(res, f), pdf = render_pdf(res, f)); TRUE },
                   error = function(e) { message("Format ", t, " non généré : ", conditionMessage(e)); FALSE })
    if (ok) { chemins[t] <- normalizePath(f); cat("  Rapport :", chemins[t], "\n") }
  }
  if (length(chemins) == 0) stop("Aucun rapport n'a pu être généré.", call. = FALSE)
  if (open_report && interactive()) {
    if (.Platform$OS.type == "windows") shell.exec(chemins[[1]]) else utils::browseURL(chemins[[1]])
  }
  invisible(if (length(chemins) == 1) unname(chemins) else chemins)
}
