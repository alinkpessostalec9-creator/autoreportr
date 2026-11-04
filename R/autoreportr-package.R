#' AutoReportR : rapports statistiques automatiques
#'
#' AutoReportR transforme une base de données en rapport statistique en HTML,
#' Word ou PDF, avec une seule commande ou depuis une plateforme interactive.
#'
#' @section Fonctions principales:
#' - [auto_report()] : génère le rapport.
#' - [auto_report_app()] : ouvre la plateforme interactive.
#' - [check_data()], [detect_types()], [detect_id_vars()], [analyze_missing()],
#'   [compute_descriptive_stats()] : étapes séparées.
#'
#' @section Clé Groq:
#' Chaque utilisateur peut créer sa propre clé sur
#' <https://console.groq.com/keys> pour obtenir les interprétations par IA.
#' Le package fonctionne aussi sans clé.
#'
#' @section Auteur:
#' ALINKPESSO Dona Ezéchiel Stalec, statisticien, fondateur de R Fan.
#'
#' Contact : alinkpessostalec9@gmail.com, WhatsApp +229 01 58 97 18 43.
#'
#' @section Collaborateur:
#' DAH SEGBE Jules
#'
#' @section Partenaires:
#' CRISTAL (Collectif de Recherche et d'Innovation en Statistique de Travail et
#' d'Application sur les Logiciels) et BUE-ENSPD (École Nationale de
#' Statistique, de Planification et de Démographie), Université de Parakou.
#'
#' @keywords internal
"_PACKAGE"

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("AutoReportR ", AR_VERSION, " | Aide : ?autoreportr | Plateforme : auto_report_app()")
}
