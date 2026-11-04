# ============================================
# AUTOREPORTR 3.0.0 - INSTALLATION
# ============================================

# Depuis GitHub, après la publication
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if ("autoreportr" %in% rownames(installed.packages())) {
  if ("package:autoreportr" %in% search()) detach("package:autoreportr", unload = TRUE)
  remove.packages("autoreportr")
}
devtools::install_github("alinkpessostalec9-creator/autoreportr", upgrade = "never")
# Pour installer depuis ton dossier sans passer par GitHub, utilise plutôt :
# devtools::install_local("F:/R Fan/AutoreportR", force = TRUE, upgrade = "never")

library(autoreportr)
packageVersion("autoreportr")   # doit afficher 3.0.0
?autoreportr
?auto_report

# Essais
auto_report(iris, title = "Analyse des iris", author = "Stalec")
auto_report(iris, title = "Analyse des iris", author = "Stalec",
            type = "word", analyse = "ub", ref_var = "Species", graph = TRUE)
# Avec l'IA : crée une clé sur https://console.groq.com/keys
# auto_report(iris, title = "Analyse des iris", author = "Stalec", key = "gsk_...")

# Plateforme interactive
# auto_report_app()
