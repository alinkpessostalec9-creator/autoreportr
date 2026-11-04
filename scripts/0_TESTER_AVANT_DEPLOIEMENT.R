# ============================================
# AUTOREPORTR 3.0.0 - TEST AVANT LE DÉPLOIEMENT
# Charge le code avec source(), sans installer le package.
# Ouvre ce fichier dans RStudio et exécute-le ligne par ligne.
# ============================================

dossier <- Sys.getenv("AUTOREPORTR_DIR", "F:/R Fan/AutoreportR")

# 1. Installer ce qui manque (une seule fois)
paquets <- c("base64enc", "e1071", "flextable", "httr", "jsonlite", "officer", "readxl", "shiny")
manquants <- paquets[!vapply(paquets, requireNamespace, logical(1), quietly = TRUE)]
if (length(manquants) > 0) install.packages(manquants)

# 2. Charger le code source
for (f in list.files(file.path(dossier, "R"), pattern = "[.]R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}

sorties <- file.path(dossier, "rapports_test")
dir.create(sorties, showWarnings = FALSE)

# Pour tester l'IA, colle ta clé ici, puis efface-la après le test.
# Le script de publication bloque l'envoi si une clé reste dans un fichier.
ma_cle <- ""
# Clé gratuite : https://console.groq.com/keys

# 3. Fonctions séparées
str(check_data(iris))
str(detect_types(iris))
detect_id_vars(iris)
analyze_missing(airquality)$summary
compute_descriptive_stats(iris)$Sepal.Length$cv

# 4. Iris : HTML simple
auto_report(iris, title = "Test 1 - Iris", author = "Stalec",
            open_report = FALSE, output_dir = sorties)

# 5. Iris : les trois formats, univariée et bivariée, graphiques automatiques
auto_report(iris, title = "Test 2 - Iris complet", author = "Stalec",
            key = ma_cle, type = c("html", "word", "pdf"), analyse = "ub",
            ref_var = "Species", graph = TRUE,
            open_report = FALSE, output_dir = sorties)

# 6. Iris : seulement des histogrammes et barres, en Word
auto_report(iris, title = "Test 3 - Graphiques choisis", author = "Stalec",
            type = "word", graph = c("hist", "bar"),
            open_report = FALSE, output_dir = sorties)

# 7. mtcars : bivariée seule, variable de croisement numérique
auto_report(mtcars, vars = c("mpg", "hp", "wt", "am"), title = "Test 4 - mtcars",
            author = "Stalec", analyse = "b", ref_var = "mpg", graph = "scatter",
            open_report = FALSE, output_dir = sorties)

# 8. airquality : valeurs manquantes
auto_report(airquality, title = "Test 5 - airquality", author = "Stalec",
            analyse = "ub", ref_var = "all", type = "html",
            open_report = FALSE, output_dir = sorties)

# 9. Base avec identifiants : id_patient et code_menage doivent être exclus
set.seed(123)
base_test <- data.frame(
  id_patient = 1:100,
  code_menage = sprintf("M%04d", 1:100),
  age = round(rnorm(100, 35, 10)),
  poids = round(rnorm(100, 68, 11), 1),
  imc = round(c(rnorm(98, 24, 3), 95, 2), 1),
  sexe = sample(c("Homme", "Femme"), 100, replace = TRUE),
  stringsAsFactors = FALSE
)
base_test$age[sample(100, 6)] <- NA
stopifnot(identical(detect_id_vars(base_test), c("id_patient", "code_menage")))
auto_report(base_test, title = "Test 6 - Identifiants", author = "Stalec",
            key = ma_cle, type = "word", analyse = "ub", ref_var = "sexe", graph = TRUE,
            open_report = FALSE, output_dir = sorties)

# 10. Un identifiant choisi volontairement doit être analysé
auto_report(base_test, vars = c("id_patient", "age"), title = "Test 7 - Identifiant choisi",
            author = "Stalec", open_report = FALSE, output_dir = sorties)

cat("\nTests terminés. Ouvre les rapports dans :", sorties, "\n")

# 11. Plateforme interactive (à lancer à la main, puis fermer pour continuer)
# auto_report_app()
# auto_report_app(iris)
