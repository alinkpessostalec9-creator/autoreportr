# ============================================
# Exemples d'utilisation d'AutoReportR v2.0
# ============================================

library(autoreportr)

# 1. Analyse basique
auto_report(mtcars)

# 2. Avec titre personnalisé
auto_report(iris, title = "Analyse des iris")

# 3. Sélection de variables spécifiques
auto_report(iris, vars = c("Sepal.Length", "Species"))

# 4. Données personnalisées avec valeurs manquantes
set.seed(123)
df <- data.frame(
  age = rnorm(100, 35, 10),
  sexe = sample(c("Homme", "Femme"), 100, TRUE),
  ville = sample(c("Paris", "Lyon", "Marseille"), 100, TRUE),
  score = runif(100, 0, 100)
)
df$age[sample(1:100, 5)] <- NA  # Ajout de valeurs manquantes

auto_report(df, title = "Analyse sondage", author = "Equipe Data")

# 5. Générer sans ouvrir automatiquement
auto_report(mtcars, open_report = FALSE)