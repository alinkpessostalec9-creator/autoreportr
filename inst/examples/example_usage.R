# Exemples d'utilisation du package AutoReportR

# Charger le package
library(autoreportr)

# Exemple 1: Analyser le jeu de données mtcars
auto_report(mtcars, title = "Analyse des véhicules mtcars")

# Exemple 2: Sélectionner certaines variables
auto_report(iris, 
            vars = c("Sepal.Length", "Sepal.Width", "Species"),
            title = "Analyse des iris - Sélection de variables")

# Exemple 3: Avec des données personnalisées
set.seed(123)
df <- data.frame(
  age = rnorm(100, mean = 35, sd = 10),
  sexe = sample(c("Homme", "Femme"), 100, replace = TRUE),
  ville = sample(c("Paris", "Lyon", "Marseille"), 100, replace = TRUE),
  score = runif(100, 0, 100),
  stringsAsFactors = FALSE
)

# Ajouter quelques valeurs manquantes
df$age[sample(1:100, 5)] <- NA
df$ville[sample(1:100, 3)] <- NA

auto_report(df, title = "Analyse d'un sondage personnalisé")