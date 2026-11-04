# AutoReportR 3.0.0

Package R qui transforme une base de données en rapport statistique HTML, Word ou PDF : qualité des données, identifiants, valeurs manquantes, analyses univariée et bivariée, graphiques, interprétations (p-value exacte, seuil de 5 %) et résumé final.

## Installation

```r
install.packages("devtools")
devtools::install_github("alinkpessostalec9-creator/autoreportr")
library(autoreportr)
```

## Utilisation

```r
auto_report(iris, title = "Analyse des iris", author = "Stalec")

auto_report(iris,
            title = "Analyse des iris",
            author = "Stalec",
            key = "gsk_...",
            type = "word",
            analyse = "ub",
            ref_var = "Species",
            graph = TRUE)

auto_report_app()
```

| Paramètre | Valeurs |
|---|---|
| `key` | Clé Groq personnelle, facultative. [Créer ma clé](https://console.groq.com/keys) |
| `type` | `"html"`, `"word"`, `"pdf"` ou plusieurs avec `c()` |
| `analyse` | `"u"` univariée, `"b"` bivariée, `"ub"` les deux |
| `ref_var` | Variable de croisement, ou `"all"` pour toutes les paires |
| `graph` | `FALSE`, `TRUE`, ou un choix parmi `"hist"`, `"box"`, `"bar"`, `"scatter"` |
| `vars` | Variables à analyser. Un identifiant cité ici est analysé |

Sans clé, le rapport utilise des interprétations automatiques. Avec une clé, seuls des résultats agrégés sont envoyés à Groq, jamais les lignes de la base. Le PDF nécessite Google Chrome ou Microsoft Edge.

## Crédits

- **Auteur :** ALINKPESSO Dona Ezéchiel Stalec
- **Collaborateur :** DAH SEGBE Jules
- **Partenaires :** CRISTAL et BUE-ENSPD (École Nationale de Statistique, de Planification et de Démographie), Université de Parakou
- **Contact :** alinkpessostalec9@gmail.com, WhatsApp +229 01 58 97 18 43

Licence MIT.
