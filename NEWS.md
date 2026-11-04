# autoreportr 3.0.0

- Chaque utilisateur fournit sa propre clé Groq avec `key`. Aucune clé n'est incluse dans le package.
- Interprétations automatiques sans clé ou si l'IA ne répond pas, et résumé final en un paragraphe.
- Rapports en HTML, Word et PDF avec `type`.
- Analyse univariée et bivariée avec `analyse` et `ref_var`.
- Graphiques au choix avec `graph`, désactivés par défaut.
- Détection et exclusion des identifiants avec `detect_id_vars()`.
- Plateforme interactive avec `auto_report_app()`.
