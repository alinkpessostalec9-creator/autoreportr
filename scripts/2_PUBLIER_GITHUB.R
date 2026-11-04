# ============================================
# AUTOREPORTR 3.0.0 - MISE À JOUR DE GITHUB
# Met à jour le dépôt existant depuis F:/R Fan/AutoreportR.
# Rien n'est supprimé ni réimporté : l'historique et le lien restent les mêmes.
# Lance d'abord 0_TESTER_AVANT_DEPLOIEMENT.R.
# ============================================

dossier <- Sys.getenv("AUTOREPORTR_DIR", "F:/R Fan/AutoreportR")
depot <- Sys.getenv("AUTOREPORTR_REMOTE", "https://github.com/alinkpessostalec9-creator/autoreportr.git")
version <- "v3.0.0"
setwd(dossier)

git <- function(..., arret = TRUE) {
  args <- c(...)
  cat("\n> git", args, "\n")
  code <- system2("git", shQuote(args))
  if (arret && code != 0) stop("Arrêt : la commande git a échoué. Rien de plus n'a été envoyé.", call. = FALSE)
  invisible(code)
}

# 1. Relier ce nouveau dossier au dépôt existant (première fois seulement)
if (!dir.exists(".git")) {
  git("init")
  git("symbolic-ref", "HEAD", "refs/heads/main")
  git("remote", "add", "origin", depot)
  git("fetch", "origin", "main", "--tags")
  git("reset", "origin/main")
} else {
  git("fetch", "origin", "main", "--tags")
}
git("config", "user.name", "alinkpessostalec9")
git("config", "user.email", "alinkpessostalec9@gmail.com")

# 2. Bloquer l'envoi si une clé Groq reste dans un fichier
fichiers <- list.files(".", recursive = TRUE, all.files = TRUE, full.names = TRUE)
fichiers <- fichiers[!grepl("^\\./(\\.git|rapports_test)/", fichiers) & !grepl("\\.(docx|pdf|png)$", fichiers)]
avec_cle <- fichiers[vapply(fichiers, function(f) {
  any(grepl("gsk_[A-Za-z0-9]{20,}", readLines(f, warn = FALSE, encoding = "UTF-8"), useBytes = TRUE))
}, logical(1))]
if (length(avec_cle) > 0) stop("Clé Groq trouvée dans : ", paste(avec_cle, collapse = ", "), ". Efface-la avant de publier.", call. = FALSE)

# 3. Voir ce qui va changer
git("add", "-A")
git("status", "--short")

# 4. Envoyer la mise à jour, sans --force
git("commit", "-m", "AutoReportR v3.0.0 - cle Groq personnelle, Word et PDF, analyse bivariee, graphiques, plateforme", arret = FALSE)
git("push", "-u", "origin", "main")

# 5. Marquer la version
if (length(system2("git", c("tag", "--list", version), stdout = TRUE)) == 0) {
  git("tag", "-a", version, "-m", "Version 3.0.0")
  git("push", "origin", version)
}

cat("\nAutoReportR 3.0.0 est publié :\nhttps://github.com/alinkpessostalec9-creator/autoreportr\n")
cat('Installation : devtools::install_github("alinkpessostalec9-creator/autoreportr")\n')
