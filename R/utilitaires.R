# Fonctions utilitaires internes
AR_VERSION <- "3.0.0"
AR_AUTEUR <- "ALINKPESSO Dona Ezéchiel Stalec"
AR_CONTACT <- "alinkpessostalec9@gmail.com | WhatsApp : +229 01 58 97 18 43"
AR_GROQ_LIEN <- "https://console.groq.com/keys"

ar_esc <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

fmt_num <- function(x, d = 2) {
  vapply(x, function(v) {
    if (is.null(v) || is.na(v)) return("-")
    if (is.infinite(v)) return(if (v > 0) "Inf" else "-Inf")
    formatC(round(v, d), format = "f", digits = d, big.mark = " ",
            decimal.mark = ",", drop0trailing = TRUE)
  }, character(1), USE.NAMES = FALSE)
}

fmt_pct <- function(x, d = 1) paste0(fmt_num(x, d), " %")

fmt_p <- function(p) {
  if (is.na(p)) return("-")
  if (p == 0) return("p < 1e-300")
  if (p < 0.001) return(paste0("p = ", sub(".", ",", formatC(p, format = "e", digits = 2), fixed = TRUE)))
  paste0("p = ", sub(".", ",", format(signif(p, 3), scientific = FALSE), fixed = TRUE))
}

date_fr <- function(d = Sys.Date()) {
  mois <- c("janvier", "février", "mars", "avril", "mai", "juin", "juillet",
            "août", "septembre", "octobre", "novembre", "décembre")
  paste(as.integer(format(d, "%d")), mois[as.integer(format(d, "%m"))], format(d, "%Y"))
}

ecrire_utf8 <- function(lignes, chemin) {
  con <- file(chemin, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(paste0(paste(lignes, collapse = "\n"), "\n"))), con)
  invisible(chemin)
}

ar_auteur_defaut <- function() {
  a <- Sys.getenv("USERNAME")
  if (!nzchar(a)) a <- Sys.getenv("USER")
  a
}
