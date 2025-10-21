# docentes_lf/master_docentes_lf.R
# --------------------------------------------------------------
# Master: Docentes – Línea Final
# --------------------------------------------------------------

rm(list = ls())
options(stringsAsFactors = FALSE)

# Paquetes base
pkgs <- c(
  "dotenv", "dplyr", "httr", "jsonlite",
  "readxl", "stringr", "tidyr", "purrr", "lubridate"
  # "googledrive", "googlesheets4"  # <-- los activamos al hacer el export
)
invisible(lapply(pkgs, function(p) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)))
invisible(lapply(pkgs, library, character.only = TRUE))

project_path <- getwd()
message("Directorio base: ", project_path)

# -----------------------------
# Cargar credenciales (.env)
# -----------------------------
dotenv::load_dot_env(".env")

# Tomar credenciales *propias* de Docentes si existen; sino, genéricas
email    <- if (nzchar(Sys.getenv("EMAIL_DOCENTES")))    Sys.getenv("EMAIL_DOCENTES")    else Sys.getenv("EMAIL")
password <- if (nzchar(Sys.getenv("PASSWORD_DOCENTES"))) Sys.getenv("PASSWORD_DOCENTES") else Sys.getenv("PASSWORD")

# Servidor y FormID de Docentes
server <- if (nzchar(Sys.getenv("SERVIDOR"))) Sys.getenv("SERVIDOR") else Sys.getenv("SERVER")
formid <- if (nzchar(Sys.getenv("FORMID_DOCENTES"))) Sys.getenv("FORMID_DOCENTES") else Sys.getenv("FORMID_DOC_LF")

# (Para el export más adelante)
creds_json <- Sys.getenv("GOOGLE_SHEETS_CREDENTIALS")
sheet_id   <- Sys.getenv("IDALERTAS")

# Validación mínima
stopifnot(nzchar(email), nzchar(password), nzchar(server), nzchar(formid))

message("Credenciales cargadas.")
message(sprintf(" - Server: %s | FormID (Docentes): %s", server, formid))
message(sprintf(" - Usuario SurveyCTO: %s", email))

# -----------------------------
# Utilidades y ejecución
# -----------------------------
source("R/utils_api.R")

# (Cuando pasemos al export, descomenta estas dos líneas si usas service account)
# if (file.exists(creds_json)) {
#   googledrive::drive_auth(path = creds_json, cache = ".secrets")
#   googlesheets4::gs4_auth(path = creds_json)
# }

# Ejecutar scripts del módulo Docentes
source("docentes_lf/scripts/1_import.R")
# source("docentes_lf/scripts/2_auditoria.R")
# source("docentes_lf/scripts/3_export.R")  

message("Master Docentes LF completado.")
