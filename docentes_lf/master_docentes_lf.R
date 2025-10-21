# master_docentes_lf.R
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, httr, jsonlite, googledrive, googlesheets4,
       writexl, haven, stringr, labelled, lubridate, gtsummary, dotenv)

SCRIPTS_DIR <- "docentes_lf/scripts"
IMP <- file.path(SCRIPTS_DIR, "1_import.R")
AUD <- file.path(SCRIPTS_DIR, "2_auditoria.R")
EXP <- file.path(SCRIPTS_DIR, "3_export.R")

# 1) Cargar credenciales
dotenv::load_dot_env(".env")   

server          <- Sys.getenv("SERVIDOR")
email           <- Sys.getenv("EMAIL_DOCENTES", unset = Sys.getenv("EMAIL"))
password        <- Sys.getenv("PASSWORD_DOCENTES", unset = Sys.getenv("PASSWORD"))
formid          <- Sys.getenv("FORMID_DOCENTES")
temp_creds_file <- Sys.getenv("GOOGLE_SHEETS_CREDENTIALS")
id_alertas      <- Sys.getenv("IDALERTAS")

# 2) Autenticación Google (una sola vez)
googledrive::drive_auth(path = temp_creds_file, cache = ".secrets")
googlesheets4::gs4_auth(path = temp_creds_file)

# 3) Cargar utilidades (safe_lib, scto_download_json_wide, normalize_multi, etc.)
UTILS <- "R/utils_api.R"
if (!file.exists(UTILS)) stop("No se encontró ", UTILS, ". Verifica la ruta.")
source(UTILS, local = .GlobalEnv, chdir = FALSE)

# Helper para ejecutar y mostrar
run <- function(path){
  if (!file.exists(path)) stop("No existe: ", path)
  message(">> Ejecutando: ", path)
  # OJO: no cambiamos el wd, para que las rutas relativas sigan desde la raíz del repo
  source(path, local = .GlobalEnv, chdir = FALSE)
}

# 4) Pipeline
run(IMP)
if (!exists("data") || !is.data.frame(data)) stop("El import no generó `data`.")

run(AUD)
if (!exists("alertas") || !is.data.frame(alertas)) stop("La auditoría no generó `alertas`.")

run(EXP)

message("✅ Pipeline docentes LF completado.")

