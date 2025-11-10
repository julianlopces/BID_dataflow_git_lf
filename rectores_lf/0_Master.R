# rectores_lf/0_Master.R
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, httr, jsonlite, googledrive, googlesheets4,
       writexl, haven, stringr, labelled, lubridate, gtsummary, dotenv)

# === Rutas ===
SCRIPTS_DIR <- "scripts"                      # <- estás dentro de rectores_lf
IMP <- file.path(SCRIPTS_DIR, "1_import.R")
AUD <- file.path(SCRIPTS_DIR, "2_auditoria.R")
EXP <- file.path(SCRIPTS_DIR, "3_export.R")

# 1) .env
dotenv::load_dot_env(".env")

server          <- Sys.getenv("SERVIDOR")
email           <- Sys.getenv("EMAIL_DOCENTES", unset = Sys.getenv("EMAIL"))
password        <- Sys.getenv("PASSWORD_DOCENTES", unset = Sys.getenv("PASSWORD"))
formid          <- Sys.getenv("FORMID_RECTORES")
temp_creds_file <- Sys.getenv("GOOGLE_SHEETS_CREDENTIALS")  
id_alertas      <- Sys.getenv("IDALERTASRE")

assign("server",          server,          .GlobalEnv)
assign("email",           email,           .GlobalEnv)
assign("password",        password,        .GlobalEnv)
assign("formid",          formid,          .GlobalEnv)
assign("temp_creds_file", temp_creds_file, .GlobalEnv)
assign("id_alertas",      id_alertas,      .GlobalEnv)

# 2) Auth Google una sola vez
googledrive::drive_auth(path = temp_creds_file, cache = ".secrets")
googlesheets4::gs4_auth(path = temp_creds_file)

# 3) Utilidades (desde la raíz del repo)
UTILS <- "R/utils_api.R"   # <- ojo: sube un nivel porque estás en rectores_lf/
if (!file.exists(UTILS)) stop("No se encontró ", UTILS, ". Verifica la ruta.")
source(UTILS, local = .GlobalEnv, chdir = FALSE)

# Helper run
run <- function(path){
  if (!file.exists(path)) stop("No existe: ", path)
  message(">> Ejecutando: ", path)
  source(path, local = .GlobalEnv, chdir = FALSE)
}

# 4) Pipeline
run(IMP); if (!exists("data")    || !is.data.frame(data))    stop("El import no generó `data`.")
run(AUD); if (!exists("alertas") || !is.data.frame(alertas)) stop("La auditoría no generó `alertas`.")
run(EXP)

message("✅ Pipeline rectores LF completado.")
