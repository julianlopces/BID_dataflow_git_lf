#### Script BID LF --------------------------------------------------------------
# Proyecto: BID LF
# Este script crea las alertas de auditoría y exporta los resultados a Google Sheets

rm(list = ls())

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(
  dplyr, tidyr, httr, jsonlite,
  googledrive, googlesheets4, writexl,
  haven, stringr, labelled, lubridate,
  gtsummary, dotenv, readxl, tibble, sf, purrr
)

project_path <- getwd()
message("Directorio base: ", project_path)

# Helper para validar env vars
is_blank <- function(x) is.na(x) || !nzchar(x)

if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  message("Cargando credenciales desde secretos en GitHub Actions...")
  server  <- Sys.getenv("SERVIDOR")
  password <- Sys.getenv("PASSWORD")
  email    <- Sys.getenv("EMAIL")
  formid   <- Sys.getenv("FORMID")
  creds    <- Sys.getenv("GOOGLE_SHEETS_CREDENTIALS")
  id_alertas <- Sys.getenv("IDALERTAS")
  
  
  # Validación temprana
  if (any(vapply(c(server, password, email, formid, creds,id_alertas), is_blank, logical(1)))) {
    stop("Faltan credenciales requeridas en variables de entorno de Actions.")
  }
  
  # Escribir JSON a archivo temporal
  temp_creds_file <- tempfile(fileext = ".json")
  writeLines(creds, temp_creds_file)
  
  # Autenticación con cuenta de servicio (sin prompts)
  googledrive::drive_auth(path = temp_creds_file, cache = ".secrets")
  googlesheets4::gs4_auth(path = temp_creds_file)
  
  
} else {
  # Local
  if (file.exists(".env")) {
    message("Archivo .env encontrado en: ", project_path)
    dotenv::load_dot_env(".env")
    server   <- Sys.getenv("SERVIDOR")
    password <- Sys.getenv("PASSWORD")
    email    <- Sys.getenv("EMAIL")
    formid   <- Sys.getenv("FORMID")
    temp_creds_file <- Sys.getenv("GOOGLE_SHEETS_CREDENTIALS") # ruta a JSON local
    id_alertas <- Sys.getenv("IDALERTAS")
    
    if (any(vapply(c(server, password, email, formid, temp_creds_file,id_alertas), is_blank, logical(1)))) {
      stop("Faltan credenciales requeridas en .env (o la ruta del JSON).")
    }
    
    googledrive::drive_auth(path = temp_creds_file, cache = ".secrets")
    googlesheets4::gs4_auth(path = temp_creds_file)
    
  } else {
    stop("No se encontró .env. Configúralo o exporta variables de entorno.")
  }
}

message("Credenciales cargadas correctamente.")
message("- Email Google Sheets: ", email)

# Función para cargar scripts secundarios
load_script <- function(script_name) {
  script_path <- file.path(project_path, "scripts", script_name)
  if (file.exists(script_path)) {
    message("Ejecutando script: ", script_name)
    source(script_path)
  } else {
    stop(paste("No se encontró el script:", script_path))
  }
}

# Ejecutar scripts secundarios en orden
load_script("Import_data_BID_LF.R")
load_script("Alertas_data_BID_LF.R")
load_script("Export_data_BID_LF.R")

message("Pipeline completado exitosamente.")
