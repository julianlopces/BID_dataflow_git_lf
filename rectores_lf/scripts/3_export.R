# rectores_lf/scripts/3_export.R
# --------------------------------------------------------------
message("==> Export Rectores LF iniciado")

suppressMessages({
  if (!requireNamespace("googledrive", quietly = TRUE)) install.packages("googledrive")
  if (!requireNamespace("googlesheets4", quietly = TRUE)) install.packages("googlesheets4")
  library(googledrive)
  library(googlesheets4)
})

# ---- Fallbacks / variables esperadas ----
# damos backups por si acaso
if (!exists("temp_creds_file") || !nzchar(temp_creds_file)) {
  temp_creds_file <- Sys.getenv("GOOGLE_SHEETS_CREDENTIALS")
}
# Para el ID del Google Sheets de rectores:
# En tu master lo pusiste como id_alertas <- Sys.getenv("IDALERTASRE")
# Respetamos 'id_alertas' si existe; si no, probamos con la env var.
if (!exists("id_alertas") || !nzchar(id_alertas)) {
  id_alertas <- Sys.getenv("IDALERTASRE")
}

# ---- Validaciones mínimas ----
if (!exists("alertas")) stop("No existe 'alertas'. Corre 2_auditoria.R antes del export.")
if (!nzchar(id_alertas)) stop("id_alertas vacío (revisa .env: IDALERTASRE).")
if (!file.exists(temp_creds_file)) stop("No existe credencial: ", temp_creds_file)

# ---- Auth + log de usuario ----
suppressMessages({
  googledrive::drive_auth(path = temp_creds_file, cache = ".secrets")
  googlesheets4::gs4_auth(path = temp_creds_file)
})

u <- tryCatch(googlesheets4::gs4_user(), error = function(e) NULL)
if (is.null(u)) {
  message("👤 gs4_user: <desautenticado>")
} else if (is.list(u)) {
  em <- tryCatch(as.character(u$email), silent = TRUE); if (inherits(em, "try-error")) em <- NA_character_
  dn <- tryCatch(as.character(u$display_name), silent = TRUE); if (inherits(dn, "try-error")) dn <- NA_character_
  message("👤 gs4_user: ", if (!is.na(em)) em else "<sin-email>",
          if (!is.na(dn)) paste0(" (", dn, ")") else "")
} else {
  message("👤 gs4_user: ", as.character(u))
}

# ---- Abrir el spreadsheet destino ----
sheet_ss <- tryCatch(
  googlesheets4::gs4_get(trimws(id_alertas)),
  error = function(e) stop("No pude abrir id_alertas: ", conditionMessage(e))
)
message("📄 Destino: ", sheet_ss$spreadsheet_id)
message("🔗 URL destino: ", sheet_ss$browser_url)

# ---- Helper de export ----
export_sheet <- function(df, ss, sheet_name, label = sheet_name, pause = 0) {
  message(sprintf("⬆️  Exportando %s → pestaña '%s'...", label, sheet_name))
  tryCatch({
    googlesheets4::sheet_write(df, ss = ss, sheet = sheet_name)
  }, error = function(e) {
    stop(sprintf("Error al exportar %s: %s", label, conditionMessage(e)))
  })
  if (pause > 0) Sys.sleep(pause)
  ok <- tryCatch({
    xr <- googlesheets4::range_read(ss, sheet = sheet_name, range = "A1:B2", col_types = "c")
    nrow(xr) >= 1
  }, error = function(e) FALSE)
  message(sprintf("✅ Verificación %s: %s", sheet_name, if (ok) "OK" else "FALLÓ (no se pudo leer)"))
  if (pause > 0) Sys.sleep(pause)
}

# ---- Exportar solo la tabla de auditoría ----
TAB_ALERTAS <- "alertas"   # nombre de pestaña para rectores

export_sheet(alertas, sheet_ss, TAB_ALERTAS, label = "auditoría rectores", pause = 1)

# Exportar tabla por colegios
if (exists("colegios_rectores") && is.data.frame(colegios_rectores)) {
  export_sheet(colegios_rectores, sheet_ss, "colegios_rectores", label = "colegios_rectores", pause = 1)
}

message("✅ Export Rectores LF finalizado.")
