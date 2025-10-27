# docentes_lf/scripts/3_export.R
# --------------------------------------------------------------
message("==> Export Docentes LF iniciado")

# Fallbacks por si en el master usaste otros nombres:
if (!exists("temp_creds_file") && exists("creds_json")) temp_creds_file <- creds_json
if (!exists("id_alertas")      && exists("sheet_id"))    id_alertas    <- sheet_id

# Validaciones
if (!exists("alertas")) stop("No existe 'alertas'. Corre 2_auditoria.R antes del export.")
if (!exists("data"))    stop("No existe 'data'. Corre 1_import.R antes del export.")
if (!exists("salones_docentes")) stop("No existe 'salones_docentes'. Corre 2_auditoria.R antes del export.")
if (!nzchar(id_alertas)) stop("id_alertas vacío.")
if (!file.exists(temp_creds_file)) stop("No existe temp_creds_file: ", temp_creds_file)

# --- Auth + logs ---
suppressMessages({
  googledrive::drive_auth(path = temp_creds_file, cache = ".secrets")
  googlesheets4::gs4_auth(path = temp_creds_file)
})
message(sprintf("🔐 Cred file: %s (exists=%s)", temp_creds_file, file.exists(temp_creds_file)))

u <- tryCatch(googlesheets4::gs4_user(), error = function(e) NULL)
if (is.null(u)) {
  message("👤 gs4_user: <desautenticado>")
} else if (is.list(u)) {
  em <- tryCatch(as.character(u$email), silent = TRUE); if (inherits(em, "try-error")) em <- NA_character_
  dn <- tryCatch(as.character(u$display_name), silent = TRUE); if (inherits(dn, "try-error")) dn <- NA_character_
  message("👤 gs4_user: ", if (!is.na(em)) em else "<sin-email>",
          if (!is.na(dn)) paste0(" (", dn, ")") else "")
} else {
  # cuando gs4_user() devuelve un vector atómico (p.ej. solo el email)
  message("👤 gs4_user: ", as.character(u))
}


# Helper export
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

# Pestañas
TAB_ALERTAS <- "alertas_docentes"
TAB_RAW     <- "data_docentes_raw"
TAB_RESUMEN <- "resumen_docentes"
TAB_SALONES <- "salones_docentes"

# Resumen KPIs
val_sum <- function(df, col) if (is.data.frame(df) && col %in% names(df)) sum(df[[col]], na.rm = TRUE) else NA_integer_
resumen_doc <- tibble::tibble(
  fecha_export   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  n_encuestas    = nrow(alertas),
  exitos         = val_sum(alertas, "Exitos"),
  alertas        = val_sum(alertas, "Alertas"),
  rechazos       = val_sum(alertas, "Rechazos"),
  tiempos_mas    = val_sum(alertas, "flag_duration_mas"),
  tiempos_menos  = val_sum(alertas, "flag_duration_menos"),
  duplicados     = val_sum(alertas, "flag_duplicated"),
  faltantes      = val_sum(alertas, "flag_missing"),
  extremos       = val_sum(alertas, "flag_extreme_values"),
  exceso_ns      = val_sum(alertas, "flag_ns")
)

# Export
export_sheet(alertas,          sheet, TAB_ALERTAS, label = "alertas docentes",   pause = 1)
export_sheet(data,             sheet, TAB_RAW,     label = "datos crudos",       pause = 1)
export_sheet(resumen_doc,      sheet, TAB_RESUMEN, label = "resumen",            pause = 1)
export_sheet(salones_docentes, sheet, TAB_SALONES, label = "salones_docentes",   pause = 1)

message("✅ Export Docentes LF finalizado.")
