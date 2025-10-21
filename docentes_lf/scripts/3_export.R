# docentes_lf/scripts/3_export.R
# --------------------------------------------------------------
# Export Docentes LF -> Google Sheets (Looker)
# Requiere objetos:
# --------------------------------------------------------------

message("==> Export Docentes LF iniciado")

# Fallbacks por si en el master usaste otros nombres:
if (!exists("temp_creds_file") && exists("creds_json")) temp_creds_file <- creds_json
if (!exists("id_alertas")      && exists("sheet_id"))    id_alertas    <- sheet_id

# Validaciones
if (!exists("temp_creds_file") || !nzchar(temp_creds_file) || !file.exists(temp_creds_file)) {
  stop("No se encontraron las credenciales de Google Sheets (temp_creds_file). Cárgalas en el master.")
}
if (!exists("id_alertas") || !nzchar(id_alertas)) {
  stop("No se encontró el ID del Google Sheet (id_alertas). Cárgalo en el master.")
}
if (!exists("alertas")) stop("No existe 'alertas'. Corre 2_auditoria.R antes del export.")
if (!exists("data"))    stop("No existe 'data'. Corre 1_import.R antes del export.")

if (!is.data.frame(alertas)) {
  stop("`alertas` no es un data.frame/tibble. Corre 2_auditoria.R en esta sesión antes del export.")
}

# Autenticación 
suppressMessages({
  googledrive::drive_auth(path = temp_creds_file, cache = ".secrets")
  googlesheets4::gs4_auth(path = temp_creds_file)
})

# Abrir el spreadsheet
sheet <- tryCatch({
  googlesheets4::gs4_get(id_alertas)
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", conditionMessage(e))
})

# Helper genérico
export_sheet <- function(df, ss, sheet_name, label = sheet_name, pause = 0) {
  message(sprintf("Exportando %s...", label))
  tryCatch({
    googlesheets4::sheet_write(df, ss = ss, sheet = sheet_name)
    message(sprintf("Datos de %s exportados correctamente.", label))
  }, error = function(e) {
    stop(sprintf("Error al exportar %s: %s", label, conditionMessage(e)))
  })
  if (pause > 0) Sys.sleep(pause)
}

# ---- Tablas a exportar ----
# Nombres de pestañas (ajústalos si quieres)
TAB_ALERTAS <- "alertas_docentes"
TAB_RAW     <- "data_docentes_raw"
TAB_RESUMEN <- "resumen_docentes"

# Resumen KPIs (coje banderas si existen; si no, 0)
`%||%` <- function(x, y) if (is.null(x)) y else x

val_sum <- function(df, col) {
  if (is.data.frame(df) && col %in% names(df)) sum(df[[col]], na.rm = TRUE) else NA_integer_
}

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

# ---- Exportar ----
export_sheet(alertas,     sheet, TAB_ALERTAS, label = "alertas docentes", pause = 3)
export_sheet(data,        sheet, TAB_RAW,     label = "datos crudos",     pause = 3)
export_sheet(resumen_doc, sheet, TAB_RESUMEN, label = "resumen",          pause = 2)

message("✅ Export Docentes LF finalizado.")

