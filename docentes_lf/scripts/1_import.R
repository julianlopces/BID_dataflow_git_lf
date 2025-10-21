# docentes_lf/scripts/1_import.R
# --------------------------------------------------------------
# Import Docentes LF (SurveyCTO -> R)  |  sin plantilla
# - Descarga wide JSON
# - Limpia strings y pilotos básicos
# - Construye nombre completo del docente a partir de:
#   teacher_name_1, teacher_name_2, teacher_lastname_1, teacher_lastname_2
#   y su segundo bloque *_99 (fallback)
# - Guarda CSV y RDS en docentes_lf/data/
# --------------------------------------------------------------

message("==> Import Docentes LF iniciado")

safe_lib(c("dplyr","stringr","tidyr","purrr"))

# 1) Descargar datos desde SurveyCTO --------------------------------------
data_raw <- scto_download_json_wide(server, formid, email, password, since = 0)
message("Descarga completa. Filas: ", nrow(data_raw), " | Cols: ", ncol(data_raw))

# 2) Limpieza básica -------------------------------------------------------
data <- data_raw %>%
  mutate(across(where(is.character), ~ str_squish(.)))

# Filtros para usuarios
if ("username" %in% names(data)) data <- data %>% filter(!username %in% c("anonymousUser"))
if ("deviceid" %in% names(data)) data <- data %>% filter(deviceid != "(web)")

# 3) Construcción de nombre completo --------------------------------------
# Definimos ayuditas para limpiar valores "vacíos" o sentinelas
to_na <- function(x) {
  x <- str_squish(x)
  x[x %in% c("", "NA", "N/A")] <- NA_character_
  x
}
is_9999_like <- function(x) x %in% c("9999","NO TIENE","No Tiene","NO","no tiene")

# Normalizamos campos base (si no existen, los creamos como NA para no romper)
need <- c("teacher_name_1","teacher_name_2","teacher_lastname_1","teacher_lastname_2",
          "teacher_name_1_99","teacher_name_2_99","teacher_lastname_1_99","teacher_lastname_2_99")
for (v in need) if (!v %in% names(data)) data[[v]] <- NA_character_

data <- data %>%
  mutate(
    # limpiar y pasar vacíos a NA
    across(all_of(need), ~ to_na(as.character(.))),
    
    # marcar sentinelas "9999"/"NO TIENE"/etc. como NA solo donde corresponde
    teacher_name_2        = if_else(is_9999_like(teacher_name_2),        NA_character_, teacher_name_2),
    teacher_lastname_2    = if_else(is_9999_like(teacher_lastname_2),    NA_character_, teacher_lastname_2),
    teacher_name_2_99     = if_else(is_9999_like(teacher_name_2_99),     NA_character_, teacher_name_2_99),
    teacher_lastname_2_99 = if_else(is_9999_like(teacher_lastname_2_99), NA_character_, teacher_lastname_2_99),
    
    # bloque 1: nombre concatenado (Nombres + Apellidos)
    nombre_b1 = case_when(
      is.na(teacher_name_2)     &  is.na(teacher_lastname_1) &  is.na(teacher_lastname_2) ~ teacher_name_1,
      is.na(teacher_name_2)     & !is.na(teacher_lastname_1) &  is.na(teacher_lastname_2) ~ str_c(teacher_name_1, teacher_lastname_1, sep = " "),
      is.na(teacher_name_2)     &  is.na(teacher_lastname_1) & !is.na(teacher_lastname_2) ~ str_c(teacher_name_1, teacher_lastname_2, sep = " "),
      is.na(teacher_name_2)     & !is.na(teacher_lastname_1) & !is.na(teacher_lastname_2) ~ str_c(teacher_name_1, teacher_lastname_1, teacher_lastname_2, sep = " "),
      !is.na(teacher_name_2)     &  is.na(teacher_lastname_1) &  is.na(teacher_lastname_2) ~ str_c(teacher_name_1, teacher_name_2, sep = " "),
      !is.na(teacher_name_2)     & !is.na(teacher_lastname_1) &  is.na(teacher_lastname_2) ~ str_c(teacher_name_1, teacher_name_2, teacher_lastname_1, sep = " "),
      !is.na(teacher_name_2)     &  is.na(teacher_lastname_1) & !is.na(teacher_lastname_2) ~ str_c(teacher_name_1, teacher_name_2, teacher_lastname_2, sep = " "),
      TRUE ~ str_c(teacher_name_1, teacher_name_2, teacher_lastname_1, teacher_lastname_2, sep = " ")
    ),
    
    # bloque 2 (_99) como fallback si el b1 no está usable
    nombre_b2 = case_when(
      is.na(teacher_name_2_99)  &  is.na(teacher_lastname_1_99) &  is.na(teacher_lastname_2_99) ~ teacher_name_1_99,
      is.na(teacher_name_2_99)  & !is.na(teacher_lastname_1_99) &  is.na(teacher_lastname_2_99) ~ str_c(teacher_name_1_99, teacher_lastname_1_99, sep = " "),
      is.na(teacher_name_2_99)  &  is.na(teacher_lastname_1_99) & !is.na(teacher_lastname_2_99) ~ str_c(teacher_name_1_99, teacher_lastname_2_99, sep = " "),
      is.na(teacher_name_2_99)  & !is.na(teacher_lastname_1_99) & !is.na(teacher_lastname_2_99) ~ str_c(teacher_name_1_99, teacher_lastname_1_99, teacher_lastname_2_99, sep = " "),
      !is.na(teacher_name_2_99)  &  is.na(teacher_lastname_1_99) &  is.na(teacher_lastname_2_99) ~ str_c(teacher_name_1_99, teacher_name_2_99, sep = " "),
      !is.na(teacher_name_2_99)  & !is.na(teacher_lastname_1_99) &  is.na(teacher_lastname_2_99) ~ str_c(teacher_name_1_99, teacher_name_2_99, teacher_lastname_1_99, sep = " "),
      !is.na(teacher_name_2_99)  &  is.na(teacher_lastname_1_99) & !is.na(teacher_lastname_2_99) ~ str_c(teacher_name_1_99, teacher_name_2_99, teacher_lastname_2_99, sep = " "),
      TRUE ~ str_c(teacher_name_1_99, teacher_name_2_99, teacher_lastname_1_99, teacher_lastname_2_99, sep = " ")
    ),
    
    nombre_b1 = if_else(str_squish(nombre_b1) == "", NA_character_, str_squish(nombre_b1)),
    nombre_b2 = if_else(str_squish(nombre_b2) == "", NA_character_, str_squish(nombre_b2)),
    
    # nombre final: prioriza bloque 1, luego bloque 2
    teacher_fullname = str_to_upper(coalesce(nombre_b1, nombre_b2))
  ) %>%
  select(-nombre_b1, -nombre_b2) %>%
  mutate(
    teacher_fullname = if_else(is.na(teacher_fullname) | str_squish(teacher_fullname) == "",
                               NA_character_, teacher_fullname)
  )


# 5) Guardado --------------------------------------------------------------
if (!dir.exists("docentes_lf/data")) dir.create("docentes_lf/data", recursive = TRUE)
tag <- format(Sys.time(), "%Y%m%d_%H%M")
write.csv(data, file = file.path("docentes_lf","data", paste0("docentes_LF_raw_", tag, ".csv")), row.names = FALSE)
saveRDS(data,  file = file.path("docentes_lf","data", paste0("docentes_LF_raw_", tag, ".rds")))

message("==> Import Docentes LF finalizado")
