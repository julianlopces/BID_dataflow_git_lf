# docentes_lf/scripts/1_import.R
# --------------------------------------------------------------
# Import Docentes LF
# --------------------------------------------------------------

message("==> Import Docentes LF iniciado")

safe_lib(c("dplyr","stringr","tidyr","purrr"))

# 1) Descargar datos desde SurveyCTO --------------------------------------
data_raw <- scto_download_json_wide(server, formid, email, password, since = 0)
message("Descarga completa. Filas: ", nrow(data_raw), " | Cols: ", ncol(data_raw))

# 2) Limpieza básica -------------------------------------------------------
data <- data_raw %>%
  mutate(across(where(is.character), ~ stringr::str_squish(.)))

# Filtros para usuarios
if ("username" %in% names(data)) data <- data %>% filter(!username %in% c("anonymousUser"))
if ("deviceid" %in% names(data)) data <- data %>% filter(deviceid != "(web)")

# 3) Construcción de nombre completo --------------------------------------
# Definimos ayuditas para limpiar valores "vacíos" o sentinelas
to_na <- function(x) {
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "N/A")] <- NA_character_
  x
}
is_9999_like <- function(x) x %in% c("9999","NO TIENE","No Tiene","NO","no tiene","99")

# Normalizamos campos base (si no existen, los creamos como NA para no romper)
need <- c("teacher_name_1","teacher_name_2","teacher_lastname_1","teacher_lastname_2",
          "teacher_name_1_99","teacher_name_2_99","teacher_lastname_1_99","teacher_lastname_2_99",
          "teacher_fullname2")
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
    nombre_b1 = dplyr::case_when(
      is.na(teacher_name_2)  &  is.na(teacher_lastname_1) &  is.na(teacher_lastname_2) ~ teacher_name_1,
      is.na(teacher_name_2)  & !is.na(teacher_lastname_1) &  is.na(teacher_lastname_2) ~ stringr::str_c(teacher_name_1, teacher_lastname_1, sep = " "),
      is.na(teacher_name_2)  &  is.na(teacher_lastname_1) & !is.na(teacher_lastname_2) ~ stringr::str_c(teacher_name_1, teacher_lastname_2, sep = " "),
      is.na(teacher_name_2)  & !is.na(teacher_lastname_1) & !is.na(teacher_lastname_2) ~ stringr::str_c(teacher_name_1, teacher_lastname_1, teacher_lastname_2, sep = " "),
      !is.na(teacher_name_2)  &  is.na(teacher_lastname_1) &  is.na(teacher_lastname_2) ~ stringr::str_c(teacher_name_1, teacher_name_2, sep = " "),
      !is.na(teacher_name_2)  & !is.na(teacher_lastname_1) &  is.na(teacher_lastname_2) ~ stringr::str_c(teacher_name_1, teacher_name_2, teacher_lastname_1, sep = " "),
      !is.na(teacher_name_2)  &  is.na(teacher_lastname_1) & !is.na(teacher_lastname_2) ~ stringr::str_c(teacher_name_1, teacher_name_2, teacher_lastname_2, sep = " "),
      TRUE ~ stringr::str_c(teacher_name_1, teacher_name_2, teacher_lastname_1, teacher_lastname_2, sep = " ")
    ),
    
    # bloque 2 (_99) como fallback si el b1 no está usable
    nombre_b2 = dplyr::case_when(
      is.na(teacher_name_2_99) &  is.na(teacher_lastname_1_99) &  is.na(teacher_lastname_2_99) ~ teacher_name_1_99,
      is.na(teacher_name_2_99) & !is.na(teacher_lastname_1_99) &  is.na(teacher_lastname_2_99) ~ stringr::str_c(teacher_name_1_99, teacher_lastname_1_99, sep = " "),
      is.na(teacher_name_2_99) &  is.na(teacher_lastname_1_99) & !is.na(teacher_lastname_2_99) ~ stringr::str_c(teacher_name_1_99, teacher_lastname_2_99, sep = " "),
      is.na(teacher_name_2_99) & !is.na(teacher_lastname_1_99) & !is.na(teacher_lastname_2_99) ~ stringr::str_c(teacher_name_1_99, teacher_lastname_1_99, teacher_lastname_2_99, sep = " "),
      !is.na(teacher_name_2_99) &  is.na(teacher_lastname_1_99) &  is.na(teacher_lastname_2_99) ~ stringr::str_c(teacher_name_1_99, teacher_name_2_99, sep = " "),
      !is.na(teacher_name_2_99) & !is.na(teacher_lastname_1_99) &  is.na(teacher_lastname_2_99) ~ stringr::str_c(teacher_name_1_99, teacher_name_2_99, teacher_lastname_1_99, sep = " "),
      !is.na(teacher_name_2_99) &  is.na(teacher_lastname_1_99) & !is.na(teacher_lastname_2_99) ~ stringr::str_c(teacher_name_1_99, teacher_name_2_99, teacher_lastname_2_99, sep = " "),
      TRUE ~ stringr::str_c(teacher_name_1_99, teacher_name_2_99, teacher_lastname_1_99, teacher_lastname_2_99, sep = " ")
    ),
    
    nombre_b1 = dplyr::if_else(stringr::str_squish(nombre_b1) == "", NA_character_, stringr::str_squish(nombre_b1)),
    nombre_b2 = dplyr::if_else(stringr::str_squish(nombre_b2) == "", NA_character_, stringr::str_squish(nombre_b2))
  )

# nombre final: PRIORIDAD 1 = teacher_fullname2 (si existe y no es 99/NA); 
#               PRIORIDAD 2 = bloque 1; PRIORIDAD 3 = bloque 2
data <- data %>%
  mutate(
    tf2_clean = dplyr::na_if(stringr::str_squish(teacher_fullname2), ""),
    tf2_clean = dplyr::if_else(tf2_clean %in% c("99","9999"), NA_character_, tf2_clean),
    teacher_fullname = stringr::str_to_upper(dplyr::coalesce(tf2_clean, nombre_b1, nombre_b2)),
    teacher_fullname = dplyr::if_else(is.na(teacher_fullname) | stringr::str_squish(teacher_fullname) == "",
                                      NA_character_, teacher_fullname)
  ) %>%
  select(-nombre_b1, -nombre_b2, -tf2_clean)

# 4) ID unificado de docente ---------------------------------------------------
# id_docentes_lf = "nombre_con_guiones_bajos" + "_" + "teacher_school"
# (normalizamos acentos y caracteres no alfanuméricos)
slugify <- function(x) {
  x <- as.character(x)
  x <- iconv(x, to = "ASCII//TRANSLIT")                  # quita acentos
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "_")    # todo lo que no sea a-z0-9 -> "_"
  x <- stringr::str_replace_all(x, "^_+|_+$", "")        # bordes
  x <- stringr::str_replace_all(x, "_{2,}", "_")         # dobles
  x
}

data <- data %>%
  mutate(
    teacher_school = as.character(teacher_school),
    nombre_slug    = slugify(teacher_fullname),
    colegio_slug   = slugify(teacher_school),
    id_docentes_lf = dplyr::if_else(!is.na(nombre_slug) & nombre_slug != "" &
                                      !is.na(colegio_slug) & colegio_slug != "",
                                    paste0(nombre_slug, "_", colegio_slug),
                                    NA_character_)
  ) %>%
  select(-nombre_slug, -colegio_slug)

# 5) Guardado --------------------------------------------------------------
if (!dir.exists("docentes_lf/data")) dir.create("docentes_lf/data", recursive = TRUE)
tag <- format(Sys.time(), "%Y%m%d_%H%M")
write.csv(data, file = file.path("docentes_lf","data", paste0("docentes_LF_raw_", tag, ".csv")), row.names = FALSE)
saveRDS(data,  file = file.path("docentes_lf","data", paste0("docentes_LF_raw_", tag, ".rds")))

message("==> Import Docentes LF finalizado")

