# docentes_lf/scripts/2_auditoria.R
# --------------------------------------------------------------
# Auditoría Docentes LF
# --------------------------------------------------------------

message("==> Auditoría Docentes LF iniciada")

safe_lib(c("dplyr","stringr","tidyr","purrr","readr"))

# 0) dataset base y para preservar orden original -----------------------------
if (!exists("data")) stop("No existe `data` desde el import. Corre 1_import primero.")
dataset <- data
cols_api_order <- names(dataset)   # orden original del import (API + tus nuevas de import)

# -- Merge con crudas_lb por teacher_fullname2 (shift/fifth) ------------------
safe_lib(c("readr","dplyr","stringr"))

# 1) Importar crudas
crudas_path <- "crudas_lb.csv"  # ajusta ruta si aplica
crudas <- readr::read_csv(crudas_path, show_col_types = FALSE)

# 2) Alinear llave como character en ambas
dataset$teacher_fullname2 <- as.character(dataset$teacher_fullname2)
crudas$teacher_fullname2  <- as.character(crudas$teacher_fullname2)

# 3) Columnas a jalar
cols_pull <- grep("^(teacher_shift_|teacher_fifth_)", names(crudas), value = TRUE)

if (length(cols_pull) > 0 && "teacher_fullname2" %in% names(crudas)) {
  # 4) Subconjunto y limpieza básica  (solo convertir ""->NA si la columna es character)
  crudas_sub <- crudas %>%
    dplyr::select(teacher_fullname2, dplyr::all_of(cols_pull)) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(cols_pull),
        ~ {
          v <- .
          if (is.character(v)) dplyr::na_if(v, "") else v
        }
      )
    ) %>%
    dplyr::filter(!is.na(teacher_fullname2))
  
  
  # 5) Asegurar que las columnas existan en dataset (crearlas con el tipo de crudas)
  for (cc in cols_pull) {
    if (!cc %in% names(dataset)) {
      if (cc %in% names(crudas_sub) && is.numeric(crudas_sub[[cc]])) {
        dataset[[cc]] <- as.numeric(NA)
      } else {
        dataset[[cc]] <- as.character(NA)
      }
    }
  }
  
  # 6) Join
  dataset <- dataset %>%
    left_join(crudas_sub, by = "teacher_fullname2", suffix = c("", "__cru"))
  
  # 7) Coalesce columna por columna, armonizando tipos
  for (cc in cols_pull) {
    cru_col <- paste0(cc, "__cru")
    if (!cru_col %in% names(dataset)) next
    
    lhs <- dataset[[cc]]
    rhs <- dataset[[cru_col]]
    
    lhs_num <- is.numeric(lhs)
    rhs_num <- is.numeric(rhs)
    
    if (lhs_num && rhs_num) {
      # ambos numéricos
      dataset[[cc]] <- dplyr::coalesce(rhs, lhs)
    } else {
      # hay mezcla de tipos -> coalesce como texto
      lhs_chr <- as.character(lhs)
      rhs_chr <- as.character(rhs)
      merged  <- dplyr::coalesce(rhs_chr, lhs_chr)
      
      # Si alguna de las dos columnas originales era numérica, intenta reconvertir
      if (lhs_num || rhs_num) {
        dataset[[cc]] <- suppressWarnings(as.numeric(merged))
      } else {
        dataset[[cc]] <- merged
      }
    }
    
    # eliminar columna temporal del join
    dataset[[cru_col]] <- NULL
  }
} else {
  message("No se encontraron columnas teacher_shift_/teacher_fifth_ en 'crudas_lb' o falta 'teacher_fullname2'. Merge omitido.")
}


# 1) Duración -----------------------------------------------------------------
alertas <- dataset

get_duration_min <- function(df){
  if ("duration" %in% names(df)) {
    as.numeric(df$duration)/60
  } else if (all(c("starttime","endtime") %in% names(df))) {
    as.numeric(difftime(lubridate::ymd_hms(df$endtime, quiet=TRUE),
                        lubridate::ymd_hms(df$starttime, quiet=TRUE),
                        units = "mins"))
  } else {
    rep(NA_real_, nrow(df))
  }
}

alertas <- alertas %>%
  mutate(duration_minutes = round(get_duration_min(cur_data()), 2))

# flags de duración por Z-score (3DE)
dur_vec <- alertas$duration_minutes
med_dur <- median(dur_vec, na.rm = TRUE)
sd_dur  <- sd(dur_vec, na.rm = TRUE)
if (!is.finite(sd_dur) || sd_dur == 0) sd_dur <- 1
alertas <- alertas %>%
  mutate(
    z_dur = (duration_minutes - med_dur) / sd_dur,
    flag_duration_mas   = if_else(!is.na(z_dur) & z_dur >  3, 1L, 0L),
    flag_duration_menos = if_else(!is.na(z_dur) & z_dur < -3, 1L, 0L)
  )

# 2) Missings -----------------------------------------------------------------
var_missing <- c(
  "teacher_loc","teacher_school",
  "historias","retos","juegos","intencion_implementacion",
  "filtro_lb",
  "gad_1","gad_2","gad_3","gad_4","gad_5","gad_6","gad_7",
  "phq_1","phq_2","phq_3","phq_4","phq_5","phq_6","phq_7","phq_8","phq_9",
  "neigh_lgtb","neigh_mig","neigh_ethnic","neigh_relig",
  "perpective_1","perpective_2","perpective_3",
  "poverty_cause",
  "migrantes_1","migrantes_2","migrantes_3","migrantes_4","migrantes_5","migrantes_6","migrantes_7","migrantes_8",
  "job_satisfaction1","job_satisfaction2","job_satisfaction4",
  "beliefs_intelligence1","gender_stereotypes1","beliefs_intelligence2","gender_stereotypes2",
  "discipline_strategies1","gender_stereotypes3","gender_stereotypes4",
  "student_expectations1","teacher_student_relations","classroom_management1","teaching_standards",
  "student_expectations2","discipline_strategies2","discipline_strategies3",
  "teaching_priorities_a","teaching_priorities_b","teaching_priorities_c","teaching_priorities_d","teaching_priorities_e",
  "teaching_strategies2","classroom_management2","educational_philosophy",
  "student_engagement1","student_engagement2","classroom_management3","teaching_strategies3",
  "perseverance",
  paste0("eyes_test_",1:36)
)
present <- intersect(var_missing, names(alertas))
if (length(present)){
  alertas <- alertas %>%
    mutate(across(all_of(present), ~ if_else(is.na(.x) | trimws(as.character(.x))=="", 1L, 0L),
                  .names = "m_{.col}"))
}

# Missings condicionados (solo si existen esas variables)
alertas <- alertas %>%
  mutate(
    sin_director = if ("teacher_director" %in% names(.)) is.na(teacher_director) | trimws(teacher_director)=="" else FALSE,
    
    m_teacher_country_o = if ("teacher_country" %in% names(.) && "teacher_country_o" %in% names(.))
      if_else(sin_director & is.na(teacher_country_o) & as.numeric(teacher_country) == 98, 1L, 0L) else NULL,
    
    m_teacher_state = if ("teacher_country" %in% names(.) && "teacher_state" %in% names(.))
      if_else(sin_director & is.na(teacher_state) & trimws(as.character(teacher_country)) == "CO", 1L, 0L) else NULL,
    
    m_teacher_city_co = if (all(c("teacher_country","teacher_city_co") %in% names(.)))
      if_else(sin_director & is.na(teacher_city_co) & trimws(as.character(teacher_country)) == "CO", 1L, 0L) else NULL,
    
    m_teacher_city = if (all(c("teacher_country","teacher_city") %in% names(.)))
      if_else(sin_director & is.na(teacher_city) & trimws(as.character(teacher_country)) != "CO", 1L, 0L) else NULL
  ) %>%
  select(-sin_director)

vars_miss <- grep("^m_", names(alertas), value = TRUE)
if (length(vars_miss)){
  alertas <- alertas %>%
    mutate(total_missing = rowSums(across(all_of(vars_miss)), na.rm = TRUE))
}

# 3) Exceso de NS/NR (99) -----------------------------------------------------
# variables explícitas
vars_99 <- intersect(c(
  "teacher_country","teacher_edu",
  "historias","retos","juegos","intencion_implementacion"
), names(alertas))


gad_vars <- grep("^gad_[1-7]$", names(alertas), value = TRUE)
phq_vars <- grep("^phq_[1-9]$", names(alertas), value = TRUE)

# Unificar y asegurar que existan en el data.frame
vars_99 <- unique(intersect(c(vars_99, gad_vars, phq_vars), names(alertas)))

if (length(vars_99) > 0) {
  alertas <- alertas %>%
    mutate(
      across(
        all_of(vars_99),
        ~ if_else(as.numeric(.x) == 99, 1L, 0L, missing = 0L),
        .names = "ns_{.col}"
      )
    )
  
  vars_ns <- grep("^ns_", names(alertas), value = TRUE)
  
  alertas <- alertas %>%
    mutate(
      total_ns = if (length(vars_ns) > 0) rowSums(across(all_of(vars_ns)), na.rm = TRUE) else 0L
    )
  
  media_ns <- mean(alertas$total_ns, na.rm = TRUE)
  sd_ns    <- sd(alertas$total_ns, na.rm = TRUE); if (!is.finite(sd_ns)) sd_ns <- 1
  
  alertas <- alertas %>%
    mutate(flag_ns = if_else(total_ns > media_ns + 3 * sd_ns, 1L, 0L))
}


# 4) Outliers (edad / nacimiento) --------------------------------------------
if ("teacher_birth" %in% names(alertas)){
  tb <- suppressWarnings(as.numeric(alertas$teacher_birth))
  med <- median(tb, na.rm = TRUE); sdv <- sd(tb, na.rm = TRUE); if (!is.finite(sdv) || sdv==0) sdv <- 1
  alertas <- alertas %>% mutate(ex_edad = as.integer(abs(tb - med) > 3*sdv))
}
if (!"ex_edad" %in% names(alertas)) {
  alertas$ex_edad <- NA_integer_
}

# 5) Duplicados con id_docentes_lf --------------------------------------------
# Si no existe (por seguridad), lo intentamos construir básico:
if (!"id_docentes_lf" %in% names(alertas)) {
  # fallback muy básico: usa fullname + school si existen
  alertas <- alertas %>%
    mutate(id_docentes_lf = if (all(c("teacher_fullname","teacher_school") %in% names(.)))
      paste0(stringr::str_replace_all(stringr::str_to_lower(teacher_fullname), "\\s+", "_"),
             "_",
             stringr::str_replace_all(stringr::str_to_lower(as.character(teacher_school)), "\\s+", "_"))
      else NA_character_)
}

alertas <- alertas %>%
  mutate(
    duplicado = if_else(
      duplicated(id_docentes_lf) | duplicated(id_docentes_lf, fromLast = TRUE),
      1L, 0L, missing = 0L
    )
  )

# 6) Alertas específicas -----------------------------------------------
# a) 'prueba_2' debe ser == 4
if (!"prueba_2" %in% names(alertas)) alertas$prueba_2 <- NA
alertas <- alertas %>%
  mutate(flag_prueba2 = if_else(!is.na(prueba_2) & suppressWarnings(as.numeric(prueba_2)) != 4, 1L, 0L, missing = 0L))

# b) 'flag_lb' categórica a partir de filtro_lb y teacher_fullname2
#   - "con línea de base"                    : filtro_lb == 1 & teacher_fullname2 != 99
#   - "con línea de base pero no marcó nombre": filtro_lb == 1 & teacher_fullname2 == 99
#   - "sin línea de base"                    : filtro_lb == 2
if (!"filtro_lb" %in% names(alertas)) alertas$filtro_lb <- NA
if (!"teacher_fullname2" %in% names(alertas)) alertas$teacher_fullname2 <- NA

alertas <- alertas %>%
  mutate(
    tf2_num = suppressWarnings(as.numeric(teacher_fullname2)),
    flag_lb = dplyr::case_when(
      suppressWarnings(as.numeric(filtro_lb)) == 1 & !is.na(tf2_num) & tf2_num != 99 ~ "con línea de base",
      suppressWarnings(as.numeric(filtro_lb)) == 1 & !is.na(tf2_num) & tf2_num == 99 ~ "con línea de base pero no marcó nombre",
      suppressWarnings(as.numeric(filtro_lb)) == 2 ~ "sin línea de base",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-tf2_num)

# 7) Monitoreo por colegios (match estudiantes) -------------------------------
suppressMessages(googlesheets4::gs4_deauth())
url_match <- "https://docs.google.com/spreadsheets/d/1hPdqGFA_Yvifx7wLYk5uBvJn7Y9hPSQ1VwsTGwF9uhE/edit?gid=0"
lista_match <- googlesheets4::read_sheet(url_match, sheet = "lista", range = "A:A", col_types = "c")
colegios_match <- unique(lista_match[[1]])

colegios_excluir <- c(
  "111001012360", "111001076767", "111001800694",
  "111001107778", "111001801268",
  "111001015601"
)

alertas <- alertas %>%
  mutate(
    lb_estudiantes_match = if_else(
      teacher_school %in% colegios_match & !(teacher_school %in% colegios_excluir),
      1L, 0L, missing = 0L
    )
  )

ids_match     <- setdiff(unique(lista_match[[1]]), colegios_excluir)
ids_docentes  <- unique(alertas$teacher_school)
ids_faltantes <- setdiff(ids_match, ids_docentes)
n_faltantes   <- length(ids_faltantes)

alertas <- alertas %>% 
  mutate(
    colegios_sin_docente = n_faltantes,
    ids_sin_docente      = if (n_faltantes > 0) paste(ids_faltantes, collapse = "; ") else NA_character_
  )

ids_docentes2 <- setdiff(unique(as.character(alertas$teacher_school)), colegios_excluir)
n_colegios_docente  <- length(ids_docentes2)
n_colegios_excluir  <- length(unique(colegios_excluir))

alertas <- alertas %>%
  mutate(
    colegios_con_docente   = n_colegios_docente,
    colegios_eliminados    = n_colegios_excluir
  )

colegios_tratamiento <- c(
  "111001014109","111001041599","111001046477","111001098876","111001098884",
  "111001100064","111001104051","111001104299","111001107867","111001107883",
  "111001800384","111001800392","111001800678","111001801080","111001801098",
  "111102000265","111102000753","111102000958","211001076958","211102000201",
  "211102000995","111001001279","111001011690","111001012360","111001013153",
  "111001013161","111001013170","111001014974","111001015598","111001015601",
  "111001024732","111001027308","111001027391","111001029114","111001034045",
  "111001079154","111001086771","111001098906","111001100056","111001104281",
  "111001104558","111001110477","111001800431","111001801241","111001002330",
  "111001015776","111001086665","111001094889","111001098949","111001104043",
  "111001104264","111001107786","111001800813","111769000174","111769000247",
  "111769000956","111769003122","111769003416","111769003424","111769004188"
)

colegios_control <- c(
  "111001002909","111001010031","111001013676","111001046591","111001086606",
  "111001086754","111001100072","111001104183","111001104302","111001104329",
  "111001106950","111001106968","111001107832","111001107875","111001800457",
  "111001800643","111001800694","111001801101","111102000281","111102000621",
  "211102000243","111001000124","111001011321","111001012343","111001013102",
  "111001013129","111001014290","111001016071","111001016098","111001016101",
  "111001016136","111001016292","111001024660","111001026964","111001027405",
  "111001033898","111001041475","111001076767","111001086649","111001098892",
  "111001100048","111001106984","111001107778","111001800554","111001801268",
  "111001025020","111001045225","111001075272","111001075515","111001093084",
  "111001104256","111001104388","111001107069","111001107077","111001800163",
  "111001801250","111001801349","111769001502","111769001871","111001092983",
  "111769003360"
)

alertas <- alertas %>%
  mutate(
    tratamiento = case_when(
      teacher_school %in% colegios_tratamiento ~ "Colegio Tratamiento",
      teacher_school %in% colegios_control     ~ "Colegio Control",
      TRUE ~ NA_character_)
  )

# 8)Monitoreo por salones  ----------------------------------------------------------

## Salones con encuesta docente -----------------------------------------------------

library(readr)   

# 0. Ruta 
ruta_tc <- "tc.csv"

# 1. Leer el CSV  
tc <- read_csv2(
  ruta_tc,
  col_types = cols(
    control     = col_character(),
    tratamiento = col_character()
  )
)

# 2. Convertir a vectores únicos
id_tc_control     <- tc %>% pull(control)     %>% na.omit() %>% unique()
id_tc_tratamiento <- tc %>% pull(tratamiento) %>% na.omit() %>% unique()

# 3. Mapeos turno y salón
map_turno <- c("1" = "MANANA",
               "2" = "TARDE",
               "3" = "UNICA")      
map_salon <- c(
  "6"  = "501", "7"  = "502", "8"  = "503", "9"  = "504",
  "10" = "505", "11" = "506", "12" = "507", "13" = "508",
  "14" = "509"
)

# 4. Pasar a largo, construir ID_TC
alertas_long <- alertas %>%
  mutate(docente_id = row_number()) %>%        # id auxiliar
  pivot_longer(
    cols = starts_with("teacher_fifth_"),
    names_to = c("turno_code", "salon_code"),
    names_pattern = "teacher_fifth_(\\d)_(\\d+)",
    values_to = "dicta",
    values_drop_na = TRUE                     # descarta NA directamente
  ) %>%
  filter(dicta == "1") %>%                    # solo donde dicta
  mutate(
    turno = map_turno[turno_code],
    salon = map_salon[salon_code],
    ID_TC = paste0(teacher_school, turno, salon)
  )

# 5. Resumir por docente  →  lista de salones + dummies
docente_flags <- alertas_long %>%
  group_by(docente_id) %>%
  summarise(
    # lista completa, ordenada y separada por "; "
    id_tc_list = paste(sort(unique(ID_TC)), collapse = "; "),
    
    # dummies tradicionales
    salon_tratamiento = as.integer(any(ID_TC %in% id_tc_tratamiento)),
    salon_control     = as.integer(any(ID_TC %in% id_tc_control)),
    
    # ➜ NUEVA dummy: ¿al menos un salón pertenece a un colegio excluido?
    salon_colegio_eliminado = as.integer(
      any( stringr::str_extract(ID_TC, "^\\d{12}") %in% colegios_excluir )
    ),
    .groups = "drop"
  )


# 6. Volver a la tabla principal
alertas <- alertas %>%
  mutate(docente_id = row_number()) %>%
  left_join(docente_flags, by = "docente_id") %>%
  select(-docente_id)

## Salones sin encuesta docente -----------------------------------------------


ids_reportados <- unique(alertas_long$ID_TC)

# -- faltantes totales
ids_faltantes_trat <- setdiff(id_tc_tratamiento, ids_reportados)
ids_faltantes_ctrl <- setdiff(id_tc_control,     ids_reportados)

# separar según colegios excluidos 
extrae_colegio <- function(x) stringr::str_extract(x, "^\\d{12}")

ids_falt_trat_in <- ids_faltantes_trat[ !extrae_colegio(ids_faltantes_trat) %in% colegios_excluir ]
ids_falt_trat_ex <- ids_faltantes_trat[  extrae_colegio(ids_faltantes_trat) %in% colegios_excluir ]

ids_falt_ctrl_in <- ids_faltantes_ctrl[ !extrae_colegio(ids_faltantes_ctrl) %in% colegios_excluir ]
ids_falt_ctrl_ex <- ids_faltantes_ctrl[  extrae_colegio(ids_faltantes_ctrl) %in% colegios_excluir ]

# contadores 
n_trat_in  <- length(ids_falt_trat_in)
n_trat_ex  <- length(ids_falt_trat_ex)
n_ctrl_in  <- length(ids_falt_ctrl_in)
n_ctrl_ex  <- length(ids_falt_ctrl_ex)

# añadir a la tabla principal 
alertas <- alertas %>%
  mutate(
    n_salones_trat_faltantes_in   = n_trat_in,
    ids_salones_trat_faltantes_in = if (n_trat_in  > 0) paste(ids_falt_trat_in,  collapse = "; ") else NA_character_,
    n_salones_trat_faltantes_ex   = n_trat_ex,
    ids_salones_trat_faltantes_ex = if (n_trat_ex  > 0) paste(ids_falt_trat_ex,  collapse = "; ") else NA_character_,
    
    n_salones_ctrl_faltantes_in   = n_ctrl_in,
    ids_salones_ctrl_faltantes_in = if (n_ctrl_in  > 0) paste(ids_falt_ctrl_in,  collapse = "; ") else NA_character_,
    n_salones_ctrl_faltantes_ex   = n_ctrl_ex,
    ids_salones_ctrl_faltantes_ex = if (n_ctrl_ex  > 0) paste(ids_falt_ctrl_ex,  collapse = "; ") else NA_character_
  )

## Desagregación total de salones (conteos basados en id_tc_list por respuesta) -----

library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(writexl)



# 0. id_tc_all (salones de la muestra)
id_tc_all <- unique(c(id_tc_control, id_tc_tratamiento))

## Dummy 'docente_muestra' (1 si al menos un ID_TC del id_tc_list está en la muestra) ----

library(stringr)

# 0) Asegurar id_tc_all
if (!exists("id_tc_all")) id_tc_all <- unique(c(id_tc_control, id_tc_tratamiento))

# 1) Comprobar que id_tc_list exista
if (!"id_tc_list" %in% names(alertas)) {
  stop("No existe 'id_tc_list' en 'alertas'. Ejecutá primero el bloque que genera id_tc_list.")
}

# 2) Separar id_tc_list y chequear pertenencia a la muestra
#    (maneja NA / cadenas vacías correctamente)
lista_componentes <- str_split(ifelse(is.na(alertas$id_tc_list), "", alertas$id_tc_list), ";\\s*")

# 3) Vector lógico: TRUE si ANY(v %in% id_tc_all)
in_muestra_vec <- vapply(lista_componentes, function(v) {
  if (length(v) == 0) return(FALSE)
  v <- v[v != ""]
  if (length(v) == 0) return(FALSE)
  any(v %in% id_tc_all)
}, logical(1))

# 4) Crear la dummy en alertas (1/0)
alertas <- alertas %>%
  mutate(docente_muestra = as.integer(in_muestra_vec))

# 5) Diagnóstico breve
message("docente_muestra creado en 'alertas'.")
message(paste0("Filas totales en 'alertas': ", nrow(alertas)))
message(paste0("Filas con docente_muestra == 1: ", sum(alertas$docente_muestra == 1, na.rm = TRUE)))
message(paste0("Filas con docente_muestra == 0: ", sum(alertas$docente_muestra == 0, na.rm = TRUE)))


# 11) Rechazos y totales para Looker ------------------------------------------
if (!"consentimiento" %in% names(alertas)) alertas$consentimiento <- NA

alertas <- alertas %>%
  mutate(
    flag_rejected       = if_else(as.numeric(consentimiento) == 2, 1L, 0L),
    flag_duplicated     = if_else(duplicado == 1, 1L, 0L),
    flag_missing        = if_else(!is.na(total_missing) & total_missing > 0, 1L, 0L),
    flag_extreme_values = if_else(!is.na(ex_edad) & ex_edad == 1, 1L, 0L, missing = 0L),
    total_encuestas     = n()
  ) %>%
  mutate(
    Exitos  = if_else(flag_duration_mas==0 & flag_duration_menos==0 &
                        flag_duplicated==0 & flag_missing==0 &
                        flag_extreme_values==0 &
                        (if ("flag_ns" %in% names(.)) flag_ns==0 else TRUE) &
                        flag_rejected==0 &
                        flag_prueba2==0,
                      1L, 0L),
    Alertas = if_else(flag_duration_mas==1 | flag_duration_menos==1 |
                        flag_duplicated==1 | flag_missing==1 |
                        (if ("flag_ns" %in% names(.)) flag_ns==1 else FALSE) |
                        flag_extreme_values==1 |
                        flag_prueba2==1,
                      1L, 0L),
    Rechazos = if_else(flag_rejected==1, 1L, 0L),
    
    tiempos_anomalos_mas   = if_else(flag_duration_mas==1, "Sí", "No"),
    tiempos_anomalos_menos = if_else(flag_duration_menos==1, "Sí", "No"),
    duplicado_lbl          = if_else(flag_duplicated==1, "Sí", "No"),
    valores_faltantes      = if_else(flag_missing==1, "Sí", "No"),
    valores_extremos       = if_else(flag_extreme_values==1, "Sí", "No"),
    exceso_ns              = if ( "flag_ns" %in% names(.) ) if_else(flag_ns==1,"Sí","No") else NA_character_
  )

# 9) Orden de columnas --------------------------------------------------------
cols_now <- names(alertas)
cols_new <- setdiff(cols_now, cols_api_order)
anclas <- intersect(c("Exitos","Alertas","Rechazos"), cols_new)
cols_new_rest <- setdiff(cols_new, anclas)
alertas <- alertas[, c(cols_api_order, anclas, sort(cols_new_rest))]

# 10) Guardar -----------------------------------------------------------------
out_dir <- "docentes_lf/data"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
tag <- format(Sys.time(), "%Y%m%d_%H%M")
readr::write_csv(alertas, file.path(out_dir, paste0("docentes_LF_audit_", tag, ".csv")))
saveRDS(alertas,              file.path(out_dir, paste0("docentes_LF_audit_", tag, ".rds")))

message("==> Auditoría Docentes LF finalizada")
