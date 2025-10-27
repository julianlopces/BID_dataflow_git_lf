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
        ~ { v <- .; if (is.character(v)) dplyr::na_if(v, "") else v }
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
    dplyr::left_join(crudas_sub, by = "teacher_fullname2", suffix = c("", "__cru"))
  
  # 7) Coalesce columna por columna, armonizando tipos
  for (cc in cols_pull) {
    cru_col <- paste0(cc, "__cru")
    if (!cru_col %in% names(dataset)) next
    
    lhs <- dataset[[cc]]
    rhs <- dataset[[cru_col]]
    
    lhs_num <- is.numeric(lhs); rhs_num <- is.numeric(rhs)
    
    if (lhs_num && rhs_num) {
      dataset[[cc]] <- dplyr::coalesce(rhs, lhs)
    } else {
      lhs_chr <- as.character(lhs); rhs_chr <- as.character(rhs)
      merged  <- dplyr::coalesce(rhs_chr, lhs_chr)
      dataset[[cc]] <- if (lhs_num || rhs_num) suppressWarnings(as.numeric(merged)) else merged
    }
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

alertas$duration_minutes <- round(get_duration_min(alertas), 2)



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
    mutate(across(all_of(present),
                  ~ if_else(is.na(.x) | trimws(as.character(.x))=="", 1L, 0L),
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
vars_99 <- intersect(c(
  "teacher_country","teacher_edu",
  "historias","retos","juegos","intencion_implementacion"
), names(alertas))

gad_vars <- grep("^gad_[1-7]$", names(alertas), value = TRUE)
phq_vars <- grep("^phq_[1-9]$", names(alertas), value = TRUE)

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
    mutate(total_ns = if (length(vars_ns) > 0) rowSums(across(all_of(vars_ns)), na.rm = TRUE) else 0L)
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
} else {
  alertas$ex_edad <- NA_integer_
}

# 4.b) Extremos: horas_implementacion_programa (3DE) -------------------------
if ("horas_implementacion_programa" %in% names(alertas)) {
  v <- suppressWarnings(as.numeric(alertas$horas_implementacion_programa))
  mu <- mean(v, na.rm = TRUE); sdv <- sd(v, na.rm = TRUE); if (!is.finite(sdv) || sdv==0) sdv <- 1
  z  <- (v - mu) / sdv
  alertas$ex_horas_impl <- as.integer(!is.na(z) & abs(z) > 3)
} else {
  alertas$ex_horas_impl <- NA_integer_
}

# 4.c) Extremos y consistencia: teacher_experience ---------------------------
if ("teacher_experience" %in% names(alertas)) {
  expv <- suppressWarnings(as.numeric(alertas$teacher_experience))
  mu <- mean(expv, na.rm = TRUE); sdv <- sd(expv, na.rm = TRUE); if (!is.finite(sdv) || sdv==0) sdv <- 1
  z  <- (expv - mu) / sdv
  alertas$ex_experience_z <- as.integer(!is.na(z) & abs(z) > 3)
  # contra edad mínima razonable (edad-18)
  edad_calc <- NA_real_
  if ("teacher_birth" %in% names(alertas)) {
    nacimiento <- suppressWarnings(as.numeric(alertas$teacher_birth))
    anio_actual <- as.numeric(format(Sys.Date(), "%Y"))
    edad_calc <- ifelse(!is.na(nacimiento) & nacimiento > 1900 & nacimiento < anio_actual,
                        anio_actual - nacimiento, NA_real_)
  }
  lim_exp <- ifelse(!is.na(edad_calc), pmax(edad_calc - 18, 0), NA_real_)
  alertas$ex_experience_vs_age <- as.integer(!is.na(expv) & !is.na(lim_exp) & expv > lim_exp)
} else {
  alertas$ex_experience_z <- NA_integer_
  alertas$ex_experience_vs_age <- NA_integer_
}

# 4.d) Flag global de extremos (sin NA) ----------------
alertas <- alertas %>%
  mutate(
    ex_edad              = if (!"ex_edad" %in% names(.)) NA_integer_ else ex_edad,
    ex_horas_impl        = if (!"ex_horas_impl" %in% names(.)) NA_integer_ else ex_horas_impl,
    ex_experience_z      = if (!"ex_experience_z" %in% names(.)) NA_integer_ else ex_experience_z,
    ex_experience_vs_age = if (!"ex_experience_vs_age" %in% names(.)) NA_integer_ else ex_experience_vs_age
  ) %>%
  mutate(
    flag_extreme_values = as.integer(
      dplyr::coalesce(ex_edad, 0L) == 1L |
        dplyr::coalesce(ex_horas_impl, 0L) == 1L |
        dplyr::coalesce(ex_experience_z, 0L) == 1L |
        dplyr::coalesce(ex_experience_vs_age, 0L) == 1L
    )
  )


# 5) Duplicados con id_docentes_lf --------------------------------------------
if (!"id_docentes_lf" %in% names(alertas)) {
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

# -- FLAG_LB: solo 2 valores -------------------------
for (v in c("filtro_lb","teacher_fullname2")) if (!v %in% names(alertas)) alertas[[v]] <- NA

alertas <- alertas %>%
  mutate(
    filtro_lb_num = suppressWarnings(as.numeric(trimws(as.character(filtro_lb)))),
    tf2_raw       = trimws(as.character(teacher_fullname2)),
    tf2_is_99     = suppressWarnings(as.numeric(tf2_raw) == 99),
    tf2_is_blank  = is.na(tf2_raw) | tf2_raw == "",
    flag_lb = dplyr::case_when(
      # Con LB: filtro==1 y teacher_fullname2 no vacío y != 99
      filtro_lb_num == 1 & !tf2_is_blank & !tf2_is_99 ~ "Con línea de base",
      # Sin LB: filtro==2  OR (filtro==1 y teacher_fullname2==99)
      filtro_lb_num == 2 | (filtro_lb_num == 1 & tf2_is_99) ~ "Sin Línea de base",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-filtro_lb_num, -tf2_raw, -tf2_is_99, -tf2_is_blank)


# c) 'flag_prueba50' -> se activa si hay cualquier valor (debería estar vacía)
if (!"prueba_50" %in% names(alertas)) alertas$prueba_50 <- NA
alertas <- alertas %>%
  mutate(flag_prueba50 = as.integer(!(is.na(prueba_50) | trimws(as.character(prueba_50))=="")))

# 7) Monitoreo por colegios (match estudiantes) -------------------------------
suppressMessages(googlesheets4::gs4_deauth())
url_match <- "https://docs.google.com/spreadsheets/d/1hPdqGFA_Yvifx7wLYk5uBvJn7Y9hPSQ1VwsTGwF9uhE/edit?gid=0"
lista_match <- googlesheets4::read_sheet(url_match, sheet = "lista", range = "A:A", col_types = "c")
colegios_match <- unique(lista_match[[1]])

colegios_excluir <- c(
  "111001012360", "111001076767", "111001800694",
  "111001107778", "111001801268", "111001015601"
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

# 8) Monitoreo por salones ----------------------------------------------------
library(readr)

# 0. Ruta 
ruta_tc <- "tc.csv"

# 1. Leer el CSV  
tc <- readr::read_csv2(
  ruta_tc,
  col_types = readr::cols(
    control     = readr::col_character(),
    tratamiento = readr::col_character()
  )
)

# 2. Convertir a vectores únicos
id_tc_control     <- tc %>% dplyr::pull(control)     %>% stats::na.omit() %>% unique()
id_tc_tratamiento <- tc %>% dplyr::pull(tratamiento) %>% stats::na.omit() %>% unique()

# 3. Mapeos turno y salón
map_turno <- c("1" = "MANANA", "2" = "TARDE", "3" = "UNICA")
map_salon <- c("6"="501","7"="502","8"="503","9"="504","10"="505","11"="506","12"="507","13"="508","14"="509")

# 4. Pasar a largo, construir ID_TC (tipos armonizados / columnas válidas) ----
cols_fifth_valid <- grep("^teacher_fifth_\\d_\\d+$", names(alertas), value = TRUE)

if (length(cols_fifth_valid)) {
  alertas <- alertas %>%
    mutate(across(all_of(cols_fifth_valid), ~ { x <- .; if (is.logical(x)) x <- as.integer(x); as.character(x) }))
}


alertas <- alertas %>% dplyr::mutate(docente_id = dplyr::row_number())

alertas_long <- alertas %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(cols_fifth_valid),
    names_to = c("turno_code", "salon_code"),
    names_pattern = "teacher_fifth_(\\d)_(\\d+)",
    values_to = "dicta",
    values_drop_na = TRUE
  ) %>%
  dplyr::filter(suppressWarnings(as.numeric(dicta)) == 1) %>%
  dplyr::mutate(
    turno = map_turno[turno_code],
    salon = map_salon[salon_code],
    ID_TC = paste0(teacher_school, turno, salon)
  )

# --- Lista de colegios a excluir (se necesita aquí y luego en el bloque 7) ---
colegios_excluir <- c(
  "111001012360","111001076767","111001800694",
  "111001107778","111001801268","111001015601"
)

# 5. Resumir por docente → lista de salones + dummies -------------------------
docente_flags <- alertas_long %>%
  dplyr::group_by(docente_id) %>%
  dplyr::summarise(
    id_tc_list = paste(sort(unique(ID_TC)), collapse = "; "),
    salon_tratamiento = as.integer(any(ID_TC %in% id_tc_tratamiento)),
    salon_control     = as.integer(any(ID_TC %in% id_tc_control)),
    salon_colegio_eliminado = as.integer(any(stringr::str_extract(ID_TC, "^\\d{12}") %in% colegios_excluir)),
    .groups = "drop"
  )

# 6. Volver a la tabla principal ----------------------------------------------
alertas <- alertas %>%
  mutate(docente_id = dplyr::row_number()) %>%
  dplyr::left_join(docente_flags, by = "docente_id")


## Salones sin encuesta docente (y contadores globales) -----------------------
ids_reportados <- unique(alertas_long$ID_TC)

ids_faltantes_trat <- setdiff(id_tc_tratamiento, ids_reportados)
ids_faltantes_ctrl <- setdiff(id_tc_control,     ids_reportados)

extrae_colegio <- function(x) stringr::str_extract(x, "^\\d{12}")

ids_falt_trat_in <- ids_faltantes_trat[ !extrae_colegio(ids_faltantes_trat) %in% colegios_excluir ]
ids_falt_ctrl_in <- ids_faltantes_ctrl[ !extrae_colegio(ids_faltantes_ctrl) %in% colegios_excluir ]

ids_falt_trat_ex <- ids_faltantes_trat[  extrae_colegio(ids_faltantes_trat) %in% colegios_excluir ]
ids_falt_ctrl_ex <- ids_faltantes_ctrl[  extrae_colegio(ids_faltantes_ctrl) %in% colegios_excluir ]

n_trat_in  <- length(ids_falt_trat_in)
n_trat_ex  <- length(ids_falt_trat_ex)
n_ctrl_in  <- length(ids_falt_ctrl_in)
n_ctrl_ex  <- length(ids_falt_ctrl_ex)

alertas <- alertas %>%
  mutate(
    n_salones_trat_faltantes_in   = n_trat_in,
    ids_salones_trat_faltantes_in = if (n_trat_in  > 0) paste(ids_falt_trat_in,  collapse = "; ") else NA_character_,
    n_salones_trat_faltantes_ex   = n_trat_ex,
    ids_salones_trat_faltantes_ex = if (n_trat_ex  > 0) paste(ids_falt_trat_ex,  collapse = "; ") else NA_character_,
    n_salones_ctrl_faltantes_in   = n_ctrl_in,
    ids_salones_ctrl_faltantes_in = if (n_ctrl_in  > 0) paste(ids_falt_ctrl_in,  collapse = "; ") else NA_character_,
    n_salones_ctrl_faltantes_ex   = n_ctrl_ex,
    ids_salones_ctrl_faltantes_ex = if (n_ctrl_ex  > 0) paste(ids_falt_ctrl_ex,  collapse = "; ") else NA_character_,
    # contadores globales constantes (solo NO excluidos)
    total_salones_trat_faltantes  = n_trat_in,
    total_salones_ctrl_faltantes  = n_ctrl_in
  )

## Dummy 'docente_muestra' (1 si algún ID_TC del id_tc_list está en la muestra)
id_tc_all <- unique(c(id_tc_control, id_tc_tratamiento))
if (!"id_tc_list" %in% names(alertas)) stop("No existe 'id_tc_list' en 'alertas'. Ejecutá primero el bloque que genera id_tc_list.")

lista_componentes <- stringr::str_split(ifelse(is.na(alertas$id_tc_list), "", alertas$id_tc_list), ";\\s*")

in_muestra_vec <- vapply(lista_componentes, function(v) {
  if (length(v) == 0) return(FALSE)
  v <- v[v != ""]
  if (length(v) == 0) return(FALSE)
  any(v %in% id_tc_all)
}, logical(1))

alertas <- alertas %>%
  mutate(docente_muestra = as.integer(in_muestra_vec))

# Forzado manual según instancia (corrección puntual) -------------------------
uuids_forzar <- c("uuid:6e51deeb-9c04-4cf4-b8a7-68b2c2ca913b",
                  "uuid:54dc52c5-7877-44d1-8010-0b7c34544176")
if ("instanceID" %in% names(alertas)) {
  alertas <- alertas %>%
    mutate(docente_muestra = if_else(instanceID %in% uuids_forzar, 1L, docente_muestra))
}

# ===== Tabla "salones_docentes" =============================================
id_tc_all <- unique(c(id_tc_control, id_tc_tratamiento))
id_tc_all_in <- id_tc_all[ !(extrae_colegio(id_tc_all) %in% colegios_excluir) ]

df_tc <- tibble::tibble(salon_id = id_tc_all_in) %>%
  dplyr::mutate(tratamiento = as.integer(salon_id %in% id_tc_tratamiento))

if (!"teacher_school_label" %in% names(alertas))
  alertas$teacher_school_label <- as.character(alertas$teacher_school)

alertas_long_in <- alertas_long %>% dplyr::filter(ID_TC %in% id_tc_all_in)

colegio_por_salon <- if (nrow(alertas_long_in)) {
  alertas %>%
    dplyr::select(docente_id, teacher_school, teacher_school_label) %>%
    dplyr::inner_join(alertas_long_in %>% dplyr::select(docente_id, ID_TC), by = "docente_id") %>%
    dplyr::group_by(ID_TC) %>%
    dplyr::summarise(colegio = dplyr::first(teacher_school_label), .groups = "drop")
} else tibble::tibble(ID_TC = character(), colegio = character())

# Resolver alias para knows/hours
knows_col <- intersect(c("teacher_knows_program", "teacher_knows_programa", "knows_program"), names(alertas))
hours_col <- intersect(c("horas_implementacion_programa", "hour_implementacion_programa", "horas_programa"), names(alertas))

alertas <- alertas %>%
  dplyr::mutate(
    knows_tmp = if (length(knows_col)) as.character(.data[[knows_col[1]]]) else NA_character_,
    hours_tmp = if (length(hours_col))  suppressWarnings(as.numeric(.data[[hours_col[1]]])) else NA_real_
  )

base_long <- if (nrow(alertas_long_in)) {
  alertas_long_in %>%
    dplyr::left_join(alertas %>% dplyr::select(docente_id, docente_muestra, knows_tmp, hours_tmp), by = "docente_id")
} else {
  tibble::tibble(ID_TC = character(), docente_muestra = integer(), knows_tmp = character(), hours_tmp = numeric())
}

salones_docentes <- base_long %>%
  dplyr::group_by(ID_TC) %>%
  dplyr::summarise(
    flag_muestra        = as.integer(any(docente_muestra == 1, na.rm = TRUE)),
    n_docentes          = dplyr::n(),
    n_docentes_muestra  = sum(docente_muestra == 1, na.rm = TRUE),
    n_conoce_programa   = sum(docente_muestra == 1 & suppressWarnings(as.numeric(knows_tmp)) == 1, na.rm = TRUE),
    flag_horas = {
      x <- suppressWarnings(as.numeric(hours_tmp[docente_muestra == 1]))
      if (length(x) == 0 || all(is.na(x))) NA_integer_ else as.integer(any(x == 1, na.rm = TRUE))
    },
    .groups = "drop"
  ) %>%
  dplyr::right_join(df_tc, by = c("ID_TC" = "salon_id")) %>%
  dplyr::mutate(
    flag_muestra        = dplyr::coalesce(flag_muestra, 0L),
    n_docentes          = dplyr::coalesce(n_docentes, 0L),
    n_docentes_muestra  = dplyr::coalesce(n_docentes_muestra, 0L),
    n_conoce_programa   = ifelse(tratamiento == 1, dplyr::coalesce(n_conoce_programa, 0L), NA_integer_),
    flag_horas          = ifelse(tratamiento == 1, flag_horas, NA_integer_)
  ) %>%
  dplyr::left_join(colegio_por_salon, by = c("ID_TC" = "ID_TC")) %>%
  dplyr::rename(salon = ID_TC) %>%
  dplyr::select(salon, colegio, flag_muestra, n_docentes, n_docentes_muestra,
                n_conoce_programa, flag_horas, tratamiento)

# Helper para normalizar flags a 0/1 (sin NA)
flag0 <- function(x) {
  y <- suppressWarnings(as.integer(x))
  y[is.na(y)] <- 0L
  y
}

# 11) Rechazos y totales para Looker ------------------------------------------
if (!"consentimiento" %in% names(alertas)) alertas$consentimiento <- NA

alertas <- alertas %>%
  mutate(
    flag_rejected       = flag0(suppressWarnings(as.numeric(consentimiento) == 2)),
    flag_duplicated     = flag0(duplicado == 1),
    flag_missing        = flag0(!is.na(total_missing) & total_missing > 0),
    flag_extreme_values = flag0(flag_extreme_values),             # del paso 4.d corregido
    flag_duration_mas   = flag0(flag_duration_mas),
    flag_duration_menos = flag0(flag_duration_menos),
    flag_prueba2        = flag0(suppressWarnings(as.numeric(prueba_2)) != 4),
    flag_prueba50       = flag0(!(is.na(prueba_50) | trimws(as.character(prueba_50))=="")),
    flag_ns             = if ("flag_ns" %in% names(.)) flag0(flag_ns) else 0L
  ) %>%
  # Suma de flags "malas" → 0 = Exitos, >0 = Alertas
  mutate(
    bad_flags_sum =
      flag_duration_mas + flag_duration_menos +
      flag_duplicated   + flag_missing        +
      flag_extreme_values + flag_ns +
      flag_rejected     + flag_prueba2 + flag_prueba50,
    Exitos  = as.integer(bad_flags_sum == 0L),
    Alertas = as.integer(bad_flags_sum >  0L),
    Rechazos = flag_rejected,
    tiempos_anomalos_mas   = if_else(flag_duration_mas==1L, "Sí", "No"),
    tiempos_anomalos_menos = if_else(flag_duration_menos==1L, "Sí", "No"),
    duplicado_lbl          = if_else(flag_duplicated==1L, "Sí", "No"),
    valores_faltantes      = if_else(flag_missing==1L, "Sí", "No"),
    valores_extremos       = if_else(flag_extreme_values==1L, "Sí", "No"),
    exceso_ns              = if ("flag_ns" %in% names(.)) if_else(flag_ns==1L,"Sí","No") else NA_character_
  ) %>%
  select(-bad_flags_sum)

# 9) Orden de columnas --------------------------------------------------------
alertas <- tibble::as_tibble(alertas)
cols_now <- names(alertas)
cols_new <- setdiff(cols_now, cols_api_order)
anclas   <- intersect(c("Exitos","Alertas","Rechazos"), cols_new)
cols_new_rest <- setdiff(cols_new, anclas)
orden_deseado <- c(cols_api_order, anclas, sort(cols_new_rest))
alertas <- dplyr::select(alertas, dplyr::any_of(orden_deseado))

# === Resumen para Looker =====================================================
resumen_looker <- alertas %>%
  dplyr::summarise(
    n_encuestas   = dplyr::n(),
    exitos        = sum(Exitos == 1, na.rm = TRUE),
    alertas       = sum(Alertas == 1, na.rm = TRUE),
    rechazos      = sum(Rechazos == 1, na.rm = TRUE),
    tiempos_mas   = sum(flag_duration_mas == 1, na.rm = TRUE),
    tiempos_menos = sum(flag_duration_menos == 1, na.rm = TRUE),
    duplicados    = sum(flag_duplicated == 1, na.rm = TRUE),
    faltantes     = sum(flag_missing == 1, na.rm = TRUE),
    extremos      = sum(flag_extreme_values == 1, na.rm = TRUE),
    exceso_ns     = if ("flag_ns" %in% names(.)) sum(flag_ns == 1, na.rm = TRUE) else NA_integer_
  ) %>%
  dplyr::mutate(fecha_export = format(Sys.time(), "%Y-%m-%d %H:%M")) %>%
  dplyr::select(fecha_export, dplyr::everything())


message("── QA rápido ──")
try(print(table(alertas$flag_lb, useNA="ifany")), silent = TRUE)
try(print(colSums(select(alertas,
                         Exitos, Alertas, Rechazos,
                         flag_duration_mas, flag_duration_menos, flag_duplicated, flag_missing,
                         flag_extreme_values, flag_ns, flag_prueba2, flag_prueba50
), na.rm = TRUE)), silent = TRUE)


# 10) Guardar -----------------------------------------------------------------
out_dir <- "docentes_lf/data"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
tag <- format(Sys.time(), "%Y%m%d_%H%M")

readr::write_csv(alertas,          file.path(out_dir, paste0("docentes_LF_audit_", tag, ".csv")))
saveRDS(alertas,                   file.path(out_dir, paste0("docentes_LF_audit_", tag, ".rds")))
readr::write_csv(resumen_looker,   file.path(out_dir, paste0("resumen_LF_", tag, ".csv")))
readr::write_csv(salones_docentes, file.path(out_dir, paste0("salones_docentes_", tag, ".csv")))

message("==> Auditoría Docentes LF finalizada")