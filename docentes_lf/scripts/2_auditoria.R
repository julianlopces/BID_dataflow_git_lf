
# --------------------------------------------------------------
# Auditoría Docentes LF
# --------------------------------------------------------------

message("==> Auditoría Docentes LF iniciada")

safe_lib(c("dplyr","stringr","tidyr","purrr","readr"))

# 0) dataset base y para preservar orden original -----------------------------
if (!exists("data")) stop("No existe `data` desde el import. Corre 1_import primero.")
dataset <- data
cols_api_order <- names(dataset)   # orden original del import (API + tus nuevas de import)

# 1) Duración -----------------------------------------------------------------
#   - Si existe `duration` (segundos), usamos eso
#   - Si no, calculamos con starttime/endtime (si existen)
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

# flags de duración por Z-score (3DE) usando mediana+MAD para robustez si hay NAs
dur_vec <- alertas$duration_minutes
med_dur <- median(dur_vec, na.rm = TRUE)
sd_dur  <- sd(dur_vec, na.rm = TRUE)
if (!is.finite(sd_dur) || sd_dur == 0) sd_dur <- 1  # evitar NaN/0
alertas <- alertas %>%
  mutate(
    z_dur = (duration_minutes - med_dur) / sd_dur,
    flag_duration_mas   = if_else(!is.na(z_dur) & z_dur >  3, 1L, 0L),
    flag_duration_menos = if_else(!is.na(z_dur) & z_dur < -3, 1L, 0L)
  )

# 2) Missings -----------------------------------------------------------------
# Lista base (ajústala si ves que falta alguna clave)
var_missing <- c(
  "teacher_loc","teacher_school",
  # nuevas LF:
  "historias","retos","juegos","intencion_implementacion",
  "filtro_lb",
  "gad_1",
  "gad_2",
  "gad_3",
  "gad_4",
  "gad_5",
  "gad_6",
  "gad_7",
  "phq_1",
  "phq_2",
  "phq_3",
  "phq_4",
  "phq_5",
  "phq_6",
  "phq_7",
  "phq_8",
  "phq_9",
  "neigh_lgtb",
  "neigh_mig",
  "neigh_ethnic",
  "neigh_relig",
  "perpective_1",
  "perpective_2",
  "perpective_3",
  "poverty_cause",
  "migrantes_1",
  "migrantes_2",
  "migrantes_3",
  "migrantes_4",
  "migrantes_5",
  "migrantes_6",
  "migrantes_7",
  "migrantes_8",
  "job_satisfaction1",
  "job_satisfaction2",
  "job_satisfaction4",
  "beliefs_intelligence1",
  "gender_stereotypes1",
  "beliefs_intelligence2",
  "gender_stereotypes2",
  "discipline_strategies1",
  "gender_stereotypes3",
  "gender_stereotypes4",
  "student_expectations1",
  "teacher_student_relations",
  "classroom_management1",
  "teaching_standards",
  "student_expectations2",
  "discipline_strategies2",
  "discipline_strategies3",
  "teaching_priorities_a",
  "teaching_priorities_b",
  "teaching_priorities_c",
  "teaching_priorities_d",
  "teaching_priorities_e",
  "teaching_strategies2",
  "classroom_management2",
  "educational_philosophy",
  "student_engagement1",
  "student_engagement2",
  "classroom_management3",
  "teaching_strategies3",
  "perseverance",
  "eyes_test_1",
  "eyes_test_2",
  "eyes_test_3",
  "eyes_test_4",
  "eyes_test_5",
  "eyes_test_6",
  "eyes_test_7",
  "eyes_test_8",
  "eyes_test_9",
  "eyes_test_10",
  "eyes_test_11",
  "eyes_test_12",
  "eyes_test_13",
  "eyes_test_14",
  "eyes_test_15",
  "eyes_test_16",
  "eyes_test_17",
  "eyes_test_18",
  "eyes_test_19",
  "eyes_test_20",
  "eyes_test_21",
  "eyes_test_22",
  "eyes_test_23",
  "eyes_test_24",
  "eyes_test_25",
  "eyes_test_26",
  "eyes_test_27",
  "eyes_test_28",
  "eyes_test_29",
  "eyes_test_30",
  "eyes_test_31",
  "eyes_test_32",
  "eyes_test_33",
  "eyes_test_34",
  "eyes_test_35",
  "eyes_test_36"
)
# crea dummies m_<var> = 1 si NA
present <- intersect(var_missing, names(alertas))
if (length(present)){
  alertas <- alertas %>%
    mutate(across(all_of(present), ~ if_else(is.na(.x) | trimws(as.character(.x))=="", 1L, 0L),
                  .names = "m_{.col}"))
}

# Missings condicionados por saltos/otros (solo si existen esas variables):
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

# total_missing = suma de todas las m_*
vars_miss <- grep("^m_", names(alertas), value = TRUE)
if (length(vars_miss)){
  alertas <- alertas %>%
    mutate(total_missing = rowSums(across(all_of(vars_miss)), na.rm = TRUE))
}

# 3) Exceso de NS/NR (99) -----------------------------------------------------
vars_99 <- intersect(c(
  "teacher_country","teacher_edu",
  # agrega aquí bloques que usan 99 como NS/NR en tu LF
  # ejemplos de baseline: "gad_1","gad_2",... etc si existieran en LF
  "historias","retos","juegos","intencion_implementacion"
), names(alertas))

if (length(vars_99)){
  alertas <- alertas %>%
    mutate(across(all_of(vars_99),
                  ~ if_else(as.numeric(.x)==99, 1L, 0L, missing = 0L),
                  .names = "ns_{.col}"))
  vars_ns <- grep("^ns_", names(alertas), value = TRUE)
  alertas <- alertas %>%
    mutate(total_ns = if (length(vars_ns)) rowSums(across(all_of(vars_ns)), na.rm = TRUE) else 0L)
  media_ns <- mean(alertas$total_ns, na.rm = TRUE)
  sd_ns    <- sd(alertas$total_ns, na.rm = TRUE); if (!is.finite(sd_ns)) sd_ns <- 1
  alertas <- alertas %>%
    mutate(flag_ns = if_else(total_ns > media_ns + 3 * sd_ns, 1L, 0L))
}

# 4) Outliers (edad / o variable de nacimiento) -------------------------------
# En baseline usaban teacher_birth (num). Mantengo esa lógica.
if ("teacher_birth" %in% names(alertas)){
  tb <- suppressWarnings(as.numeric(alertas$teacher_birth))
  med <- median(tb, na.rm = TRUE); sdv <- sd(tb, na.rm = TRUE); if (!is.finite(sdv) || sdv==0) sdv <- 1
  alertas <- alertas %>% mutate(ex_edad = as.integer(abs(tb - med) > 3*sdv))
}
# Asegurar que ex_edad exista aunque no haya teacher_birth
if (!"ex_edad" %in% names(alertas)) {
  alertas$ex_edad <- NA_integer_
}

# 5) Duplicados ---------------------------------------------------------------
# Usa tu `teacher_fullname` (creada en el import) + colegio.
caract_especi <- c("á"="a","é"="e","í"="i","ó"="o","ú"="u","Á"="A","É"="E","Í"="I","Ó"="O","Ú"="U","ñ"="n","Ñ"="N")

if ("teacher_fullname" %in% names(alertas)) {
  alertas <- alertas %>%
    mutate(
      fullnorm = stringr::str_squish(stringr::str_replace_all(stringr::str_to_upper(teacher_fullname), caract_especi))
    )
} else {
  # si faltara, lo armamos desde los 4 componentes (por si acaso)
  for (v in c("teacher_name_1","teacher_name_2","teacher_lastname_1","teacher_lastname_2"))
    if (!v %in% names(alertas)) alertas[[v]] <- NA_character_
  alertas <- alertas %>%
    mutate(
      fullnorm = stringr::str_squish(stringr::str_replace_all(
        stringr::str_to_upper(paste(teacher_name_1, teacher_name_2, teacher_lastname_1, teacher_lastname_2)),
        caract_especi))
    )
}
if (!"teacher_school" %in% names(alertas)) alertas$teacher_school <- NA_character_

alertas <- alertas %>%
  mutate(duplicado = if_else(
    duplicated(select(., fullnorm, teacher_school)) |
      duplicated(select(., fullnorm, teacher_school), fromLast = TRUE),
    1L, 0L, missing = 0L
  ))

# 6) Monitoreo por colegios (opcional CSV local) ------------------------------

# 9.  Monitoreo por colegios ---------------------------------------------------

## Match encuestas docentes vs encuestas estudiantes ---------------------------
# Se busca que no se haya filtrado la encuesta en colegios que no forman parte del proyecto

library(googlesheets4)

# 1.  Leer la lista de colegios desde la hoja de cálculo
gs4_deauth()     
url_match <- "https://docs.google.com/spreadsheets/d/1hPdqGFA_Yvifx7wLYk5uBvJn7Y9hPSQ1VwsTGwF9uhE/edit?gid=0"

lista_match <- read_sheet(url_match, sheet = "lista", range = "A:A", col_types = "c")

# 2.  Vector con los colegios de la lista
colegios_match <- unique(lista_match[[1]])

# 3.  Vector de colegios a excluir
colegios_excluir <- c(
  "111001012360", "111001076767", "111001800694",
  "111001107778", "111001801268",
  "111001001279", "111001015601"
)

# 4.  Crear la dummy en alertas
alertas <- alertas %>%
  mutate(
    lb_estudiantes_match = if_else(
      teacher_school %in% colegios_match & !(teacher_school %in% colegios_excluir),
      1L, 0L, missing = 0L
    )
  )

## Colegios sin encuesta docente ---------------------------------------------

# 1.  Conjuntos de IDs
ids_match     <- setdiff(unique(lista_match[[1]]), colegios_excluir)   
ids_docentes  <- unique(alertas$teacher_school)                        

# 2.  Diferencia: solo los colegios de estudiantes que faltan en las encuestas docentes
ids_faltantes <- setdiff(ids_match, ids_docentes)

# 3.  Contador
n_faltantes <- length(ids_faltantes)

# 4.  Añadir columnas constantes a toda la tabla
alertas <- alertas %>% 
  mutate(
    colegios_sin_docente = n_faltantes,
    ids_sin_docente      = if (n_faltantes > 0) 
      paste(ids_faltantes, collapse = "; ") 
    else 
      NA_character_
  )

## Colegios con encuesta docente -----------------------------------------------


# Vector único de colegios presentes en la encuesta docente
ids_docentes2 <- setdiff(
  unique(as.character(alertas$teacher_school)),  # todos los que aparecen
  colegios_excluir                               # quita los no válidos
)

# Contador
n_colegios_docente <- length(ids_docentes2)

## Colegios eliminados ---------------------------------------------------------

n_colegios_excluir <- length(unique(colegios_excluir))


alertas <- alertas %>%
  mutate(
    
    colegios_con_docente   = n_colegios_docente,
    colegios_eliminados    = n_colegios_excluir
  )


## Clasificación por control y tratamiento--------------------------------------

colegios_tratamiento <- c(
  "111001014109", "111001041599", "111001046477", "111001098876", "111001098884",
  "111001100064", "111001104051", "111001104299", "111001107867", "111001107883",
  "111001800384", "111001800392", "111001800678", "111001801080", "111001801098",
  "111102000265", "111102000753", "111102000958", "211001076958", "211102000201",
  "211102000995", "111001001279", "111001011690", "111001012360", "111001013153",
  "111001013161", "111001013170", "111001014974", "111001015598", "111001015601",
  "111001024732", "111001027308", "111001027391", "111001029114", "111001034045",
  "111001079154", "111001086771", "111001098906", "111001100056", "111001104281",
  "111001104558", "111001110477", "111001800431", "111001801241", "111001002330",
  "111001015776", "111001086665", "111001094889", "111001098949", "111001104043",
  "111001104264", "111001107786", "111001800813", "111769000174", "111769000247",
  "111769000956", "111769003122", "111769003416", "111769003424", "111769004188"
)

colegios_control <- c(
  "111001002909", "111001010031", "111001013676", "111001046591", "111001086606",
  "111001086754", "111001100072", "111001104183", "111001104302", "111001104329",
  "111001106950", "111001106968", "111001107832", "111001107875", "111001800457",
  "111001800643", "111001800694", "111001801101", "111102000281", "111102000621",
  "211102000243", "111001000124", "111001011321", "111001012343", "111001013102",
  "111001013129", "111001014290", "111001016071", "111001016098", "111001016101",
  "111001016136", "111001016292", "111001024660", "111001026964", "111001027405",
  "111001033898", "111001041475", "111001076767", "111001086649", "111001098892",
  "111001100048", "111001106984", "111001107778", "111001800554", "111001801268",
  "111001025020", "111001045225", "111001075272", "111001075515", "111001093084",
  "111001104256", "111001104388", "111001107069", "111001107077", "111001800163",
  "111001801250", "111001801349", "111769001502", "111769001871", "111001092983",
  "111769003360"
)

alertas <- alertas %>%
  mutate(
    tratamiento = case_when(
      teacher_school %in% colegios_tratamiento  ~ "Colegio Tratamiento",
      teacher_school %in% colegios_control  ~ "Colegio Control",
      TRUE ~ NA_character_))



# 8) Rechazos y totales para Looker ------------------------------------------
# consentimiento == 2 -> rechazo (ajusta si tu código es otro)
if (!"consentimiento" %in% names(alertas)) alertas$consentimiento <- NA

alertas <- alertas %>%
  mutate(
    flag_rejected = if_else(as.numeric(consentimiento) == 2, 1L, 0L),
    flag_duplicated = if_else(duplicado == 1, 1L, 0L),
    flag_missing = if_else(!is.na(total_missing) & total_missing > 0, 1L, 0L),
    flag_extreme_values = if_else(!is.na(ex_edad) & ex_edad == 1, 1L, 0L, missing = 0L),
    total_encuestas = n()
  ) %>%
  mutate(
    Exitos  = if_else(flag_duration_mas==0 & flag_duration_menos==0 &
                        flag_duplicated==0 & flag_missing==0 &
                        flag_extreme_values==0 & (if ("flag_ns" %in% names(.)) flag_ns==0 else TRUE) &
                        flag_rejected==0, 1L, 0L),
    Alertas = if_else(flag_duration_mas==1 | flag_duration_menos==1 |
                        flag_duplicated==1 | flag_missing==1 |
                        (if ("flag_ns" %in% names(.)) flag_ns==1 else FALSE) |
                        flag_extreme_values==1, 1L, 0L),
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
# si quieres anclar algunas nuevas al principio del bloque nuevo, lístalas aquí:
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
