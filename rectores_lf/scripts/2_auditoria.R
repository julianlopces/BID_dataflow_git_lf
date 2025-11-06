# rectores_lf/scripts/2_auditoria.R
# --------------------------------------------------------------
# Auditoría Rectores LF
# --------------------------------------------------------------

message("==> Auditoría Rectores LF iniciada")

safe_lib(c("dplyr","stringr","tidyr","purrr","readr"))

# helper: verdadero si la celda está vacía (NA o string vacío)

col_get <- function(df, nm) if (nm %in% names(df)) df[[nm]] else rep(NA, nrow(df))

is_blank <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  is.na(x) | trimws(as.character(x)) == ""
}

to_num <- function(x) suppressWarnings(as.numeric(x))
# 0) Base y orden original ----------------------------------------------------
if (!exists("data")) stop("No existe `data` desde el import. Corre 1_import primero.")
alertas <- data
cols_api_order <- names(alertas)   # incluye fullname1_rector, fullname2, id_rector

# 1) IMPUTACIONES -------------------------------------------------------------
# 1.a) cargo := 3 si cargo_99 incluye (coordinador/a con o sin tilde/errores)
norm_txt <- function(x){
  x <- tolower(as.character(x))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x
}
pat_coord <- "(coordinador|coordinadora|cordinador|cordinadora|coodinador)"
alertas <- alertas %>%
  mutate(
    cargo = dplyr::case_when(
      !is.na(cargo) ~ as.numeric(cargo),                                    
      TRUE ~ NA_real_
    ),
    cargo = ifelse(
      grepl(pat_coord, norm_txt(cargo_99 %||% ""), perl = TRUE), 3, cargo
    )
  )

# 1.b) Asignaciones manuales por instanceID (rector y cargo)
fix_ids <- c(
  "uuid:5d828220-0787-46e3-94a9-85dab102145e",
  "uuid:6cbbb1ae-36e5-4baf-9a4c-6000fa84385b",
  "uuid:d1884aa2-c615-49ca-a0e1-42e37e35e960",
  "uuid:db6f858b-2f3a-41e8-a5e3-b519f0c75184",
  "uuid:f86a1544-5abd-442b-810e-0dd91cffecdf",
  "uuid:94069506-9e33-48a0-b7fa-b1cf4c906605",
  "uuid:0017d5d5-c92a-4447-9c17-cd1328398fdf",
  "uuid:c8a2bac7-44de-41ac-a342-c53149e0f6ab",
  "uuid:8ebb043c-fcee-499a-bf2e-b1604268ce22",
  "uuid:2abea777-bbfb-46df-be59-bcf41d0d45d6",
  "uuid:7d0fb92b-b3c9-4fb4-bab2-7189e8858eca",
  "uuid:7a7a2f65-9869-4514-a503-a90ad675b96c",
  "uuid:868c3f4b-8aef-4224-ad4e-5288cbaad4af",
  "uuid:d165f48c-6141-408f-9cf6-a06e61a1d5f3",
  "uuid:7e27c060-e4c9-43d6-aeeb-bc0955045e69",
  "uuid:6c86aa33-2f26-429c-b1c1-a2f9fad25a56",
  "uuid:b8e5ba24-7f2b-437a-8238-b3672036da05",
  "uuid:6341cd38-1377-4828-a0c5-3d651a98f20c",
  "uuid:85783c47-8b41-4857-91c3-077154e94aea",
  "uuid:87e4e7c1-8ccd-4b95-a99a-660269c4a741",
  "uuid:9170532e-51fb-4769-9e8a-7c53f409e9cc",
  "uuid:8b8e8123-d77f-4f8f-9062-03a741dc87ae",
  "uuid:323a8f45-947a-4f54-89e5-3bbb5d36b6fe",
  "uuid:37f61a11-da19-4af7-8192-1a3bb1f5aacf"
)
fix_rector <- c(2,2,1,2,2,2,2,2,1,2,2,1,2,2,1,2,2,1,2,2,2,2,2,2)
fix_cargo  <- c(2,2,NA,3,3,1,3,2,NA,2,3,NA,1,3,NA,3,3,NA,2,2,3,3,2,3)

lk_rector <- tibble::tibble(instanceID = fix_ids, rector_fix = fix_rector, cargo_fix = fix_cargo)

alertas <- alertas %>%
  left_join(lk_rector, by = "instanceID") %>%
  mutate(
    rector = ifelse(!is.na(rector_fix), rector_fix, as.numeric(rector)),
    cargo  = ifelse(!is.na(cargo_fix),  cargo_fix,  as.numeric(cargo))
  ) %>%
  select(-rector_fix, -cargo_fix)

# 2) MISSING FLAGS ------------------------------------------------------------
`%blank%` <- function(x) { is.na(x) | trimws(as.character(x))=="" }

# 2.a) lista de variables para m_*
vars_miss <- c(
  "consentimiento","rector",
  "p101_teacher_name_1","p101_teacher_name_2",
  "p101_teacher_lastname_1","p101_teacher_lastname_2",
  "p102_genero","p103","p105","p106","p107",
  "p108_0","p108","p109","p110"
)
# Variables a evaluar para missing "simples"
var_missing <- c(
  "consentimiento","rector",
  "p101_teacher_name_1","p101_teacher_name_2",
  "p101_teacher_lastname_1","p101_teacher_lastname_2",
  "p102_genero","p103","p105","p106","p107",
  "p108_0","p108","p109","p110"
)

present <- intersect(var_missing, names(alertas))
if (length(present)) {
  alertas <- alertas %>%
    mutate(
      across(
        all_of(present),
        ~ if_else(is_blank(.x), 1L, 0L),
        .names = "m_{.col}"
      )
    )
}

# 2.b) Missings condicionales 
# Traemos vectores "seguros" (si no existe la col -> vector NA)
rc   <- to_num(col_get(alertas, "rector"))
cg   <- col_get(alertas, "cargo");      cg_n   <- to_num(cg)
cg99 <- col_get(alertas, "cargo_99")

p103 <- to_num(col_get(alertas, "p103"))
p104 <- col_get(alertas, "p104")

p110 <- to_num(col_get(alertas, "p110"))

p111_raw <- col_get(alertas, "p111");    p111 <- to_num(p111_raw);   p111_99 <- col_get(alertas, "p111_99")
p112_raw <- col_get(alertas, "p112");    p112 <- to_num(p112_raw);   p112_99 <- col_get(alertas, "p112_99")
p113_raw <- col_get(alertas, "p113");    p113 <- to_num(p113_raw);   p113_99 <- col_get(alertas, "p113_99")
p114     <- col_get(alertas, "p114")
p115_raw <- col_get(alertas, "p115");    p115 <- to_num(p115_raw);   p115_99 <- col_get(alertas, "p115_99")
p116     <- col_get(alertas, "p116")
p117_raw <- col_get(alertas, "p117");    p117 <- to_num(p117_raw);   p117_99 <- col_get(alertas, "p117_99")

alertas <- alertas %>%
  mutate(
    # cargo vacío solo si rector == 2
    m_cargo    = as.integer(!is.na(rc)  & rc  == 2  & is_blank(cg)),
    # descripción de "otro" vacía cuando cargo == 99
    m_cargo_99 = as.integer(!is.na(cg_n) & cg_n == 99 & is_blank(cg99)),
    
    # p104 vacío cuando p103 == 2
    m_p104     = as.integer(!is.na(p103) & p103 == 2 & is_blank(p104)),

    # “99 y sin especificar”
    m_p111_99  = as.integer(!is.na(p111) & p111 == 99 & is_blank(p111_99)),
    m_p112_99  = as.integer(!is.na(p112) & p112 == 99 & is_blank(p112_99)),
    m_p113_99  = as.integer(!is.na(p113) & p113 == 99 & is_blank(p113_99)),
    m_p115_99  = as.integer(!is.na(p115) & p115 == 99 & is_blank(p115_99)),
    m_p117_99  = as.integer(!is.na(p117) & p117 == 99 & is_blank(p117_99))
  )
# 2.c) total_missings
miss_cols <- grep("^m_", names(alertas), value = TRUE)
alertas <- alertas %>%
  mutate(total_missings = if (length(miss_cols)) rowSums(across(all_of(miss_cols)), na.rm = TRUE) else 0L)

# 3) TIEMPO (z-score 3DE) -----------------------------------------------------
get_duration_min <- function(df){
  if ("duration" %in% names(df)) {
    as.numeric(df$duration)/60
  } else if (all(c("starttime","endtime") %in% names(df))) {
    as.numeric(difftime(lubridate::ymd_hms(df$endtime, quiet=TRUE),
                        lubridate::ymd_hms(df$starttime, quiet=TRUE),
                        units = "mins"))
  } else rep(NA_real_, nrow(df))
}

alertas <- alertas %>%
  mutate(duration_minutes = round(get_duration_min(cur_data()), 2))

dv <- alertas$duration_minutes
mu <- mean(dv, na.rm = TRUE); sdv <- sd(dv, na.rm = TRUE); if (!is.finite(sdv) || sdv==0) sdv <- 1
z  <- (dv - mu) / sdv
alertas <- alertas %>%
  mutate(
    flag_tiempomas   = as.integer(!is.na(z) & z >  3),
    flag_tiempomenos = as.integer(!is.na(z) & z < -3)
  )

# 4) DUPLICADOS ---------------------------------------------------------------
alertas <- alertas %>%
  mutate(flag_duplicados = as.integer(duplicated(id_rector) | duplicated(id_rector, fromLast = TRUE)))

# 5) OTROS (muchos 99) --------------------------------------------------------
# Contamos 99 en variables conocidas + todas las *_99 que existen
vars_99_base <- intersect(c("cargo","p104","p111","p112","p113","p115","p117"), names(alertas))
vars_99_suf  <- grep("_99$", names(alertas), value = TRUE)
vars_99 <- unique(c(vars_99_base, vars_99_suf))

alertas <- alertas %>%
  mutate(
    across(all_of(vars_99),
           ~ as.integer(suppressWarnings(as.numeric(.)) == 99),
           .names = "ns_{.col}")
  )
ns_cols <- grep("^ns_", names(alertas), value = TRUE)
alertas <- alertas %>%
  mutate(total_ns = if (length(ns_cols)) rowSums(across(all_of(ns_cols)), na.rm = TRUE) else 0L)

mu_ns <- mean(alertas$total_ns, na.rm = TRUE)
sd_ns <- sd(alertas$total_ns, na.rm = TRUE); if (!is.finite(sd_ns) || sd_ns==0) sd_ns <- 1
alertas <- alertas %>%
  mutate(flag_otros = as.integer(total_ns > mu_ns + 3*sd_ns))

# 6) FLAG_RECTOR --------------------------------------------------------------
rc_raw <- col_get(alertas, "rector")
rc     <- to_num(rc_raw)
cg_raw <- col_get(alertas, "cargo")
cg     <- to_num(cg_raw)

alertas <- alertas %>%
  mutate(
    flag_rector = as.integer(
      is_blank(rc_raw) |                            # 1) rector vacío
        (!is.na(cg) & cg %in% c(1, 2, 99))            # 2) cargo en 1/2/99
    )
  )

# 7) CONTADORES --------------------------------------------------------------
#CONTADORES COLEGIO 
n_colegios          <- dplyr::n_distinct(alertas$teacher_school_label)
n_colegios_validos  <- alertas %>%
  group_by(teacher_school_label) %>%
  summarise(has_ok = any(flag_rector == 0, na.rm = TRUE), .groups = "drop") %>%
  summarise(n = sum(has_ok, na.rm = TRUE)) %>% pull(n)

alertas <- alertas %>%
  mutate(n_colegios = n_colegios,
         n_colegios_validos = n_colegios_validos)

# === CONTEO DE MISSINGs 
miss_cols <- grep("^m_", names(alertas), value = TRUE)

alertas <- alertas %>%
  mutate(
    total_missings = if (length(miss_cols)) {
      rowSums(across(all_of(miss_cols), ~ as.integer(replace_na(., 0L))), na.rm = TRUE)
    } else 0L,
    flag_missings = as.integer(total_missings > 0)
  )


# 8) TOTALES / CLASIFICACIÓN --------------------------------------------------

# Lista de flags a contar 
flag_cols <- c(
  "flag_missings",
  "flag_tiempomas",
  "flag_tiempomenos",
  "flag_duplicados",
  "flag_otros",
  "flag_rector"
)
flag_cols <- intersect(flag_cols, names(alertas))

alertas <- alertas %>%
  mutate(
    rechazos = as.integer(suppressWarnings(as.numeric(consentimiento)) == 2),
    total_alertas = if (length(flag_cols)) {
      rowSums(across(all_of(flag_cols), ~ as.integer(replace_na(., 0L))), na.rm = TRUE)
    } else 0L,
    alertas = as.integer(total_alertas > 0 & rechazos==0),
    exitos  = as.integer(rechazos == 0 & alertas == 0)
  )

# ===== Tabla por COLEGIOS (con TRATAMIENTO) ==================================
safe_lib(c("dplyr","stringr","readr"))

# 1) Leer la referencia de colegios (COD_DANE, EST_EDUC, TRATAMIENTO)
colegios_ref <- readr::read_csv("adjunto_colegio.csv", show_col_types = FALSE)

# Normalizamos y nos quedamos con lo necesario
colegios_base <- colegios_ref %>%
  dplyr::transmute(
    id_colegio            = stringr::str_pad(as.character(COD_DANE), 12, pad = "0"),
    teacher_school_label  = as.character(EST_EDUC),
    # TRATAMIENTO a dummy 0/1 de forma robusta
    TRATAMIENTO = {
      v <- as.character(TRATAMIENTO)
      vn <- suppressWarnings(as.numeric(v))
      # si viene texto, mapeamos a 0/1
      ifelse(
        is.na(vn),
        ifelse(toupper(trimws(v)) %in% c("1","SI","SÍ","TRUE","TRATAMIENTO","T"), 1L,
               ifelse(toupper(trimws(v)) %in% c("0","NO","FALSE","CONTROL","C"), 0L, NA_integer_)),
        as.integer(vn)
      )
    }
  )

# 2) Colegios a excluir (no deben aparecer)
colegios_excluir <- c("111001012360","111001076767","111001800694",
                      "111001107778","111001001279","111001801268","111001015601")

colegios_base <- colegios_base %>%
  dplyr::filter(!id_colegio %in% colegios_excluir) %>%
  dplyr::distinct(id_colegio, .keep_all = TRUE)

# 3) Conteo de representantes por colegio desde la base auditada
#    (representante = flag_rector == 0 y encuesta no rechazada)
#    p108 es el código de 12 dígitos del colegio en la encuesta
id_enc_col <- if ("p108" %in% names(alertas)) "p108" else NA_character_

conteo_repres <- alertas %>%
  dplyr::mutate(
    id_colegio = if (!is.na(id_enc_col))
      stringr::str_pad(as.character(.data[[id_enc_col]]), 12, pad = "0")
    else NA_character_,
    valido_representante = (flag_rector == 0 & rechazos == 0)
  ) %>%
  dplyr::group_by(id_colegio) %>%
  dplyr::summarise(n_repres = sum(valido_representante, na.rm = TRUE), .groups = "drop")

# 4) Armar la tabla final de colegios (incluye TODOS los colegios de la ref,
#    aunque no tengan encuesta aún -> n_repres = 0)
colegios_rectores <- colegios_base %>%
  dplyr::left_join(conteo_repres, by = "id_colegio") %>%
  dplyr::mutate(
    n_repres    = dplyr::coalesce(n_repres, 0L),
    TRATAMIENTO = dplyr::coalesce(TRATAMIENTO, 0L)  # si faltara, asumimos 0
  ) %>%
  dplyr::select(id_colegio, teacher_school_label, TRATAMIENTO, n_repres) %>%
  dplyr::arrange(id_colegio)


# (Opcional) chequeo rápido de filas esperadas
message("colegios_rectores: filas = ", nrow(colegios_rectores))

# 9) ORDEN DE COLUMNAS --------------------------------------------------------
#  (i) columnas crudas del import en su orden, (ii) m_*, (iii) total_missings,
# (iv) flags, (v) contadores n_colegios*, (vi) rechazos/alertas/exitos.
m_cols      <- grep("^m_", names(alertas), value = TRUE)
flag_cols   <- c("flag_missing","flag_tiempomas","flag_tiempomenos","flag_duplicados",
                 "flag_otros","flag_rector")
flag_cols   <- intersect(flag_cols, names(alertas))
counter_cols<- c("n_colegios","n_colegios_validos")
tail_cols   <- c("rechazos","total_alertas","alertas","exitos")
ns_cols     <- grep("^ns_", names(alertas), value = TRUE)   # los dejamos al final, detrás de flags

orden <- c(
  cols_api_order,
  m_cols,
  "total_missings",
  flag_cols,
  counter_cols,
  # opcionales diagnósticos
  "duration_minutes","total_ns", ns_cols,
  tail_cols
)
orden <- unique(c(orden, names(alertas)))  # por si faltó algo

# === Base cruda con orden del Survey + 3 variables nuevas al final ============
survey_order <- attr(data, "survey_order")
if (is.null(survey_order)) {
  # fallback por si no se añadió el atributo en import
  survey_order <- names(data)
}

final_order_cruda <- c(
  survey_order[survey_order %in% names(data)],
  c("fullname1_rector","fullname2","id_rector")
)

base_cruda_orden <- dplyr::select(data, dplyr::any_of(final_order_cruda))

# Guardar esa versión ordenada
out_dir <- "rectores_lf/data"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
tag <- format(Sys.time(), "%Y%m%d_%H%M")

readr::write_csv(base_cruda_orden, file.path(out_dir, paste0("rectores_LF_cruda_orden_", tag, ".csv")))
saveRDS(base_cruda_orden,          file.path(out_dir, paste0("rectores_LF_cruda_orden_", tag, ".rds")))

alertas <- dplyr::select(alertas, any_of(orden))

message("==> Auditoría Rectores LF finalizada")
