# rectores_lf/scripts/1_import.R
message("==> Import Rectores LF iniciado")

safe_lib(c("dplyr","stringr","tidyr","purrr","readr","lubridate"))

# --- checks de credenciales que setea 0_Master.R ---
if (!exists("server") || !exists("email") || !exists("password") || !exists("formid"))
  stop("Faltan credenciales o formid. Revisa 0_Master.R y .env")

# --- 1) descarga cruda (usa tu util de utils_api.R) ---
# Compatibilidad con distintas firmas de scto_download_json_wide
.argnames <- try(names(formals(scto_download_json_wide)), silent = TRUE)

if (inherits(.argnames, "try-error") || is.null(.argnames)) {
  stop("No encuentro scto_download_json_wide() en utils_api.R")
}

if (all(c("server","formid","email","password") %in% .argnames)) {
  raw_df <- scto_download_json_wide(server = server, formid = formid,
                                    email  = email,  password = password)
} else if (all(c("server","formid","user","password") %in% .argnames)) {
  raw_df <- scto_download_json_wide(server = server, formid = formid,
                                    user   = email,  password = password)
} else {
  # última opción: llamada posicional (server, formid, email, password)
  raw_df <- scto_download_json_wide(server, formid, email, password)
}
rm(.argnames)

survey_order <- names(raw_df)

# --- 2) helpers para nombres ---
SENTINEL <- c("99","999","9999","99999")

# normaliza un token: a character, trim, upper; NA si vacío o sentinel
norm_token <- function(x){
  if (is.numeric(x)) x <- format(x, trim = TRUE, scientific = FALSE)
  x <- as.character(x)
  x <- trimws(x)
  if (is.na(x) || x == "" || x %in% SENTINEL) return(NA_character_)
  toupper(x)
}

# concatena tokens ya normalizados, saltando NA; si no hay nada -> NA
make_fullname <- function(df, cols){
  purrr::pmap_chr(df[, cols, drop = FALSE], function(...){
    xs <- list(...)
    xs <- vapply(xs, norm_token, FUN.VALUE = character(1))
    xs <- xs[!is.na(xs) & xs != ""]
    if (length(xs) == 0) NA_character_ else paste(xs, collapse = " ")
  })
}

# --- 3) variables nuevas ---
# define columnas que usaremos (por si faltan, créalas vacías para no romper)
need_cols1 <- c("p101_teacher_name_1","p101_teacher_name_2",
                "p101_teacher_lastname_1","p101_teacher_lastname_2")
need_cols2 <- c("p101_teacher_name_1","p101_teacher_lastname_1")

for (c in unique(c(need_cols1, need_cols2))) {
  if (!c %in% names(raw_df)) raw_df[[c]] <- NA
}

df <- raw_df

# fullname1_rector: (name1, name2, lastname1, lastname2), todos en UPPER, omite 99/999...
fullname1_rector <- make_fullname(df, need_cols1)

# fullname2: (name1, lastname1), misma lógica
fullname2 <- make_fullname(df, need_cols2)

# id_rector = fullname2 + "_" + teacher_school_label
# fallback si no existe teacher_school_label: usa teacher_school
school_lbl <- dplyr::coalesce(
  df$teacher_school_label %||% NULL,
  df$teacher_school        %||% NULL
)


norm_id_piece <- function(x){
  x <- ifelse(is.na(x), NA_character_, x)
  ifelse(is.na(x), NA_character_,
         stringr::str_replace_all(stringr::str_to_lower(trimws(x)), "\\s+", "_"))
}
id_rector <- ifelse(is.na(fullname2) | is.null(school_lbl) | is.na(school_lbl),
                    NA_character_,
                    paste0(norm_id_piece(fullname2), "_", norm_id_piece(as.character(school_lbl))))


# --- 4) objeto final `data` = crudas + columnas nuevas ---
data <- df %>%
  mutate(
    fullname1_rector = fullname1_rector,
    fullname2        = fullname2,
    id_rector        = id_rector
  ) %>%
  # convertir strings vacíos a NA
  mutate(across(where(is.character), ~na_if(.x, ""))) %>%
  tibble::as_tibble()

attr(data, "survey_order") <- survey_order

message("==> Import Rectores LF finalizado; n=", nrow(data))
