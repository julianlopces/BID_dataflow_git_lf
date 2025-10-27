#### Importar datos desde Survey ####

# Asegurarse de que las credenciales necesarias estén disponibles
if (exists("email") && exists("password") && exists("server") && exists("formid")) {
  message("Credenciales de Survey cargadas correctamente.")
} else {
  stop("No se encontraron las credenciales de Survey. Asegúrate de cargarlas desde el script maestro.")
}


data_ejemplo <-read_excel("data/data_ejemplo_bid.xlsx")
vars_needed <- colnames(data_ejemplo)


# Completar aquí para descargar datos de survey 

# Download de API ----------------------------------------------------------

## Conect to SurveyCTO ----------------------------------------------------------------

API <- paste0('https://',server,'.surveycto.com/api/v2/forms/data/wide/json/',formid,'?date=0')


## Import data -------------------------------------------------------------

max_attempts <- 10
attempt <- 1

repeat {
  # Llamada a la API
  dataset_json <- POST(
    url = API,
    config = authenticate(email, password),
    add_headers("Content-Type: application/json"),
    encode = 'json'
  )
  
  # Convertir JSON a data frame
  data <- jsonlite::fromJSON(rawToChar(dataset_json$content), flatten = TRUE)
  
  # Si df es un data frame válido, salir del ciclo
  if (is.data.frame(data)) break
  
  # Si se alcanzó el número máximo de intentos, lanzar error y salir
  if (attempt >= max_attempts) {
    stop("Se alcanzó el número máximo de intentos sin obtener un data frame válido.")
  }
  
  # Esperar antes de reintentar
  Sys.sleep(300)
  attempt <- attempt + 1
}

# Transformar base de datos ----------------------------------------------------


for (v in vars_needed) {
  if (!(v %in% names(data))) {
    data[[v]] <- rep(NA, nrow(data))
  }
}

# Organizar variables

# Reordenar y dejar las demás al final
otras_vars <- setdiff(names(data), vars_needed)
data <- data[ , c(vars_needed, otras_vars)]


# Nombres de las variables de opción múltiple
multi_vars <- c("which_data",
                "aldea_juegos")



for (var in multi_vars) {
  var_cols <- names(data)[startsWith(names(data), paste0(var, "_")) & !grepl("_o$", names(data))]
  
  if (length(var_cols) > 0) {
    data <- data %>%
      rowwise() %>%
      mutate(!!var := {
        vals <- c_across(all_of(var_cols))
        
        if (all(is.na(vals))) {
          NA_character_
        } else {
          activos <- which(vals == 1)
          if (length(activos) == 0) NA_character_ else {
            seleccionados <- gsub(paste0("^", var, "_"), "", var_cols[activos])
            paste(seleccionados, collapse = ",")
          }
        }
      }) %>%
      ungroup()
  }
}

# # Unificar variables

data <- data %>%
  mutate(
    # Reemplazar strings vacíos en columnas "pull" por NA
    across(contains("pull"), ~if_else(str_squish(.) == "", NA_character_, .)),
    
    # Limpiar nombres
    name1 = na_if(str_squish(student_name1), ""),
    name2 = na_if(str_squish(student_name2), ""),
    name3 = na_if(str_squish(student_name3), ""),
    name4 = na_if(str_squish(student_name4), ""),
    
    # Convertir "9999" en NA explícitamente
    name2 = if_else(name2 == "9999" | name2 %in% c("NO TIENE","No Tiene","NO","no tiene"), NA_character_, name2),
    name4 = if_else(name4 == "9999" | name4 %in% c("NO TIENE","No Tiene","NO","no tiene"), NA_character_, name4),
    
    # Construcción condicional del nombre completo
    nombre_concatenado = case_when(
      is.na(name2) & !is.na(name3) & !is.na(name4) ~ str_c(name1, name3, name4, sep = " "),
      is.na(name2) & !is.na(name3) & is.na(name4) ~ str_c(name1, name3, sep = " "),
      is.na(name2) & is.na(name3) & !is.na(name4) ~ str_c(name1, name4, sep = " "),
      is.na(name2) & is.na(name3) & is.na(name4) ~ name1,
      !is.na(name2) & !is.na(name3) & is.na(name4) ~ str_c(name1, name2, name3, sep = " "),
      !is.na(name2) & is.na(name3) & !is.na(name4) ~ str_c(name1, name2, name4, sep = " "),
      !is.na(name2) & is.na(name3) & is.na(name4) ~ str_c(name1, name2, sep = " "),
      TRUE ~ str_c(name1, name2, name3, name4, sep = " ")
    ),
    
    # Limpiar espacios finales
    nombre_concatenado = str_squish(nombre_concatenado),
    
    # Consolidar variables de juegos
    games_1_1 = coalesce(games_1_1_1, games_1_1_2),
    games_1_2 = coalesce(games_1_2_1, games_1_2_2),
    games_1_3 = coalesce(games_1_3_1, games_1_3_2),
    games_1_4 = coalesce(games_1_4_1, games_1_4_2),
    games_1_5 = coalesce(games_1_5_1, games_1_5_2),
    games_1_6 = coalesce(games_1_6_1, games_1_6_2),
    games_2_1 = coalesce(games_2_1_1, games_2_1_2),
    games_2_2 = coalesce(games_2_2_1, games_2_2_2),
    games_2_3 = coalesce(games_2_3_1, games_2_3_2),
    games_2_4 = coalesce(games_2_4_1, games_2_4_2),
    games_2_5 = coalesce(games_2_5_1, games_2_5_2),
    games_2_6 = coalesce(games_2_6_1, games_2_6_2),
    age_final = coalesce(edad_pull,edad_corr,student_age),
    gender_final = coalesce(genero_pull,gender),
    gender_final = case_when(
      gender_final == "F" ~ "2",
      gender_final == "M" ~ "1",
      TRUE ~ gender_final)) %>%
  mutate(
    # Reemplazar strings vacíos en columnas específicas de nombres "reject" por NA
    across(all_of(c("student_name1_reject", "student_name2_reject", 
                    "student_name3_reject", "student_name4_reject")), 
           ~if_else(str_squish(.) == "", NA_character_, .)),
    
    # Limpiar nombres
    name1_reject = na_if(str_squish(student_name1_reject), ""),
    name2_reject = na_if(str_squish(student_name2_reject), ""),
    name3_reject = na_if(str_squish(student_name3_reject), ""),
    name4_reject = na_if(str_squish(student_name4_reject), ""),
    
    # Convertir "9999" en NA explícitamente
    name2_reject = if_else(name2_reject == "9999", NA_character_, name2_reject),
    name4_reject = if_else(name4_reject == "9999", NA_character_, name4_reject),
    
    # Construcción condicional del nombre completo
    nombre_concatenado_reject = case_when(
      is.na(name2_reject) & !is.na(name3_reject) & !is.na(name4_reject) ~ str_c(name1_reject, name3_reject, name4_reject, sep = " "),
      is.na(name2_reject) & !is.na(name3_reject) & is.na(name4_reject) ~ str_c(name1_reject, name3_reject, sep = " "),
      is.na(name2_reject) & is.na(name3_reject) & !is.na(name4_reject) ~ str_c(name1_reject, name4_reject, sep = " "),
      is.na(name2_reject) & is.na(name3_reject) & is.na(name4_reject) ~ name1_reject,
      !is.na(name2_reject) & !is.na(name3_reject) & is.na(name4_reject) ~ str_c(name1_reject, name2_reject, name3_reject, sep = " "),
      !is.na(name2_reject) & is.na(name3_reject) & !is.na(name4_reject) ~ str_c(name1_reject, name2_reject, name4_reject, sep = " "),
      !is.na(name2_reject) & is.na(name3_reject) & is.na(name4_reject) ~ str_c(name1_reject, name2_reject, sep = " "),
      TRUE ~ str_c(name1_reject, name2_reject, name3_reject, name4_reject, sep = " ")
    ),
    
    # Limpiar espacios finales
    nombre_concatenado_reject = str_squish(nombre_concatenado_reject),
    # Nombre final priorizando nombre_pull sobre el concatenado
    name_final = str_to_upper(coalesce(nombre_pull, nombre_concatenado, nombre_concatenado_reject)),
    name_final = if_else(str_squish(name_final) == "", NA_character_, name_final))



# Juntar variables finales de la aldea


vars <- paste0("aldea_preguntas_add_", 1:20)

data <- data %>%
  # Asegurar que "" o espacios queden como NA en las columnas objetivo
  mutate(across(all_of(vars), ~ na_if(trimws(as.character(.)), ""))) %>%
  # Coalesce por columnas: toma, en cada fila, el primer no-NA entre las 20 columnas
  mutate(aldea_preguntas_add_total = coalesce(!!!syms(vars)))


vars2 <- paste0("aldea_preguntas_", c(1,3,5,7,9,11,13,15,17,19))

data <- data %>%
  # Asegurar que "" o espacios queden como NA en las columnas objetivo
  mutate(across(all_of(vars2), ~ na_if(trimws(as.character(.)), ""))) %>%
  # Coalesce por columnas: toma, en cada fila, el primer no-NA entre las 20 columnas
  mutate(aldea_preguntas_total = coalesce(!!!syms(vars2)))

# Unificar datos

# Eliminar pilotos

data <- data %>%
  filter(!username %in% c("anonymousUser"),deviceid != "(web)")


data <- data %>%
  filter(!student_id %in% c(1:25))

# Correcciones actualización tablets

data <- data %>%
  mutate(
    # usar primero la condición con el valor original
    school_final = if_else(colegio_pull_id == "111001e11", "111001025020", school_final),
    sede_pull_id = if_else(colegio_pull_id == "111001e11", "11100102502001", sede_pull_id),
    sede_final   = if_else(colegio_pull_id == "111001e11", "11100102502001", sede_final),
    # y al final actualizar la propia llave
    colegio_pull_id = if_else(colegio_pull_id == "111001e11", "111001025020", colegio_pull_id)
  )

# Ajustar Linea de base


ID_lbase_corregido <- read_sheet(id_alertas,
                                          sheet = "ID revisados")


data <- data %>%
  mutate(lb_pull = if_else(student_id %in% ID_lbase_corregido$ID_4, "1",lb_pull))


# Corregir ID 


data <- data %>%
  left_join(ID_lbase_corregido %>% select(ID,ID_4), by = c("student_id" = "ID"))%>%
  mutate(student_id = if_else(!is.na(ID_4),ID_4,student_id))



# School_final_sede_final

data <- data %>%
  mutate(
    school_final= if_else(is.na(school_final) & assent == 1, coalesce(colegio_corr,colegio_pull_id,student_school),school_final),
    sede_final= if_else(is.na(sede_final) & assent == 1, coalesce(sede_corr,sede_pull_id,student_sede),sede_final)
  )


