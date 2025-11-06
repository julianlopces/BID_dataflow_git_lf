# Reporte NSNR


sheet <- tryCatch({
  gs4_get(id_alertas)
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})

alertas_import <- googlesheets4::read_sheet(sheet,sheet = "alertas_estudiantes")


sum(duplicated(alertas_import$student_id_final))



odk <- readxl::read_xlsx("G:/Unidades compartidas/EQUILIBRIUM   Intranet 🌍📂/🥁 PROYECTOS/🏆 Proyectos S. Desarrollo/148. BID Cohesión Social en Escuelas de Bogotá/Implementación/Instrumento/Instrumentos línea final/Instrumento_BID_Bogotá_linea_final.xlsx")


palabras <- c(
  "frecuencia",
  "acuerdo",
  "pais",
  "pais_padres",
  "fichasdevuelta",
  "frecuencia_paises",
  "frecuencia_ind",
  "frecuencia_col",
  "ejercicios",
  "ejercicios2"
)

# crear el patrón para str_detect
pattern <- paste(palabras, collapse = "|")



odk_99 <- odk %>% 
  filter(str_detect(string = type,pattern))%>%
  pull(name)



vars_nose <- paste0("ns_",odk_99)


alertas_nose <- alertas_import %>%
  mutate(across(
    all_of(odk_99),
    ~ if_else(as.numeric(.x) == 99, 1, 0),
    .names = "nose_{.col}"
  ))


reporte_nose <- alertas_nose %>%
  select(starts_with("nose_"))%>%
  pivot_longer(cols = everything(),
               names_to = "vars",
               values_to = "ns")%>%
  group_by(vars)%>%
  summarise(total_ns = sum(ns,na.rm=T))%>%
  arrange((desc(total_ns)))%>%
  mutate(perc_ns = round(total_ns/nrow(alertas_import[alertas_import$assent == 1,]),4)*100)



reporte_nose_clean <- reporte_nose %>%
  mutate(vars = trimws(str_remove(vars,"nose_")))%>%
  left_join(odk %>% select(name,label), by = c("vars" = "name"))



# Helper genérico para exportar a Google Sheets con manejo de errores y pausa opcional
export_sheet <- function(df, ss, sheet_name, label = sheet_name, pause = 0) {
  message(sprintf("Exportando %s...", label))
  tryCatch({
    sheet_write(df, ss = ss, sheet = sheet_name)
    message(sprintf("Datos de %s exportados correctamente.", label))
  }, error = function(e) {
    stop(sprintf("Error al exportar %s: %s", label, conditionMessage(e)))
  })
  if (pause > 0) Sys.sleep(pause)
}


export_sheet(reporte_nose_clean,sheet, "reporte_ns_nr", label = "alertas", pause = 5)


