# Seguimiento por colegio detallado


seguimiento_colegios_lb <- alertas_sin_duplicados %>%
  mutate(
    en_lista  = student_id_yesno == "1" & assent == "1",
    sin_lista = student_id_yesno == "2" & assent == "1",
    colegio_final = coalesce(colegio_str, colegio_pull),
    school_final  = coalesce(colegio_pull_id, school_final),
    # convertir "1"/"0" a lógico
    lb_flag = lb_pull == "1"
  ) %>%
  filter(!is.na(school_final)) %>%
  group_by(school_final) %>%
  summarise(
    total_encuestas = n(),
    Rechazos      = sum(flag_rejected  == 1 & !lb_flag, na.rm = TRUE),
    Rechazos_lb   = sum(flag_rejected  == 1 &  lb_flag, na.rm = TRUE),
    Ausentes      = sum(flag_ausente   == 1 & !lb_flag, na.rm = TRUE),
    Ausentes_lb   = sum(flag_ausente   == 1 &  lb_flag, na.rm = TRUE),
    Retirado      = sum(flag_retirado  == 1 & !lb_flag, na.rm = TRUE),
    Retirado_lb   = sum(flag_retirado  == 1 &  lb_flag, na.rm = TRUE),
    Limitacion    = sum(flag_limitacion== 1 & !lb_flag, na.rm = TRUE),
    Limitacion_lb = sum(flag_limitacion== 1 &  lb_flag, na.rm = TRUE),
    en_lista      = sum(en_lista  & !lb_flag, na.rm = TRUE),
    en_lista_lb   = sum(en_lista  &  lb_flag, na.rm = TRUE),
    sin_lista     = sum(sin_lista & !lb_flag, na.rm = TRUE),
    sin_lista_lb  = sum(sin_lista &  lb_flag, na.rm = TRUE),
    # OJO con nombres reales de columnas (alertas/exitos en minúsculas si así están)
    alertas       = sum(Alertas == 1 & !lb_flag, na.rm = TRUE),
    alertas_lb    = sum(Alertas == 1 &  lb_flag, na.rm = TRUE),
    exitos        = sum(Exitos  == 1 & !lb_flag, na.rm = TRUE),
    exitos_lb     = sum(Exitos  == 1 &  lb_flag, na.rm = TRUE),
    tratamiento   = first(tratamiento),
    # conteo de línea base (casos con lb_pull == "1")
    l_base        = sum(lb_flag, na.rm = TRUE)
  )



# Agregar meta de lb

lbase <- read_sheet(id_alertas,
                    sheet = "meta_lb")

lbase$COD_COLEGIO <- as.character(lbase$COD_COLEGIO)


seguimiento_colegios_detalle <- lbase %>%
  full_join(seguimiento_colegios_lb, by = c("COD_COLEGIO" = "school_final"))%>%
  mutate(tratamiento = if_else(COD_COLEGIO %in% colegios_tratamiento,
                               "Tramiento","Control"),
         avance_total = (exitos/TOTAL) * 100,
         avance_lb = ((exitos_lb + alertas_lb)/TOTAL_LB) * 100)%>%
  arrange(desc(Ausentes_lb))



