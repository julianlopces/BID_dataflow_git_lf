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

seguimiento_colegios_detalle <- lbase %>%
  full_join(seguimiento_colegios_lb, by = c("COD_COLEGIO" = "school_final"))%>%
  mutate(tratamiento = if_else(COD_COLEGIO %in% colegios_tratamiento,
                               "Tramiento","Control"),
         avance_total = (exitos/TOTAL) * 100,
         avance_lb = ((exitos_lb + alertas_lb)/TOTAL_LB) * 100)%>%
  arrange(desc(Ausentes_lb))



# Estado desconocido 


lista_estudiantes  <- read_sheet(id_alertas,
                    sheet = "bid_listado")%>%
  mutate(ID = as.character(ID))%>%
  filter(!ID %in% ID_lbase_corregido$ID )

lista_estudiantes_pendiente <- lista_estudiantes %>%
  filter(!ID %in% alertas_sin_duplicados$student_id_final)



pendiente_estudiantes_colegios <- lista_estudiantes_pendiente %>%
  group_by(COD_COLEGIO)%>%
  summarise(pendientes = sum(LB == 0, na.rm = TRUE),
            pendientes_lb = sum(LB == 1, na.rm = TRUE))%>%
  arrange(desc(pendientes_lb))%>%
  mutate(COD_COLEGIO = as.character(COD_COLEGIO))



# Añadir pendientes


seguimiento_colegios_detalle_final <- seguimiento_colegios_detalle %>%
  left_join(pendiente_estudiantes_colegios, by = "COD_COLEGIO")%>%
  mutate(estado_conocido = (l_base/TOTAL_LB)*100,
         estado_conocido = if_else(estado_conocido > 100,100,estado_conocido))%>%
  arrange(desc(estado_conocido))


# Colegios priorizados

colegios_priorizados <- seguimiento_colegios_detalle_final %>%
  select(COD_COLEGIO,COLEGIO,avance_lb,Ausentes,Ausentes_lb,pendientes,pendientes_lb)%>%
  group_by(COD_COLEGIO,COLEGIO)%>%
  mutate(total_faltantes = sum(Ausentes,Ausentes_lb,pendientes,pendientes_lb,na.rm = TRUE))%>%
  filter((Ausentes >= 3 | Ausentes_lb >= 3 | pendientes >= 3 | pendientes_lb >= 3) & avance_lb > 0)%>%
  arrange(desc(total_faltantes))



