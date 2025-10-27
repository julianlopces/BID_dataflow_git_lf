### Seguimiento estudiantes sin estado colegios >= 80 %>%

colegios_mas_80 <- seguimiento_colegios_2 %>%
  filter(avance_lb >= 80 & avance_lb < 100)%>%
  pull(COD_COLEGIO)

# Importar base de datos

lista_instrumento <- readr::read_csv("data/lista_instrumento.csv")

revisar_sin_id <- lista_instrumento %>%
  mutate(sin_id = str_length(ID) == 5)%>%
  filter(sin_id | lag(sin_id) | lead(sin_id))%>%
  mutate(NOMBRE_COMPLETO = trimws(NOMBRE_COMPLETO),
    duplicado = (duplicated(NOMBRE_COMPLETO, CODIGO_COMPUESTO) |
                   duplicated(NOMBRE_COMPLETO, CODIGO_COMPUESTO,
                              fromLast = T)))%>%
  filter(duplicado)%>%
  mutate(COD_SEDE = as.character(COD_SEDE))%>%
  arrange(NOMBRE_COMPLETO,sin_id)%>%
  mutate(ID_4 = if_else(sin_id == TRUE, lag(ID),ID),
         LB = if_else(sin_id == FALSE, lead(LB), LB ))%>%
  select(ID,ID_4,LB,NOMBRE_COMPLETO,COLEGIO,CODIGO_COMPUESTO)



revision_manual <- lista_instrumento %>%
  mutate(sin_id = str_length(ID) == 5)%>%
  filter(sin_id | lag(sin_id) | lead(sin_id))%>%
  mutate(NOMBRE_COMPLETO = trimws(NOMBRE_COMPLETO),
         duplicado = (duplicated(NOMBRE_COMPLETO, CODIGO_COMPUESTO) |
                        duplicated(NOMBRE_COMPLETO, CODIGO_COMPUESTO,
                                   fromLast = T)))%>%
  filter(!duplicado)


revision_manual_2 <- revision_manual %>%
  filter(sin_id | lag(sin_id) | lead(sin_id))%>%
  mutate(mismo_salon = duplicated(CODIGO_COMPUESTO) | duplicated(CODIGO_COMPUESTO, fromLast = T))%>%
  filter(mismo_salon)%>%
  arrange(NOMBRE_COMPLETO, sin_id)%>%
  select(ID,LB,COLEGIO,CODIGO_COMPUESTO,NOMBRE_COMPLETO)



  
revision_manual_5 <- edit(revision_manual_3)



revision_manual_4 <- revision_manual_5 %>%
  filter(is.na(REV_MANUAL))%>%
  mutate(sin_id = str_length(ID) == 5,
         digitos = str_length(ID))%>%
  arrange(NOMBRE_COMPLETO,digitos)



id_4 <- revision_manual_4 %>% filter(digitos == 4)


id_5 <- revision_manual_4 %>% filter(digitos == 5)%>%
  mutate(ID_4 = id_4$ID, NOMBRE_CORRECTO = id_4$NOMBRE_COMPLETO,
         CODIGO_COMPUESTO_4 = id_4$CODIGO_COMPUESTO)


  select(ID,ID_4,LB,NOMBRE_CORRECTO,COLEGIO,CODIGO_COMPUESTO)%>%
  rename(NOMBRE_COMPLETO = NOMBRE_CORRECTO)

ID_revisados <- rbind(revisar_sin_id,id_5)



export_sheet(ID_revisados, sheet, "ID revisados",  label = "ID revisados",pause = 5)


  

export_sheet(revisar_sin_id, sheet, "ID crudos revisados 1",  label = "ID revisados",pause = 5)
export_sheet(id_5, sheet, "ID crudos revisados 2",  label = "ID revisados",pause = 5)

  

sum(alertas$student_id %in% ID_revisados$ID_4)

sum(alertas$student_id %in% ID_revisados$ID)





lista_instrumento_corr <- lista_instrumento %>%
  filter(!ID %in% ID_lbase_corregido$ID)%>%
  mutate(LB = if_else(ID %in% ID_lbase_corregido$ID_4,1,LB),
         COD_SEDE = as.character(COD_SEDE))%>%
  filter(! ID %in% c(1:25))




lbase_corregido <- lista_instrumento_corr %>%
  group_by(COD_COLEGIO,COD_SEDE)%>%
  summarise(TOTAL_LB = sum(LB),
            TOTAL = n())%>%
  mutate(COD_COLEGIO = as.character(COD_COLEGIO))%>%
  left_join(lbase %>% select(COD_COLEGIO,COLEGIO), by = "COD_COLEGIO")%>%
  relocate(COLEGIO ,.after = COD_SEDE)
  
    
export_sheet(lbase_corregido, sheet, "meta_lb", label = "colegios",         pause = 5)
export_sheet(lista_instrumento_corr, sheet, "listado corregido LF", label = "colegios",         pause = 5)




    