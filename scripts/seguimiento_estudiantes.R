### Seguimiento estudiantes sin estado colegios >= 80 %>%

colegios_mas_80 <- seguimiento_colegios_2 %>%
  filter(avance_lb >= 80 & avance_lb < 100)%>%
  pull(COD_COLEGIO)

# Importar base de datos

