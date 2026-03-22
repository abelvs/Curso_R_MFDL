pacman::p_load("tidyverse", "data.table", "readxl", "janitor", "sf", "writexl", "stringi")

#Cargamos shape alcaldias
shape_munis <- st_read("01_Inputs/Sesion_2/Shape_CDMX/shape_munis_cdmx.shp") %>% 
  filter(CVE_ENT == "09")


#Cargamos primer archivo de lluvias
lluvias_ene <- fread("01_Inputs/Sesion_2/Lluvias_CONAGUA/ene.csv", encoding = "Latin-1") %>% 
  clean_names() %>% 
  filter(edo == "DF") %>% 
  mutate(coords = paste0(lat, ",", lon))


#Generamos puntos de estaciones
estaciones_sf <- st_as_sf(lluvias_ene, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(shape_munis))


#ploteamos
plot(st_geometry(shape_munis), col = "lightblue", main="Alcaldías CDMX")
plot(st_geometry(estaciones_sf), col = "red", add = TRUE)

#Generamos base de tomas con alcaldía (vemos que punto cae en que alcaldia y le pegamos el nombre)
estaciones_con_alcaldia <- st_join(estaciones_sf, shape_munis["NOMGEO"], join = st_within)

cat_estaciones <- lluvias_ene %>% 
  left_join((estaciones_con_alcaldia %>% select(clave, NOMGEO))) %>% 
  rename(alcaldia = NOMGEO) %>% 
  filter(!is.na(alcaldia)) %>% 
  mutate(alcaldia_clean = str_to_upper(alcaldia),
         alcaldia_clean = stri_trans_general(alcaldia_clean, "Latin-ASCII"),
         alcaldia_clean = str_replace_all(alcaldia_clean, "\\.", "")) %>%  
  select(clave, estacion, alcaldia, alcaldia_clean, lat, lon, coords)


write_xlsx(cat_estaciones, "01_Inputs/Sesion_2/Catalogo_estaciones.xlsx")


#Hechos de tránsito----
hechos <- fread("01_Inputs/Sesion_2/nuevo_acumulado_hechos_de_transito.csv") %>% 
  mutate(fecha_evento = as.Date(fecha_evento, format = "%d/%m/%Y"),
         mes = month(fecha_evento),
         ano = year(fecha_evento)) %>% 
  mutate(alcaldia = recode(alcaldia, 
                           "CUAJIMALPA" = "CUAJIMALPA DE MORELOS",
                           "MAGDALENA CONTRERAS" = "LA MAGDALENA CONTRERAS",
                           "GUSTAVO A. MADERO" = "GUSTAVO A MADERO")) %>% 
  filter(ano == 2023)

write_xlsx(hechos, "01_Inputs/Sesion_2/Hechos_de_transito_clean.xlsx")


