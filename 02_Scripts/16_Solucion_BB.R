
pacman::p_load("dplyr", "ggplot2", "lubridate", "data.table", "janitor", "stringr",
               "httr", "jsonlite", "stringi", "ggthemes", "scales", "purrr", "tibble", "readxl", "tidyr",
               "sf", "viridis")

# CARGA DE DATOS ---

#Cargamos catálogo de localidades

cat_localidades <- fread("01_Inputs/Sesion 6/AGEEML_20264121939532.csv",
                         encoding = "Latin-1") %>% 
  clean_names()

#convertimos a numéricas las columnas de población y vivienda.
#creamos clave de localidad a 9 dígitos, de municipio a 5 y de entidad a 2

cat_clean <- cat_localidades %>% 
  mutate(across(c(contains("pob_"), total_de_viviendas_habitadas), as.numeric)) %>% 
  mutate(clave_localidad = str_pad(cvegeo, side = "left", width = 9, pad = "0"),
         clave_municipio = substr(clave_localidad, 1,5),
         clave_entidad = substr(clave_municipio, 1,2),
         clave_concat = str_to_upper(stri_trans_general(paste0(nom_ent, nom_mun, nom_loc), "Latin-ASCII")))

#Generamos catálogo municipal con poblacion y viviendas

cat_municipal <- cat_clean %>% 
  group_by(clave_municipio) %>% 
  summarise(clave_entidad = first(clave_entidad),
            nom_ent = first(nom_ent),
            nom_mun = first(nom_mun),
            pob_total = sum(pob_total, na.rm = T),
            pob_masculina = sum(pob_masculina, na.rm = T),
            pob_femenina = sum(pob_femenina, na.rm = T),
            total_de_viviendas_habitadas = sum(total_de_viviendas_habitadas, na.rm = T))


cat_entidad <- cat_municipal %>% 
  group_by(clave_entidad) %>% 
  summarise(nom_ent = first(nom_ent),
            nom_mun = first(nom_mun),
            pob_total = sum(pob_total, na.rm = T),
            pob_masculina = sum(pob_masculina, na.rm = T),
            pob_femenina = sum(pob_femenina, na.rm = T),
            total_de_viviendas_habitadas = sum(total_de_viviendas_habitadas, na.rm = T))

#Cargamos base de BB----

dir_banbi <- read_xlsx("01_Inputs/Sesion 6/Cat_Babien.xlsx") %>% 
  clean_names()

cat_clean_sin_dups <- cat_clean %>% distinct(clave_concat, .keep_all = T)

dir_banbi_clean <- dir_banbi %>% 
  mutate(entidad = recode(entidad,
                          "Coahuila" = "Coahuila de Zaragoza",
                          "Michoacán" = "Michoacán de Ocampo",
                          "Veracruz" = "Veracruz de Ignacio de la Llave"),
         clave_concat = str_to_upper(stri_trans_general(paste0(entidad,
                                                               municipio_alcaldia,
                                                               localidad), "Latin-ASCII"))) %>% 
  left_join((cat_clean_sin_dups %>% select(clave_concat, clave_localidad, clave_municipio, clave_entidad)), by = "clave_concat")
  
filtro <- dir_banbi_clean %>% filter(is.na(clave_municipio))


#Analisis----

tabla_entidad <- dir_banbi_clean %>% 
  group_by(clave_entidad) %>% 
  summarise(bancos = n()) %>% 
  left_join(cat_entidad, by = "clave_entidad") %>% 
  mutate(bancos_por_persona = bancos/pob_total,
         bancos_por_vivienda = bancos/total_de_viviendas_habitadas) %>% 
  select(clave_entidad, nom_ent, pob_total, bancos, bancos_por_persona, bancos_por_vivienda)

tabla_mun <- dir_banbi_clean %>% 
  group_by(clave_municipio) %>% 
  summarise(bancos = n()) %>% 
  full_join(cat_municipal, by = "clave_municipio") %>% 
  mutate(bancos_por_persona = bancos/pob_total,
         bancos_por_vivienda = bancos/total_de_viviendas_habitadas) %>% 
  select(clave_municipio, nom_ent, nom_mun, pob_total, bancos, bancos_por_persona, bancos_por_vivienda)




#Descargamos geometrias ----

lista_claves <- sprintf("%02d", 1:32)

# Función para leer geometría municipal por estado
leer_municipios_sf <- function(cve_ent) {
  url <- paste0("https://gaia.inegi.org.mx/wscatgeo/v2/geo/mgem/", cve_ent)
  st_read(url, quiet = TRUE)
}

# Descargar todos los municipios y generar sf
mun_sf <- map_dfr(lista_claves, leer_municipios_sf)


#Generar sf estatal

ent_sf <- mun_sf %>% 
  group_by(cve_ent) %>% 
  summarise()



#Generamos mapas

#Mapa 1_ DEnsisdad de BB ppor entidad

ent_sf_cobertura <- ent_sf %>% 
  left_join(tabla_entidad, by = c("cve_ent" = "clave_entidad"))

ggplot()+
  geom_sf(data = ent_sf_cobertura,
          aes(fill = bancos_por_vivienda))+
  scale_fill_viridis()+
  theme_void() +
  labs(title = "Cobertura de Banco del Bienestar por entidad")


#Mapa 2 Densidad de BB por municipio (Edomex)

mun_sf_cobertura <- mun_sf %>% left_join(tabla_mun, by = c("cvegeo" = "clave_municipio"))

entidad_clave <- "07"

ggplot()+
  geom_sf(data = (mun_sf_cobertura %>% filter(cve_ent == entidad_clave)),
          aes(fill = bancos))+
  geom_point(data = (dir_banbi_clean %>%  filter(clave_entidad == entidad_clave)), aes(x = longitud, y = latitud),
             color = "grey80")+
  scale_fill_viridis(option = "mako")+
  theme_void() +
  labs(title = "Cobertura de Banco del Bienestar por entidad")
  