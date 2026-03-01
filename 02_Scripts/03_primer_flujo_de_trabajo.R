############################################
# Primer flujo de trabajo
############################################

#Task:

library(tidyverse)
library(data.table)
library(janitor)
library(readxl)
library(openxlsx)
library(writexl)

#Cargamos la bdd----

df <- fread("01_Inputs/Sesion_1/Airbnb.csv") %>% 
  clean_names() 


#Task. Crear un indice de satisfacción que considere la satisfacción promedio (guest_satisfaction_score) entre el precio. 
#Ubicar la ciudad, tipo de apartamento (room_type) y cercanía (en 3 categorías) para obtener el tipo de propiedad que genera 
#mayor satisfacción entre clientes


tipo_cambio <- 17.80

df_analisis <- df %>% 
  mutate(price_mxn = price_total * tipo_cambio,
         cat_distancia_centro = case_when(distance_city_center >= 10 ~ "+10km",
                                          distance_city_center > 5 & distance_city_center < 10 ~ "5 a 10 km",
                                          distance_city_center > 2 & distance_city_center <= 5 ~ "2 a 5 km",
                                          distance_city_center > 1 & distance_city_center <= 2 ~ "1 a 2 km",
                                          distance_city_center <= 1 ~ "- 1 km",
                                          TRUE ~ NA_character_ )) %>% 
  group_by(city, room_type, cat_distancia_centro) %>% 
  summarise(transacciones = n(),
            precio_promedio = mean(price_mxn),
            satisfaccion_promedio = mean(guest_satisfaction_score)) %>% 
  ungroup() %>% 
  mutate(indice_satisfaccion = satisfaccion_promedio/precio_promedio)



mejor_propiedad <- df_analisis %>% 
  slice_max(indice_satisfaccion, n = 1)


write_xlsx(df_analisis, "03_Outputs/Resultados_analisis.xlsx")
