############################################
# Operaciones básicas con tidyverse
############################################

library(tidyverse)
library(data.table)
library(janitor)
library(readxl)
library(openxlsx)


#Cargamos el dataset----

df <- fread("01_Inputs/Sesion_1/Airbnb.csv") %>% 
  clean_names() 

#Recomendaciones de exploracion inicial----

summary(df)
glimpse(df)

table(df$country_name)


# Creamos columnas de interés (mutate)----

#Manejo de strings
df_coords <- df %>% 
  mutate(coordenadas = paste0(latitude, ", ", longitude))

df_nombre_ubic <- df %>%
  mutate(nom_ubicacion = paste(city, state, country_name, sep = ", "))

table(df_nombre_ubic$nom_ubicacion)


#Operaciones numéricas

df_agg_score <- df %>% mutate(agg_score = (guest_satisfaction_score+cleanliness_score)/2)

mean(df_agg_score$agg_score)

df_agg_score <- df %>% mutate(agg_score = (guest_satisfaction_score/10 + cleanliness_score) / 2)

mean(df_agg_score$agg_score)

#Uso de vectores para mutate()

tipo_de_cambio <- 17.23

df_mxn <- df %>%  
  mutate(price_mxn = price_total * tipo_de_cambio)

mean(df_mxn$price_mxn)

#Condicionales

hist(df$distance_city_center)

df_central <- df %>% 
  mutate(es_centrico = ifelse(distance_city_center <= 5, 1,0))

summary(df$distance_city_center)

df_central_cats <- df_central %>% 
  mutate(cat_distancia_centro = case_when(distance_city_center >= 10 ~ "Muy lejos",
                                          distance_city_center > 5 & distance_city_center < 10 ~ "Lejos",
                                          distance_city_center > 2 & distance_city_center <= 5 ~ "Cerca",
                                          distance_city_center > 1 & distance_city_center <= 2 ~ "Muy cerca",
                                          distance_city_center <= 1 ~ "En el centro",
                                          TRUE ~ NA_character_)) %>%
  select(distance_city_center, cat_distancia_centro, es_centrico)

head(df_central_cats, 20)

tabyl(df_central$es_centrico)

#Recodficaciones

df_recode <- df %>% 
  mutate(country_name = recode(country_name, "Holy See (Vatican City State)" = "Vatican City"))

table(df_recode$country_name)


#Agrupaciones y summarize

df_tablas <- df %>% 
  mutate(cat_distancia_centro = case_when(distance_city_center >= 10 ~ "Muy lejos",
                                          distance_city_center > 5 & distance_city_center < 10 ~ "Lejos",
                                          distance_city_center > 2 & distance_city_center <= 5 ~ "Cerca",
                                          distance_city_center > 1 & distance_city_center <= 2 ~ "Muy cerca",
                                          distance_city_center <= 1 ~ "En el centro",
                                          TRUE ~ NA_character_)) %>%
  mutate(country_name = recode(country_name, "Holy See (Vatican City State)" = "Vatican City")) %>% 
  mutate(price_mxn = price_total * tipo_de_cambio) %>% 
  select(country_name, city, distance_city_center, cat_distancia_centro, price_mxn)

df_tablas_grouped <- df_tablas %>% 
  group_by(country_name, cat_distancia_centro)


tabla_paises_distancia_precio <- df_tablas %>% 
  group_by(city) %>% 
  summarise(total = n(),
            distancia_promedio = mean(distance_city_center, na.rm = T),
            precio_promedio = mean(price_mxn, na.rm = T),
            precio_minimo = min(price_mxn, na.rm = T),
            precio_maximo = max(price_mxn, na.rm = T))

tabla_categorias <- df_tablas %>% 
  group_by(cat_distancia_centro) %>% 
  summarise(total = n(),
            precio_promedio = mean(price_mxn, na.rm =T),
            precio_maximo = max(price_mxn, na.rm = T),
            precio_minimo = min(price_mxn, na.rm = T)) %>% 
  arrange(-precio_promedio)


plot(df$distance_city_center,
        df$price_total)







