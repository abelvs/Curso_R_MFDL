pacman::p_load("dplyr", "ggplot2", 
                  "lubridate", "data.table", "janitor", "stringr",
                  "httr", "jsonlite", "stringi", "ggthemes", "scales", "purrr", "tibble", "readxl", "tidyr",
                  "sf", "viridis")

# CARGA DE DATOS ---

# Cargar el catálogo de localidades con codificación Latin-1 y limpiar nombres de columnas

# Convertir a numéricas las columnas de población y vivienda
# Crear clave de localidad a 9 dígitos (con padding), clave municipal (5 dígitos) y clave de entidad (2 dígitos)

# Agregar el catálogo a nivel municipal: sumar población y viviendas por municipio

# Agregar el catálogo a nivel entidad: sumar población y viviendas por entidad


# CARGA DE DATOS BANCO DEL BIENESTAR ----

# Cargar directorio de sucursales del Banco del Bienestar y limpiar nombres de columnas

# Estandarizar nombres de entidades para que coincidan con el catálogo
# Generar una llave de concatenación que permita hacer el join con el catálogo
# Hacer el join con el catálogo para obtener las claves geográficas

# Revisar registros que no encontraron municipio en el join (diagnóstico de calidad)


# ANÁLISIS ----

# Calcular número de sucursales por entidad
# Pegar datos de población y vivienda
# Calcular indicadores: sucursales por persona y sucursales por vivienda

# Mismo ejercicio a nivel municipal
# Usar full_join para conservar municipios sin sucursales


# DESCARGA DE GEOMETRÍAS ----

# Generar vector de claves de entidad del 01 al 32 con padding

# Definir función para descargar geometría municipal de una entidad desde la API de INEGI

# Aplicar la función a todas las entidades y unir en un solo sf

# Disolver geometrías municipales para obtener polígonos estatales


# MAPAS ----

# Mapa 1: pegar indicadores de cobertura al sf estatal y mapear sucursales por vivienda

# Mapa 2: filtrar municipios de Estado de México, pegar indicadores
# y agregar puntos con la ubicación de cada sucursal