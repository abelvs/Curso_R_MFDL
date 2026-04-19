pacman::p_load("dplyr", "data.table", "janitor",
               "readxl", "tidyr","writexl", "stringr", "scales", "ggplot2", "srvyr")


# CARGA DE DATOS ----
ruta <- "01_Inputs/Sesion 5/"

cuestionario <- fread() %>% clean_names()



# RECODIFICACIÓN DE VARIABLES ----

# Recodifica p4_1 (confianza en personas) en 4 categorías usando case_when
# 0-4 → Baja/Nula | 5 → Neutral | 6-10 → Alta/Moderadamente alta | NA → NS/NR

# Aplica la misma recodificación a todas las columnas p4_2_01 a p4_2_16 usando across()


# ANÁLISIS SIN PONDERAR (exploración) ----

# Genera una tabla de frecuencias de confianza en personas con tabyl()

# Pivota las columnas p4_2_* a formato largo (pivot_longer) y calcula la proporción por institución

# Une el catálogo de instituciones para obtener los nombres legibles (left_join)

# ¿Por qué los porcentajes no coinciden con los del INEGI? Escribe tu respuesta aquí:
# R:


# DISEÑO MUESTRAL CON srvyr ----

# Activa la opción survey.lonely.psu = "adjust" antes de declarar el diseño

# Configura el diseño muestral con as_survey_design()
# indicando UPM, estrato, factor de expansión y nest = TRUE

# Calcula la proporción ponderada de confianza en personas con intervalo de confianza


# TABLAS PONDERADAS POR INSTITUCIÓN ----

# Calcula la proporción ponderada para Empleados Gobierno Federal (p4_2_10)
# y agrega la columna "institucion" con el nombre correspondiente

# Repite para Empleados Gobierno Municipal (p4_2_12)

# Repite para Empleados Gobierno Estatal (p4_2_11)

# Repite para Gobierno Federal (p4_2_01)

# Repite para Gobierno Estatal (p4_2_05)

# Repite para Gobierno Municipal (p4_2_03)

# Une todos los tibbles con bind_rows() en un solo objeto llamado completa_ponderada


# VISUALIZACIÓN — GRÁFICA DE BARRAS AL 100% ----

# Define el vector de colores para las 4 categorías
# teal → Alta | azul oscuro → Neutral | magenta → Baja/Nula | naranja → NS/NR

# Ordena las categorías de valor como factor con el orden correcto para la leyenda

# Ordena las instituciones como factor de arriba hacia abajo (Gobierno Municipal al fondo)

# Construye la gráfica con geom_col(position = position_fill()) y width = 0.65

# Agrega etiquetas de porcentaje dentro de cada segmento con geom_text
# Oculta etiquetas con prop < 0.03

# Aplica tema minimal, oculta eje X, centra el título y coloca la leyenda abajo