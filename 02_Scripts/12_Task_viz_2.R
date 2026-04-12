pacman::p_load("dplyr", "data.table", "janitor",
               "readxl", "tidyr","writexl", "stringr", "scales", "ggplot2")


# ---------------------------------------------------------------------
# BLOQUE: Cargar datos
# ---------------------------------------------------------------------
# Cargar la base de homicidios
# Archivo: "Ejemplo_2_incidencia_estatal_delitos_2015_2025_oct.xlsx"
# Objetivo: Crear un objeto llamado 'df' con la información de homicidios por entidad
# Notas: Asegúrate de tener el paquete 'readxl' cargado antes de ejecutar
df <- read_excel()

# ---------------------------------------------------------------------
# BLOQUE: Población CONAPO
# ---------------------------------------------------------------------
# Cargar la base de homicidios
# Archivo: "01_input/Pob_conapo_2015_2025.xlsx"
# Objetivo: Crear un objeto llamado 'poblacion_conapo' que contenga la población
# total por entidad de 2023 a 2024




# ---------------------------------------------------------------------
# BLOQUE: Homicidios por Estado
# ---------------------------------------------------------------------
# Objetivo: Crear una tabla 'homicidios_por_estado' con los datos de 
# el total de homicidios y la poblacion de cada entidad por año

homicidios_por_estado <- df 
# 1. Filtrar sólo homicidio doloso
# 2. Unir desde la izquierda el df 'poblacion_conapo'


# 3. Revisamos: ¿Se unen todos los estados?
homicidios_por_estado %>% 
  filter(is.na(poblacion_total)) %>% 
  distinct(entidad)

# 4. Agregar una recodificación de valores en la declaración de la tabla si es necesario



# ---------------------------------------------------------------------
# BLOQUE: Homicidios por estado (Tasas)
# ---------------------------------------------------------------------
# Objetivo: Agregar a la tabla anterior tasa por cada 100 mil habitantes
# y las tasas de variación anual nominal y porcentual

homicidios_por_estado_tasas <- homicidios_por_estado
# 1. Usar mutate() para generar (tasa_100)
# 2. Ordenar por entidad y año usando arrange()
# 3. Usar mutate para generar 'var_nominal' y 'var_porcentual'


#Revisamos: ¿Las tasas se generan correctamente en TODOS los años?
View(homicidios_por_estado_tasas)

# 4. Agregar group_by() en caso necesario



# ---------------------------------------------------------------------
# BLOQUE: Comparación 2023 vs. 2024
# ---------------------------------------------------------------------
# Objetivo: Utilizar la tabla de datos completa para obtener un 
# objeto exportable que contenga UNA fila por entidad, el total 2023, 2024,
# la tasa_100 para 2024 y la variación porcentual entre 2023 y 2024


comp_2023_2024 <- homicidios_por_estado_tasas
# 1. FIltrar los años de interés
# 2. Eliminar las columnas obsoletas: poblacion_total, subtipo_de_delito, var_nominal
# 3. Pivotear para obtener la tabla en formato ancho
# 4. Eliminar columnas obsoletas (tasa y variacion 2023)


#Viz de prueba


comp_2023_2024 %>% 
  ggplot(aes(x = reorder(entidad, -var_porcentual_2024), y = var_porcentual_2024))+
  geom_col()+
  theme_minimal()+
  labs(title = "Variación porcentual del homicidio doloso por entidad",
       x = "",
       y = "Variación porcentual",
       subtitle = "2023-2024")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))






#visualización aplicada


tema_propio<- theme_minimal(base_size = 12) +
  theme(
    # Título en bold y color #a57f2c
    plot.title = element_text(face = "bold", color = "grey10", size = 16),
    # Subtítulo en color #a57f2c
    plot.subtitle = element_text(face = "bold", color = "#a57f2c", size = 12),
    # Quitar grid
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Fondo blanco
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    # Quitar líneas de los ejes
    axis.line = element_blank(),
    axis.ticks = element_blank())


#Tarea: Crear una col llamada "signo" con que sea "Negativo" si la variación porcentual es 
#-0 y "Positivo" si es > 0
#2. Crear una col "color" con niveles condicionales a la variación porcentual: 
# si es >0.5 será "Rojo", Si es >0 será "Rojo Claro", Si es negativa, será "Dorado"


#Tarea 2: Agregar Elementos de la Viz de la Conferencia: Texto, Etiquetas, colores variables, línea en 0 






