
pacman::p_load("tidyverse", "ggplot2", "readxl", "janitor", "data.table", "maps", "scales")

options(scipen = 999)



df <- midwest

#Histogramas (1 variable continua)
df %>% 
  ggplot(aes(x = percchildbelowpovert)) +
  geom_histogram()

#Línea de densidad (1 variable continua)
df %>% 
  ggplot(aes(x = percchildbelowpovert)) +
  geom_density()

#Boxplot (1 variable continua)
df %>% 
  ggplot(aes(x = state, y = percchildbelowpovert)) +
  geom_boxplot()

#Barras (Conteo de variables categóricas)
df %>% 
  ggplot(aes(x = state)) +
  geom_bar()

#Barras (Conteo de una variable categórica y una continua)
df %>% 
  ggplot(aes(x = state, y = percamerindan)) +
  geom_col()

#Puntos (scatter) (interacción de dos variables continuas)

df %>% 
  ggplot(aes(x = percchildbelowpovert, y = perchsd))+
  geom_point()


#Texto

df %>% 
  ggplot(aes(x = percchildbelowpovert, y = perchsd))+
  geom_point()+
  geom_text(aes(label = county))


#Filtros
df %>% 
  filter(state == "IL") %>% 
  ggplot(aes(x = percchildbelowpovert, y = perchsd))+
  geom_point()+
  geom_text(aes(label = county))


df %>% 
  filter(state == "IL") %>% 
  sample_n(10) %>% 
  ggplot(aes(x = county, y = percbelowpoverty))+
  geom_col()+
  coord_flip()


#Aes fill en función de variable categórica (diferencias?)

df %>% 
  sample_n(10) %>% 
  ggplot(aes(x = county, y = percbelowpoverty, fill = state))+
  geom_col(aes(fill = state))+
  coord_flip()

df %>% 
  sample_n(10) %>% 
  ggplot(aes(x = county, y = percbelowpoverty))+
  geom_col(fill = "darkred")+
  coord_flip()





#Gráfica puntos----

df %>% 
  ggplot(aes(x = percchildbelowpovert, y = perchsd))+
  geom_point()

df %>% 
  ggplot(aes(x = percchildbelowpovert, y = perchsd, color = state))+
  geom_point()


#Aes size en función de una variable continua

df %>% 
  ggplot(aes(x = percchildbelowpovert, y = perchsd, color = state, size = poptotal))+
  geom_point()



#Agregamos línea de tendencia (geom especial) (ojo con presets)


df %>% 
  ggplot(aes(x = percchildbelowpovert, y = perchsd, color = state, size = poptotal))+
  geom_point()+
  geom_smooth(method = "lm") 



#Task 1 Generar una sola línea y silenciar el aviso de "size"

?geom_smooth



#Task 2: Utilizar scale_color_brewer para definir la paleta de colores

?scale_color_brewer

  

#Task 3: Utilizar scale_x_continous y scale_y_continous para definir límites e incluir formato %

?scale_x_continuous
?label_percent

    


#Task 4: Agregar etiquetas

?labs



#Task 5: Agregar tema
?theme
?element_text






