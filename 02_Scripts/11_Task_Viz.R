
pacman::p_load("tidyverse", "ggplot2", "readxl", "janitor", "data.table", "maps", "scales")

options(scipen = 999)


df <- fread("01_Inputs/Sesion_3/earthquake1826_2026.csv") %>% 
  clean_names()


glimpse(df)

summary(df)


#EDA (hist de mag, depth)


#(plot de depth+mag)


#¿Plot latitude longitude?









#Bonus----

map("world",
    col = "#2b2b2b",     
    fill = TRUE,
    bg = "#0f0f0f",      
    lwd = 0.5)

points(df$longitude, df$latitude, col = "#c234bd")





#Task: Crear plot de sismos

world_map <- map_data("world")

df_plot <- df %>% 
  mutate(mes = month(time))

df_plot %>% 
  ggplot(aes(x = longitude,
             y = latitude))+
  geom_point()
