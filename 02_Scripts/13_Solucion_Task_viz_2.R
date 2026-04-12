pacman::p_load("dplyr", "data.table", "janitor",
               "readxl", "tidyr","writexl", "stringr", "scales", "ggplot2")


#Cargamos base de homicidios octubre

df <- read_excel("01_Inputs/Sesion 4/Ejemplo_2_incidencia_estatal_delitos_2015_2025_oct.xlsx") %>% 
  clean_names()

poblacion_conapo <- read_xlsx("01_Inputs/Sesion 4/Pob_conapo_2015_2025.xlsx")



homicidios_por_estado <- df %>% 
  mutate(entidad = recode(entidad, 
                          "Coahuila de Zaragoza" = "Coahuila",
                          "Michoacán de Ocampo" = "Michoacán",
                          "Veracruz de Ignacio de la Llave" = "Veracruz")) %>% 
  filter(subtipo_de_delito == "Homicidio doloso") %>% 
  left_join(poblacion_conapo, by = c("entidad", "ano"))

#Revisamos
homicidios_por_estado %>% 
  filter(is.na(poblacion_total)) %>% 
  distinct(entidad)


#Creamos la tabla

homicidios_por_estado_tasas <- homicidios_por_estado %>% 
  mutate(tasa_100 = (total/poblacion_total)*100000) %>% 
  arrange(entidad, ano) %>% 
  group_by(entidad) %>% 
  mutate(var_nominal = total - lag(total),
         var_porcentual = (var_nominal)/lag(total)) %>% 
  ungroup()

comp_2023_2024 <- homicidios_por_estado_tasas %>% 
  filter(ano %in% 2023:2024) %>% 
  select(-c(poblacion_total, subtipo_de_delito, var_nominal)) %>% 
  pivot_wider(names_from = ano,
              values_from = c(total, tasa_100, var_porcentual)) %>% 
  select(-c(tasa_100_2023, var_porcentual_2023))




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


comp_2023_2024 %>% 
  mutate(signo = ifelse(var_porcentual_2024 < 0, "Negativo", "Positivo"),
         color = case_when(var_porcentual_2024 > .5 ~ "Rojo",
                           var_porcentual_2024 > 0 ~ "Rojo Claro",
                           var_porcentual_2024 < 0 ~ "Dorado")) %>%
  ggplot(aes(x = reorder(entidad, -var_porcentual_2024), y = var_porcentual_2024))+
  geom_col(aes(fill = color),
           show.legend = F)+
  geom_text(data = . %>%  filter(var_porcentual_2024 < 0),
            aes(label = percent(round(var_porcentual_2024, 3))),
            vjust = 0.25,
            hjust = 1,
            size = 3.5,
            fontface = "bold",
            angle = 90,
            color = "#8d6e4a")+
  geom_text(data = . %>%  filter(var_porcentual_2024 >0),
            aes(label = percent(round(var_porcentual_2024, 3))),
            vjust = 0.25,
            hjust = -0.1,
            size = 3.5,
            fontface = "bold",
            angle = 90,
            color = "#8d6e4a")+
  scale_fill_manual(values = c("Rojo" = "#691a30",
                               "Rojo Claro" = "#a12243",
                               "Dorado" = "#bc955c"))+
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(-.75,2.5))+
  tema_propio+
  geom_hline(yintercept = 0, color = "grey50")+
  labs(x = "",
       y = "Variación",
       title = "VARIACION DEL HOMICIDIO DOLOSO POR ENTIDAD FEDERATIVA",
       subtitle = "2023 a 2024",
       caption = "Elaboración propia con datos del SESNSP")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))


ggsave(filename="03_outputs/Graficas/Barras_Entidad_2023_2024_formato.png",
       width=12, 
       height=7, 
       units="in", 
       dpi=300, 
       bg="transparent")





