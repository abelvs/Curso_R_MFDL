pacman::p_load("dplyr", "data.table", "janitor",
               "readxl", "tidyr","writexl", "stringr", "scales", "ggplot2", "srvyr")


# CARGA DE DATOS ----
ruta <- "01_Inputs/Sesion 5/"

demograficos_persona <- fread(paste0(ruta, "conjunto_de_datos_tviv_encoap2025/conjunto_de_datos/conjunto_de_datos_tviv_encoap2025.csv")) %>% clean_names()
cuestionario <- fread(paste0(ruta, "conjunto_de_datos_tb_sec_ivaix_encoap2025/conjunto_de_datos/conjunto_de_datos_tb_sec_ivaix_encoap2025.csv")) %>% clean_names()



# RECODIFICACIÓN ----
cuestionario_clean <- cuestionario %>%
  mutate(conf_personas = case_when(p4_1 %in% 0:4  ~ "Baja/Nula",
                                   p4_1 == 5      ~ "Neutral",
                                   p4_1 %in% 6:10 ~ "Alta/Moderadamente alta",
                                   TRUE           ~ "No sabe / No responde")) %>% 
  mutate(across(c(p4_2_01:p4_2_16),
                ~ case_when(. %in% 0:4  ~ "Baja/Nula",
                            . == 5      ~ "Neutral",
                            . %in% 6:10 ~ "Alta/Moderadamente alta",
                            TRUE        ~ "No sabe / No responde")))



confianza_persona <- cuestionario_clean %>%
  tabyl(conf_personas)

confianza_instituciones <- cuestionario_clean %>% 
  select(p4_2_01:p4_2_16) %>% 
  pivot_longer(cols = everything(),
               names_to = "institucion",
               values_to = "valor") %>% 
  count(institucion, valor) %>% 
  group_by(institucion) %>% 
  mutate(prop = n / sum(n))

catalogo_instituciones <- read_xlsx("01_Inputs/Sesion 5/Catalogo_Instituciones.xlsx")

confianza_instituciones_clean <- confianza_instituciones %>% 
  mutate(institucion = str_to_upper(institucion)) %>% 
  left_join(catalogo_instituciones, by = c("institucion" = "clave"))


#Ojo: Los porcentajes no coinciden porque no estamos ponderando según el diseño muestral

# DISEÑO MUESTRAL ----
options(survey.lonely.psu = "adjust")

encuesta_ponderada <- cuestionario_clean %>%
  as_survey_design(ids = upm_dis, strata = est_dis, weights = fac_sel, nest = TRUE)

# ANÁLISIS ----
confianza_persona_ponderada <- encuesta_ponderada %>%
  group_by(conf_personas) %>%
  summarise(prop = survey_prop(vartype = "ci")) 


#confianza instituciones

#Tabla

empleados_gob_fed <- encuesta_ponderada %>% 
  group_by(p4_2_10) %>% 
  summarise(prop = survey_prop(vartype = "ci")) %>% 
  rename(valor = p4_2_10) %>% 
  mutate(institucion = "Empleados Gobierno Federal") %>% 
  select(institucion, valor, prop)

empleados_mun <- encuesta_ponderada %>% 
  group_by(p4_2_12) %>% 
  summarise(prop = survey_prop(vartype = "ci"))%>% 
  rename(valor = p4_2_12) %>% 
  mutate(institucion = "Empleados Gobierno Municipal") %>% 
  select(institucion, valor, prop)

empleados_est <- encuesta_ponderada %>% 
  group_by(p4_2_11) %>% 
  summarise(prop = survey_prop(vartype = "ci"))%>% 
  rename(valor = p4_2_11) %>% 
  mutate(institucion = "Empleados Gobierno Estatal") %>% 
  select(institucion, valor, prop)

gob_fed <- encuesta_ponderada %>% 
  group_by(p4_2_01) %>% 
  summarise(prop = survey_prop(vartype = "ci")) %>% 
  rename(valor = p4_2_01) %>% 
  mutate(institucion = "Gobierno Federal") %>% 
  select(institucion, valor, prop)

gob_est <- encuesta_ponderada %>% 
  group_by(p4_2_05) %>% 
  summarise(prop = survey_prop(vartype = "ci"))%>% 
  rename(valor = p4_2_05) %>% 
  mutate(institucion = "Gobierno Estatal") %>% 
  select(institucion, valor, prop)

gob_mun <- encuesta_ponderada %>% 
  group_by(p4_2_03) %>% 
  summarise(prop = survey_prop(vartype = "ci"))%>% 
  rename(valor = p4_2_03) %>% 
  mutate(institucion = "Gobierno Municipal") %>% 
  select(institucion, valor, prop)


completa_ponderada <- bind_rows(empleados_gob_fed,
                                empleados_mun,
                                empleados_est,
                                gob_fed,
                                gob_est,
                                gob_mun)





#Gráfica

# Paleta de colores igual a la gráfica original
colores <- c(
  "Alta/Moderadamente alta" = "#2EBFA5",   # teal
  "Neutral" = "#1B2A6B",   # azul oscuro
  "Baja/Nula"= "#C7407B",   # rosa/magenta
  "No sabe / No responde" = "#E8973A"    # naranja
)

# Orden de las categorías en la leyenda y en las barras
orden_valores <- c(
  "Alta/Moderadamente alta",
  "Neutral",
  "Baja/Nula",
  "No sabe / No responde"
)

# Orden de instituciones (de arriba hacia abajo en la gráfica)
orden_instituciones <- c(
  "Gobierno Municipal",
  "Gobierno Estatal",
  "Gobierno Federal",
  "Empleados Gobierno Estatal",
  "Empleados Gobierno Municipal",
  "Empleados Gobierno Federal"
)

df <- completa_ponderada %>%
  mutate(valor = factor(valor, levels = orden_valores),
         institucion = factor(institucion, levels = orden_instituciones))

ggplot(df, aes(x = prop, y = institucion, fill = valor)) +
  geom_col(position = position_fill(reverse = T),width= 0.65) +
  geom_text(aes(label = ifelse(prop >= 0.03, scales::percent(prop, accuracy = 0.1), "")),
            position = position_fill(vjust = 0.5, reverse = T),
            color = "white",
            size     = 3.2,
            fontface = "bold") +
  scale_x_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0, 0.01))) +
  scale_fill_manual(values = colores,
                    breaks = orden_valores) +
  labs(title    = "Gráfica 2",
       subtitle = "Población de 18 años y más, según nivel de confianza en la administración pública\n2025\n(porcentaje)",
       x = NULL,
       y = NULL,
       fill= NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        plot.subtitle = element_text(hjust = 0.5, size = 10, lineheight = 1.3),
        legend.position = "bottom",
        legend.key.size = unit(0.45, "cm"),
        legend.text = element_text(size = 9),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_blank())

