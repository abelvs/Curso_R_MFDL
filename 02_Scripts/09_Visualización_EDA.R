
pacman::p_load("tidyverse", "ggplot2", "readxl", "janitor", "data.table", "maps", "scales")

options(scipen = 999)



df <- midwest

?midwest

glimpse(midwest)
summary(midwest)


#Conteo de variables categóricas----
table(midwest$state)
barplot(table(midwest$state))



#Dispersión de variables continuas----

hist(midwest$poptotal)

hist(midwest$perchsd)

hist(midwest$percadultpoverty, breaks = 50)



#Identificación de outliers----

summary(midwest$percchildbelowpovert)

boxplot(midwest$percchildbelowpovert)
boxplot(midwest$percchildbelowpovert ~ midwest$state)




#Interacción de variables continuas----

plot(midwest$percchildbelowpovert, midwest$perchsd)

abline(lm(midwest$perchsd ~ midwest$percchildbelowpovert, data = midwest),
       col = "red")

# Titulos y colores
plot(midwest$percchildbelowpovert,
     midwest$perchsd,
     col = as.factor(midwest$state),
     pch = 19,
     xlab = "Porcentaje de pobreza infantil",
     ylab = "Porcentaje de población con educación media superior",
     main = "Relación entre pobreza infantil y educación media superior")
abline(lm(midwest$perchsd ~ midwest$percchildbelowpovert, data = midwest),
       col = "red")

#Genera el impacto deseado?

#Versión GGPLOT

ggplot(midwest, aes(x = percchildbelowpovert, y = perchsd)) +
  geom_point(aes(size = poptotal, color = state),
             alpha = 0.65) +
  geom_smooth(method = "lm", se = FALSE,
              color = "#FFFFFF", linewidth = 1) +
  scale_size_continuous(range = c(1.5, 8)) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title = "Pobreza infantil vs educación media superior",
       subtitle = "Condados del Midwest (EE.UU.)",
       x = "Porcentaje de pobreza infantil",
       y = "Porcentaje con educación media superior",
       color = "Estado",
       size = "Población total") +
  theme_minimal(base_size = 13) +
  theme(plot.background = element_rect(fill = "#0B0F14", color = NA),
        panel.background = element_rect(fill = "#0B0F14", color = NA),
        panel.grid.major = element_line(color = "#1F2933"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 18, color = "white"),
        plot.subtitle = element_text(size = 12, color = "gray70"),
        axis.title = element_text(face = "bold", color = "white"),
        axis.text = element_text(color = "gray80"),
        legend.background = element_rect(fill = "#0B0F14", color = NA),
        legend.key = element_rect(fill = "#0B0F14", color = NA),
        legend.title = element_text(face = "bold", color = "white"),
        legend.text = element_text(color = "gray80"),
        legend.position = "bottom")

