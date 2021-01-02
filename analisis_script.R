## CECH 2021 - PROCESSING SCRIPT ##

# Carga de paquetes y base de datos -------------------------------------------------------

library("haven")
library("dplyr")
library("readxl")
library("tidyverse")

#cargamos base de datos
lima <- read_excel("Encuesta Limache 2.xlsx", sheet = 2)

# Recodificación de variables y nombres de variables

  ## Edad ##

lima$Edad <- as.numeric(lima$Edad)
class(lima$Edad)
lima <- mutate(lima, Edadrec = car::recode(lima$Edad, "17:21 = 1; 22:35 = 2;
                                                           36:50 = 3; 50:90 =4; else = NA"))
lima <- mutate(lima, Edadrec = recode(lima$Edadrec,"1" = "Centennials","2" = "Millennials",
                                      "3" = "Gen X", "4" = "Boomers"))


wea <-table(lima$Edadrec)

round((prop.table(wea)*100),2)

wea2 <-table(lima$Genero)
round((prop.table(wea2)*100),2)

# Construcción de variables de probabilidad de voto 

ProbVoto <- rowSums(CECh_semi[23:28], na.rm = T)
CECh_semi <-data.frame(CECh_semi, ProbVoto)

#Creación de Ponderador por Generación

CECh_semi <- mutate(CECh_semi, GenPonde = recode(CECh_semi$Edadrec,"Centennials" = "1.72","Millenials" = "0.97",
                                                 "Gen X" = "0.85", "Boomers" = "1.04"))
CECh_semi$GenPonde <- as.numeric(CECh_semi$GenPonde)
class(CECh_semi$GenPonde)

# Construcción de tablas

# ¿Quienes son los votantes probables? División por género, grupo etáreo y [REDACTED]

ctable(CECh_semi$VotoProb, CECh_semi$Sexo,report.nas = FALSE, headings = FALSE, prop = "r")
ctable(CECh_semi$VotoProb, CECh_semi$Edadrec, report.nas = FALSE, headings = FALSE, prop = "c")


# Construcción de gráficos

  ## Edad y sexo de votantes probables  ##

    ## Género de votantes probables ##

GrafSex <-ggplot(data = subset(CECH_filtrada, !is.na(Sexo)),
                 aes(x = factor(Sexo),
                     y = prop.table(stat(count)),
                     weight = Pond,
                     fill = factor(VotoProb),
                     label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  labs(title = "Distribución por género, \n según probabilidad de voto",
       x = "Género", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta de Conducta Electoral 2020") +
  xlab("Género") + ylab("Porcentaje") +
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_manual("Probabilidad de ir a votar",
                    values = c("#FF6666", "#00CC66"),
                    labels = c("Probabilidad media", "Probabilidad alta")) +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Género', fill = 'Probabilidad de Voto') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~VotoProb)
plot(GrafSex)

ggsave(GrafSex, filename = "resultados/GrafSex.png",
       dpi = 400, width = 8, height = 7)

    ## Grupos generacional de los votantes ##

GrafGen <-ggplot(data = subset(CECH_filtrada, !is.na(Edadrec)),
                 mapping = aes(x = factor(Edadrec),
                               y = prop.table(stat(count)),
                               fill = factor(VotoProb),
                               label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  labs(title = "Distribución por género, según probabilidad de voto",
       x = "Género", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta de Conducta Electoral 2020") +
  xlab("Grupo generacional") + ylab("Porcentajes") +
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_manual("Probabilidad de ir a votar",
                    values = c("#FF6666", "#00CC66"),
                    labels = c("Probabilidad media", "Probabilidad alta")) +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  ggtitle("Distribución de grupos generacionales, según probabilidad de voto") + 
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic"))
plot(GrafGen)

ggsave(GrafGen, filename = "resultados/GrafGen.png",
       dpi = 400, width = 12, height = 7)

  ## Nota al alcalde según probabilidad de voto



  ## Distribución de la posición política de Limache
  ## Posición Política
  ## Principal problema