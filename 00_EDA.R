# inspiración https://ecofeminita.github.io/EcoFemiData/informe_desigualdad_genero/trim_2019_03/informe.nb.html#


library(tidyverse)
library(googlesheets4)
library(gargle)
library(funModeling)
options(scipen = 999)


kiwi <- sheets_read("1aeuu9dVfN42EjyvbmhEcsf0ilSz2DiXU-0MpnF896ss")

glimpse(kiwi)

# Representación de género en la encuesta
kiwi %>% 
  select(Género) %>% 
  group_by(Género) %>% 
  summarise (n = n()) %>% 
  mutate(freq = n/sum(n)) 

# Respuestas por países
paises <- kiwi %>% 
  select(`País en el que trabajas`) %>% 
  mutate(cuenta = 1) %>% 
  group_by(`País en el que trabajas`) %>% 
  summarise(Cuenta = sum(cuenta)) %>% 
  arrange(-Cuenta)

gt::gt(paises)

# Respuestas por provincia (Sólo para Argentina)
provincias <- kiwi %>% 
  filter(`País en el que trabajas` == "Argentina") %>% 
  mutate(cuenta = 1) %>% 
  group_by(`Provincia donde trabajas`) %>% 
  summarise(Cuenta = sum(cuenta)) %>% 
  arrange(-Cuenta)


gt::gt(provincias)


liderazgo <- kiwi %>% 
  select(Género, `¿En qué puesto trabajás?`,`País en el que trabajas` ,`¿Cuál es tu remuneración BRUTA MENSUAL en tu moneda local? (antes de impuestos y deducciones)`)

names(liderazgo) <- c("genero", "puesto","pais", "sueldo")


head(liderazgo)

kiwi %>% 
  select(`Rubro de la empresa`) %>% 
  group_by(`Rubro de la empresa`) %>% 
  count(sort = TRUE) %>% 
  print(n = 28)

# Exploración para Argentina
liderazgo <- liderazgo %>% 
  filter(!is.na(puesto)) %>% 
  mutate(sueldo = as.numeric(unlist(sueldo)),
         cuenta = 1) %>% 
  filter(pais == "Argentina")

liderazgo %>% group_by(puesto, genero) %>% 
  summarise(sueldo_prom = mean(sueldo), 
            cuenta = sum(cuenta)) %>% 
  print(n = nrow(.))

liderazgo %>% 
  group_by(puesto, genero) %>% 
  summarise(salarios = list(mean_se(sueldo))) %>% 
  unnest(salarios) %>% 
  ggplot(aes(x = puesto, y = y, fill = genero))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = ymin,ymax = ymax), position = "dodge")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "bottom")

library(ggthemes)
library(scales)

liderazgo %>% 
  filter(puesto %in% c("Gerente", "HRBP", "Analista")) %>% 
  mutate(puesto = factor(puesto, levels = c("Gerente", "HRBP", "Analista"))) %>% 
  group_by(puesto, genero) %>% 
  summarise(suel_prom = mean(sueldo)) %>% 
  ggplot(aes(puesto, suel_prom, fill = genero))+
  geom_col(position = "dodge")+
  labs(title = "Sueldo promedio por puesto y género en Argentina",
       subtitle = "En pesos argentinos",
       x= "", y = "", fill = "Género",
       caption = "Fuente: Encuesta KIWI de Sueldos de RRHH Latam \n Club de R para RRHH")+
  theme_minimal() +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))

liderazgo %>% 
  filter(puesto %in% c("Gerente", "HRBP", "Analista")) %>% 
  mutate(puesto = factor(puesto, levels = c("Gerente", "HRBP", "Analista"))) %>% 
  group_by(puesto, genero) %>% 
  summarise(suel_prom = mean(sueldo)) %>% 
  ggplot(aes(puesto, suel_prom, fill = genero))+
  geom_col(position = "dodge")+
  labs(title = "Sueldo promedio por puesto y género en Argentina",
       subtitle = "En pesos argentinos",
       x= "", y = "", fill = "Género",
       caption = "Fuente: Encuesta KIWI de Sueldos de RRHH Latam \n Club de R para RRHH")+
  theme_minimal() +
  scale_fill_colorblind()+
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))


#### Análisis de preguntas bizarras####
preg <- kiwi[,c(2,41)] 

names(preg) <- c("genero", "pregunta")

preg %>% 
  filter(str_detect(pregunta, "hijo[s]")|str_detect(pregunta, "casad[ao]|casar")) %>% 
  print(n=nrow(.))

preg %>% 
  filter(str_detect(pregunta, "hijo[s]")|str_detect(pregunta, "casad[ao]|casar")) %>% 
  group_by(genero) %>% 
  summarise (n = n()) %>% 
  mutate(freq = n/sum(n)) 

#### Wordcloud ####

library(wordcloud2)
library(webshot)
library(tidytext)

vacias <- get_stopwords("es")

vacias <- vacias %>%
  rename(palabra = word)

vacias <- read_csv("https://tinyurl.com/7PartidasVacias",
                   locale = default_locale())

vacias_custom <- tibble(palabra = c("coeficiente", "puesto", "puestos", "operativos"))


coment <- kiwi[,61]
coment <- coment %>% 
  filter(!is.na(Comentarios)) %>% 
  mutate(Comentarios = as.character(Comentarios)) %>% 
  unnest_tokens(palabra, Comentarios)


coment_bi <- coment %>% 
  anti_join(vacias) %>% 
  anti_join(vacias_custom)


coment_bi %>%
  count(palabra, sort = TRUE) %>%
  ungroup() %>%
  wordcloud2(size = 0.6, shape = "diamond", color = "random-light", backgroundColor = "#1C2833")

