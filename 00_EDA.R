# inspiración https://ecofeminita.github.io/EcoFemiData/informe_desigualdad_genero/trim_2019_03/informe.nb.html#
# inspiración: https://blog.datawrapper.de/gendercolor/
# Verde Club: #009204
# Azul Club: #344D7E


# Paquetes y opciones -------------------------------------

library(tidyverse)
library(googlesheets4)
library(gargle)
library(gt)
library(extrafont)
library(ggthemes)
library(scales)

options(scipen = 999)

loadfonts(quiet = TRUE)

# Estilo de los gráficos
estilo <- theme(panel.grid = element_blank(),
                plot.background = element_rect(fill = "#fbfcfc"),
                panel.background = element_blank(),
                text = element_text(family = "Roboto"))

genero <- c("#8624F5", "#1FC3AA", "#FFD129", "#75838F") #Violeta - Verde - Amarillo - Gris

# Carga de datos ------------------------------------------


kiwi <- sheets_read("1aeuu9dVfN42EjyvbmhEcsf0ilSz2DiXU-0MpnF896ss")


# Análisis exploratorio ----------------------------------

glimpse(kiwi)



# Respuestas por países
paises <- kiwi %>% 
  select(`País en el que trabajas`) %>% 
  mutate(cuenta = 1) %>% 
  group_by(`País en el que trabajas`) %>% 
  summarise(Cuenta = sum(cuenta)) %>% 
  filter(Cuenta > 4) %>% 
  arrange(-Cuenta)

gt(paises) %>% 
  tab_header(title = "Cantidad de respuestas por país",
             subtitle = "Países con más de 5 respuestas") 

# Respuestas por provincia (Sólo para Argentina)
provincias <- kiwi %>% 
  filter(`País en el que trabajas` == "Argentina") %>% 
  mutate(cuenta = 1) %>% 
  group_by(`Provincia donde trabajas`) %>% 
  summarise(Cuenta = sum(cuenta)) %>% 
  arrange(-Cuenta)


gt(provincias)


liderazgo <- kiwi %>% 
  select(Género, `¿En qué puesto trabajás?`,`País en el que trabajas` ,`¿Cuál es tu remuneración BRUTA MENSUAL en tu moneda local? (antes de impuestos y deducciones)`)

names(liderazgo) <- c("genero", "puesto","pais", "sueldo")


head(liderazgo)

kiwi %>% 
  select(`Rubro de la empresa`) %>% 
  group_by(`Rubro de la empresa`) %>% 
  count(sort = TRUE) %>% 
  print(n = nrow(.))

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

#### Comparación Dolar ####

# Sección para comparar los sueldos en dólares en los países con más de 5 respuestas.

# Fuente: Banco Central de cada país al 19/10/2020
# Dólar oficial - Precio de Venta

pais <- c("Argentina", "Bolivia", "Chile", "México", "Paraguay", "Perú", "Uruguay")
tipo_cambio <- c(82.5, 6.96, 795.68, 21.38, 7027.43, 3.597, 48.892)

tc <- tibble (pais, tipo_cambio) # Creo una tabla con los tipos de cambio de los países con más de 5 respuestas

# Preparación de datos

sueldos_dolar <- kiwi %>% 
  filter(Trabajo !="Freelance") %>% 
  select(Género, `¿En qué puesto trabajás?`,`País en el que trabajas` ,
         `¿Cuál es tu remuneración BRUTA MENSUAL en tu moneda local? (antes de impuestos y deducciones)`,
         `Tipo de contratación`)

names(sueldos_dolar) <- c("genero", "puesto","pais", "sueldo", "contrato")

# Eliminar los puestos que no sirven al análisis
sueldos_dolar <- sueldos_dolar %>% 
  mutate(puesto = str_trim(puesto, side = "both")) %>% 
  filter(puesto != "Juzgado Civil y Comercial", puesto != "Pasante", 
         puesto != "Programador", puesto != "Jefe de Proyecto", 
         contrato != "Pasante")

# Controlar la cantidad de casos de contratos part time
sueldos_dolar %>% 
  group_by(contrato) %>% 
  count(pais) %>% 
  filter(contrato == "Part time")

# Agrego un multiplicador de sueldos para convertir los sueldos part time en full time
sueldos_dolar <- sueldos_dolar %>% 
  left_join(tc, by="pais") %>% 
  mutate(multiplicador = if_else(contrato == "Part time", 1.5, 1),
         sueldo = as.numeric(unlist(sueldo)),
         sueldo_ft = sueldo * multiplicador,    # Hace la equivalencia de un sueldo part time a full time
         sueldo_dolar = sueldo_ft/tipo_cambio,  # Convierto los sueldos a dólares
         cuenta = 1)

summary(sueldos_dolar)

funModeling::profiling_num(sueldos_dolar)

# Dado que los percentiles 5 y 95 están en U$412 y 2795 respectivamente, 
# podamos todo lo que esté fuera de ese rango

mediana_pais <- sueldos_dolar %>% 
  filter(pais %in% c("Argentina", "Bolivia", "Chile", "Paraguay", "Uruguay"),
         between(sueldo_dolar,400,3000)) %>% 
  group_by(pais) %>% 
  summarise(sueldop = list(mean_se(sueldo_dolar)),
            cant = sum(cuenta)) %>% 
  unnest(cols = c(sueldop)) %>%
  print(n = nrow(.)) %>% 
  filter(cant>4)

sueldo_dolar_pais <- sueldos_dolar %>% 
  filter(pais %in% c("Argentina", "Bolivia", "Chile", "Paraguay", "Uruguay"), 
         between(sueldo_dolar, 400,3000))
  
# Gráfico
# Opción A
ggplot(mediana_pais, aes(pais, y =  y))+
  geom_col(fill = "#344D7E") +
  geom_errorbar(aes(ymin = ymin,ymax = ymax), position = "dodge", color = "#75838F")+
  geom_point(data = sueldo_dolar_pais, aes(x = pais, y = sueldo_dolar), alpha = 0.3, size = 3)+
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))+
  labs(title = "Mediana salarial por país",
       subtitle = "Sueldos de RRHH en U$S",
       caption = "Fuente: Encuesta KIWI de Sueldos de RRHH para Latam \n Países con más de 5 respuestas",
       x = "", y = "")

# Opción B
ggplot(mediana_pais, aes(x = reorder(pais, -y), y =  y))+
  geom_col(fill = "#344D7E", alpha = 0.85) +
  geom_errorbar(aes(ymin = ymin,ymax = ymax), position = "dodge", color = "#75838F")+
  geom_point(data = sueldo_dolar_pais, aes(x = pais, y = sueldo_dolar), 
             alpha = 0.3, size = 2, color = "#75838F")+
  geom_text(aes(label = round(x=y, 0), vjust = -0.5, fontface = "bold"), color = "black")+
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))+
  labs(title = "Mediana salarial por país",
       subtitle = "Sueldos de RRHH en U$S",
       caption = "Fuente: Encuesta KIWI de Sueldos de RRHH para Latam \n Países con más de 5 respuestas",
       x = "", y = "") + 
  estilo


# Representación en puestos de liderazgo ------------------


# Representación de género en la encuesta
kiwi %>% 
  select(Género) %>% 
  group_by(Género) %>% 
  summarise (n = n()) %>% 
  mutate(freq = n/sum(n)) 

diversidad <- kiwi %>% 
  filter(Trabajo !="Freelance") %>% 
  mutate(genero = fct_collapse(Género, 
                               "No binario"= c("Género diverso (género diverso / género fluido /otras minorías)", "No binario")))

div <- diversidad %>% 
select(genero) %>% 
  mutate(genero = factor(genero, 
                         levels = c("Femenino", "Masculino", 
                                    "No binario", "Prefiero no responder"))) %>% 
  group_by(genero) %>% 
  summarise (n = n()) %>% 
  mutate(freq = n/sum(n)) %>% 
  arrange(-n)

# Compute the cumulative percentages (top of each rectangle)
div$ymax <- cumsum(div$freq)

# Compute the bottom of each rectangle
div$ymin <- c(0, head(div$ymax, n=-1))

# Compute label position
div$labelPosition <- (div$ymax + div$ymin) / 2

# Compute a good label
div$label <- paste0(div$genero, "\n Cant: ", div$n)

# Make the plot
ggplot(div, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=genero)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +# Try to remove that to see how to make a pie chart
  scale_fill_manual(values = c("#8624F5",  "#1FC3AA", "#FFD129","#75838F")) +
  theme_void() +
  theme(legend.position = "right",
        panel.background = element_blank(),
        text = element_text(family = "Roboto")) +
  labs(title = "Cantidad de respuestas según género",
       fill = "Género", 
       caption = "Fuente: Encuesta KIWI de Sueldos de RRHH para Latam")

# Tabla de distribución de género
gt(div %>% select(genero, n, freq)) %>% 
  tab_header(title = "Cantidad de respuestas según género") %>% 
  tab_source_note(source_note = "Fuente: Encuesta KIWI de Sueldos de RRHH para Latam") %>% 
  fmt_percent(columns = "freq", decimals = 1) %>% 
  cols_label(genero = "Género",
             n = "Cantidad",
             freq = "Porcentaje") %>% 
  cols_align(align = "center", 
             columns = vars(n, freq))


# Representación en puestos de liderazgo


div

kiwi %>% 
  filter(Trabajo != "Freelance") %>% 
  select(Género) %>% 
  group_by(Género) %>% 
  summarise (n = n()) %>% 
  mutate(freq = n/sum(n)) 

lideres <- diversidad %>% 
 select(genero = `Género`,
         puesto = `¿En qué puesto trabajás?`,
         equipo = `¿Cuántas personas tenés a cargo? (poné 0 si no tenés gente a cargo?`) %>% 
  filter(puesto != "Juzgado Civil y Comercial", puesto != "Pasante", 
         puesto != "Programador", puesto != "Jefe de Proyecto") %>% 
  mutate(genero = fct_recode(genero,
                             "No binario" = "Género diverso (género diverso / género fluido /otras minorías)"))


# Propoción de líderes hombres y mujeres
lideres_genero <- lideres %>% 
  filter(genero %in% c("Masculino","Femenino")) %>% 
  group_by(genero) %>%
  mutate(gente_a_cargo = if_else(puesto %in% c("Responsable", "Jefe", "Gerente", 
                                               "Supervisor", "Director"),1,0)) %>%
  summarise(lider = sum(gente_a_cargo)) %>% 
  left_join(div) %>% 
  select(genero, lider, n) %>% 
  mutate(proporcion = percent(lider/n))


# Gráfico
lideres_genero %>% 
  mutate(porc_lider = lider/n, 
         porc_no_lider = 1 - porc_lider) %>% 
  pivot_longer(cols = c(porc_lider, porc_no_lider),
               names_to = "es_lider", 
               values_to = "valores") %>% 
  mutate(es_lider = factor(es_lider, 
                           levels = c("porc_lider", "porc_no_lider"), 
                           labels = c("Líder", "No Líder"))) %>% 
  ggplot(aes(x= genero, y = valores, fill = es_lider))+
  geom_col(position = "fill")+
  estilo +
  scale_fill_manual(values = c("#344D7E", "#75838F")) +
  labs(title = "Proporción de Líderes según género",
       x = "", y = "", fill = "", 
       caption = "Fuente: Encuesta KIWI de Sueldos de RRHH para Latam")

  
