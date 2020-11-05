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
library(ggalt) 
options(scipen = 999)

loadfonts(quiet = TRUE)

# Estilo de los gráficos
estilo <- theme(panel.grid = element_blank(),
                plot.background = element_rect(fill = "#fbfcfc"),
                panel.background = element_blank(),
                text = element_text(family = "Roboto"))

genero <- c("#8624F5", "#1FC3AA", "#FFD129", "#75838F") #Violeta - Verde - Amarillo - Gris

colores <-  c("#8624F5", "#1FC3AA")

fuente <- "Fuente: Encuesta KIWI de Sueldos de RRHH para Latam"


# Carga de datos ------------------------------------------


kiwi <- sheets_read("1aeuu9dVfN42EjyvbmhEcsf0ilSz2DiXU-0MpnF896ss")


# Análisis exploratorio ----------------------------------

glimpse(kiwi)

view(kiwi)




# Respuestas por países
paises <- kiwi %>% 
  select(`País en el que trabajas`) %>% 
  mutate(cuenta = 1) %>% 
  group_by(`País en el que trabajas`) %>% 
  summarise(Cuenta = sum(cuenta)) %>% 
  filter(Cuenta > 4) %>% 
  arrange(-Cuenta)


gt(paises) %>% 
  tab_header(title = "Cantidad de respuestas por País",
             subtitle = "Países con más de 5 respuestas") #agregue esto

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

view(liderazgo)


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

listado_preg <- preg %>% 
  filter(str_detect(pregunta, "hijo[s]")|
           str_detect(pregunta, "casad[ao]|casar")| 
           str_detect(pregunta,"novio")|
           str_detect(pregunta,"chongo")) %>% 
  print(n=nrow(.))

preg %>% 
  filter(str_detect(pregunta, "hijo[s]")|
           str_detect(pregunta, "casad[ao]|casar")| 
           str_detect(pregunta,"novio")|
           str_detect(pregunta,"chongo")) %>% 
  group_by(genero) %>% 
  summarise (n = n()) %>% 
  mutate(freq = percent(n/sum(n)))

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

numericos <- funModeling::profiling_num(sueldos_dolar)
poda_p05 <- numericos[5,6]
poda_p95 <- numericos[5,10]

# Dado que los percentiles 5 y 95 están en U$412 y 2795 respectivamente, 
# podamos todo lo que esté fuera de ese rango

mediana_pais <- sueldos_dolar %>% 
  filter(pais %in% c("Argentina", "Bolivia", "Chile", "Paraguay", "Uruguay", "Perú"),
         between(sueldo_dolar,poda_p05,poda_p95)) %>% 
  group_by(pais) %>% 
  summarise(sueldop = list(mean_se(sueldo_dolar)),
            cant = sum(cuenta)) %>% 
  unnest(cols = c(sueldop)) %>%
  filter(cant>4) %>% 
  print(n = nrow(.)) 
 
sueldo_dolar_pais <- sueldos_dolar %>% 
  filter(pais %in% c("Argentina", "Bolivia", "Chile", "Paraguay", "Uruguay"), 
         between(sueldo_dolar, 400,3000))
  
# Gráfico
ggplot(mediana_pais, aes(x = reorder(pais, -y), y =  y))+
  geom_col(fill = "#344D7E", alpha = 0.85) +
  geom_errorbar(aes(ymin = ymin,ymax = ymax), position = "dodge", color = "#75838F")+
  geom_point(data = sueldo_dolar_pais, aes(x = pais, y = sueldo_dolar), 
             alpha = 0.3, size = 2, color = "#75838F")+
  geom_text(aes(label = round(x=y, 0), vjust = 1.5, fontface = "bold"),size = 4, color = "white")+
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
  mutate(genero = fct_recode(Género, 
                               "No binario"= c("Género diverso (género diverso / género fluido /otras minorías)")))

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
       caption = fuente)

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



# Gráfico de proporción de liderzgo según género.
lideres_genero %>% 
  mutate(porc_lider = lider/n, 
         porc_no_lider = 1 - porc_lider) %>% 
  pivot_longer(cols = c(porc_lider, porc_no_lider),
               names_to = "es_lider", 
               values_to = "valores") %>% 
  mutate(es_lider = factor(es_lider, 
                           levels = c("porc_no_lider", "porc_lider"), 
                           labels = c("No Líder","Líder"))) %>% 
  ggplot(aes(x= genero, y = valores, fill = es_lider))+
  geom_col(position = "fill")+
  estilo +
  scale_fill_manual(values = c("#75838F", "#344D7E")) +
  labs(title = "Proporción de Líderes según género",
       x = "", y = "", fill = "", 
       caption = fuente)

# Test de hipótesis para validar diferencias de resultados
# Hay que verificar si la proporción de líderes hombres es mayor que la proporción de líderes mujeres

# Creo un dataframe para analizar proporciones de hombres y de mujeres en puestos de liderazgo y de no-liderazgo
test_lider <- lideres_genero %>% 
  mutate(no_lider = n - lider) %>%        # Columna de no líderes
  select(genero, lider, no_lider) %>%     # selecciono columnas de interés
  pivot_longer(cols = c(lider, no_lider), # Hago un dataset largo para analizar después
               names_to = "es_lider", values_to = "conteo")

# Del total de respuestas me interesa sólo ver cuáles son los hombres con puesto de liderazgo
test_lider$cat <- c(0,0,1,0)

# Extraigo el mu para decidir si la diferencia es significativa y pasarlo a la fórmula del test.
prop_mujer_lid <- pull(lideres_genero[1,2]/lideres_genero[1,3])

# Realizo el test de hipótesis.
# H0 = Las proporciones de líderes hombres y mujeres son iguales
# H1 = La proporción de hombres líderes es mayor que la proporción de mujeres líderes.
resultados_test <- broom::tidy(t.test(test_lider$cat, mu = prop_mujer_lid, alternative = "greater"))


valor_test <- if(resultados_test[1,3] > 0.05) {
  print("la diferencia es estadísticamente significativa")
} else {
  print("la diferencia no es estadísticamente significativa")
}


# Nombres de RRHH --------------------------------------------------------

nombres <- kiwi[ , 32]

nombres <- nombres %>% 
  rename(nombre = `¿Cómo se llama el área en tu empresa?`) %>% 
  filter(!is.na(nombre))

nombres %>% 
  group_by(nombre) %>% 
  count(sort = T) %>% 
  print(n = nrow(.))

distintos <- distinct(.data = nombres) %>% count() %>% pull()

nombres <- nombres %>% 
  mutate(nombre = str_to_title(nombre, locale = "es")) %>% 
  group_by(nombre) %>% 
  count(sort = T, name = "rtas") %>% 
  head(10) %>% 
  ungroup()

nombres

ggplot(nombres, aes(x = rtas, y = reorder(nombre,rtas))) +
  geom_col(fill = "#344D7E") +
  estilo +
  labs(title = "Top 10 de nombres para el área de RRHH", 
       x = "", y = "", caption = fuente)

gt(nombres) %>% 
  cols_label(nombre = "Nombre del Área",
             rtas = "Respuestas") %>% 
  tab_style(style = list(
    cell_fill = "#F8FF00"),
    locations = cells_body(
      columns = vars(nombre),
      rows = nombre == "Oficina De Personal"
    ))

# Brecha salarial prueba -------------------------------------------------
library(eph)
library(ggthemes)


base_individual <- get_microdata(year = 2019, trimester = 3, type = "individual")

base_individual <- base_individual %>% 
  mutate(Sexo = as.character(CH04),
         Sexo = case_when(Sexo=="1" ~ "Varones",
                          Sexo=="2" ~ "Mujeres"),
         PP04D_COD = as.character(PP04D_COD),
         PP04D_COD = case_when(nchar(PP04D_COD) == 5 ~ PP04D_COD,
                               nchar(PP04D_COD) == 4 ~ paste0("0", PP04D_COD),
                               nchar(PP04D_COD) == 3 ~ paste0("00", PP04D_COD),
                               nchar(PP04D_COD) == 2 ~ paste0("000", PP04D_COD),
                               nchar(PP04D_COD) == 1 ~ paste0("0000", PP04D_COD)),
         CALIFICACION = substr(PP04D_COD, 5, 5),
         CALIFICACION = case_when(CALIFICACION=="1" ~ "Profesionales",
                                  CALIFICACION=="2" ~ "Técnicos",
                                  CALIFICACION=="3" ~ "Operativos",
                                  CALIFICACION=="4" ~ "No Calificados",
                                  TRUE ~ "0"),
         CALIFICACION = factor(CALIFICACION, c("No Calificados", "Operativos", "Técnicos", "Profesionales")),
         JERARQUIA = substr(PP04D_COD, 3, 3),
         JERARQUIA = case_when(JERARQUIA %in% c("0", "2") ~ "Dirección o Jefes",
                               JERARQUIA=="1" ~ "Cuentapropia",
                               JERARQUIA=="3" ~ "Trabajadores Asalariados",
                               TRUE ~ "0"),
         JERARQUIA = factor(JERARQUIA, c("Dirección o Jefes", "Trabajadores Asalariados", "Cuentapropia")),
         NIVEL_EDUCATIVO = case_when(NIVEL_ED %in% c(1, 7) ~ "Sin Instrucción",
                                     NIVEL_ED %in% c(2, 3) ~ "Primaria",
                                     NIVEL_ED %in% c(4, 5) ~ "Secundaria",
                                     NIVEL_ED == 6         ~ "Superior",
                                     NIVEL_ED == 9         ~ "NS/NR"),
         NIVEL_EDUCATIVO = factor(NIVEL_EDUCATIVO, levels = c("Sin Instrucción", "Primaria", "Secundaria", "Superior")),
         GRUPO_EDAD = case_when(CH06 >= 14 & CH06 <= 29 ~ "de 14 a 29 años",
                                CH06 >= 30 & CH06 <= 64 ~ "de 30 a 64 años"))

# colores = c("#aa165a","#16aa66")
colores = c("#FE1764", "#00BDD6")

# Ocupades
tabla8 <- base_individual %>% 
  filter(CALIFICACION != "0",
         ESTADO == 1) %>% 
  group_by(Sexo, CALIFICACION) %>% 
  summarise(IOP_mensual  = round(weighted.mean(P21, PONDIIO), 2)) 


tabla8_graf <- tabla8 %>% 
  pivot_wider(., names_from = Sexo, values_from = IOP_mensual) %>% 
  mutate(brecha = percent((Varones-Mujeres)/Varones, 1),
         x = (Varones+Mujeres)/2)

tabla8_graf

library(ggalt)

ggplot(tabla8_graf, 
       aes(x = Mujeres, xend = Varones, y = CALIFICACION, 
           group = CALIFICACION, label = brecha)) +
  geom_dumbbell(color = "#808080",
                size_x = 3, size_xend = 3,
                colour_x = colores[1],
                colour_xend = colores[2]) +
  geom_text(data = tabla8_graf, 
            aes(x, CALIFICACION, label = brecha), nudge_y = .2) +
  labs(title = "Brecha de ingresos de la ocupación principal
       por sexo y calificación ocupacional",
       x = "Ingreso Mensual",
       y = NULL, 
       caption = fuente) +
  scale_color_manual(values = colores) +
  theme_minimal()
 

# Brecha salarial posta ------------------------------------

liderazgo %>% 
  group_by(puesto) %>% 
  summarise(total = sum(cuenta)) %>% 
  arrange(-total)

brecha <- liderazgo %>% 
  filter(genero %in% c("Femenino", "Masculino"), 
         puesto %in% c("Director", "Gerente", "HRBP","Responsable", "Analista", "Administrativo")) %>% 
  mutate(puesto = factor(puesto, levels = c("Administrativo","Analista", 
                                            "HRBP", "Responsable","Gerente","Director" ))) %>% 
  select(-pais) %>% 
  group_by(genero, puesto) %>% 
  summarise(media_salarial = mean(sueldo))

brecha

brecha_graf <- brecha %>% 
  pivot_wider(., names_from = genero, values_from = media_salarial) %>% 
  mutate(brecha = percent((Masculino-Femenino)/Masculino, 1),
         x = (Masculino + Femenino)/2)

brecha_graf

ggplot(brecha_graf, 
       aes(x = Femenino, xend = Masculino, y = puesto, 
           group = puesto, label = brecha)) +
  geom_dumbbell(color = "#808080",
                size_x = 3, size_xend = 3,
                colour_x = colores[1],
                colour_xend = colores[2]) +
  geom_text(data = brecha_graf, 
            aes(x, puesto, label = brecha), nudge_y = .2) +
  labs(title = "Brecha salarial por puestos",
       subtitle = "Sueldos promedios en Argentina",
       x = "",
       y = NULL, 
       caption = fuente) +
  scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  scale_color_manual(values = colores) +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#fbfcfc"),
        text = element_text(family = "Roboto"))


## Salarios 

#¿Cómo es un salario típico en cada región?
#Esto, a nivel comparativo, a grosso modo y sin contemplar otras variables.
#1- Segun Pais: (No se la conversion)

#2- En Argentina, por zona

#ver como usar el left_join()para regiones

#3-Edad Vs Salarios

Edad <- kiwi %>%
    select("Tipo de contratación","Trabajo",Género, Edad,`¿En qué puesto trabajás?`, `¿Cuál es tu remuneración BRUTA MENSUAL en tu moneda local? (antes de impuestos y deducciones)`)

names(Edad) <- c("Jornada","Trabajo","genero","edad","puesto","sueldo")

head(Edad)

Edad <- Edad %>% 
  filter(!is.na(puesto)) %>%
  filter(edad>18) %>% 
  filter(Jornada=="Full time") %>% 
  filter(Trabajo=="Relación de Dependencia")%>% 
  mutate(sueldo = as.numeric(unlist(sueldo)),
         cuenta = 1) 

head(Edad)

Edad %>% group_by(edad, genero) %>% 
  summarise(sueldo_prom = mean(sueldo), 
            cuenta = sum(cuenta)) %>% 
  print(n = nrow(.))


Edad2 <- Edad %>%
  mutate(sueldo = as.numeric(unlist(sueldo))) %>% 
    mutate(Rangos_Edad = case_when(
    edad %in% 18:30 ~ "Hasta 30",
    edad %in% 31:40 ~ "Entre 31 y 40",
    edad %in% 41:50 ~ "Entre 41 y 50",
    edad %in% 51:70 ~ "Más de 50"),
    Rangos_Edad = factor(Rangos_Edad, 
                         levels = c("Hasta 30", "Entre 31 y 40","Entre 41 y 50", "Más de 50"))) 
  

Edad3<- Edad2 %>%
  select(Rangos_Edad, sueldo) %>%
  filter(sueldo>1) %>% 
  group_by(Rangos_Edad) %>% 
  summarise(Sueldo_Promedio = mean(sueldo)) # ver si estan bien los promedios

#tabla

gt(Edad3) %>% 
  tab_header(title = "Sueldos por Rango de Edad")

#Grafico 

Edad3 %>% 
  ggplot(aes(x = reorder(Rangos_Edad, Sueldo_Promedio), y = Sueldo_Promedio)) +
  geom_col() +
  coord_flip() +
  labs(title = "Sueldo promedio por área",
       subtitle = "En pesos argentinos",
       x= "", y = "", fill = "Género",
       caption = "Fuente: Encuesta KIWI de Sueldos de RRHH Latam \n Club de R para RRHH")+
  theme_minimal() +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))

#Salarios segun puestos 

puestos<-kiwi %>% 
  select("Tipo de contratación","Trabajo","País en el que trabajas",Género, Edad,`¿En qué puesto trabajás?`, `¿Cuál es tu remuneración BRUTA MENSUAL en tu moneda local? (antes de impuestos y deducciones)`)

names(puestos) <- c("Jornada","Trabajo","pais","genero","edad","puesto","sueldo")

puestos

#rta por puesto 

puestos1 <- puestos %>% 
  filter(pais=="Argentina") %>% 
  filter(!is.na(puesto)) %>% 
  mutate(cuenta = 1) %>% 
  group_by(`puesto`) %>% 
  summarise(Cuenta = sum(cuenta)) %>% 
  arrange(-Cuenta) %>% 
  print(n = nrow(.))

puestos1

#tabla por puestos

gt(puestos1) %>% 
  tab_header(title = "Cantidad por puestos",
             subtitle ="En Argentina")

#Promedio por puestos 


puestos2<- puestos %>%
  select(puesto,pais,sueldo) %>%
  filter(pais=="Argentina") %>% 
  filter(!is.na(puesto)) %>%
  filter(sueldo>1000) %>% 
  group_by(puesto) %>%
  mutate(sueldo = as.numeric(unlist(sueldo)),
         cuenta = 1) %>% 
  summarise(Sueldo_Promedio = mean(sueldo)) %>% 
  print(n = nrow(.))

#tabla

gt(puestos2) %>% 
  tab_header(title = "Sueldos Promedio por puestos",
             subtitle ="En Argentina")

#filtramos mas los puestos:

puestos 

puestos3<- puestos %>%
  select(puesto,pais,sueldo) %>%
  filter(`pais`=="Argentina") %>% 
  filter(!is.na(puesto)) %>%
  filter(sueldo>1000) %>% 
  mutate(cuenta = 1) %>%
  group_by(puesto) %>%
  mutate(sueldo = as.numeric(unlist(sueldo)),
         cuenta = 1) %>% 
  summarise(Sueldo_Promedio = mean(sueldo)) %>% 
  print(n = nrow(.))

view(puest)


puestos3

