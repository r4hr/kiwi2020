# inspiración https://ecofeminita.github.io/EcoFemiData/informe_desigualdad_genero/trim_2019_03/informe.nb.html#
# inspiración: https://blog.datawrapper.de/gendercolor/
# Verde Club: #009204
# Azul Club: #344D7E

# Paquetes y opciones -------------------------------------

library(tidyverse)      # Transformar y limpiar datos
library(googlesheets4)  # Leer datos desde Google Sheets
library(gargle)         # Corregir lectura de símbolos especiales desde Google Sheets
library(gt)             # Dar formato a las tablas
library(extrafont)      # Permite utilizar otras fuentes en los gráficos y salidas
library(ggthemes)       # Amplía las posibilidades estéticas de ggplot2
library(scales)         # Permite cambiar los formatos de decimales, de porcentajes, etc.
library(ggalt)          # Nuevos tipos de geom para ggplot2. Para realizar el gráfico de gap salarial
library(funModeling)    # Para explorar datos y modelos
library(forcats)

options(scipen = 999)   # Modifica la visualización de los ejes numérico a valores nominales

loadfonts(quiet = TRUE) # Permite cargar en R otros tipos de fuentes.

# Estilo limpio sin líneas de fondo
estilo <- theme(panel.grid = element_blank(),
                plot.background = element_rect(fill = "#FBFCFC"),
                panel.background = element_blank(),
                text = element_text(family = "Roboto"))

# Estilo limpio con líneas de referencia verticales en gris claro
estilov <- theme(panel.grid = element_blank(),
                 plot.background = element_rect(fill = "#FBFCFC"),
                 panel.background = element_blank(),
                 axis.line.x = element_line(color = "#AEB6BF"),
                 text = element_text(family = "Roboto")) 
  

# Estilo limpio con líneas de referencia horizontales en gris claro
estiloh <- theme(panel.grid = element_blank(),
                 plot.background = element_rect(fill = "#FBFCFC"),
                 panel.background = element_blank(),
                 axis.line.y = element_line(color = "#AEB6BF"),
                 text = element_text(family = "Roboto"))

genero <- c("#8624F5", "#1FC3AA", "#FFD129", "#75838F") #Violeta - Verde - Amarillo - Gris

colores <-  c("#8624F5", "#1FC3AA")

azul <- "#344D7E"

# Creo un objeto con un texto que se va a repetir mucho a lo largo del análisis
fuente <- "Fuente: Encuesta KIWI de Sueldos de RRHH para Latam"


# Carga de datos ------------------------------------------

original <- sheets_read("1aeuu9dVfN42EjyvbmhEcsf0ilSz2DiXU-0MpnF896ss")



# Preprocesamiento ---------------------------------------
kiwi <- original

limpios <- make.names(colnames(kiwi))
colnames(kiwi) <- limpios

names(kiwi)
rm(limpios)

kiwi <- kiwi %>% 
  select(-X.Querés.contestar.más.preguntas....31, 
         -X.Querés.contestar.más.preguntas....42) %>% 
  rename(genero = Género,
         genero_diverso = X.Te.identificás.como.LGBT...lesbiana..gay..bisexual..transexual..otra.minoría.sexual..,
         edad = Edad,
         discapacidad = X.Tenés.alguna.discapacidad.,
         nivel_formacion = `Máximo.nivel.de.formación`,
         carrera_grado = X.Qué.carrera.de.grado.estudiaste.,
         tipo_universidad = X.En.qué.tipo.de.universidad.estudiaste.tu.carrera.de.grado.,
         pais = País.en.el.que.trabajas,
         provincia = Provincia.donde.trabajas,
         trabajo = Trabajo,
         rubro = Rubro.de.la.empresa,
         dotacion = X.Cuántos.empleados.tiene.la.empresa.,
         origen_capital = Origen.del.capital,
         dotacion_rh = X.Cuántas.personas.integran.el.área.de.RRHH.,
         puesto = X.En.qué.puesto.trabajás.,
         tipo_contratacion = Tipo.de.contratación,
         funcion_rh = X.Cuál.es.tu.función.principal.en.RRHH.,
         personas_a_cargo = "X.Cuántas.personas.tenés.a.cargo...poné.0.si.no.tenés.gente.a.cargo.",
         anios_en_empresa = "X.Hace.cuántos.años.trabajas.en.la.empresa.donde.estás...0.para.menos.de.un.año.",
         anios_en_puesto = "X.Hace.cuántos.años.estás.en.tu.puesto.actual...0.para.menos.de.un.año.",
         anios_experiencia = X.Cuántos.años.de.experiencia.tenés.en.RRHH.,
         sueldo_bruto = X.Cuál.es.tu.remuneración.BRUTA.MENSUAL.en.tu.moneda.local...antes.de.impuestos.y.deducciones.,
         beneficios = X.Qué.beneficios.tenés.,
         bono = X.Recibís.bonos.,
         ajuste = X.Tuviste.ajustes.por.inflación.en.2020.,
         ajuste_porcentaje = X.Cuál.fue.el.porcentaje.de.aumento.acumulado.que.tuviste.en.2020.,
         ajuste_mes = Mes.del.último.ajuste,
         otros_proyectos = X.Trabajás.en.proyectos.independientes.además.de.tu.empleo.,
         erp = X.Qué.sistema.de.gestión.de.RRHH.usan.en.tu.empresa.,
         nombre_area = X.Cómo.se.llama.el.área.en.tu.empresa.,
         mate = X.Se.podía.tomar.mate.en.las.oficinas.de.tu.empresa...antes.del.COVID.19.,
         idioma_exigencia = X.Te.exigieron.saber.un.idioma.extranjero..inglés..portugués..etc...para.entrar.a.trabajar.en.tu.empresa.,
         idioma_porcentaje = X.Qué.porcentaje.del.tiempo.usas.el.idioma.extranjero.en.tu.puesto.actual.,
         contactos_linkedin = "X.Cuántos.contactos.tenés.en.LinkedIn...poné.0.si.no.tenés.cuenta.de.LinkedIn.",
         satisfaccion = X.Qué.tan.satisfecho.estás.con.tu.empresa.,
         busqueda = X.Estás.buscando.trabajo.,
         beneficios_expectativa = X.Qué.beneficios.te.gustaría.tener.,
         rh_una_palabra = Definí.a.RRHH.con.una.sola.palabra,
         pregunta_bizarra = X.Cuál.es.la.pregunta.más.bizarra.que.te.han.hecho.has.hecho.en.una.entrevista.,
         teletrabajo = X.Estás.trabajando.desde.tu.casa.,
         elementos = X.Qué.elementos.te.proveyó.la.empresa.para.que.puedas.trabajar.desde.tu.casa.,
         valoracion_gestion_empresa = X.Cómo.valorarías.la.gestión.de.tu.empresa.en.este.nuevo.contexto.,
         registro_fiscal = X.Cómo.estás.registrado.a.fiscalmente.,
         anios_freelance = X.Hace.cuántos.años.trabajás.como.freelance.,
         lugar_trabajo = X.Dónde.trabajás.habitualmente...sin.considerar.la.coyuntura.por.COVID.19.,
         exporta = X.Exportás.tus.servicios.,
         medio_pago_exterior = Si.exportás.servicios...a.través.de.qué.medios.de.pago.recibís.los.pagos.del.exterior.,
         cuotas = X.Aceptás.pagos.en.cuotas.,
         colaboracion_freelance = X.Trabajás.con.otros.freelancers.de.tu.mismo.rubro.,
         servicio_busqueda = X.Tu.servicio.principal.está.relacionado.con.búsqueda.y.selección.,
         busqueda_it = X.Te.dedicás.principalmente.a.realizar.búsquedas.de.IT.Tecnología.,
         trabajo_a_riesgo =X.Trabajás.a.riesgo.,
         coeficiente = X.Cuál.es.el.coeficiente.que.cobrás.por.tus.servicios.,
         base_coeficiente = El.coeficiente.lo.calculás.sobre.,
         garantia = X.Ofrecés.garantía.,
         servicio_principal = X.Cuál.es.el.servicio.principal.que.brindas...si.brindás.más.de.un.servicio..elegí.el.que.más.ingresos.genere.,
         valor_hora = X.Cuál.es.el.valor.hora.promedio.que.ofrecés...moneda.local.)

# Base de freelancers
freelo <- kiwi %>% 
  filter(trabajo == "Freelance")

# Base de empleados en relación de dependencia
rh <- kiwi %>% 
  filter(trabajo == "Relación de Dependencia") %>% 
  mutate(sueldo_bruto = as.numeric(unlist(sueldo_bruto)),
         puesto = factor(puesto))

# Cuenta la cantidad de puestos distintos
rh %>% 
  select(puesto) %>% 
  distinct(puesto) %>% 
  count() %>% 
  pull()

# Quiero ver todos los puestos y referencia salarial
rh %>% 
  group_by(puesto) %>% 
  summarise(sueldo_mediana = median(sueldo_bruto),
            total = n()) %>% 
  print(n = nrow(.))

  
# Unificación de puestos
rh <- rh %>% 
  filter(puesto != "Juzgado Civil y Comercial",
         puesto != "Programador",
         puesto != "Cuidado",
         puesto != "Asesor",
         puesto != "Jefe de Proyecto") %>% 
  mutate(puesto = str_trim(puesto, side = "both"), # Elimina espacios vacíos
         puesto = fct_collapse(puesto, "Gerente" = "Superintendente"),
         puesto = fct_collapse(puesto, "Director" = "Director ( escalafón municipal)"),
         puesto = fct_collapse(puesto, "HRBP" = c("Senior Consultoría", "specialist", "especialista",
                                                  "Especialista en selección IT", "Recruiter")),
         puesto = fct_collapse(puesto, "Responsable" = c("Coordinación", "Coordinador de Payroll",
                                                         "Encargado", "Supervisor")),
         puesto = fct_collapse(puesto, "Administrativo" = c("Asistente", "Asistente RRHH", "Aux", 
                                                            "Capacitador", "Consultor Ejecutivo",
                                                            "consultor jr")),
         puesto = fct_collapse(puesto, "Analista" = c("Asesoramiento", "Consultor", "Generalista", 
                                                      "Reclutadora", "Selectora", "Senior"))) %>% 
  select(Marca.temporal:valoracion_gestion_empresa)

# Cuento la cantidad de puestos diferentes luego de la limpieza.
rh %>% 
  select(puesto) %>% 
  distinct(puesto) %>% 
  count() %>% 
  pull()

# En la columna de contactos hay gente que usó el símbolo "+" lo cual impide deshacer la lista
# Busco manualmente los índices que tienen el símbolo +
rh$contactos_linkedin
rh$contactos_linkedin[c(1:400)]

# Modifica los registros
rh$contactos_linkedin[[17]] <- 500
rh$contactos_linkedin[[25]] <- 1000
rh$contactos_linkedin[[72]] <- 500
rh$contactos_linkedin[[85]] <- 1000
rh$contactos_linkedin[[151]] <- 800
rh$contactos_linkedin[[217]] <- 500
rh$contactos_linkedin[[225]] <- 1000
rh$contactos_linkedin[[272]] <- 500
rh$contactos_linkedin[[285]] <- 1000
rh$contactos_linkedin[[342]] <- 1000
rh$contactos_linkedin[[366]] <- 500
rh$contactos_linkedin[[386]] <- 30000
rh$contactos_linkedin[[389]] <- 500
rh$contactos_linkedin[[403]] <- 5000
rh$contactos_linkedin[[450]] <- 500
rh$contactos_linkedin[[464]] <- 500
rh$contactos_linkedin[[601]] <- 500
rh$contactos_linkedin[[604]] <- 500

# En las siguientes columnas, Google Sheet no reconoce el punto como símbolo decimal
# Corrijo manualmente los registros para que sea un valor numérico
rh$anios_en_empresa[[21]]  <- 1.5
rh$anios_en_empresa[[53]]  <- 1.5
rh$anios_en_empresa[[117]] <- 1.5
rh$anios_en_empresa[[181]] <- 2.5
rh$anios_en_empresa[[222]] <- 0
rh$anios_en_empresa[[282]] <- 4.5
rh$anios_en_empresa[[346]] <-  3.5
rh$anios_en_empresa[[360]] <- 1.5
rh$anios_en_empresa[[365]] <- 1.5
rh$anios_en_empresa[[553]] <- 1.5
rh$anios_en_empresa[[653]] <- 1.5


rh$anios_en_puesto[[21]]  <- 1.5
rh$anios_en_puesto[[53]]  <- 1.5 
rh$anios_en_puesto[[117]] <- 1.5
rh$anios_en_puesto[[161]] <- 2.5
rh$anios_en_puesto[[181]] <- 2.5
rh$anios_en_puesto[[346]] <- 2.5
rh$anios_en_puesto[[360]] <- 1.5
rh$anios_en_puesto[[365]] <- 1.5
rh$anios_en_puesto[[553]] <- 1.5


rh$anios_experiencia[[117]] <- 1.5
rh$anios_experiencia[[360]] <- 3.5
rh$anios_experiencia[[365]] <- 1.5
rh$anios_experiencia[[402]] <- 0.5
rh$anios_experiencia[[492]] <- 2.4
rh$anios_experiencia[[529]] <- 6.5
rh$anios_experiencia[[653]] <- 4.5


rh$ajuste_porcentaje[[61]]  <-  9.5
rh$ajuste_porcentaje[[62]]  <- 12.69
rh$ajuste_porcentaje[[90]]  <- 23.7
rh$ajuste_porcentaje[[119]] <- 7.3
rh$ajuste_porcentaje[[225]] <- 2.5
rh$ajuste_porcentaje[[242]] <- 20.7
rh$ajuste_porcentaje[[262]] <- 15.4
rh$ajuste_porcentaje[[337]] <- 2.5
rh$ajuste_porcentaje[[350]] <- 14.52
rh$ajuste_porcentaje[[433]] <- 12.8
rh$ajuste_porcentaje[[434]] <- 12.8
rh$ajuste_porcentaje[[435]] <- 8.11
rh$ajuste_porcentaje[[449]] <- 13.5
rh$ajuste_porcentaje[[463]] <- 22.7
rh$ajuste_porcentaje[[488]] <- 5.5
rh$ajuste_porcentaje[[513]] <- 30.5
rh$ajuste_porcentaje[[529]] <- 28.06
rh$ajuste_porcentaje[[581]] <- 0
rh$ajuste_porcentaje[[592]] <- 50
rh$ajuste_porcentaje[[615]] <- 16.2
rh$ajuste_porcentaje[[630]] <- 0.5
rh$ajuste_porcentaje[[640]] <- 35.2
rh$ajuste_porcentaje[[648]] <- 44.5


rh <- unnest(data = rh, cols = c(anios_en_empresa, anios_en_puesto, anios_experiencia,
                                 ajuste_porcentaje, contactos_linkedin), keep_empty = TRUE)

# Análisis exploratorio ----------------------------------

glimpse(rh)

rh %>% 
  group_by(puesto) %>% 
  summarise(sueldo_mediana = median(sueldo_bruto),
            total = n()) %>% 
  print(n = nrow(.))

# Países de donde recibimos respuestas
rh %>% 
  select(pais) %>% 
  distinct(pais) %>% 
  count() %>% 
  pull()


# Respuestas por países
paises <- rh %>% 
  select(pais) %>% 
  mutate(cuenta = 1) %>% 
  group_by(pais) %>% 
  summarise(Cuenta = sum(cuenta)) %>% 
  filter(Cuenta > 4) %>% 
  arrange(-Cuenta)

gt(paises) %>% 
  tab_header(title = "Cantidad de respuestas por país",
             subtitle = "Países con más de 5 respuestas") 

# Respuestas por provincia (Sólo para Argentina)
provincias <- rh %>% 
  filter(pais == "Argentina") %>% 
  mutate(cuenta = 1) %>% 
  group_by(provincia) %>% 
  summarise(Cuenta = sum(cuenta)) %>% 
  arrange(-Cuenta)


gt(provincias)


liderazgo <- rh %>% 
  select(genero, puesto,pais ,sueldo_bruto)

# Respuestas por rubro
rh %>% 
  select(rubro) %>% 
  group_by(rubro) %>% 
  count(sort = TRUE) %>% 
  print(n = nrow(.)) # Imprime en consola todos los resultados

# Exploración para Argentina
liderazgo <- liderazgo %>% 
  filter(!is.na(puesto)) %>% 
  mutate(cuenta = 1) %>% 
  filter(pais == "Argentina")

liderazgo %>% group_by(puesto, genero) %>% 
  summarise(sueldo_prom = mean(sueldo_bruto), 
            cuenta = sum(cuenta)) %>% 
  print(n = nrow(.))

liderazgo %>% 
  group_by(puesto, genero) %>% 
  summarise(salarios = list(mean_se(sueldo_bruto))) %>% 
  unnest(salarios) %>% 
  ggplot(aes(x = puesto, y = y, fill = genero))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = ymin,ymax = ymax), position = "dodge")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "bottom")




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

sueldos_dolar <- rh %>% 
  select(puesto, sueldo_bruto, pais, tipo_contratacion) %>% 
  filter(puesto != "Juzgado Civil y Comercial", puesto != "Pasante", 
         puesto != "Programador", puesto != "Jefe de Proyecto", 
         tipo_contratacion != "Pasante")

# Controlar la cantidad de casos de contratos part time
sueldos_dolar %>% 
  group_by(tipo_contratacion) %>% 
  count(pais) %>% 
  filter(tipo_contratacion == "Part time")

# Agrego un multiplicador de sueldos para convertir los sueldos part time en full time
sueldos_dolar <- sueldos_dolar %>% 
  left_join(tc, by="pais") %>% 
  mutate(multiplicador = if_else(tipo_contratacion == "Part time", 1.5, 1),
         sueldo_ft = sueldo_bruto * multiplicador,    # Hace la equivalencia de un sueldo part time a full time
         sueldo_dolar = sueldo_ft/tipo_cambio,  # Convierto los sueldos a dólares
         cuenta = 1)

summary(sueldos_dolar)



numericos <- profiling_num(sueldos_dolar)
poda_p05 <- numericos[5,6]
poda_p95 <- numericos[5,10]

# Dado que los percentiles 5 y 95 están en U$400 y 2689 respectivamente, 
# podamos todo lo que esté fuera de ese rango

media_pais <- sueldos_dolar %>% 
  filter(pais %in% c("Argentina", "Bolivia", "Chile", "Paraguay", "Uruguay", "Perú"),
         between(sueldo_dolar,poda_p05,poda_p95)) %>% 
  group_by(pais) %>% 
  summarise(sueldop = list(mean_se(sueldo_dolar)),
            cant = sum(cuenta)) %>% 
  unnest(cols = c(sueldop)) %>%
  print(n = nrow(.)) 
 
sueldo_dolar_pais <- sueldos_dolar %>% 
  filter(pais %in% c("Argentina", "Bolivia", "Chile", "Paraguay", "Uruguay", "Perú"),
         between(sueldo_dolar, 400,3000))
  
# Gráfico
ggplot(media_pais, aes(x = reorder(pais, -y), y =  y))+
  geom_col(fill = "#344D7E", alpha = 0.85) +
  geom_errorbar(aes(ymin = ymin,ymax = ymax), position = "dodge", color = "#75838F")+
  geom_point(data = sueldo_dolar_pais, aes(x = pais, y = sueldo_dolar), 
             alpha = 0.3, size = 2, color = "#75838F")+
  geom_text(aes(label = round(x=y, 0), vjust = 1.5, fontface = "bold"),size = 4, color = "white")+
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))+
  labs(title = "Mediana salarial por país",
       subtitle = "Sueldos de RRHH en U$S",
       caption = "Fuente: Encuesta KIWI de Sueldos de RRHH para Latam \n Países con 5 respuestas o más",
       x = "", y = "") + 
  estilo


# Sueldos Argentina --------------------------------------
rh_ar <- rh %>% 
  filter(pais == "Argentina")


# Poda de sueldos entre percentiles 5 y 95
numericos2 <- rh_ar %>% 
  select_if(is.numeric) %>% 
  profiling_num()

p_05 <- numericos2[5,6]
p_95 <-numericos2[5,10]

rh_ar <- rh_ar %>% 
  filter(between(sueldo_bruto, p_05, p_95))

rubro <- rh_ar %>% 
  select(rubro, sueldo_bruto) %>% 
  group_by(rubro) %>% 
  summarise(mediana_sueldo = median(sueldo_bruto),
            respuestas = n()) %>% 
  arrange(-respuestas)

print(rubro, n = 20)


rubro %>% 
  filter(respuestas > 12) %>% 
  ggplot(aes(x = mediana_sueldo, y = reorder(rubro, mediana_sueldo))) +
  geom_col(fill = azul) +
  geom_text(aes(label = round(x=mediana_sueldo, 0), hjust = 1.5),size = 3, color = "white") +
  labs(title = "Mediana salarial por rubro",
       subtitle = "Datos de Argentina - en AR$",
       x = "", y = "",
       caption = fuente) +
  scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ";")) +
  estilov +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Representación en puestos de liderazgo ------------------


# Representación de género en la encuesta
kiwi %>% 
  select(Género) %>% 
  group_by(Género) %>% 
  summarise (n = n()) %>% 
  mutate(freq = n/sum(n)) 

diversidad <- kiwi %>% 
  filter(Trabajo !="Freelance") %>% 
  mutate(genero = fct_collapse_recode(Género, 
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
  mutate(genero = fct_collapse_recode(genero,
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



# Brecha salarial posta ------------------------------------

liderazgo %>% 
  group_by(puesto) %>% 
  summarise(total = sum(cuenta)) %>% 
  arrange(-total)

brecha <- liderazgo %>% 
  filter(genero %in% c("Femenino", "Masculino"), 
         puesto %in% c("Director", "Gerente", "Jefe", "HRBP","Responsable", "Analista", "Administrativo")) %>% 
  mutate(puesto = factor(puesto, levels = c("Administrativo","Analista", 
                                            "HRBP", "Jefe", "Responsable","Gerente","Director" ))) %>% 
  select(-pais) %>% 
  group_by(genero, puesto) %>% 
  summarise(media_salarial = mean(sueldo_bruto))



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


# Satisfacción -------------------------

satisf <- kiwi %>% 
  filter(!is.na(`¿Qué tan satisfecho estás con tu empresa?`),
         `País en el que trabajas` == "Argentina") %>% 
  select(Género, `¿Cuál es tu remuneración BRUTA MENSUAL en tu moneda local? (antes de impuestos y deducciones)`, `¿Qué tan satisfecho estás con tu empresa?`)

names(satisf) <- c("genero", "sueldo", "satisfaccion")

satisf <- satisf %>% 
  mutate(sueldo = as.numeric(unlist(sueldo))) %>% 
  filter(sueldo > 30000)

profiling_num(satisf)

p25 <- profiling_num(satisf)[1,7]
p50 <- profiling_num(satisf)[1,8]
p75 <- profiling_num(satisf)[1,9]

satisf <- satisf %>% 
  mutate(cuartil = case_when(
    sueldo  <   p25  ~ "1Q",
    sueldo %in% p25:p50 ~ "2Q",
    sueldo %in% p50:p75 ~ "3Q",
    TRUE ~ "4Q"),
    satisfaccion = factor(satisfaccion, 
                          levels = c(1,2,3,4,5)), 
    cuenta = 1)

library(networkD3)


satisf <- satisf %>% 
  group_by(satisfaccion, cuartil) %>% 
  summarise(value = sum(cuenta))

nodes <- data.frame(name=c(as.character(satisf$satisfaccion),
                           as.character(satisf$cuartil)) %>% 
                      unique())

satisf$IDsource <- match(satisf$satisfaccion, nodes$name) - 1
satisf$IDtarget <-match(satisf$cuartil, nodes$name) - 1


sankeyNetwork(Links = satisf,
              Nodes = nodes,
              Value = "value",
              NodeID = "name",
              sinksRight=TRUE, nodeWidth=40, fontSize=13, nodePadding=20)


puestos <- kiwi %>%
  filter(!is.na(`¿En qué puesto trabajás?`)) %>% 
  select(pais = `País en el que trabajas`, 
         rol = `¿Cuál es tu función principal en RRHH?`,
         puesto = `¿En qué puesto trabajás?`,
         sueldo =`¿Cuál es tu remuneración BRUTA MENSUAL en tu moneda local? (antes de impuestos y deducciones)`) %>% 
  mutate(sueldo = as.numeric(unlist(sueldo)),
         cuenta = 1)



puestos %>% 
  filter(pais == "Argentina") %>% 
  group_by(rol, puesto) %>% 
  summarise(promedio = mean(sueldo),
            cant = sum(cuenta)) %>% 
  print(n = nrow(.))

# Educación (Daniela)

recorte_educacion <- rh %>%
  select(nivel_formacion, carrera_grado,
         tipo_universidad, trabajo, sueldo_bruto, puesto, funcion_rh, pais, genero)

#Relacion tipo de universidad vs cargo que ocupa

ggplot(recorte_educacion, (aes(x = puesto, fill = tipo_universidad))) + #Tipo de universidad y cargo
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x="",y="")

#Composicion del area segun tipo de universidad

recorte_area <- recorte_educacion %>%
  select(tipo_universidad, carrera_grado, funcion_rh, puesto)



ggplot(recorte_area, aes(x = tipo_universidad)) + #Tipo de universidad y cargo
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x="",y="") 


#Composición del área según carrera

library(forcats)

recorte_educacion$carrera_grado <- as.factor(recorte_educacion$carrera_grado) 
summary(recorte_educacion$carrera_grado) 
levels(recorte_educacion$carrera_grado) 

ggplot(recorte_educacion, aes(x = carrera_grado, fill = tipo_universidad)) + #HORROR
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x="",y="") 

rh <- rh %>% 
  filter(puesto != "Juzgado Civil y Comercial",
         puesto != "Programador",
         puesto != "Cuidado") %>% 
  mutate(puesto = str_trim(puesto, side = "both"), # Elimina espacios vacíos
         puesto = fct_collapse_recode(puesto, "Gerente" = "Superintendente"),
         puesto = fct_collapse(puesto, "HRBP" = c("Asesor", "Asesoramiento", 
                                                  "Senior Consultoría", "specialist"),
                               "Administrativo" = c("Asistente","Aux", "Asistente RRHH"))) 


recorte_educacion <- recorte_educacion %>%
  mutate(carrera_grado = fct_collapse(carrera_grado, "Abogacía" = c("Abogacía, Escribanía, Lic en RRHH", "Abogacía"),
                        "Administración de Empresas" = c ("Administración de Empresas", "Lic en Administracion"),
                        "Contador Público" = c ("Contador Público", "Economia", "Economía"),
                        "RRHH / RRLL / RRTT" = c ("Estudié en la Universidad Analista en Recursos Humanos", "RRHH&Coaching ontologico profesional."),
                        "Ingenieria industrial" = c ("Ing en sistemas", "Ingeniería Comercial", "Ingenieria Electrónica","Ingeniería Industrial"),
                        "Psicología" = c ("Psicologia","Psicología Industrial", "Psicología social","Psicología Social / Lic. en Dirección de las Organizaciones")))

"Otros" = c ("Actuario", "Administración y sistemas", "Antropologia", "Ciencias de la educacion", 
             "Ciencias Políticas", "Contador Público/Lic. En LetrasContador Público/Lic. En Letras", "Economia y RR.HH",
             "Graduada en lic rrhh y abogacia", "Lic Administracion - Contador Publico - Abogado en curso",
             "Lic educacion", "Lic en Turismo", "Lic. En nutricion", "Lic. En nutricion", "Literatura","sistemas", "Sistemas_información", "Comunicación_social" = c ("Caomunicacion social me especialice en RRHH", "Comunicación", "Comunicación social", "Comunicación Social", "Lic. en Ciencias de la Comunicación",                                                                                                                                          "Licenciatura en Comunicación Social", "Licenciatura en Comunicación Social")
             "Relaciones internacionales", "Relaciones Internacionales", "Traductorado de ingles", "Analista de Sistemas", "Licenciatura en Sistemas", "Marketing", "Lic. RRPP", "Relaciones Públicas","Rrpp")

#Relacion nivel de estudios/ cargo que ocupa

ggplot(recorte_educacion, (aes(x = nivel_formacion, fill = puesto))) + #Tipo de universidad y cargo
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x="",y="")

fct_collapse_collapse(recorte_educacion$puesto, 
             Administrativo = c ("Administrativo"),
             HRBP = c ("HRBP"),
             Analista = c ("Analista","Capacitador","Reclutadora", "Recruiter"),
             Consultor = c ("Consultor", "Consultor Ejecutivo", "consultor jr"),
             Supervisor = c ("Coordinación", "Coordinador de Payroll", "Supervisor" ),
             Director = c ("Director", "Director ( escalafón municipal)"),
             Encargado =c ("Encargado","Responsable"), #Hay que ver
             Especialista = c ("especialista", "Especialista en selección IT"),
             Generalista = c ("Generalista"),
             Gerente = c ("Gerente"),
             Jefe = c ("Jefe de Proyecto"),
             Pasante = C ("Pasante")

levels(recorte_educacion$puesto)


#Nivel estudios/ remuneracion

ne_salario <- recorte_educacion 
  
ne_salario %>% #Problemas con la moneda
  select(pais, sueldo_bruto, nivel_formacion) %>%
  filter(pais == "Argentina", nivel_formacion != "Secundario en curso") %>%
  group_by(nivel_formacion) %>% 
  summarise(media_salarial = mean(sueldo_bruto)) %>%
  ggplot(aes (x= nivel_formacion, y = media_salarial)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x="",y="")

ne_salario %>% 
  select(pais, nivel_formacion, genero, puesto) %>%
  filter(pais == "Argentina"| puesto == "Director"| puesto == "Gerente" | puesto == "HRBP") %>%
  group_by(nivel_formacion) %>% 
  ggplot(aes (x= nivel_formacion, fill = genero)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x="",y="")

# 
