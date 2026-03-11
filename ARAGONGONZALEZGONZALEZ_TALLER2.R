# =============================================================================
# Taller 2. Taller de Programación en R
# Facultad de Economía
# Universidad de los Andes
# Profesor : Santiago Neira 
# Estudiantes:
# - Mauricio Aragón - 201729052
# - Alejandra González - 202111607
# - Valentina González - 202111608
# =============================================================================

# -----------------------------------------------------------------------------
# Paquetes necesarios y preparación del ambiente de trabajo
# -----------------------------------------------------------------------------

# Limpiamos el espacio de trabajo
rm(list = ls())

# Instalación y carga de paquetes necesarios
required_packages <- c("openxlsx", "ggplot2", "dplyr", "lubridate", "readr",
                       "writexl", "tidyr")

# Función para instalar paquetes si no están instalados
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
}

install_if_missing(required_packages)

# Cargar librerías
lapply(required_packages, library, character.only = TRUE)


#  Configuración del directorio de trabajo ----------------------------------
# Obtener usuario del sistema para crear ruta dinámica
username <- Sys.getenv("USERNAME")

# Construir ruta hacia los datos usando el usuario actual
path_taller <- paste0("C:/Users/", username, "/OneDrive/Documents/GitHub/MEcA/Taller R/Taller2")

# Establecer directorio de trabajo
setwd(path_taller)

# -----------------------------------------------------------------------------
# Punto 1. Conformación y análisis base de datos
# -----------------------------------------------------------------------------

# ---------- 1.1. Pegue de las bases de datos ----------

bases <- c("Fuerza de trabajo.csv","Ocupados.csv","No ocupados.csv")
lista_bases <- list()

# Para cada mes, vamos a crear la base que conglomere las 4 bases de trabajo:
#     1. Características generales, seguridad social en salud y educación.CSV
#     2. Fuerza de trabajo.CSV
#     3. Ocupados.CSV
#     4. No Ocupados.CSV

# Para cada uno de los 12 meses del 2025 vamos a:
for (n in 1:12){
  
  # Primero, leemos el que será nuestro módulo básico
  # Usamos read_csv2 para indicarle a R que el separador es ";", locale para
  # decirle que lea tíldes y acentuaciones y col_types para que asuma que todos
  # los datos son tipo texto
  base_main <- read_csv2(paste0("Data/GEIH_2025/",n,"/Características generales, seguridad social en salud y educación.csv"),
                               locale = locale(encoding = "latin1"), col_types = cols(.default = "c"))
  

  # Luego, creamos una llave que será con la que pegaremos las demás bases.
  # Esta llave es la información que caracteriza a una persona (ORDEN) en un 
  # hogar (SECUENCIA_P) dentro de una vivienda (DIRECTORIO)
  base_main <- mutate(base_main,
                  LLAVE = paste0(DIRECTORIO, SECUENCIA_P, ORDEN))
  
  # Ahora creamos otro loop que haga para cada una de las bases a pegar (es
  # decir para las bases 2, 3 y 4):
  for (i in bases){
    
    # Lea la base del mes
    base_join <- read_csv2(paste0("Data/GEIH_2025/",n,"/",i),
                      locale = locale(encoding = "latin1"), col_types = cols(.default = "c"))
    
    # Cree la llave que anteriormente describimos
    base_join <- mutate(base_join,LLAVE = paste0(DIRECTORIO, SECUENCIA_P, ORDEN))
    
    # Eliminar variables que estén en la base pero que ya estén en el módulo básico
    # para evitar tener variables duplicadas
    base_join <- select(base_join, LLAVE, names(base_join)[!names(base_join) %in% names(base_main)])
    
    # Hacer un pegue de la base con el módulo básico dejando todas las observaciones
    # de esa base original (incluso si no tienen datos en la base que se está pegando)
    base_main <- base_main %>% left_join(base_join, by = "LLAVE")
  }
  
  # Finalmente nombra la base con el número del mes
  assign(paste0("base_",n),base_main)
  
  # Y creamos una lista con todas las bases para hacer el pegue de todos los meses
  # más adelante
  lista_bases[[n]] <- base_main
}


# Ahora, vamos a hacer el pegue de las bases por mes que acabamos de crear.

# Para eso, empezamos por llamar la lista de bases mensuales que creamos. 
# Luego, con bind_rows apilamos todos los data frames (es decir unimos 
# verticalmente las bases)

## CAMBIÉ ESTO: base_total <- bind_rows(mget(paste0("base_", 1:12)))
base_total <- bind_rows(lista_bases)

# Ya con la base lista nos vamos a quedar sólo con las variables de interés
base_final <- select(base_total, PERIODO, DIRECTORIO, SECUENCIA_P, ORDEN,
                     P3271, P3042, P6090, P6040, P6160, DPTO, FEX_C18, PT,
                     PET, FT, FFT, P6240, OCI, RAMA2D_R4, P6430, INGLABO, DSI)

# Ajustamos fechas: hacemos esto para que R reconozca los meses de observaciones
# con fechas (particulamente días) no convencionales como "20251252"
base_final$PERIODO <- as.Date(paste0(substr(base_final$PERIODO,1,6),"01"),"%Y%m%d")
base_final$mes <- month(base_final$PERIODO, label = FALSE)
base_final$dia <- day(base_final$PERIODO)
base_final$anio <- year(base_final$PERIODO)

# Convertimos todo a numérico
base_final <- base_final %>%
  mutate(across(everything(), as.numeric))

# Renombramos las variables para tener mejor manejo de la información
base_final <- base_final %>%
  rename(
    sexo = P3271,
    nivel_edu = P3042,
    eps = P6090,
    edad = P6040,
    leer_escr = P6160,
    act_ocup = P6240,
    tipo_empl = P6430
  ) %>% 
  mutate(
    sexo = factor(sexo, levels=c(1,2), labels = c("Hombre","Mujer")),
    nivel_edu = factor(nivel_edu, levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,99),
                       labels = c("Ninguno","Preescolar","Básica Primaria","Básica Secundaria",
                                  "Media Académica","Media Técnica","Normalista","Técnico Profesional",
                                  "Tecnológica","Universitaria","Especialización","Maestría",
                                  "Doctorado","No sabe, No informa")),
    eps = factor(eps, levels=c(1,2,99), labels = c("Sí","No", "No sabe, No informa")),
    leer_escr = factor(leer_escr, levels = c(1,2), labels = c("Sí","No")),
    act_ocup = factor(act_ocup, levels = c(1,2,3,4,5,6), labels = c("Trabajando",
                                                                    "Buscando Trabajo",
                                                                    "Estudiando",
                                                                    "Oficios del Hogar",
                                                                    "Incapacitado",
                                                                    "Otra actividad")),
    tipo_empl = factor(tipo_empl, levels = c(1,2,3,4,5,6,7,8),
                      labels = c("Obrero o Empl. Empresa Part","Obrero o Empl. del Gobierno",
                                 "Empleado doméstico","Trabajador por cuenta propia",
                                 "Patrón o empleador","Trabajador familiar sin rem.",
                                 "Jornalero o peón","Otro"))
  )
  


# -----------------------------------------------------------------------------
# Punto 2. Construcción de los principales indicadores de mercado laboral
# -----------------------------------------------------------------------------

# ---------- 2.1. Tasas y poblaciones ----------

# Vamos a contruir una tabla con los principales indicadores de mercado laboral 
# para el total nacional para los meses comprendidos entre enero y diciembre 
# de 2025. 

# Primero, construimos el data frame: 
tabla_indicadores <- base_final %>%
  
  # Calculamos por mes los valores de los elementos que componen los indicadores
  group_by(mes) %>%
  summarise(
    poblacion_total = sum(FEX_C18, na.rm = TRUE),
    PET = sum(PET * FEX_C18, na.rm = TRUE), # Población en edad de trabajar
    FT = sum(ifelse(is.na(FT),0,FT) * FEX_C18, na.rm = TRUE), # Fuerza de trabajo
    # Convertimos los NA en 0 para asegurarnos que no hayan errores de cálculo
    ocupados = sum(OCI * FEX_C18, na.rm = TRUE),
    desocupados = sum(DSI * FEX_C18, na.rm = TRUE)) %>%
  
  # Luego calculamos los indicadores
  mutate(
    TGP = round((FT / PET) * 100,2),
    TO = round((ocupados / PET) * 100,2),
    TD = round((desocupados / FT) * 100,2)) %>%
  
  # Dejámos sólo la información que se pide en la tabla
  select(mes, TGP, TO, TD, poblacion_total, PET, FT, ocupados, desocupados)

View(tabla_indicadores)

# Ahora, rotamos la tabla para que los meses queden en las columnas, cambiamos
# los nombres para que los nombres queden como la tabla de muestra
tabla_indicadores_final <- tabla_indicadores %>%
  pivot_longer(-mes) %>%
  pivot_wider(names_from = mes, 
              values_from = value)

tabla_indicadores_final$name <- c(
  "Tasa Global de Participación (TGP)",
  "Tasa de Ocupación (TO)",
  "Tasa de Desempleo (TD)",
  "Población total",
  "Población en edad de trabajar (PET)",
  "Fuerza de trabajo",
  "Población Ocupada",
  "Población Desocupada"
)


colnames(tabla_indicadores_final) <- c( "Tasas (%) y poblaciones",
  "Enero","Febrero","Marzo","Abril","Mayo","Junio",
  "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"
)

View(tabla_indicadores_final)

# Finalmente, lo exportamos a Excel
write_xlsx(tabla_indicadores_final, "Data/outputs/tablas/indicadores_mercado_laboral_2025.xlsx")


# ---------- 2.2. Análisis de las tasas de mercado laboral ----------

# Vamos a crear un gráfico de serie de tiempo en el que se muestra el 
# comportamiento de la TGP, TO y TD entre enero-diciembre 2025 para el total
# nacional.

# Para ello, primero seleccionamos la data relevante del framework antes
# creado. Es importante mencionar que vamos a trabajar con el framework no
# "formateado". Es decir, aquel que tiene los meses como observaciones. Con ello
# aseguramos poder graficar como serie de tiempo los datos.

tasas_grafico_22 <- tabla_indicadores %>%
  select(mes, TGP, TO, TD) %>%
  mutate(mes = factor(mes,
                      levels = 1:12,
                      labels = c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                                 "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")))

colnames(tasas_grafico_22) <- c( "mes", "Tasa Global de Participación (TGP)",
        "Tasa de Ocupación (TO)", "Tasa de Desempleo (TD)")

View(tasas_grafico_22)

# Ahora, hacemos el gráfico a doble eje:
p_doble_eje <- ggplot(tabla_indicadores, aes(x = mes)) +
  
  # Primera línea: TGP
  geom_line(aes(y = TGP, color = "TGP"), linewidth = 1.5) +
  geom_point(aes(y = TGP, color = "TGP"), size = 3) +
  
  # Segunda línea: TO
  geom_line(aes(y = TO, color = "TO"), linewidth = 1.5) +
  geom_point(aes(y = TO, color = "TO"), size = 3) +
  
  # Tercera línea: TD (reescalada)
  geom_line(aes(y = (TD - min(TD)) / (max(TD) - min(TD)) *
      (max(TGP) - min(TGP)) + min(TGP),
    color = "TD"), linewidth = 1.5) +
  geom_point(aes(y = (TD - min(TD)) / (max(TD) - min(TD)) *
      (max(TGP) - min(TGP)) + min(TGP),
    color = "TD"), size = 3) +
  
  # Configurar escalas
  scale_y_continuous(
    name = "TGP y TO (%)",
    sec.axis = sec_axis(
      ~ (. - min(tabla_indicadores$TGP)) *
        (max(tabla_indicadores$TD) - min(tabla_indicadores$TD)) /
        (max(tabla_indicadores$TGP) - min(tabla_indicadores$TGP)) +
        min(tabla_indicadores$TD),
      name = "TD (%)"
    )
  ) +
  
  scale_x_continuous(
    breaks = 1:12,
    labels = c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
               "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  ) +
  
  scale_color_manual(
    values = c(
      "TGP" = "darkgreen",
      "TO" = "darkblue",
      "TD" = "darkred"
    )
  ) +
  
  labs(
    title = "Colombia: Indicadores del mercado laboral (2025)",
    subtitle = "Tasa Global de Participación (TGP), Ocupación (TO) y Desempleo (TD)",
    x = "Mes",
    color = "Indicador",
    caption = "Fuente: GEIH 2025 - DANE"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.left = element_text(color = "darkgreen", size = 11),
    axis.title.y.right = element_text(color = "darkred", size = 11),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray50")
  )

p_doble_eje

# Finalmente, guardamos la imagen en formato PNG
ggsave(
  filename = "Data/outputs/graficos/grafico_22_TGP_TO_TD.png",
  plot = p_doble_eje, #Gráfico a guardar
  width = 10, #Tamaño
  height = 6,
  dpi = 300,
  units = "in" #Unidades del tamaño
)

# El análisis de la tabla está en el PDF adjunto :)

# ---------- 2.3. Población ocupada según ramas de actividad ----------

# Vamos a crear un gráfico de barras horizontales del número de personas 
# ocupadas según ramas de actividad económica para el mes de abril 2025 
# para el total agregado de las 13 ciudades y áreas metropolitanas que agrupa 
# la encuesta.

# Para ello, primero filtramos en la base GEIH 2025 que construimos antes
# los ocupados del mes de abril

abril_ocupados <- base_final %>%
  filter(mes == 4, OCI == 1)
View(abril_ocupados)

# Luego, realizamos la construcción de la variable de rama de actividad a
# un dígito usando la clasificación a dos dígitos (variable RAMA2D_R4):

abril_ocupados <- abril_ocupados %>%
  mutate(rama = case_when(
    
    RAMA2D_R4 == "1" | RAMA2D_R4 == '01' | RAMA2D_R4 == '02' | RAMA2D_R4 == '2' |
      RAMA2D_R4 == '3' | RAMA2D_R4 == '03' ~ "Agricultura, ganadería, caza, silvicultura y pesca",
    
    RAMA2D_R4 == "05" | RAMA2D_R4 == '5' | RAMA2D_R4 == '06' |
      RAMA2D_R4 == '6'  | RAMA2D_R4 == '07' | RAMA2D_R4 == '7' |
      RAMA2D_R4 == '08' | RAMA2D_R4 == '8' |
      RAMA2D_R4 == '9' | RAMA2D_R4 == '09' ~
      "Explotación de minas y canteras",
    
    RAMA2D_R4 >= '10' & RAMA2D_R4 <= '33' ~  "Industrias manufactureras",
    
    RAMA2D_R4 == "35" | RAMA2D_R4 == '36' | RAMA2D_R4 == '37' | 
      RAMA2D_R4 == '38' | RAMA2D_R4 == '39' ~ 
      "Suministro de electricidad, gas, agua y gestión de desechos",
    
    RAMA2D_R4 == "41" | RAMA2D_R4 == '42' | RAMA2D_R4 == '43' ~ "Construcción",
    
    RAMA2D_R4 == "45" | RAMA2D_R4 == '46' | RAMA2D_R4 == '47' ~
      "Comercio y reparación de vehículos",
    
    RAMA2D_R4 == "55" | RAMA2D_R4 == '56' ~ "Alojamiento y servicios de comida",
    
    RAMA2D_R4 == "49" | RAMA2D_R4 == '50' | RAMA2D_R4 == '51' | 
      RAMA2D_R4 == '52' | RAMA2D_R4 == '53' ~ "Transporte y almacenamiento",
    
    RAMA2D_R4 >= '58' & RAMA2D_R4 <= '63' ~ "Información y comunicaciones",
    
    RAMA2D_R4 == "64" | RAMA2D_R4 == '65' | RAMA2D_R4 == '66' ~ 
      "Actividades financieras y de seguros",
    
    RAMA2D_R4 == '68' ~ 
      "Actividades inmobiliarias",
    
    RAMA2D_R4 >= '69' & RAMA2D_R4 <= '82' ~ 
      "Actividades profesionales, científicas, técnicas y de servicios administrativos",
    
    RAMA2D_R4 >= '84' & RAMA2D_R4 <= '88' ~
      "Administración pública y defensa, educación y atención de la salud humana",
    
    RAMA2D_R4 >= '90' & RAMA2D_R4 <= '99' ~ 
      "Actividades artísticas, entretenimiento, recreación y otras actividades de servicios"
    
  ))

# Ahora, creamos un nuevo dataframe con el total expandido (aplicando el factor 
# de expansión) del número de personas ocupadas por rama de actividad
ocupados_rama <- abril_ocupados %>%
  group_by(rama) %>%
  summarise(
    # Sumamos por el factor de expansión porque eso nos dice en la población
    # cuantás personas hay representadas por cada rama
    ocupados = sum(FEX_C18, na.rm = TRUE) 
  ) %>%
  arrange(ocupados)
View(ocupados_rama)

# Graficamos:
grafico_ramas_23 <- ggplot(ocupados_rama, aes(x = ocupados, y = reorder(rama, ocupados))) +
  
  geom_bar(stat = "identity", fill = "steelblue") +
  
  geom_text(aes(label = scales::comma(round(ocupados,0))),
            hjust = -0.1,
            size = 3.5) +
  
  scale_x_continuous(
    labels = scales::label_number(scale = 1e-6, suffix = " M")
  ) +
  
  labs(
    title = "Personas ocupadas por rama de actividad económica",
    subtitle = "Total 13 ciudades y áreas metropolitanas - Abril 2025",
    x = "Número de personas ocupadas (en millones)",
    y = "Rama de actividad económica",
    caption = "Fuente: GEIH 2025 - DANE"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

grafico_ramas_23

# Finalmente, guardamos la imagen en formato PNG
ggsave(
  filename = "Data/outputs/graficos/grafico_23_N_personas_ocupadas_rama.png",
  plot = grafico_ramas_23, #Gráfico a guardar
  width = 10, #Tamaño
  height = 6,
  dpi = 300,
  units = "in" #Unidades del tamaño
)


# ---------- 2.4. Población ocupada según posición ocupacional ----------

# Vamos a crear un gráfico de torta (pie) con la distribución de personas 
# ocupadas según posición ocupacional para el mes de diciembre 2025 para el 
# total nacional.

# Para ello, primero filtramos en la base GEIH 2025 que construimos antes
# los ocupados del mes de diciembre
diciembre_ocupados <- base_final %>%
  filter(mes == 12, OCI == 1)

# Ahora, creamos el dataframe con ocupados expandidos por el factor de expansión
# por posición ocupacional y creamos la proporción de ocupados
posicion_ocupacional <- diciembre_ocupados %>%
  group_by(tipo_empl) %>%
  summarise(
    ocupados = sum(FEX_C18, na.rm = TRUE)
  ) %>%
  mutate(
    proporcion = ocupados / sum(ocupados)
  ) %>%
  # Ordenamos alfabéticamente por posición ocupacional
  arrange(tipo_empl)

View(posicion_ocupacional)

# Luego, graficamos (a saber, un gráfico de pie es igual a un gráfico de barras
# pero en coordenadas polares):
grafico_pie_24 <- ggplot(posicion_ocupacional,
                      aes(x = "", y = proporcion, fill = tipo_empl)) +
  
  geom_bar(stat = "identity", width = 1) +
  
  coord_polar("y") +
  
  geom_text(
    aes(label = ifelse(proporcion >= 0.03,
                       scales::percent(proporcion, accuracy = 0.1),
                       "")),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  
  labs(
    title = "Distribución de personas ocupadas según posición ocupacional",
    subtitle = "Total nacional - Diciembre 2025",
    fill = "Posición ocupacional",
    caption = "Fuente: GEIH 2025 - DANE",
    y = "Proporción de ocupados"
  ) +
  
  theme_minimal() +
  
  scale_fill_brewer(palette = "Dark2")+
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )
grafico_pie_24

# Finalmente, guardamos la imagen en formato PNG
ggsave(
  filename = "Data/outputs/graficos/grafico_24_personas_ocupadas_posicion.png",
  plot = grafico_pie_24, #Gráfico a guardar
  width = 10, #Tamaño
  height = 6,
  dpi = 300,
  units = "in" #Unidades del tamaño
)

