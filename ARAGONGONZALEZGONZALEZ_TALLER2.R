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
                       "writexl", "tidyr", "ggrepel", "geodata", "scales",
                      "terra", "sf", "patchwork")

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
path_taller <- paste0("C:/Users/", username, "/OneDrive/Documents/GitHub/MEcA/Taller R/Taller2/Data")

# Establecer directorio de trabajo
setwd(path_taller)

# -----------------------------------------------------------------------------
# Punto 1. Conformación y análisis base de datos
# -----------------------------------------------------------------------------

# ---------- 1.1. Pegue de las bases de datos ----------

bases <- c("Fuerza de trabajo.csv","Ocupados.csv","No ocupados.csv")

# Agosto y diciembre tienen bases con nombres diferentes (más espacios). Por ello,
# las definimos para que R pueda leerlas correctamente.
bases_excep <- c("/Características generales, seguridad social en salud y  educación.csv",
                 "Fuerza de trabajo.csv")
lista_bases <- list()
descrip <- data.frame(mes = 0, base = "",nobs = 0,nvar = 0)

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
  if (n !=8){
    base_main <- read_csv2(paste0("GEIH_2025/",n,"/Características generales, seguridad social en salud y educación.csv"),
                                locale = locale(encoding = "latin1"), col_types = cols(.default = "c"))

    descrip <- bind_rows(descrip,data.frame(mes = n, base = "CAT",
                                            nobs = nrow(base_main),nvar = ncol(base_main)))
  
  } else{
    base_main <- read_csv2(paste0("GEIH_2025/",n,bases_excep[1]),
                           locale = locale(encoding = "latin1"), col_types = cols(.default = "c"))
    descrip <- bind_rows(descrip,data.frame(mes = n, base = "CAT",
                                            nobs = nrow(base_main),nvar = ncol(base_main)))
    
  }
  # Luego, creamos una llave que será con la que pegaremos las demás bases.
  # Esta llave es la información que caracteriza a una persona (ORDEN) en un 
  # hogar (SECUENCIA_P) dentro de una vivienda (DIRECTORIO)
  base_main <- mutate(base_main,
                  LLAVE = paste0(DIRECTORIO, SECUENCIA_P, ORDEN))
  
  # Ahora creamos otro loop que haga para cada una de las bases a pegar (es
  # decir para las bases 2, 3 y 4):
  for (i in 1:length(bases)){
    
    # Lea la base del mes
    if ((n == 12) & (i==1)){
      base_join <- read_csv2(paste0("GEIH_2025/",n,"/",bases_excep[2]),
                             locale = locale(encoding = "latin1"), col_types = cols(.default = "c"))

      #  Agregue una línea más con la información descriptiva de esa base al data frame
      # de "estadísticas descritivas"
      descrip <- bind_rows(descrip,data.frame(mes = n, base = bases[i],
                                              nobs = nrow(base_join),nvar = ncol(base_join)))
    } else{
      base_join <- read_csv2(paste0("GEIH_2025/",n,"/",bases[i]),
                             locale = locale(encoding = "latin1"), col_types = cols(.default = "c"))
      descrip <- bind_rows(descrip,data.frame(mes = n, base = bases[i],
                                              nobs = nrow(base_join),nvar = ncol(base_join)))
    }
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

base_total <- bind_rows(lista_bases)

# ---------- 1.2. Descripción general de los datos ----------

# Completamos la tabla con la descripción de los datos y la exportamos a Excel

descrip <- bind_rows(descrip,data.frame(mes = 2025, base = "Total",
                                        nobs = nrow(base_total),nvar = ncol(base_total)))

# Cambiamos los nombres de los meses para una mejor interpretación
descrip_final <- descrip 

descrip_final$mes <- c(
  "0",
  "Enero", "Enero", "Enero", "Enero",
  "Febrero", "Febrero", "Febrero", "Febrero",
  "Marzo", "Marzo", "Marzo", "Marzo",
  "Abril", "Abril", "Abril", "Abril",
  "Mayo", "Mayo", "Mayo", "Mayo",
  "Junio", "Junio", "Junio", "Junio",
  "Julio", "Julio", "Julio", "Julio",
  "Agosto", "Agosto", "Agosto", "Agosto",
  "Septiembre", "Septiembre", "Septiembre", "Septiembre",
  "Octubre", "Octubre", "Octubre", "Octubre",
  "Noviembre", "Noviembre", "Noviembre", "Noviembre",
  "Diciembre", "Diciembre", "Diciembre", "Diciembre",
  "Total Año 2025"
)

write_xlsx(descrip_final, "outputs/tablas/descripcionGEIH.xlsx")

# Realizamos un par de gráficos para ilustrar lo encontrado
# Filtro la base de categorías generales
meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
              "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

resumen_cat <- descrip %>% 
  filter(base=="CAT") %>% 
  mutate(mes = factor(meses[mes], levels = meses))

# Gráfico categorías generales
grafico_obscg_12 <- ggplot(resumen_cat, aes(x = mes, y = nobs)) +
  geom_bar(stat = "identity", width=0.5, fill="darkblue") +
  labs(
    title = "Observaciones por mes - Características Generales",
    x = "Mes",
    y = "Número de observaciones",
    caption = "Fuente: DANE – GEIH 2025"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
    )

grafico_obscg_12

# Guardamos la imagen en formato PNG
ggsave(
  filename = "outputs/graficos/grafico_12_obscg.png",
  plot = grafico_obscg_12, #Gráfico a guardar
  width = 10, #Tamaño
  height = 6,
  dpi = 300,
  units = "in" #Unidades del tamaño
)

# Filtro la base de fuerza de trabajo
resumen_ft <- descrip %>% 
  filter(base %in% c("Ocupados.csv","No ocupados.csv")) %>% 
  mutate(mes = factor(meses[mes], levels = meses))

# Gráfico fuerza de trabajo
grafico_obsft_12 <- ggplot(resumen_ft, aes(x = mes, y = nobs, fill=base)) +
  geom_bar(stat = "identity", width=0.5) +
  labs(
    title = "Observaciones por mes - Fuerza de Trabajo",
    x = "Mes",
    y = "Número de observaciones",
    caption = "Fuente: DANE – GEIH 2025"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

grafico_obsft_12

# Guardamos la imagen en formato PNG
ggsave(
  filename = "outputs/graficos/grafico_12_obsft.png",
  plot = grafico_obsft_12, #Gráfico a guardar
  width = 10, #Tamaño
  height = 6,
  dpi = 300,
  units = "in" #Unidades del tamaño
)


# ---------- 1.3. Selección de las variables relevantes para el análisis ----------

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
base_final$PERIODO <- as.Date(base_final$PERIODO)

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

# Vamos a construir una tabla con los principales indicadores de mercado laboral 
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
    # Convertimos los NA en 0 para asegurarnos de que no haya errores de cálculo
    ocupados = sum(OCI * FEX_C18, na.rm = TRUE),
    desocupados = sum(DSI * FEX_C18, na.rm = TRUE)) %>%
  
  # Luego calculamos los indicadores
  mutate(
    TGP = round((FT / PET) * 100,2),
    TO = round((ocupados / PET) * 100,2),
    TD = round((desocupados / FT) * 100,2)) %>%
  
  # Dejamos sólo la información que se pide en la tabla
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
  "Enero\n2025","Febrero\n2025","Marzo\n2025","Abril\n2025","Mayo\n2025","Junio\n2025",
  "Julio\n2025","Agosto\n2025","Septiembre\n2025","Octubre\n2025","Noviembre\n2025","Diciembre\n2025"
)

View(tabla_indicadores_final)

# Finalmente, lo exportamos a Excel
write_xlsx(tabla_indicadores_final, "outputs/tablas/indicadores_mercado_laboral_2025.xlsx")


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
  filename = "outputs/graficos/grafico_22_TGP_TO_TD.png",
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
  
  geom_bar(stat = "identity", fill = "darkblue") +
  
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
  filename = "outputs/graficos/grafico_23_N_personas_ocupadas_rama.png",
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

posicion_ocupacional <- diciembre_ocupados %>%
  mutate(tipo_empl = as.character(tipo_empl)) %>% 
  group_by(tipo_empl) %>%
  summarise(
    ocupados = sum(FEX_C18, na.rm = TRUE)
  ) %>%
  mutate(
    proporcion = ocupados / sum(ocupados)
  ) %>%
  arrange(tipo_empl)

View(posicion_ocupacional)

# Luego, graficamos (a saber, un gráfico de pie es igual a un gráfico de barras
# pero en coordenadas polares):

# Vamos a crear una etiqueta con el porcentaje (Ej: Obrero o Empl. Empresa 
# Part (45.7%)) para poderla ubicar en la leyenda del gráfico

posicion_ocupacional <- posicion_ocupacional %>%
  arrange(tipo_empl) %>%
  mutate(
    etiqueta_leyenda = paste0(
      tipo_empl,
      " (",
      scales::percent(proporcion, accuracy = 0.1),
      ")"
    ),
    etiqueta_leyenda = factor(etiqueta_leyenda, levels = etiqueta_leyenda)
  )

# Y usando eso, creamos el gráfico de pie

grafico_pie_24 <- ggplot(posicion_ocupacional,
                      aes(x = "", y = proporcion, fill = etiqueta_leyenda)) +
  
  geom_bar(stat = "identity", width = 1) +
  
  coord_polar("y") +
  
  geom_text(
    aes(label = ifelse(proporcion >= 0.1,
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
  
  scale_fill_brewer(palette = "Spectral")+
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank() 
  )
grafico_pie_24


# Finalmente, guardamos la imagen en formato PNG
ggsave(
  filename = "outputs/graficos/grafico_24_personas_ocupadas_posicion.png",
  plot = grafico_pie_24, #Gráfico a guardar
  width = 10, #Tamaño
  height = 6,
  dpi = 300,
  units = "in" #Unidades del tamaño
)

# -----------------------------------------------------------------------------
# Punto 3. Análisis socioeconómico de la población colombiana 
# -----------------------------------------------------------------------------

# Volvemos al database completo para salvar variables adicionales necesarias para 
# un análisis de características más completo. 

# Salvamos: 
# 1. Características generales, seguridad social en salud y educación: 
#    Parentesco con el jefe del hogar (P6050)
#    Padre reside en el hogar (P6081)
#    Madre reside en el hogar (P6083)


# 2. Fuerza de trabajo:
#    Aunque desea trabajar, ¿por qué no hizo diligencias el último mes? (P6310)


# 3. No Ocupados y Ocupados:
#    Durante cuántas semanas ha estado o estuvo...... buscando trabajo? (P7250)
#    ¿Está ... cotizando actualmente a un fondo de pensiones? (P1519)
#    ¿Cuál es la razón principal por la que trabaja en forma independiente? (P1879)
#    Si le ofrecieran un empleo como asalariado, en el cuál ganara lo mismo que obtiene 
#    actualmente pero con prestaciones, ¿aceptaría? (P1805)
#    ¿qué hizo principalmente en el último mes para conseguir un trabajo o instalar un negocio? (P6290)
#    ¿Por qué medio principal, consiguió su empleo actual? (P3363)



# Disclaimer: es necesario crear la variable categórica de: 
# ¿qué hizo el último mes para conseguir un trabajo o instalar un negocio? (P6290)

base_nueva <- base_total %>%
  mutate(
    P6290 = case_when(
      P6280 == 1 & P3362S1 == 1 ~ 1,
      P6280 == 1 & P3362S2 == 1 ~ 2,
      P6280 == 1 & P3362S3 == 1 ~ 3,
      P6280 == 1 & P3362S4 == 1 ~ 4,
      P6280 == 1 & P3362S5 == 1 ~ 5,
      P6280 == 1 & P3362S6 == 1 ~ 6,
      P6280 == 1 & P3362S7 == 1 ~ 7,
      P6280 == 1 & P3362S8 == 1 ~ 8,
      TRUE ~ NA_real_
    )
  )


#Seleccionamos 
base_nueva <- select(base_nueva, PERIODO, DIRECTORIO, SECUENCIA_P, ORDEN, CLASE, 
                     P3271, P3042, P6090, P6040, P6160, DPTO, FEX_C18, PT,
                     PET, FT, FFT, P6240, OCI, RAMA2D_R4, P6430, INGLABO, DSI,
                     P3042, P6050, P6081, P6083, P6310, P7250, P1519, P1879, P1805, 
                     P6280, P6290, P3363)


# Ajustamos fechas: hacemos esto para que R reconozca los meses de observaciones
# con fechas (particulamente días) no convencionales como "20251252"
base_nueva$PERIODO <- as.Date(paste0(substr(base_nueva$PERIODO,1,6),"01"),"%Y%m%d")
base_nueva$mes <- month(base_nueva$PERIODO, label = FALSE)
base_nueva$dia <- day(base_nueva$PERIODO)
base_nueva$anio <- year(base_nueva$PERIODO)

# Convertimos todo a numérico
base_nueva <- base_nueva %>%
  mutate(across(everything(), as.numeric))

# Renombramos las variables para tener mejor manejo de la información
base_nueva <- base_nueva %>%
  rename(
    sexo = P3271,
    nivel_edu = P3042,
    eps = P6090,
    edad = P6040,
    leer_escr = P6160,
    act_ocup = P6240,
    tipo_empl = P6430,
    urbano_rural = CLASE, 
    jefe_hogar = P6050, 
    madre_hogar = P6081, 
    padre_hogar = P6083,
    tiempo_buscando = P7250, 
    formal = P1519,  
    razon_indep = P1879, 
    si_prestaciones = P1805,
    forma_buscar = P6290, 
    forma_encontrar = P3363, 
    no_busco_empleo = P6310
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
                                  "Jornalero o peón","Otro")),
    
    forma_buscar = factor(forma_buscar, levels = c(1,2,3,4,5,6,7,8), 
                          labels = c("Ayuda de conocidos", "Hojas de vida a empleadores", 
                                     "Agencias, servicios o bolsas de empleo", "Clasificados", 
                                     "Presentó a convocatorias", "Prep. inicio de negocio", 
                                     "Otros", "No sabe, no informa")), 
    
    forma_encontrar = factor(forma_encontrar, levels = c(1,2,3,4,5,6,9), 
                             labels = c("Ayuda de conocidos", "Hojas de vida a empleadores",
                                        "Agencias, servicios o bolsas de empleo", "Clasificados", 
                                        "Presentó a convocatorias", "Otros", "No sabe, no informa")), 
    
    urbano_rural = factor(urbano_rural, levels = c(1,2),
                          labels = c("Urbano", "Rural")),
    
    DPTO = factor(DPTO, levels = c(05,08,11,13,15,17,18,19,20,23,25,27,41,44,47,50,52,54,63,66,68,70,73,76),
                  labels = c("Antioquia","Atlántico","Bogotá, d.C.","Bolívar","Boyacá",
                             "Caldas","Caquetá","Cauca","Cesar","Córdoba","Cundinamarca","Chocó",
                             "Huila","La guajira","Magdalena","Meta","Nariño","Norte de santander",
                             "Quindio","Risaralda","Santander","Sucre","Tolima","Valle del cauca")),
    
    no_busco_empleo = factor(no_busco_empleo, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 
                             labels = c ("Ya encontró trabajo", 
                                         "No hay trabajo disponible en la ciudad ó región / no encuentra trabajo en su oficio o profesión", 
                                         "Está esperando que lo llamen o esperando temporada alta", 
                                         "No sabe como buscarlo", "Está cansado de buscar", 
                                         "Carece de la experiencia necesaria", 
                                         "No tiene recursos para instalar un negocio", 
                                         "Los empleadores lo consideran muy joven o muy viejo", 
                                         "Usted se considera muy joven o muy viejo", 
                                         "Responsabilidades familiares", "Problemas de salud", 
                                         "Está estudiando", "Otro"))
  )

# Vamos a graficar

# ------------------------------- Gráfico 1: -----------------------------------
# Panorama por género sobre los retornos a la educación en el mercado laboral colombiano 

# Tipo de gráfico: 
# - Bubble plot

# Conjunto de variables utilizadas:
# - Nivel educativo
# - Sexo
# - Urbano - rural 
# - Ingresos laborales
# - Número de ocupados


# Filtramos por ocupados y agrupamos por nivel educativo, género y clase (urbano-rural)

bubble_data <- base_nueva %>%
  filter(OCI == 1) %>% 
  group_by(nivel_edu, sexo, urbano_rural) %>%
  summarise(
    ingreso_prom = weighted.mean(INGLABO, FEX_C18, na.rm = TRUE),
    ocupados = sum(FEX_C18, na.rm = TRUE)
  ) %>%
  filter(ingreso_prom <= 20000000)%>%
  ungroup() 


# Exploramos resultados a un nivel más amplio de agregación --------------------

# %>%
#mutate(
#  nivel_edu_grupo = case_when(
#    nivel_edu %in% c("Ninguno","Preescolar") ~ "Sin educación",
#    nivel_edu == "Básica Primaria" ~ "Primaria",
#    nivel_edu %in% c("Básica Secundaria","Media Académica","Media Técnica") ~ "Secundaria",
#    nivel_edu %in% c("Técnico Profesional","Tecnológica","Normalista") ~ "Técnica",
#    nivel_edu %in% c("Universitaria","Especialización","Maestría","Doctorado") ~ "Universitaria+",
#    TRUE ~ NA_character_
#  )
# )
#----------------------------------------------------------------------------------


# Graficamos un Bubble Plot con: 
## Educación, ingreso, género (color), número de trabajadores (tamano de burbuja), facetas: zona urbano / rural

grafico_bubbleplot_31 <- ggplot(bubble_data,
                                aes(x = nivel_edu,
                                    y = ingreso_prom,
                                    size = ocupados,
                                    color = sexo)) +
  
  geom_point(alpha = 0.7,
             position = position_dodge(width = 0.4)) +
  
  geom_hline(yintercept = 1423500,
             linetype = "dashed",
             color = "red",
             linewidth = 0.8) +
  
  facet_wrap(~ urbano_rural, nrow = 1) +
  
  scale_size(
    range = c(3,15),
    labels = scales::label_number(big.mark = ".")
  ) +
  
  scale_y_continuous(
    labels = scales::label_number(big.mark = "."),
    expand = expansion(mult = c(0.1,0.05))
  ) +
  
  scale_color_manual(
    values = c(
      "Mujer" = "#F4A6A6",
      "Hombre" = "#8EC6FF"
    )
  ) +
  
  labs(
    title = "Panorama por género sobre los retornos a la educación en el mercado laboral colombiano",
    subtitle = "Promedio anual",
    x = "Nivel educativo",
    y = "Ingreso laboral promedio mensual (COP)",
    size = "Número de trabajadores",
    color = "Género",
    caption = "Fuente: GEIH 2025 - DANE"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.title = element_text(face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

grafico_bubbleplot_31


# Finalmente, guardamos la imagen en formato PNG
ggsave(
  filename = "outputs/graficos/grafico_31_retornor_educacion_genero.png",
  plot = grafico_bubbleplot_31, #Gráfico a guardar
  width = 10, #Tamaño
  height = 6,
  dpi = 300,
  units = "in" #Unidades del tamaño
)



# ------------------------------- Gráfico 2: -----------------------------------
# ----   Participación femenina y diferencias de ingresos por sector -----------

# Tipo de gráfico: 
# - Scatter plot


# Conjunto de variables utilizadas:
# - Ramas de actividad
# - Sexo
# - Ingresos laborales



# Ajustamos ramas de actividad

base_segregacion <- base_nueva %>%
  mutate(rama = case_when( #Hay que hacer los dos tipos de casos para los 1 dígito con cero a la izquierda.
    
    RAMA2D_R4 == "1" | RAMA2D_R4 == '01' | RAMA2D_R4 == '02' | RAMA2D_R4 == '2' |
      RAMA2D_R4 == '3' | RAMA2D_R4 == '03' ~ "Agro",
    
    RAMA2D_R4 == "05" | RAMA2D_R4 == '5' | RAMA2D_R4 == '06' |
      RAMA2D_R4 == '6'  | RAMA2D_R4 == '07' | RAMA2D_R4 == '7' |
      RAMA2D_R4 == '08' | RAMA2D_R4 == '8' |
      RAMA2D_R4 == '9' | RAMA2D_R4 == '09' ~
      "Minería",
    
    RAMA2D_R4 >= '10' & RAMA2D_R4 <= '33' ~  "Manufactura",
    
    RAMA2D_R4 == "35" | RAMA2D_R4 == '36' | RAMA2D_R4 == '37' | 
      RAMA2D_R4 == '38' | RAMA2D_R4 == '39' ~ 
      "Electro, gas y agua",
    
    RAMA2D_R4 == "41" | RAMA2D_R4 == '42' | RAMA2D_R4 == '43' ~ "Construcción",
    
    RAMA2D_R4 == "45" | RAMA2D_R4 == '46' | RAMA2D_R4 == '47' ~
      "Vehículos",
    
    RAMA2D_R4 == "55" | RAMA2D_R4 == '56' ~ "Alojamiento",
    
    RAMA2D_R4 == "49" | RAMA2D_R4 == '50' | RAMA2D_R4 == '51' | 
      RAMA2D_R4 == '52' | RAMA2D_R4 == '53' ~ "Transporte",
    
    RAMA2D_R4 >= '58' & RAMA2D_R4 <= '63' ~ "Información",
    
    RAMA2D_R4 == "64" | RAMA2D_R4 == '65' | RAMA2D_R4 == '66' ~ 
      "Finanaciera",
    
    RAMA2D_R4 == '68' ~ 
      "Inmobiliaria",
    
    RAMA2D_R4 >= '69' & RAMA2D_R4 <= '82' ~ 
      "Profesionales",
    
    RAMA2D_R4 >= '84' & RAMA2D_R4 <= '88' ~
      "Sector público, eduación y salud",
    
    RAMA2D_R4 >= '90' & RAMA2D_R4 <= '99' ~ 
      "Arte y recreación"
    
  ))



# Generamos la base por rama con la información del ingreso promedio de las mujeres, el ingreso
# promedio general y la proporción de mujeres. 

base_segregacion <- base_segregacion %>%
  group_by(rama) %>%
  summarise(
    ingreso_mujeres = weighted.mean(INGLABO[sexo == "Mujer"], FEX_C18[sexo == "Mujer"], na.rm = TRUE),
    ingreso_prom = weighted.mean(INGLABO, FEX_C18, na.rm = TRUE),
    porc_mujeres = sum(FEX_C18[sexo == "Mujer"], na.rm = TRUE) / sum(FEX_C18, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(porc_mujeres), !is.na(ingreso_mujeres), !is.na(ingreso_prom))  # eliminar NA


# Formato largo (eso nos permite tener una observación para mujeres y el total, así graficamos 
# las diferencias)

base_grafico_long <- base_segregacion %>%
  filter(!is.na(porc_mujeres), !is.na(ingreso_mujeres), !is.na(ingreso_prom)) %>%
  select(rama, porc_mujeres, ingreso_prom, ingreso_mujeres) %>%
  tidyr::pivot_longer(cols = c(ingreso_prom, ingreso_mujeres),
                      names_to = "tipo_ingreso",
                      values_to = "ingreso") %>%
  mutate(tipo_ingreso = ifelse(tipo_ingreso == "ingreso_prom", "Total", "Mujeres"))


# Gráficamos
grafico_part_fem <- ggplot(base_grafico_long, aes(x = porc_mujeres, y = ingreso, color = rama)) +
  geom_point(aes(shape = tipo_ingreso, fill = tipo_ingreso), size = 4, stroke = 1.2) +
  geom_line(aes(group = rama), linetype = "dashed", color = "gray70") +  # línea para guiar visual
  geom_text_repel(data = subset(base_grafico_long, tipo_ingreso == "Mujeres"),
                  aes(label = rama),
                  size = 3.5,
                  max.overlaps = 20,
                  nudge_y = 0.05 * max(base_grafico_long$ingreso, na.rm = TRUE)) +
  
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +  
  
  scale_shape_manual(values = c("Total" = 21, "Mujeres" = 16)) +  # 21 = círculo con borde, 16 = sólido
  scale_fill_manual(values = c("Total" = "white", "Mujeres" = "pink")) +
  labs(
    x = "Proporción de mujeres en el sector",
    y = "Ingreso promedio (millones COP)",
    color = "Sector económico",
    shape = "Ingreso",
    fill = "Ingreso",
    title = "Ingreso promedio vs. ingreso de mujeres por sector según participación femenina",
    subtitle = "Nivel nacional",
    caption = "Fuente: GEIH 2025 - DANE"
  ) +
  theme_minimal()+ 
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank())

grafico_part_fem

# Finalmente, guardamos la imagen en formato PNG
ggsave(
  filename = "outputs/graficos/grafico_32_ingresos_y_participacion_femenina.png",
  plot = grafico_part_fem, #Gráfico a guardar
  width = 10, #Tamaño
  height = 6,
  dpi = 300,
  units = "in" #Unidades del tamaño
)



# ------------------------------- Gráfico 3: -----------------------------------
# -------------  Autoempleo como refugio ante el desempleo --------------------

#Hipótesis: una parte del autoempleo es un segmento de refugio ante la falta de empleo formal

# Tipo de gráfico: 
# - Mapa de calor


# Conjunto de variables utilizadas:
# - Ocupación
# - Razón del autoempleo
# - Si dejaría el autoempleo por una trabajo formal 
# - Departamento




# Descargarmos fronteras de Colombia a nivel de departamento (shapefiles)
colombia <- gadm("COL", level = 1, path = tempdir()) # level = 1 -> departamentos
# Convertir a sf 
colombia_sf <- st_as_sf(colombia)


# Consideramos personas que están en autoempleo por falta de empleo formal (no consiguieron pero lo quisieran)
base_autoempleo <- base_nueva %>%
  filter(OCI == 1) %>%
  mutate(autoempleo_refugio = ifelse(razon_indep == 1 | si_prestaciones == 1, 1, 0))


# Calculamos la proporción de ocupados en cada departamento que están en autoempleo de refugio
autoempleo_dpto <- base_autoempleo %>%
  group_by(DPTO) %>%
  summarise(
    total_ocupados = sum(FEX_C18, na.rm = TRUE),  # población total ocupada
    n_autoempleo_refugio = sum(FEX_C18 * autoempleo_refugio, na.rm = TRUE),  # población en autoempleo refugio
    pct_autoempleo_refugio = n_autoempleo_refugio / total_ocupados,  # proporción ponderada
    .groups = "drop"
  )

# Verificamos que todos los nombres de deptos coindidan
setdiff(unique(base_autoempleo$DPTO), colombia_sf$NAME_1)
# Corregimos:
base_autoempleo <- base_autoempleo %>%
  filter(!is.na(DPTO))%>%
  mutate(
    DPTO = case_when(
      DPTO == "Quindio" ~ "Quindío",
      DPTO == "Bogotá, d.C." ~ "Bogotá D.C.",
      DPTO == "La guajira" ~ "La Guajira",
      DPTO == "Norte de santander" ~ "Norte de Santander",
      DPTO == "Valle del cauca" ~ "Valle del Cauca",
      TRUE ~ DPTO
    )
  )


# Unimos el mapa con la base de autoempleo
colombia_map <- colombia_sf %>%
  left_join(autoempleo_dpto, by = c("NAME_1" = "DPTO"))

# Graficamos el mapa
autoempleo_anual <- ggplot(colombia_map) +
  geom_sf(aes(fill = pct_autoempleo_refugio), color = "white") +
  scale_fill_viridis_c(option = "plasma", labels = percent_format(accuracy = 1)) +
  labs(
    fill = "% autoempleo como refugio",
    title = "Autoempleo como refugio frente a la falta de empleo formal",
    subtitle = "Nivel departamental",
    caption = "Fuente: GEIH 2025 - DANE"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

autoempleo_anual

autoempleo_sector <- base_autoempleo %>%
  filter(autoempleo_refugio == 1)  



#---------------------- Comparación trimestral ---------------------------------

# Creamos división trimestral
base_autoempleo_t <- base_autoempleo %>%
  mutate(
    TRIMESTRE = case_when(
      mes %in% 1:3 ~ "Ene-Mar",
      mes %in% 4:6 ~ "Abr-Jun",
      mes %in% 7:9 ~ "Jul-Sep",
      mes %in% 10:12 ~ "Oct-Dic"
    )
  )


# Calculamos las proporciones ponderadas de autoempleo como refugio

autoempleo_trimestre <- base_autoempleo_t %>%
  group_by(TRIMESTRE, DPTO) %>%
  summarise(
    total_ocupados = sum(FEX_C18, na.rm = TRUE),
    n_autoempleo_refugio = sum(FEX_C18 * autoempleo_refugio, na.rm = TRUE),
    pct_autoempleo_refugio = n_autoempleo_refugio / total_ocupados,
    .groups = "drop"
  )


# Descargamos el shapefile de departamentos
colombia <- gadm("COL", level = 1, path = tempdir())
colombia_sf <- st_as_sf(colombia)

# Creamos combinación completa de departamentos y trimestres
trimestres <- c("Ene-Mar", "Abr-Jun", "Jul-Sep", "Oct-Dic")
deptos <- colombia_sf$NAME_1
completa <- expand.grid(DPTO = deptos, TRIMESTRE = trimestres, stringsAsFactors = FALSE)


# Unimos con los datos reales de autoempleo
autoempleo_trimestre_completa <- completa %>%
  left_join(autoempleo_trimestre, by = c("DPTO", "TRIMESTRE")) %>%
  mutate(
    TRIMESTRE = factor(TRIMESTRE, levels = trimestres)
  )

# Unimos con el mapa
colombia_map_t <- colombia_sf %>%
  left_join(autoempleo_trimestre_completa, by = c("NAME_1" = "DPTO"))



# Graficamos facetados por trimestre
autoempleo_refugio_trim <- ggplot(colombia_map_t) +
  geom_sf(aes(fill = pct_autoempleo_refugio), color = "black", size = 0.3) +  # bordes siempre visibles
  scale_fill_viridis_c(
    option = "plasma",
    labels = percent_format(accuracy = 1),
    na.value = "gray95"  # departamentos sin datos en gris claro
  ) +
  facet_wrap(~TRIMESTRE) +  # facetas ordenadas
  labs(
    fill = "% Autoempleado ante el desempleo",
    title = "Evolución trimestral del autoempleo como refugio frente a la falta de empleo formal",
    subtitle = "Nivel departamental",
    caption = "Fuente: GEIH 2025 - DANE"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",       # movemos la leyenda
    plot.title = element_text(hjust = 0.5),   # título centrado
    plot.subtitle = element_text(hjust = 0.5)
  )

autoempleo_refugio_trim


#Creamos una tabla del total de autoempleo

autoempleo_totales <- autoempleo_trimestre_completa %>%
  group_by(DPTO) %>%
  summarise(
    total_autoempleo = sum(n_autoempleo_refugio, na.rm = TRUE)
  )

# Construimos un gráfico de barras 

barras_autoemp_total <- autoempleo_totales %>%
  arrange(total_autoempleo) %>%
  ggplot(aes(x = reorder(DPTO, total_autoempleo), y = total_autoempleo)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Departamento",
    y = "Total de personas en autoempleo",
    title = "Total de personas en autoempleo por departamento",
    caption = "Fuente: GEIH 2025 - DANE"
  ) +
  theme_minimal()

barras_autoemp_total

# Combinamos gráficas 

autoempleo_2 <- autoempleo_refugio_trim + barras_total + 
  plot_layout(ncol = 2, widths = c(2, 1)) & 
  theme(plot.margin = margin(10, 10, 10, 10))

autoempleo_2

# Finalmente, guardamos las imagenes en formato PNG
ggsave(
  filename = "outputs/graficos/grafico_33_autoempleo_como_refugio.png",
  plot = autoempleo_refugio_trim, #Gráfico a guardar
  width = 10, #Tamaño
  height = 6,
  dpi = 300,
  units = "in" #Unidades del tamaño
)


ggsave(
  filename = "outputs/graficos/grafico_33.2_autoempleo_map+barras.png",
  plot = autoempleo_2,
  width = 16,      # un poco más ancho para balancear mapa+barras
  height = 7,      # altura suficiente para las facetas
  dpi = 300,
  units = "in"
)




# ------------------------------- Gráfico 4: -----------------------------------
# ------------   Impedimentos a la búsqueda de empleo por género ---------------

# Tipo de gráfico: 
# - Geom_point


# Conjunto de variables utilizadas:
# - Razones por las cuáles no buscó empleo
# - Sexo



# Creamos una tabla resumen con porcentaje ponderado
motivos_no_busqueda <- base_nueva %>%
  filter(!is.na(no_busco_empleo)) %>%  # omitimos NA
  group_by(no_busco_empleo, sexo) %>%
  summarise(
    total_personas = sum(FEX_C18, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(sexo) %>%
  mutate(
    pct_personas = total_personas / sum(total_personas)  # porcentaje dentro de cada sexo
  )

# Calculamos el total de respondientes
totales_sexo <- motivos_no_busqueda %>%
  group_by(sexo) %>%
  summarise(
    total = sum(total_personas, na.rm = TRUE),
    .groups = "drop"
  )

totales_sexo


# Creamos etiquetas de leyenda con esos totales 

labels_fill <- paste0(totales_sexo$sexo, " (", scales::comma(totales_sexo$total), ")")
names(labels_fill) <- totales_sexo$sexo


# Graficamos
no_busco_empleo <- ggplot(motivos_no_busqueda, aes(x = pct_personas, y = reorder(no_busco_empleo, pct_personas), color = sexo)) +
  geom_point(size = 3) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Hombre" = "#8EC6FF", "Mujer" = "#F4A6A6"),
                     labels = labels_fill  # leyenda con total solo de quienes respondieron
  ) +
  labs(
    x = "Porcentaje de personas",
    y = "Motivo por no buscar empleo",
    color = "Sexo",
    title = "Motivos por los que no buscó empleo el último mes",
    subtitle = "Población no ocupada - Nivel Nacional",
    caption = "Fuente: GEIH 2025 - DANE"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )

no_busco_empleo

# Finalmente, guardamos la imagen en formato PNG
ggsave(
  filename = "outputs/graficos/grafico_34_no_busco_empleo.png",
  plot = no_busco_empleo, #Gráfico a guardar
  width = 10, #Tamaño
  height = 6,
  dpi = 300,
  units = "in" #Unidades del tamaño
)



# ----------------------- Gráfico 5: -------------------------------------------
# ------------- Tiempo buscando empleo y Maneras de Buscar ---------------------


# 1. Tiempo buscando por departamento

# Podemos hacer un mapa para evaluar en qué depto es más difícil conseguir empleo
mapa_busqueda <- base_nueva %>%
  filter(!is.na(tiempo_buscando)) %>%
  group_by(DPTO) %>%
  summarise(
    tiempo_prom = weighted.mean(tiempo_buscando, FEX_C18, na.rm = TRUE),
    .groups = "drop"
  )



# Verificamos que todos los nombres de deptos coindidan
setdiff(unique(mapa_busqueda$DPTO), colombia_sf$NAME_1)
# Corregimos:
mapa_busqueda <- mapa_busqueda %>%
  filter(!is.na(DPTO))%>%
  mutate(
    DPTO = case_when(
      DPTO == "Quindio" ~ "Quindío",
      DPTO == "Bogotá, d.C." ~ "Bogotá D.C.",
      DPTO == "La guajira" ~ "La Guajira",
      DPTO == "Norte de santander" ~ "Norte de Santander",
      DPTO == "Valle del cauca" ~ "Valle del Cauca",
      TRUE ~ DPTO
    )
  )


# Pegamos el shapefile
colombia_timeline <- colombia_sf %>%
  left_join(mapa_busqueda, by = c("NAME_1" = "DPTO"))


# Graficamos
tiempo_buscando <- ggplot(colombia_timeline) +
  geom_sf(aes(fill = tiempo_prom), color = "black", size = 0.3) +  # bordes siempre visibles
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "gray95",   # departamentos sin datos
    name = "Semanas promedio"
  ) +
  labs(
    title = "Tiempo promedio buscando empleo por departamento",
    subtitle = "Población no ocupada",
    caption = "Fuente: GEIH 2025 - DANE"
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

tiempo_buscando

# Finalmente, guardamos la imagen en formato PNG
ggsave(
  filename = "outputs/graficos/grafico_35_tiempo_buscando.png",
  plot = tiempo_buscando, #Gráfico a guardar
  width = 10, #Tamaño
  height = 6,
  dpi = 300,
  units = "in" #Unidades del tamaño
)




# 2. Comparativa en medios de búsqueda y consecusión de empleo------------------



# a. Creamos función para homologar las categorías para poder hacer las comparaciones válidas
# Hacemos una función para no tener que repetir en cada base
homologar_canal <- function(x) {
  case_when(
    x == "Ayuda de conocidos" ~ "Red de contactos",
    x == "Hojas de vida a empleadores" ~ "Búsqueda directa",
    x == "Agencias, servicios o bolsas de empleo" ~ "Intermediación formal",
    x %in% c("Clasificados", "Presentó a convocatorias") ~ "Convocatorias/avisos",
    x == "Prep. inicio de negocio" ~ "Emprendimiento",
    x == "Otros" ~ "Otros",
    x == "No sabe, no informa" ~ "NS",
    TRUE ~ NA_character_
  )
} 

# b. Cómo buscan empleo los desocupados
busqueda <- base_nueva %>%
  filter(!is.na(forma_buscar)) %>%
  mutate(canal = homologar_canal(forma_buscar)) %>%
  group_by(canal) %>%
  summarise(
    total = sum(FEX_C18, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct = total / sum(total),
    tipo = "Busca empleo"
  )



# c. Cómo encontraron empleo los ocupados
encuentro <- base_nueva %>%
  filter(!is.na(forma_encontrar)) %>%
  mutate(canal = homologar_canal(forma_encontrar)) %>%
  group_by(canal) %>%
  summarise(
    total = sum(FEX_C18, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct = total / sum(total),
    tipo = "Consiguió empleo"
  )

# Confirmamos
unique(busqueda$canal)
unique(encuentro$canal)
# Algunos intentan emprender, pero eso no siempre se refleja como “encontró empleo”



# Armamos la base de comparación
comparacion <- bind_rows(busqueda, encuentro) %>%
  group_by(canal) %>%
  mutate(
    orden = pct[tipo == "Consiguió empleo"][1]  # toma ese valor por canal
  ) %>%
  ungroup()


# Graficamos 
gap_buscar_encontrar <- ggplot(comparacion, aes(x = reorder(canal, orden), y = pct, fill = tipo)) +
  geom_col(position = position_dodge(width = 0.9)) +
  
  geom_text(aes(label = scales::percent(pct, accuracy = 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.2,
            size = 3) +
  
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  
  scale_fill_manual(values = c(
    "Busca empleo" = "#F4A6A6",
    "Consiguió empleo" = "#8EC6FF"
  )) +
  
  labs(
    x = "Canal",
    y = "Porcentaje",
    fill = "",
    title = "¿Cómo buscan empleo vs cómo realmente lo consiguen?",
    subtitle = "Nivel Nacional",
    caption = "Fuente: GEIH 2025 - DANE"
  ) +
  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5), 
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

gap_buscar_encontrar



# Finalmente, guardamos la imagen en formato PNG
ggsave(
  filename = "outputs/graficos/grafico_35.2_gap_buscar_encontrar.png",
  plot = gap_buscar_encontrar, #Gráfico a guardar
  width = 10, #Tamaño
  height = 6,
  dpi = 300,
  units = "in" #Unidades del tamaño
)
