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
required_packages <- c("openxlsx", "ggplot2", "dplyr", "lubridate", "readr")

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

for (n in 1:12){
  base_main <- read_csv2(paste0("GEIH_2025/",
                                        n,"/Características generales, seguridad social en salud y educación.csv"),
                                 locale = locale(encoding = "latin1"), col_types = cols(.default = "c"))
  
  base_main <- mutate(base_main,
                  LLAVE = paste0(DIRECTORIO, SECUENCIA_P, ORDEN))
  for (i in bases){
    base_join <- read_csv2(paste0("GEIH_2025/",n,"/",i),
                      locale = locale(encoding = "latin1"), col_types = cols(.default = "c"))
    base_join <- mutate(base_join,LLAVE = paste0(DIRECTORIO, SECUENCIA_P, ORDEN))
    base_join <- select(base_join, LLAVE, names(base_join)[!names(base_join) %in% names(base_main)])
    base_main <- base_main %>% left_join(base_join, by = "LLAVE")
  }
  assign(paste0("base_",n),base_main)
}

base_total <- bind_rows(mget(paste0("base_", 1:12)))

base_final <- select(base_total, PERIODO, DIRECTORIO, SECUENCIA_P, ORDEN,
                     P3271, P3042, P6090, P6040, P6160, DPTO, FEX_C18, PT,
                     PET, FT, FFT, P6240, OCI, RAMA2D_R4, P6430, INGLABO, DSI)

base_final <- base_final %>%
  mutate(across(everything(), as.numeric))

base_final$PERIODO <- as.Date(as.character(base_final$PERIODO), '%Y%m%d')

base_final$mes <- month(base_final$PERIODO, label = FALSE)
base_final$dia <- day(base_final$PERIODO)
base_final$anio <- year(base_final$PERIODO)


base_final <- base_final %>%
  rename(
    sexo = P3271,
    nivel_edu = P3042,
    eps = P6090,
    edad = P6040,
    leer_escr = P6160,
    act_ocup = P6240,
    pos_empl = P6430
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
    pos_empl = factor(pos_empl, levels = c(1,2,3,4,5,6,7,8),
                      labels = c("Obrero o Empl. Empresa Part","Obrero o Empl. del Gobierno",
                                 "Empleado doméstico","Trabajador por cuenta propia",
                                 "Patrón o empleador","Trabajador familiar sin rem.",
                                 "Jornalero o peón","Otro"))
  )
  


