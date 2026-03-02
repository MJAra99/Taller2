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
required_packages <- c("openxlsx", "ggplot2", "dplyr", "lubridate")

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
# Punto 1. Manipulación de Datos y Programación Básica
# -----------------------------------------------------------------------------

# ---------- 1.1. Semilla y Reproducibilidad ----------

# A continuación, se define la semilla para asegurar reproducibilidad


