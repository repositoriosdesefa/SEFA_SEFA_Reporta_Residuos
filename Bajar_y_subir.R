####################################################################-
##########  Consultas SQL a bases de Oracle a través de R  #########-
############################# By LE ################################-

################ I. Librerías, drivers y directorio ################

# I.1 Librerías

# i) RJDBC
#install.packages("DBI")
library(DBI)
#install.packages("rJava")
library(rJava)
#install.packages("RJDBC")
library(RJDBC)


# ii) Librerias complementarias
#install.packages("googledrive")
library(googledrive)
#install.packages("googlesheets4")
library(googlesheets4)
#install.packages("httpuv")
library(httpuv)
#install.packages("purrr")
library(purrr)
#install.packages("blastula")
library(blastula)
#install.packages("lubridate")
library(lubridate)
#install.packages("stringr")
library(stringr)

# I.2 Drivers

# i) Oracle
# Driver OJDBC
rutaDriver <- ""
oracleDriver <- JDBC("",
                     classPath=rutaDriver)
#*El driver debe estar descargado y en una ubicación fija

# ii) Google
correo_usuario <- ""
drive_auth(email = correo_usuario) 
gs4_auth(token = drive_auth(email = correo_usuario), 
         email = correo_usuario)
#*El token debe estar almacenado y con los permisos de Google

# I.3 Directorio

# i) Local
directorio <- ""
consulta_dir <- file.path(directorio, "Consultas")
#*Establecer el directorio donde se encuentran las consultas

# ii) Parámetros
base_rr_gs <- ""
hoja_base_rr_gs <- "RR"
consulta_rr <- ""

#-----------------------------------------------------------------

################ II. Establecimiento de conexión ################

# II.1 Credenciales
usuario <- ""
clave <- ""
hostSEFA <- ""
#*Información sensible y privada

# II.2 Conexión
conexionSEFA <- dbConnect(oracleDriver, hostSEFA,
                          usuario, clave)
#*Se debe contar con credenciales para establecer la conexión

#-----------------------------------------------------------------

############## III. Descarga y carga de información ##############

# III.1 Funciones

# i) Lectura de SQL
getSQL <- function(filepath){
  con = file(filepath, "r")
  sql.string <- ""
  while (TRUE){
    line <- readLines(con, n = 1)
    if ( length(line) == 0 ){
      break
    }
    line <- gsub("\\t", " ", line)
    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }
    sql.string <- paste(sql.string, line)
  }
  close(con)
  return(sql.string)
}

# ii) Función de descarga y carga de información
baja_y_sube <- function(consulta, ID, hoja){
  
  consulta_ruta = file.path(consulta_dir, consulta)
  query = getSQL(consulta_ruta)
  # Referencia a objeto en el ambiente global
  datos = dbGetQuery(conexionSEFA, query)
  write_sheet(datos, ID, hoja)
  # hoja_rango = paste0("'",hoja, "'!A2")
  # range_write(ID, data = datos,
  #             range = hoja_rango,
  #             col_names = F)
}

# iii) Función robustecida de descarga y carga de información
R_baja_y_sube <- function(consulta, ID, hoja){
  out = tryCatch(baja_y_sube(consulta, ID, hoja),
                 error = function(e){
                   baja_y_sube(consulta, ID, hoja) 
                 })
  return(out)
}

# III.2 Descarga y carga de información
R_baja_y_sube(consulta_rr, base_rr_gs, hoja_base_rr_gs)

# III.3 Cierre de conexión
dbDisconnect(conexionSEFA)
