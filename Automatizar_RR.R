####################################################################-
############  Generacion masiva de oficios automatizados  ###########-
############################# By LE ################################-

# Parámetros Locales
DIRECTORIO <- ""
PROYECTO <- file.path(DIRECTORIO, "")
CARPETA <- file.path(PROYECTO, "")
# Parámetros No Locales (En la web)
NOMBRE_PEDIDO <- "RR - "
MATRIZ <- ""
REGISTRO <- ""
TABLA_INSUMOS <- ""
TABLA_CONTACTO_OD <- ""
TABLA_REGISTRO <- ""
TABLA_REGISTRO_ALERTA <- ""
MOD_SC <- "Oficio_Alerta_sc.Rmd"
MOD <- "Oficio_Alerta.Rmd"

##################### I. Librerias y parámetros #####################
# I.1 Librerias y conexión a base----

# i) Librerias
library(dplyr)
library(readxl)
library(rmarkdown)
library(purrr)
library(lubridate)
library(blastula)
library(googledrive)
library(googlesheets4)
library(stringr)

# ii) Google
correo_usuario <- ""
drive_auth(email = correo_usuario)
gs4_auth(token = drive_auth(email = correo_usuario),
         email = correo_usuario)

# iii) Conexion a las base
RR_INSUMOS <- read_sheet(ss = MATRIZ,
                               sheet = TABLA_INSUMOS)
CONTACTO_OD <- read_sheet(ss = MATRIZ,
                          sheet = TABLA_CONTACTO_OD, skip = 1)

# I.2 Parámetros para envío de correos ----

# 0) Mes actual
mes_actual <- month(now(), label=TRUE, abbr = FALSE)
mes_actual_min <- str_to_lower(mes_actual)

# i) Envio de correos

encargado_control <- c( "")


code_actores <- c("")


sefa_actores <- c("",
                  "",
                  "",
                  "")

sefa_desarrolladores  <- c("",
                          "",
                           "")

  
dest_cc_control <- c(sefa_actores, sefa_desarrolladores, code_actores)


# ii) Email: Cabecera
Arriba <- add_image(
  file = "https://i.imgur.com/0y3eIfd.png",
  width = 1000,
  align = c("right"))
Cabecera <- md(Arriba)

# iii) Email: Pie de página
Logo_Oefa <- add_image(
  file = "https://i.imgur.com/ImFWSQj.png",
  width = 280)
Pie_de_pagina <- blocks(
  md(Logo_Oefa),
  block_text(md("Av. Faustino Sánchez Carrión N.° 603, 607 y 615 - Jesús María"), align = c("center")),
  block_text(md("Teléfonos: 204-9900 Anexo 7154"), align = c("center")),
  block_text("www.oefa.gob.pe", align = c("center")),
  block_text(md("**Síguenos** en nuestras redes sociales"), align = c("center")),
  block_social_links(
    social_link(
      service = "Twitter",
      link = "https://twitter.com/OEFAperu",
      variant = "dark_gray"
    ),
    social_link(
      service = "Facebook",
      link = "https://www.facebook.com/oefa.peru",
      variant = "dark_gray"
    ),
    social_link(
      service = "Instagram",
      link = "https://www.instagram.com/somosoefa/",
      variant = "dark_gray"
    ),
    social_link(
      service = "LinkedIn",
      link = "https://www.linkedin.com/company/oefa/",
      variant = "dark_gray"
    ),
    social_link(
      service = "YouTube",
      link = "https://www.youtube.com/user/OEFAperu",
      variant = "dark_gray"
    )
  ),
  block_spacer(),
  block_text(md("Imprime este correo electrónico sólo si es necesario. Cuidar el ambiente es responsabilidad de todos."), align = c("center"))
)


# iu) Botón encargado

boton_encargado <- add_cta_button(
  url = "",
  text = "Oficios - Reporta Residuos"
)

########################### II. Funciones ###########################
# II.1 Funcion de renderizado de documento
auto_lec_rep <- function(oficina,
                         ht, num, lugar, nombre, 
                         efa, defa, prefa, direfa,
                         oci, doci, proci, diroci, 
                         firma, aux_1, aux_2, aux_3){
  
  # Se eliminan carácteres especiales
  efa_n = gsub("Ñ", "N", efa)
  
  # Carpeta de la oficina
  carpeta_oficina = file.path(CARPETA, oficina)
  
  # Se selecciona el Rmd según el tipo
  if(oficina == "Sede Central del OEFA"){
    MODELO_RMD = MOD_SC
    carpeta_oficina = file.path(CARPETA, oficina)
    ht_n = ""
  } else {
    MODELO_RMD = MOD
    carpeta_oficina = file.path(CARPETA, oficina)
    ht_n = ""
  }
  
  rmarkdown::render(input = file.path(PROYECTO, MODELO_RMD),
                    # Heredamos los par?metros desde la matriz de insumos
                    params = list(HT_1 = ht,
                                  N_OFICIO_2 = num,
                                  LUGAR_3 = lugar,
                                  DESTINATARIO_4 = nombre,
                                  EFA_5 = efa,
                                  DPTO_EFA_6 = defa,
                                  PROV_EFA_7 = prefa,
                                  DIR_EFA_8 = direfa,
                                  OCI_9 = oci,
                                  DPTO_OCI_10 = doci,
                                  PROV_OCI_11 = proci,
                                  DIR_OCI_12 = diroci,
                                  FIRMANTE_13 = firma,
                                  ADICIONAL_14 = aux_1,
                                  ADICIONAL_15 = aux_2,
                                  ADICIONAL_16 = aux_3),
                    output_file = paste0(carpeta_oficina,
                                         "/",
                                         NOMBRE_PEDIDO,
                                         ht_n,
                                         " ",
                                         efa_n))
}

# II.2 Funcion robustecida
R_auto_lec_rep <- function(oficina,
                           ht, num, lugar, nombre, 
                           efa, defa, prefa, direfa,
                           oci, doci, proci, diroci, 
                           firma, aux_1, aux_2, aux_3){
  out = tryCatch(auto_lec_rep(oficina,
                              ht, num, lugar, nombre, 
                              efa, defa, prefa, direfa,
                              oci, doci, proci, diroci, 
                              firma, aux_1, aux_2, aux_3),
                 error = function(e){
                   auto_lec_rep(oficina,
                                ht, num, lugar, nombre, 
                                efa, defa, prefa, direfa,
                                oci, doci, proci, diroci, 
                                firma, aux_1, aux_2, aux_3) 
                 })
  return(out)
}


########################### III. Renderizado ###########################
# III.1 Selección de datos
INSUMOS <- RR_INSUMOS %>%
  filter(ADICIONAL_14!=0)  %>%
  arrange('OD COMPETENTE', EFA_5)

# III.2 Generación de documentos
pwalk(list(INSUMOS$`OD COMPETENTE`,
           INSUMOS$HT_1,
           INSUMOS$N_OFICIO_2,
           INSUMOS$LUGAR_3,
           INSUMOS$DESTINATARIO_4,
           INSUMOS$EFA_5,
           INSUMOS$DPTO_EFA_6,
           INSUMOS$PROV_EFA_7,
           INSUMOS$DIR_EFA_8,
           INSUMOS$OCI_9,
           INSUMOS$DPTO_OCI_10,
           INSUMOS$PROV_OCI_11,
           INSUMOS$DIR_OCI_12,
           INSUMOS$FIRMANTE_13,
           INSUMOS$ADICIONAL_14,
           INSUMOS$ADICIONAL_15,
           INSUMOS$ADICIONAL_16),
      slowly(R_auto_lec_rep,
             rate_backoff(10, max_times = Inf)))

#-----------------------------------------------------------------

######### IV Actualización de base y envío de correo #############

# IV.0 Insumos ----
# i) Preparamos la información a ser subida en la base auxiliar de documentos emitidos
BASE_DOC_MES <- INSUMOS %>%
  select(`NOMBRE DE LA EFA`, `OD COMPETENTE`,
         `COD (APLICATIVO PLANEFA`,
         DEPARTAMENTO, PROVINCIA, ADICIONAL_15) %>%
  mutate(PERIODO = year(now()),
         MES = mes_actual,
         COD_DOC = paste(PERIODO, MES, 
                         ADICIONAL_15, 
                         sep = "-"))  %>%
  select(-ADICIONAL_15)  %>%
  arrange(`OD COMPETENTE`)

# ii) Subida de información a nivel de EFA-Documento
# Cuento cuántas filas hay ya registradas
REGISTRO_DOCS <- read_sheet(REGISTRO, sheet = TABLA_REGISTRO)
num_filas <- nrow(REGISTRO_DOCS) + 2
# Subo la información en el rango, de acuerdo al número de filas
rango <- paste0("'",TABLA_REGISTRO, "'!A", num_filas)
range_write(REGISTRO, 
            data = BASE_DOC_MES,
            range = rango,
            col_names = F)

# iii) Preparo la información a nivel de alertas para la base auxiliar
ALERTAS <- read_sheet(MATRIZ, sheet = "RR")
TABLA_DIAS_TRANSCURRIDOS <- read_sheet(MATRIZ, sheet = "AUX")
TABLA_ALERTAS <- merge(ALERTAS, TABLA_DIAS_TRANSCURRIDOS)

ALERTAS_NO_ATENDIDAS <- TABLA_ALERTAS %>%
  filter(DIAS_TRANSCURRIDOS >= 7,
         ESTADO_ALERTA == "Alerta válida") %>%
  select(ID_GEN, NOMBRE_LISTA_EFA, UBIGEO_PROV, OD_COMPETENTE_PROVINCIAL,
         FECHA_TOMA, HORA_FOTO, DEPARTAMENTO, PROVINCIA,
         DISTRITO, LONGITUD, LATITUD)  %>%
  mutate(PERIODO = year(now()),
         MES = mes_actual,
         COD_DOC = paste(PERIODO, MES, 
                         UBIGEO_PROV, 
                         sep = "-"))  %>%
  select(-UBIGEO_PROV)  %>%
  arrange(DISTRITO,FECHA_TOMA)

#iv) Subida de información a nivel de alertas
# Cuento las filas que hay registradas
REGISTRO_ALERTA <- read_sheet(REGISTRO, sheet = TABLA_REGISTRO_ALERTA)
num_filas <- nrow(REGISTRO_ALERTA) + 2
# Sobreescribo según la cantidad de filas
RANGO_ALERTAS <- paste0("'",TABLA_REGISTRO_ALERTA, "'!A", num_filas)
range_write(REGISTRO, 
            data = ALERTAS_NO_ATENDIDAS,
            range = RANGO_ALERTAS,
            col_names = F)
# IV.1 Correo a ODES y SC ----

correo_OD <- function(od, jefe,
                      dest1, dest2, dest3,
                      num_oficios, url_od){
  
  # Destinatarios
  dest_od = c(dest1, dest2, dest3)
  dest_od_cc = sefa_actores
  dest_od_bcc = sefa_desarrolladores
  
  # Estilo de texto
  od_n = paste0("**", od, "**")
  num_oficios_n = paste0("**", num_oficios, "**")
  
  # Botón
  boton_oficios <- add_cta_button(
    url = url_od,
    text = "Oficios de alertas de Reporta Residuos"
  )
  
  # Composición de correo
  Asunto_OD <- paste0("Oficios Automatizados de Reporta Residuos | Oficios de alertas de ", mes_actual_min)
  Cuerpo_del_mensaje_OD <- blocks(
    md(c("
Buenas tardes, ", jefe, ":   
       ",
         od_n,
"
El Equipo de Proyectos e Innovación de la Subdirección de Seguimiento de Entidades de Fiscalización
Ambiental informa que se han generado exitosamente los documentos automatizados para el 
seguimiento de alertas no atendidas por las municipalidades: ", num_oficios_n, " oficios de alertas para las municipalidades provinciales.

Estos documentos fueron elaborados de manera automatizada, gracias a la información registrada 
en el aplicativo Reporta Residuos.

Pueden acceder a los archivos haciendo click en el siguiente botón:")),
    
    md(c(boton_oficios)),
    md(c("
 
 ***
 **Es importante tomar en cuenta las siguientes indicaciones:**
 - Una vez que el documento se sube al SIGED, este debe **eliminarse de la carpeta compartida** (Desde el Drive). 
 - Les recordamos que los plazos de envío de los oficios es de tres días hábiles en promedio.
 - Para modificaciones del documento, este debe descargarse y abrirse en Word. **No utilizar Google Docs para editar el documento**,
   dado que podría generar conflictos en el formato.
 - Este correo electrónico ha sido generado de manera automática. Para mayor información contactarse con proyectossefa@oefa.gob.pe.
 - El uso de lenguajes de programación de alto nivel para facilitar el trabajo realizado en SEFA es parte de un proyecto impulsado desde la Subdirección.")))
  
  # IV.1.1 Email: Composición ----
  email_OD <- compose_email(
    header = Cabecera,
    body = Cuerpo_del_mensaje_OD, 
    footer = Pie_de_pagina,
    content_width = 1000
  )
  
  # IV.1.2 Email: Envío ----
  smtp_send(
    email_OD,
    to = dest_od,
    from = c("SEFA - Equipo de Proyectos e Innovación" = ""),
    subject = Asunto_OD,
    cc = dest_od_cc,
    bcc = dest_od_bcc,
    credentials = creds_key(id = ""),
    verbose = TRUE
  )
  
}

# Función robustecida
R_correo_OD <- function(od, jefe,
                        dest1, dest2, dest3, 
                        num_oficios, url_od){
  out = tryCatch(correo_OD(od, jefe,
                           dest1, dest2, dest3,
                           num_oficios, url_od),
                 error = function(e){
                   correo_OD(od, jefe,
                             dest1, dest2,dest3,
                             num_oficios, url_od) 
                 })
  return(out)
}


# Envio de correos 


CONTACTO_OD <- CONTACTO_OD %>%
  filter(!is.na(ID_OD_COMPLETO),
         CONTACTO_OD$NUM_OFICIOS != 0) %>%
  arrange(NUM_OFICIOS)

pwalk(list(CONTACTO_OD$ID_OD_COMPLETO,
           CONTACTO_OD$`NOMBRE JEFE DE OD`,
           CONTACTO_OD$CORREO_OD_CONTACTO_PLANEFA,
           CONTACTO_OD$CORREO_JEFE,
           CONTACTO_OD$CORREO_ENCARGADO_RR,
           CONTACTO_OD$NUM_OFICIOS,
           CONTACTO_OD$URL_CARPETA),
      slowly(R_correo_OD, 
             rate_backoff(10, max_times = Inf)))



# IV.2 Correo a encargado ----
Asunto_encargado <- paste0("Oficios Automatizados de Reporta Residuos | Seguimiento a oficios de alertas de ", 
                           mes_actual_min)
num_oficios_encargado<- paste0("**",nrow(INSUMOS),"**")

# Cuerpo del mensaje
Cuerpo_del_mensaje_encargado <- blocks(
  md(c("
Buenas tardes, Maria del Carmen Perea:

El Equipo de Proyectos e Innovación de la Subdirección de Seguimiento de Entidades de Fiscalización
Ambiental informa que se han generado exitosamente los documentos automatizados para el 
seguimiento de alertas no atendidas por las municipalidades: ", num_oficios_encargado, " oficios de alertas para las municipalidades provinciales.

Estos documentos fueron elaborados de manera automatizada, gracias a la información registrada 
en el aplicativo Reporta Residuos.

Pueden acceder a los archivos haciendo click en el siguiente botón:")),
  
  md(c(boton_encargado)),
  md(c("
 
 ***
 **Es importante tomar en cuenta las siguientes indicaciones:**
 - Para modificaciones del documento, este debe descargarse y abrirse en Word. **No utilizar Google Docs para editar el documento**,
   dado que podría generar conflictos en el formato.
 - Este correo electrónico ha sido generado de manera automática. Para mayor información contactarse con proyectossefa@oefa.gob.pe.
 - El uso de lenguajes de programación de alto nivel para facilitar el trabajo realizado en SEFA es parte de un proyecto impulsado desde la Subdirección.")))


  # IV.2.1 Email: Composición ----
email_encargado <- compose_email(
  header = Cabecera,
  body = Cuerpo_del_mensaje_encargado, 
  footer = Pie_de_pagina,
  content_width = 1000
)

  # IV.2.2 Email: Envío ----
smtp_send(
  email_encargado,
  to = encargado_control,
  from = c("SEFA - Equipo de Proyectos e Innovación" = ""),
  subject = Asunto_encargado,
  cc = dest_cc_control,
  credentials = creds_key(id = ""),
  verbose = TRUE
)
