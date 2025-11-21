#!/usr/bin/env Rscript

library(DBI)
library(RSQLite)

ruta_db <- "Z:/z_PISA-Uruguay/z_IT/FormGURI/data/encuesta.sqlite"

# Crear carpeta si no existe
if (!dir.exists("data")) dir.create("data")

con <- dbConnect(RSQLite::SQLite(), ruta_db)

tablas <- dbListTables(con)

# Vaciar logs
if ("logs" %in% tablas) {
  dbExecute(con, "DELETE FROM logs;")
  message("✔ Se vació la tabla 'logs'. (0 filas ahora)")
} else {
  message("La tabla 'logs' no existe.")
}

# Vaciar respuestas
if ("respuestas" %in% tablas) {
  dbExecute(con, "DELETE FROM respuestas;")
  message("✔ Se vació la tabla 'respuestas'. (0 filas ahora)")
} else {
  message("La tabla 'respuestas' no existe.")
}

dbDisconnect(con)
message("✔ Conexión cerrada.")


