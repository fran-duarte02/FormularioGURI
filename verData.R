library(DBI)
library(RSQLite)

# 1. Definir la ruta de la base
db_path <- "Z:/z_PISA-Uruguay/z_IT/FormGURI/data/encuesta.sqlite"

# 2. Verificar que existe
if (!file.exists(db_path)) stop("❌ No se encontró la base en: ", db_path)

# 3. Conectar
con <- dbConnect(SQLite(), db_path)

# 4. Listar tablas
tablas <- dbListTables(con)
print(tablas)  # verás "logs" y "respuestas"

# 5. Cargar tabla respuestas
if ("respuestas" %in% tablas) {
  respuestas_df <- dbReadTable(con, "respuestas")
  head(respuestas_df)  # muestra las primeras filas con edad, genero, satis, comentario
} else {
  respuestas_df <- NULL
  warning("La tabla 'respuestas' no existe.")
}

# 6. (Opcional) Cargar logs
if ("logs" %in% tablas) {
  logs_df <- dbReadTable(con, "logs")
  head(logs_df)
} else {
  logs_df <- NULL
  warning("La tabla 'logs' no existe.")
}

# 7. Cerrar conexión
dbDisconnect(con)


