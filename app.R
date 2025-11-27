
library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(bslib)

db_path <- "Z:/z_PISA-Uruguay/z_IT/FormGURI/data/encuesta.sqlite"
uploads_dir <- dirname(db_path)
dir.create(uploads_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Crear DB si no existe ----
if (!file.exists(db_path)) {
  con0 <- dbConnect(SQLite(), db_path)
  dbExecute(con0, "
    CREATE TABLE respuestas (
      user_id TEXT PRIMARY KEY,
      edad INTEGER,
      genero TEXT,
      satis TEXT,
      comentario TEXT,
      detalle_alta TEXT,
      draft INTEGER DEFAULT 0,
      filename TEXT,
      fecha TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );
  ")
  dbExecute(con0, "
    CREATE TABLE logs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id TEXT,
      accion TEXT,
      ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );
  ")
  dbDisconnect(con0)
} else {
  con0 <- dbConnect(SQLite(), db_path)
  info <- dbGetQuery(con0, "PRAGMA table_info(respuestas);")
  cols <- info$name
  if (!"detalle_alta" %in% cols) dbExecute(con0, "ALTER TABLE respuestas ADD COLUMN detalle_alta TEXT;")
  if (!"draft" %in% cols) dbExecute(con0, "ALTER TABLE respuestas ADD COLUMN draft INTEGER DEFAULT 0;")
  if (!"filename" %in% cols) dbExecute(con0, "ALTER TABLE respuestas ADD COLUMN filename TEXT;")
  dbDisconnect(con0)
}

# ---- Tema violeta ----
tema_violeta <- bs_theme(
  version = 4,
  bg = "#FFFF",
  fg = "#331d3d",
  primary = "#9B59B6",
  secondary = "#331d3d",
  success = "#6CC4A6",
  base_font = font_google("Inter")
)

css_violeta <- "
                body { background-color: #F7F3FA; }
                h3, h2, .title { color: #4A2170; }
                .btn-primary { background-image: linear-gradient(90deg,#6a2f9b,#9B59B6); border-color: #4a1f66; box-shadow: none; }
                .progress { background-color: #efe9f7; }
                .progress-bar { background-image: linear-gradient(90deg,#9B59B6,#9B59B6); }
                .table-striped > tbody > tr:nth-of-type(odd) { background-color: #faf6fc; }
                .table-hover tbody tr:hover { background-color: #f0e9f8; }
                .form-control:focus { border-color: #8f5fb7; box-shadow: 0 0 0 0.2rem rgba(91,44,130,0.15); }
                .small-note { color: #6f4b84; font-size: 12px; }
              "

# ---- ADMIN PASSWORD -----------
ADMIN_PASS <- "PISAURUGUAY2025"  # CAMBIAR antes de producción

# ---- UI ----
ui <- fluidPage(
  theme = tema_violeta,
  tags$head(tags$style(HTML(css_violeta))),
  titlePanel(tags$span(style = "color:#4A2170;", "Encuesta GURI")),
  tags$hr(),
  uiOutput("main_ui"),
  tags$hr(),
)

# ---- SERVER ----
server <- function(input, output, session) {
  params <- reactive(parseQueryString(session$clientData$url_search))
  
  # logging
  log_action <- function(uid, msg) {
    con <- dbConnect(SQLite(), db_path)
    dbExecute(con, "INSERT INTO logs (user_id, accion) VALUES (?, ?)", params = list(uid, msg))
    dbDisconnect(con)
  }
  
  # ADMIN login
  admin_mode_requested <- reactive({ !is.null(params()$admin) && params()$admin == "1" })
  admin_ok <- reactiveVal(FALSE)
  
  observeEvent(admin_mode_requested(), {
    if (admin_mode_requested()) {
      showModal(modalDialog(
        title = "Acceso administrador",
        passwordInput("admin_pass_modal", "Contraseña:", placeholder = "Ingresá la contraseña de admin"),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("admin_login_btn_modal", "Acceder", class = "btn-primary")
        ),
        easyClose = FALSE
      ))
    }
  }, ignoreNULL = TRUE)
  
  observeEvent(input$admin_login_btn_modal, {
    req(input$admin_pass_modal)
    if (input$admin_pass_modal == ADMIN_PASS) {
      admin_ok(TRUE)
      removeModal()
      log_action("admin", paste0("Login admin exitoso - ", Sys.time()))
      step("admin")
      showNotification("Acceso correcto — bienvenido admin.", type = "message")
    } else {
      log_action("admin", paste0("Intento fallido: ", Sys.time()))
      showNotification("Contraseña incorrecta.", type = "error")
    }
  })
  
  is_admin_auth <- reactive({ isTRUE(admin_ok()) })
  
  # user_id
  user_id <- reactive({
    if (is_admin_auth()) return("admin")
    uid <- params()$user_id
    if (is.null(uid) || uid == "") {
      showModal(modalDialog(title = "Error", "No se proporcionó un user_id. No se puede continuar.", easyClose = FALSE, footer = NULL))
      stop("user_id obligatorio")
    }
    uid
  })
  
  # cargar respuestas existentes para ese usuario
  existing <- reactive({
    if (is_admin_auth()) return(NULL)
    con <- dbConnect(SQLite(), db_path)
    row <- dbGetQuery(con, "SELECT * FROM respuestas WHERE user_id = ?", params = list(params()$user_id))
    dbDisconnect(con)
    if (nrow(row) > 0) return(row[1, ])
    NULL
  })
  
  step <- reactiveVal("start")
  initialized <- reactiveVal(FALSE)
  observeEvent(TRUE, {
    if (initialized()) return()
    if (admin_mode_requested()) {
      step("admin")
    } else {
      if (!is.null(existing())) {
        step("already")
        log_action(params()$user_id, "Se detectó respuesta previa")
      } else {
        step("paso1")
        log_action(ifelse(is.null(params()$user_id), "anon", params()$user_id), "Nuevo inicio de encuestado")
      }
    }
    initialized(TRUE)
  }, once = TRUE)

  progress_bar <- function(pct) {
    tags$div(class = "progress",
             tags$div(class = "progress-bar", role = "progressbar",
                      style = sprintf("width:%d%%;", pct),
                      `aria-valuenow` = pct, `aria-valuemin` = 0, `aria-valuemax` = 100,
                      paste0(pct, "%")
             ))
  }
  
  paso1_ui <- function(saved) {
    tagList(
      h3("Paso 1: Datos Básicos"),
      progress_bar(33),
      numericInput("edad", "Edad:", value = saved$edad %||% NA, min = 0, step = 1),
      selectInput("genero", "Género:", choices = c("Masculino","Femenino","Otro"),
                  selected = saved$genero %||% ""),
      sliderInput("energia", "¿Nivel de energía hoy? (slider):", min = 0, max = 10, value = 5),
      actionButton("next1", "Siguiente", class = "btn-primary")
    )
  }
  
  paso2_ui <- function(saved) {
    tagList(
      h3("Paso 2: Opinión"),
      progress_bar(66),
      conditionalPanel("input.edad >= 18",
                       selectInput("satis", "Nivel de satisfacción:",
                                   choices = c("Baja","Media","Alta"),
                                   selected = saved$satis %||% "")
      ),
      conditionalPanel("input.satis == 'Alta'",
                       textInput("detalle_alta", "¿Qué te gustó especialmente?", value = saved$detalle_alta %||% "")
      ),
      radioButtons("confianza", "¿Qué tan confiable encontrás la app?",
                   choices = c("Nada","Poco","Regular","Mucho"), selected = "Regular"),
      actionButton("prev2", "Atrás"),
      actionButton("next2", "Siguiente", class = "btn-primary")
    )
  }
  
  paso3_ui <- function(saved) {
    tagList(
      h3("Paso 3: Comentarios"),
      progress_bar(100),
      textAreaInput("comentario", "Comentario (máx 500 chars):", value = saved$comentario %||% "", rows = 4),
      tags$div(textOutput("char_count"), class = "small-note"),
      fileInput("archivo", "Adjuntar archivo (opcional):", accept = c("image/*", ".pdf")),
      # preview de la imagen
      tags$div(style = "margin-top:8px;", imageOutput("archivo_preview", width = "250px")),
      checkboxInput("confirm_read", "Confirmo que la información es veraz", value = FALSE),
      actionButton("prev3", "Atrás"),
      actionButton("preview", "Preview"),
      actionButton("submit", "Guardar", class = "btn-primary")
    )
  }
  
  validate_paso1 <- reactive({
    if (is.null(input$edad) || is.na(input$edad) || input$edad <= 0) return("Edad inválida.")
    if (is.null(input$genero) || input$genero == "") return("Debe elegir un género.")
    NULL
  })
  
  validate_paso2 <- reactive({
    if (!is.null(input$edad) && input$edad >= 18) {
      if (is.null(input$satis) || input$satis == "") return("Debe elegir nivel de satisfacción.")
    }
    NULL
  })
  
  validate_final <- reactive({
    if (!isTRUE(input$confirm_read)) return("Debés confirmar que la información es veraz.")
    if (!is.null(input$comentario) && nchar(input$comentario) > 500) return("Comentario demasiado largo.")
    NULL
  })
  
  autosave_vals <- reactive({
    list(
      edad = input$edad,
      genero = input$genero,
      satis = input$satis,
      comentario = input$comentario,
      detalle_alta = input$detalle_alta,
      filename = if (!is.null(input$archivo)) input$archivo$name else NA
    )
  })
  
  observeEvent(autosave_vals(), {
    if (is_admin_auth()) return()
    vals <- autosave_vals()
    if (all(sapply(vals, function(x) is.null(x) || (is.character(x) && x == "") || is.na(x)))) return()
    con <- dbConnect(SQLite(), db_path)
    dbExecute(con, "
      INSERT INTO respuestas (user_id, edad, genero, satis, comentario, detalle_alta, draft, filename)
      VALUES (:user_id, :edad, :genero, :satis, :comentario, :detalle_alta, 1, :filename)
      ON CONFLICT(user_id) DO UPDATE SET
        edad = excluded.edad,
        genero = excluded.genero,
        satis = excluded.satis,
        comentario = excluded.comentario,
        detalle_alta = excluded.detalle_alta,
        draft = 1,
        filename = excluded.filename,
        fecha = CURRENT_TIMESTAMP
    ",
              params = list(
                user_id = params()$user_id,
                edad = ifelse(is.null(vals$edad), NA, as.integer(vals$edad)),
                genero = ifelse(is.null(vals$genero), NA, vals$genero),
                satis = ifelse(is.null(vals$satis), NA, vals$satis),
                comentario = ifelse(is.null(vals$comentario), NA, vals$comentario),
                detalle_alta = ifelse(is.null(vals$detalle_alta), NA, vals$detalle_alta),
                filename = ifelse(is.null(vals$filename), NA, vals$filename)
              ))
    dbDisconnect(con)
    log_action(params()$user_id, "Autosave (draft)")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$preview, {
    preview_list <- list(
      edad = input$edad,
      genero = input$genero,
      satis = input$satis,
      detalle_alta = input$detalle_alta,
      comentario = input$comentario,
      energia = input$energia,
      confianza = input$confianza,
      filename = if (!is.null(input$archivo)) input$archivo$name else NA
    )
    showModal(modalDialog(
      title = "Previsualización",
      renderUI({
        tagList(lapply(names(preview_list), function(nm) {
          tags$p(tags$b(nm), ": ", as.character(preview_list[[nm]]))
        }))
      }),
      footer = tagList(modalButton("Cerrar"), actionButton("confirm_from_preview", "Confirmar y guardar"))
    ))
  })
  
  observeEvent(input$confirm_from_preview, {
    removeModal()
    isolate({ session$sendInputMessage("submit", list()) })
  })
  
  # ---- preview de imagen ----
  output$archivo_preview <- renderImage({
    if (!is.null(input$archivo) && length(input$archivo$datapath) > 0) {
      src <- input$archivo$datapath
      fname <- input$archivo$name
    } else {
      ex <- existing()
      src <- NULL; fname <- NULL
      if (!is.null(ex) && !is.null(ex$filename) && !is.na(ex$filename) && ex$filename != "") {
        candidate <- file.path(uploads_dir, "uploads", paste0(params()$user_id, "_", ex$filename))
        if (file.exists(candidate)) {
          src <- candidate
          fname <- ex$filename
        }
      }
    }
    if (is.null(src)) return(NULL)
    ext <- tolower(tools::file_ext(src))
    img_exts <- c("png","jpg","jpeg","gif","bmp","webp")
    if (!(ext %in% img_exts)) {
      return(NULL)
    }
    contentType <- switch(ext,
                          png = "image/png",
                          jpg = "image/jpeg",
                          jpeg = "image/jpeg",
                          gif = "image/gif",
                          bmp = "image/bmp",
                          webp = "image/webp",
                          "image/*")
    list(src = src, contentType = contentType, width = 250, alt = fname)
  }, deleteFile = FALSE)
  
  observeEvent(input$archivo, {
    f <- input$archivo
    if (is.null(f)) return()
    dest_dir <- file.path(uploads_dir, "uploads")
    dir.create(dest_dir, showWarnings = FALSE)
    safe_user <- ifelse(is.null(params()$user_id), "unknown", params()$user_id)
    dest <- file.path(dest_dir, paste0(safe_user, "_", f$name))
    file.copy(from = f$datapath, to = dest, overwrite = TRUE)
    con <- dbConnect(SQLite(), db_path)
    dbExecute(con, "INSERT INTO respuestas (user_id, filename, draft) VALUES (?, ?, 1)
                    ON CONFLICT(user_id) DO UPDATE SET filename = excluded.filename, draft = 1, fecha = CURRENT_TIMESTAMP",
              params = list(safe_user, f$name))
    dbDisconnect(con)
    log_action(safe_user, paste("Adjuntó archivo:", f$name))
    showNotification("Archivo subido correctamente.", type = "message")
  })
  
  observeEvent(input$submit, {
    if (!is.null(validate_paso1())) { showNotification(validate_paso1(), type = "error"); step("paso1"); return() }
    if (!is.null(validate_paso2())) { showNotification(validate_paso2(), type = "error"); step("paso2"); return() }
    if (!is.null(validate_final())) { showNotification(validate_final(), type = "error"); return() }
    
    con <- dbConnect(SQLite(), db_path)
    dbExecute(con, "
      INSERT INTO respuestas (user_id, edad, genero, satis, comentario, detalle_alta, draft, filename)
      VALUES (:user_id, :edad, :genero, :satis, :comentario, :detalle_alta, 0, :filename)
      ON CONFLICT(user_id) DO UPDATE SET
        edad = excluded.edad,
        genero = excluded.genero,
        satis = excluded.satis,
        comentario = excluded.comentario,
        detalle_alta = excluded.detalle_alta,
        draft = 0,
        filename = excluded.filename,
        fecha = CURRENT_TIMESTAMP
    ",
              params = list(
                user_id = params()$user_id,
                edad = as.integer(input$edad),
                genero = ifelse(is.null(input$genero), NA, input$genero),
                satis = ifelse(is.null(input$satis), NA, input$satis),
                comentario = ifelse(is.null(input$comentario), NA, input$comentario),
                detalle_alta = ifelse(is.null(input$detalle_alta), NA, input$detalle_alta),
                filename = ifelse(!is.null(input$archivo), input$archivo$name, NA)
              ))
    dbDisconnect(con)
    
    log_action(params()$user_id, "Guardó encuesta (final)")
    showModal(modalDialog("✔ Tus respuestas han sido guardadas (final).", easyClose = TRUE))
    step("final")
  })
  
  # ---- Admin UI ---
  admin_ui_core <- function() {
    tagList(
      h2("Panel Admin"),
      fluidRow(
        column(6,
               h3("Respuestas"),
               tableOutput("tabla_admin"),
               br(),
               textInput("del_user", "Borrar respuesta (user_id):", value = ""),
               actionButton("del_user_btn", "Eliminar usuario", class = "btn-danger"),
               br(), br(),
               actionButton("clear_all_btn", "Vaciar toda la base (respuestas + logs)", class = "btn-warning")
        ),
        column(6,
               downloadButton("download_csv", "Exportar respuestas a CSV")
        )
      ),
      br()
    )
  }
  
  output$tabla_admin <- renderTable({
    con <- dbConnect(SQLite(), db_path)
    df <- dbReadTable(con, "respuestas")
    dbDisconnect(con)
    if (nrow(df) == 0) return(NULL)
    df
  }, striped = TRUE, hover = TRUE)
  
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("respuestas_", Sys.Date(), ".csv"),
    content = function(file) {
      con <- dbConnect(SQLite(), db_path)
      df <- dbReadTable(con, "respuestas")
      dbDisconnect(con)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$del_user_btn, {
    uid <- isolate(input$del_user)
    if (is.null(uid) || uid == "") { showNotification("Ingresá un user_id válido para eliminar.", type = "error"); return() }
    showModal(modalDialog(
      title = "Confirmar eliminación",
      paste0("Se eliminará la respuesta de: ", uid),
      footer = tagList(modalButton("Cancelar"), actionButton("confirm_del_user", "Confirmar", class = "btn-danger"))
    ))
  })
  
  observeEvent(input$confirm_del_user, {
    removeModal()
    uid <- isolate(input$del_user)
    con <- dbConnect(SQLite(), db_path)
    dbExecute(con, "DELETE FROM respuestas WHERE user_id = ?", params = list(uid))
    dbExecute(con, "DELETE FROM logs WHERE user_id = ?", params = list(uid))
    dbDisconnect(con)
    log_action("admin", paste0("Eliminar usuario ", uid))
    showNotification(paste0("Usuario ", uid, " eliminado."), type = "message")
  })
  
  observeEvent(input$clear_all_btn, {
    showModal(modalDialog(
      title = "VACIO TOTAL",
      "Se borrarán todas las respuestas y logs. ¿Confirmás?",
      footer = tagList(modalButton("Cancelar"), actionButton("confirm_clear_all", "Borrar todo", class = "btn-danger"))
    ))
  })
  
  observeEvent(input$confirm_clear_all, {
    removeModal()
    con <- dbConnect(SQLite(), db_path)
    dbExecute(con, "DELETE FROM respuestas;")
    dbExecute(con, "DELETE FROM logs;")
    dbDisconnect(con)
    log_action("admin", "Vació toda la base")
    showNotification("Base vaciada (respuestas + logs).", type = "warning")
    step("admin")
  })
  
  # ---- Main UI  ----
  output$main_ui <- renderUI({
    st <- step()
    saved <- existing() %||% list()
    switch(st,
           "admin" = {
             if (is_admin_auth()) {
               admin_ui_core()
             } else {
               tagList(h3("Acceso administrador — autenticación requerida."))
             }
           },
           "already" = tagList(
             h3("Ya completaste esta encuesta."),
             p("¿Querés editar tus respuestas?"),
             actionButton("go_edit", "Editar"),
             actionButton("go_exit", "Salir")
           ),
           "paso1" = paso1_ui(saved),
           "paso2" = paso2_ui(saved),
           "paso3" = paso3_ui(saved),
           "final" = h3("✔ Gracias, tus datos fueron guardados.")
    )
  })
  
  observeEvent(input$next1, {
    if (!is.null(validate_paso1())) { showNotification(validate_paso1(), type = "error"); return() }
    log_action(params()$user_id, "Paso1 -> Paso2")
    step("paso2")
  })
  
  observeEvent(input$prev2, { log_action(params()$user_id, "Paso2 -> Paso1"); step("paso1") })
  observeEvent(input$next2, {
    if (!is.null(validate_paso2())) { showNotification(validate_paso2(), type = "error"); return() }
    log_action(params()$user_id, "Paso2 -> Paso3")
    step("paso3")
  })
  observeEvent(input$prev3, { log_action(params()$user_id, "Paso3 -> Paso2"); step("paso2") })
  
  observeEvent(input$go_edit, { step("paso1"); log_action(params()$user_id, "Usuario eligió editar encuesta") })
  observeEvent(input$go_exit, { step("final"); log_action(params()$user_id, "Usuario eligió no editar y salir") })
  
  output$char_count <- renderText({
    n <- if (!is.null(input$comentario)) nchar(input$comentario) else 0
    paste0(n, " / 500 caracteres")
  })
}

`%||%` <- function(x, y) if (!is.null(x)) x else y

shinyApp(ui, server)

    
