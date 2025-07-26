# CARGA DE PAQUETES
if(!require(pacman)) install.packages("pacman")
if(!require(devtools)) install.packages("devtools")
if(!require(SMRD)) devtools::install_github("Auburngrads/SMRD")
pacman::p_load("shiny","rintrojs","gt","SMRD","readxl")

# CARGA DE BASES PRECARGADAS (.RData)
carpeta <- "datos"
archivos <- list.files(carpeta, pattern = "\\.RData$", full.names = TRUE)
for(archivo in archivos){
  load(archivo)
}

# INTERFAZ DE USUARIO
ui <- fluidPage(
  introjsUI(),  # Necesario para el tutorial
  
  tags$div(
    style = "display: flex; justify-content: center; align-items: center; gap: 20px; margin-top: 20px; margin-bottom: 10px;",
    tags$img(src = "logo3.png", height = "100px", alt = "Logo UNAL"),
    tags$h2("Gráfico de Eventos Recurrentes", style = "margin: 0; font-weight: bold;")
  ),
  tags$hr(style = "margin-bottom: 20px;"),
  
  
  sidebarLayout(
    sidebarPanel(
      actionButton("ayuda", "📘 Mostrar tutorial", class = "btn btn-primary", style = "margin-bottom: 15px;"),
      
      radioButtons("data_input", "Fuente de los datos:",
                   choices = list("Bases de datos de R" = "predefined",
                                  "Subir archivo propio" = "upload")) %>%
        tagAppendAttributes(`data-step` = 1, 
                            `data-intro` = "Selecciona si deseas usar un conjunto de datos precargado o subir uno propio."),
      
      uiOutput("data_ui1"),
      
      hr(),
      h4("Variables de la base de datos:"),
      uiOutput("col_id1") %>%
        tagAppendAttributes(`data-step` = 2, `data-intro` = "Selecciona la variable que identifica a cada individuo."),
      uiOutput("col_time1") %>%
        tagAppendAttributes(`data-step` = 3, `data-intro` = "Selecciona la variable de tiempo a falla o censura."),
      uiOutput("col_event1") %>%
        tagAppendAttributes(`data-step` = 4, `data-intro` = "Selecciona la variable indicadora de falla o censura."),
      h4("Autores:"),
      tags$ul(
        tags$li(tags$a(href="mailto:marjaramillogo@unal.edu.co", "Maria F. Jaramillo-Gómez")),
        tags$li(tags$a(href="mailto:mcjarami@unal.edu.co", "Mario C. Jaramillo-Elorza")),
        tags$li(tags$a(href="mailto:cmlopera@unal.edu.co", "Carlos M. Lopera-Gómez"))
      ),
      h4("Correspondencia:"),
      tags$ul(
        tags$li(tags$a(href="mailto:mcjarami@unal.edu.co", "Mario C. Jaramillo-Elorza"))
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen", 
                 verbatimTextOutput("summary_text") %>%
                   tagAppendAttributes(`data-step` = 5, `data-intro` = "En la pestaña Resumen, podrás visualizar un resumen estadístico de la base de datos de eventos recurrentes, incluyendo el número de recurrencias, el tiempo mínimo y máximo de las recurrencias, etc.")),
        
        tabPanel("Gráfico", 
                 plotOutput("mcf_plot", height = "600px"))
      ) %>%
        tagAppendAttributes(`data-step` = 6, `data-intro` = "En la pestaña Gráfico, podrás visualizar el gráfico de eventos recurrentes para la base de datos seleccionada o cargada")
      
    )
    # ),
    # 
    # # Marca de agua
    # tags$div(
    #   style = "text-align: center; font-size: 13px; color: #888; margin-top: 40px; margin-bottom: 10px;",
    #   "Creado por Grupo de investigación en Estadística, Universidad Nacional de Colombia - Sede Medellín"
  )
)

# SERVIDOR
server <- function(input, output, session) {
  
  # Inicia el tutorial al presionar el botón
  observeEvent(input$ayuda, {
    introjs(session, options = list(
      "nextLabel" = "Siguiente",
      "prevLabel" = "Anterior",
      "doneLabel" = "Finalizar",
      "showProgress" = TRUE,
      "scrollToElement" = TRUE
    ))
  })
  
  # Bases de datos precargadas
  bases_de_datos <- c("BrakingGrids", "ComputerLab", "Cylinder", 
                      "Grids1", "Grids2", "HPCRepairs", "MachineH", 
                      "SystemE", "ValveSeat", "WorkStation")
  
  # Función para leer archivos subidos
  read_input_data <- function(file_input, delimiter) {
    tryCatch({
      ext <- tools::file_ext(file_input$name)
      if (ext == "xlsx") {
        read_excel(file_input$datapath)
      } else {
        read.table(file_input$datapath, header = TRUE, sep = delimiter)
      }
    })
  }
  
  # Datos reactivos
  datos1 <- reactive({
    if (input$data_input == "predefined") {
      req(input$base_datos1)
      get(input$base_datos1)
    } else {
      req(input$file1)
      read_input_data(input$file1, input$delimiter1)
    }
  })
  
  # UI dinámico para cargar archivo o seleccionar base
  output$data_ui1 <- renderUI({
    if (input$data_input == "predefined") {
      selectInput("base_datos1", "Selecciona Dataset:", choices = bases_de_datos)
    } else {
      tagList(
        fileInput("file1", "Sube archivo:") %>%
          tagAppendAttributes(`data-step` = 7, `data-intro` = "Sube un archivo .csv o .xlsx desde tu equipo."),
        selectInput("delimiter1", "Delimitador:",
                    choices = c("Coma" = ",", "Tab" = "\t", "Punto y coma" = ";", "Espacio" = " ")) %>%
          tagAppendAttributes(`data-step` = 8, `data-intro` = "Selecciona el tipo de separador del archivo.")
      )
    }
  })
  
  # Actualización de variables disponibles según dataset
  observe({
    req(datos1())
    cols <- names(datos1())
    updateSelectInput(session, "col_id1", choices = cols, selected = cols[1])
    updateSelectInput(session, "col_time1", choices = cols, selected = cols[2])
    updateSelectInput(session, "col_event1", choices = cols, selected = cols[3])
  })
  
  # UI para variables
  output$col_id1 <- renderUI({
    req(datos1())
    cols <- names(datos1())
    selectInput("col_id1", "Variable ID:", choices = cols, selected = cols[1])
  })
  
  output$col_time1 <- renderUI({
    req(datos1())
    cols <- names(datos1())
    selectInput("col_time1", "Variable tiempo:", choices = cols, selected = cols[2])
  })
  
  output$col_event1 <- renderUI({
    req(datos1())
    cols <- names(datos1())
    selectInput("col_event1", "Variable tipo evento:", choices = cols, selected = cols[3])
  })
  
  # Conversión a RDU
  rdu1 <- reactive({
    req(datos1(), input$col_id1, input$col_time1)
    frame.to.rdu(datos1(), ID.column = input$col_id1,
                 time.column = input$col_time1,
                 event.column = input$col_event1,
                 data.title = {input$base_datos1 %||% input$file1$name})
  })
  
  # Gráfico MCF
  output$mcf_plot <- renderPlot({
    req(rdu1())
    event.plot(rdu1())
  })
  
  # Resumen del RDU
  output$summary_text <- renderPrint({ 
    req(rdu1())
    summary(rdu1())
  })
}

# INICIAR LA APP
shinyApp(ui = ui, server = server)
