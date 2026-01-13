library(shiny)
library(sf)
library(DT)
library(leaflet)
library(mapview)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput('curr_file', 'Select file', accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE),
      DTOutput('layers')
    ),
    mainPanel(
      leafletOutput("map", width = '100vw', height='95vh')
    )
  )
)

server <- function(input, output, session) {

  dat <- reactiveValues(
    file = NA,
    layers = NA,
    sf = NA
  )

  observe({
    req(input$curr_file$datapath)
    wd <- getwd()
    sourcedir <- dirname(input$curr_file$datapath[[1]])
    setwd(sourcedir)
    for(i in 1:nrow(input$curr_file)){
      file.rename(input$curr_file$datapath[i], input$curr_file$name[i])
    }
    setwd(wd)
    dat$file <- file.path(sourcedir, input$curr_file$name[grep(pattern="*.shp$", input$curr_file$name)])
    dat$layers <- st_layers(dat$file)
  })

  output$layers <- renderDT({
    req(dat$layers)
    datatable(
      dat$layers,
       selection = list(
         selected = if(nrow(dat$layers) == 1) 1 else NULL,
         target = 'row'
       )
    )
  }, options = list(
       pageLength = 10
     )
  )

  observe({
    req(input$layers_rows_selected)
    dat$sf <- st_read(dat$file, layer = dat$layers[input$layers_rows_selected,1])
  })
  
  output$map <- renderLeaflet({
    req(dat$sf)
    mapview(dat$sf)@map
  })

}

shinyApp(ui, server)
