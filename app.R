library(shiny)
library(sf)
library(DT)
library(leaflet)
library(mapview)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput('curr_file', 'Select all components of any number of shapefiles', accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE),
      DTOutput('shapefiles'),
      DTOutput('layers')
    ),
    mainPanel(
      leafletOutput("map", width = '100vw', height='95vh')
    )
  )
)

server <- function(input, output, session) {

  dat <- reactiveValues(
    files = NA,
    shapefiles = NA,
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
    dotshp <- input$curr_file$name[grep(pattern="*.shp$", input$curr_file$name)]
    files <- file.path(sourcedir, dotshp)
    layers <- lapply(files, st_layers)
    dat$shapefiles <- data.frame(
      file = dotshp, 
      layers = sapply(layers, \(x) paste(x[,1], collapse=',')),
      geometry_types = sapply(layers, \(x) paste(x[,2], collapse=','))
    )
    dat$files <- files
    dat$layers <- layers
  })

  output$shapefiles <- renderDT({
    req(dat$shapefiles)
    datatable(
      dat$shapefiles,
       selection = list(
         mode = 'single',
         selected = if(nrow(dat$shapefiles) == 1) 1 else NULL,
         target = 'row'
       )
    )
  })
  
  output$layers <- renderDT({
    req(dat$layers, input$shapefiles_rows_selected)
    datatable(
      dat$layers[[input$shapefiles_rows_selected]],
       selection = list(
         mode = 'single',
         selected = if(nrow(dat$layers[[input$shapefiles_rows_selected]]) == 1) 1 else NULL,
         target = 'row'
       )
    )
  })

  observe({
    req(input$shapefiles_rows_selected,input$layers_rows_selected)
    dat$sf <- st_read(
      dat$files[[input$shapefiles_rows_selected]], 
      layer = dat$layers[[input$shapefiles_rows_selected]][input$layers_rows_selected,1]
    )
  })
  
  output$map <- renderLeaflet({
    req(dat$sf)
    mapview(dat$sf)@map
  })

}

shinyApp(ui, server)
