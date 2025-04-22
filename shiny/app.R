# app.R
library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(DBI)
library(RPostgres)
library(ggplot2)

# load the shapefile
vic_lga_sf <- st_read("LGA_shapefile_cleaned/vic_lga_cleaned.shp", quiet=TRUE)

# connect to the neondb
con <- dbConnect(
  Postgres(),
  host     = Sys.getenv("NEON_HOST"),
  port     = Sys.getenv("NEON_PORT"),
  dbname   = Sys.getenv("NEON_DB"),
  user     = Sys.getenv("NEON_USER"),
  password = Sys.getenv("NEON_PASS")
)
dbExecute(con, "SET search_path TO lga_map, public;") # set to the lga schema

# Get the data from the tables: 
council_df <- dbGetQuery(con, 'SELECT * FROM "CouncilInfo"')
stat_df    <- dbGetQuery(con, 'SELECT * FROM "LgaStatistics"')
nat_df     <- dbGetQuery(con, '
  SELECT ln.lga_code, n.nationality, ln.count
  FROM "LgaNationality" ln
  JOIN "Nationality" n USING(nationality_id)
') %>%
  filter(!nationality %in% c("Australia","New Zealand")) %>%
  group_by(lga_code) %>%
  slice_max(count, n = 10, with_ties = FALSE) %>%
  ungroup()
lang_df    <- dbGetQuery(con, '
  SELECT lp.lga_code, l.language, lp.count
  FROM "LgaLanguageProficiency" lp
  JOIN "Language" l USING(language_id)
') %>%
  filter(!is.na(language)) %>%
  group_by(lga_code) %>%
  slice_max(count, n = 10, with_ties = FALSE) %>%
  ungroup()

dbDisconnect(con) # disconnect from db

# join static parts onto the sf
vic_lga_sf <- vic_lga_sf %>%
  left_join(council_df, by = "lga_code") %>%
  left_join(stat_df,    by = "lga_code")

# metric choices for dropdown - useful if user wants to visualuse map based on the stats. 
stat_choices <- stat_df %>%
  select(-lga_code) %>%
  select_if(is.numeric) %>%
  names()

# UI
ui <- fluidPage(
  titlePanel("Victoria LGAs Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "stat_choice",
        "Colour by statistic:",
        choices = stat_choices,
        selected = stat_choices[1]
      )
    ),
    mainPanel(
      leafletOutput("map", height = 600),
      hr(),
      uiOutput("info")    
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # make the shapefiles and lga reactive
  sf_reactive <- reactive({
    chosen <- input$stat_choice
    vic_lga_sf %>% mutate(selected = .data[[chosen]])
  })
  
  # pallete change for map
  pal_reactive <- reactive({
    vals <- sf_reactive()$selected
    colorBin("YlOrRd", domain = vals, bins = 5, na.color = "#ccc")
  })
  
  # initial map renderere
  output$map <- renderLeaflet({
    pal_fn <- pal_reactive()
    leaflet(sf_reactive()) %>%
      addTiles() %>%
      addPolygons(
        layerId   = ~lga_code,
        fillColor = ~pal_fn(selected),
        weight    = 1,
        color     = "#444",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2, color = "#000",
          fillOpacity = 0.9, bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal    = pal_fn,
        values = ~selected,
        title  = input$stat_choice,
        position = "bottomright"
      )
  })
  
  # update map colour based on user option
  observeEvent(input$stat_choice, {
    pal_fn <- pal_reactive()
    leafletProxy("map", data = sf_reactive()) %>%
      clearShapes() %>%
      addPolygons(
        layerId   = ~lga_code,
        fillColor = ~pal_fn(selected),
        weight    = 1,
        color     = "#444",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2, color = "#000",
          fillOpacity = 0.9, bringToFront = TRUE
        )
      ) %>%
      clearControls() %>%
      addLegend(
        pal    = pal_fn,
        values = sf_reactive()$selected,
        title  = input$stat_choice,
        position = "bottomright"
      )
  })
  
  # when user click on lga, show the details and plots
  observeEvent(input$map_shape_click, {
    code   <- input$map_shape_click$id
    council<- council_df %>% filter(lga_code == code)
    stats  <- stat_df    %>% filter(lga_code == code)
    topnat <- nat_df     %>% filter(lga_code == code)
    toplang<- lang_df    %>% filter(lga_code == code)
    
    output$info <- renderUI({
      tagList(
        h3(council$lga_name),
        p(strong("Council: "),      council$council_name),
        p(strong("Council Info: "), council$council_info),
        p(strong("Phone: "),        council$phone),
        p(strong("Email: "),        council$email),
        hr(),
        tabsetPanel(
          tabPanel("Summary", 
                   tableOutput("tblSummary")
          ),
          tabPanel("Nationalities", 
                   plotOutput("plotNat", height = 300)
          ),
          tabPanel("Languages", 
                   plotOutput("plotLang", height = 300)
          )
        )
      )
    })
    
    output$tblSummary <- renderTable({
      stats %>%
        select(-lga_code) %>%
        pivot_longer(everything(),
                     names_to  = "Metric",
                     values_to = "Value") %>%
        mutate(
          Metric = str_to_title(str_replace_all(Metric, "_", " "))
        )
    }, rownames = FALSE)
    
    output$plotNat <- renderPlot({
      ggplot(topnat, aes(x = reorder(nationality, count), y = count)) +
        geom_col(fill = "#2c7fb8") + coord_flip() +
        labs(x = NULL, y = "Count", title = "Top 10 Nationalities") +
        theme_minimal()
    })
    
    output$plotLang <- renderPlot({
      ggplot(toplang, aes(x = reorder(language, count), y = count)) +
        geom_col(fill = "#7fcdbb") + coord_flip() +
        labs(x = NULL, y = "Count", title = "Top 10 Languages") +
        theme_minimal()
    })
  })
}

shinyApp(ui, server)
