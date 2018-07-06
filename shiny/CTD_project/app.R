
# Prep --------------------------------------------------------------------

library(shiny)
library(maps)
library(ggplot2)
library(dplyr)

# setwd("shiny/CTD_project/") # For in-app testing
load("CTD_pre_interp.Rdata")
load("CTD_interp_clim.Rdata")
load("CTD_interp_yearly.Rdata")
load("CTD_interp_monthly.Rdata")
# CTD_interp_tester <- CTD_interp_tester %>% 
#   arrange(depth)
# CTD_interp_tester <- as.data.frame(CTD_interp_tester)
# metadata <- CTD_interp_monthly
# type <- unique(CTD_interp_monthly$type)
# type <- c("CTD", "Bongo", "Calvet")
# dates <- levels(CTD_pre_interp$month)
# depths <- unique(CTD_interp_tester$depth)
# variables <- c("temp", "salinity", "oxygen")

# The UI ------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("CTD Project"),
  
  
  # Side bar of subsetting options ------------------------------------------
  
  sidebarLayout(
    sidebarPanel(
      # selectInput(inputId = "Type",
      #             label = "Choose instrument type(s)", 
      #             choices = type, multiple = T),
      selectInput(inputId = "Variable",
                  label = "Choose one variable",
                  choices = list("temp", "salinity", "oxygen"),
                  multiple = F),
      radioButtons(inputId = "Base",
                   label = "Interpolation",
                   choices = list("None", "Monthly", "Yearly", "Climatology"),
                   inline = TRUE,
                   selected = "None"),
      # selectInput(inputId = "Date",
      #             label = "Choose date(s)",
      #             choices = dates, 
      #             multiple = F),
      uiOutput("Year"),
      uiOutput("Month"),
      radioButtons(inputId = "Measure",
                  label = "Top down or bottom up?",
                  choices = list("Top", "Bottom"),
                  inline = TRUE,
                  selected = "Top"),
      uiOutput("Depth"),
      actionButton("Filter", "Filter Data")),
    
    
    # The main panel for figure plotting --------------------------------------
    
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 p(),
                 p("Hello and welcome to the CTD project."),
                 br(),
                 p("The purpose of this web app is to serve as a proof of concept that the CTD
                   and other similar 3D data collected within the oceans bordering South Africa 
                   may be hosted online in a location that is freely accessible to anyone that
                   would like to use the data. This is still very much in a trial phase and is 
                   currently in active development."),
                 p("Please send any input you may have to Robert Schlegel at:"),
                 p("robwschlegel@gmail.com"),
                 br(),
                 p("The 'Information' tab above contains several sub-tabs that will explain in 
                   detail how this web app functions."),
                 p("The 'Map' tab above is where the data visualisation occurs.")),
        navbarMenu(title = "Information",
                   tabPanel("Data Selection",
                            p(),
                            p("In order to make a sub-setted selection of the data available please
                              first choose which month you would like. Currently one may also choose
                              which depth layer (rounded to the nearest 10 metres) and abiotic variable
                              one would like to display."),
                            p("Please note that the data binned into each month category are the mean values
                              for all of the data detected per pixel for all of the years of data collected
                              at that pixel. This effectively makes the data shown here monthly climatologies
                              of the coastal oceans around South Africa. Be cautious with the interpretation
                              of these data as the sample sizes per pixel and month differ wildly and are not
                              controlled for here as this is meant to serve as a proof of concept. A more
                              developed version of this product will include the sample size for the mean
                              value per pixel as part of the meta-data."),
                            p("Much more is being planned and the current offerings made available for 
                              sub-setting will be widely expanded upon. Both in the number of ways in
                              which one may sub-set and the kinds of sub-setting that will be possible.")),
                   tabPanel("Meta-data definitions",
                            p(),
                            p("Presently the meta-data that exists 'behind' the data shown here are not
                              available with this demo-product. In it's final state, when one downloads 
                              data from this site the accompanying meta-data will be included with it.")),
                   tabPanel("Downloading Data",
                            p(),
                            p("In order to download data, plesae first make the selection as outlined in 
                              the 'Data Selection' sub-tab and then click on the 'Download' button at the
                              bottom left corner of this app.")),
                   tabPanel("Updates",
                            p(),
                            p("2018-06-14: The project goes live."),
                            p("2018-06-14: Added land mask, changed colour palettes for 
                              different variables, and added some explanatory text."),
                            p("2018-06-22: Fixes to linearly interpolated monthly climatologies."),
                            p("2018-07-05: Bottom depth filter added."))),
        tabPanel("Map", plotOutput("map1")))
    )
  ),
  downloadButton("save_data_all", "Download All"),
  downloadButton("save_data_mean", "Download Mean")
)


# The server --------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {


# The subsetted CTD data --------------------------------------------------
  
  base <- reactive({
    if(input$Base == "None"){
      base <- CTD_pre_interp
    } else if(input$Base == "Climatology"){
      base <- CTD_interp_clim
    } else if(input$Base == "Yearly"){
      base <- CTD_interp_yearly
    } else if(input$Base == "Monthly"){
      base <- CTD_interp_monthly
    }
  })
  
  output$Year <-  renderUI({
    base <- base()
    if(input$Base %in% c("None", "Monthly", "Yearly")){
      selectInput('year2', 'Pick years(s)', 
                  choices = unique(base$year),
                  multiple = T)
    } else if(input$Base == "Climatology") {
    }
  })
  
  output$Month <-  renderUI({
    base <- base()
    if(input$Base %in% c("None", "Monthly", "Climatology")){
      selectInput('month2', 'Pick month(s)', 
                  choices = unique(base$month)[order(unique(base$month))],
                  multiple = T)
    } else if(input$Base == "Yearly") {
    }
  })
  
  output$Depth <- renderUI({
    base <- base()
    if(input$Measure == "Top"){
      selectInput('depth2', 'Pick depth(s)', choices = unique(base$depth),
                  multiple = T)
    } else if(input$Measure == "Bottom") {
      selectInput('depth2', 'Pick depth(s)',
                  choices = unique(base$depth_to)[order(unique(base$depth_to))],
                  multiple = T)
    }
  })
  
  CTD <- eventReactive(input$Filter, {
      CTD <- base()
      if(input$Base %in% c("None", "Monthly")){
        CTD <- CTD[CTD$year %in% input$year2, ]
        CTD <- CTD[CTD$month %in% input$month2, ]
        CTD$date <- paste0(CTD$year, "-", CTD$month)
        CTD$month <- NULL
        CTD$year <- NULL
      } else if(input$Base == "Yearly") {
        CTD <- CTD[CTD$year %in% input$year2, ]
        CTD$date <- CTD$year
        CTD$year <- NULL
      } else if(input$Base == "Climatology") {
        CTD <- CTD[CTD$month %in% input$month2, ]
        CTD$date <- CTD$month
        CTD$month <- NULL
      }
      CTD <- as.data.frame(CTD)
      CTD$z <- as.vector(CTD[,colnames(CTD) == input$Variable])
      CTD <- CTD[complete.cases(CTD$z),]
      CTD <- CTD %>% 
        select(-temp, -salinity, -oxygen)
      if(input$Measure == "Top") {
        CTD <- CTD[CTD$depth %in% input$depth2, ]
      } else if(input$Measure == "Bottom"){
        CTD <- CTD[CTD$depth_to %in% input$depth2, ]
      }
      })


# The map -----------------------------------------------------------------

  output$map1 <- renderPlot({
    
    # base <- base()
    # 
    # col_range <- c(min(base[,colnames(base) == input$Variable], na.rm = T),
    #                max(base[,colnames(base) == input$Variable], na.rm = T))

    CTD <- CTD()
    viri_col <- "A"
    # if(CTD$variable == "temp") viri_col <- "A"
    # if(CTD$variable == "salinity") viri_col <- "D"
    # if(CTD$variable == "oxygen") viri_col <- "E"
    
    CTD2 <- CTD %>% 
      group_by(lon, lat) %>% 
      summarise(z = mean(z, na.rm = T)) %>% 
      ungroup()

    # The map
    ggplot(CTD2, aes(x = lon, y = lat)) +
      borders(fill = "grey80", colour = "black") +
      geom_raster(aes(fill = z), stat = "identity") +
      scale_fill_viridis_c(input$Variable, option = viri_col) +
      coord_equal(xlim = c(13, 34), ylim = c(-26, -38)) +
      # lims(fill = col_range) +
      labs(y = "", x = "")
  })
  

# The saving --------------------------------------------------------------

  output$save_data_all <- downloadHandler(
    filename = "CTD_all.csv",
    content = function(file) {
      CTD <- CTD()
      colnames(CTD)[7] <- input$Variable
      write.csv(CTD, file, row.names = F)
    }
  )
  
  output$save_data_mean <- downloadHandler(
    filename = "CTD_mean.csv",
    content = function(file) {
      CTD <- CTD()
      CTD <- CTD %>%
        group_by(month, lon, lat)
      if(input$Measure == "Top") {
        CTD <- CTD %>% 
          mutate(depth_range = paste0(min(depth, na.rm = T),
                                      "-", max(depth, na.rm = T))) %>% 
          group_by(month, lon, lat, depth_range) %>%
          summarise(z = round(mean(z, na.rm = T), 2))
      } else if(input$Measure == "Bottom"){
        CTD <- CTD %>% 
          mutate(depth_to_range = paste0(min(depth_to, na.rm = T),
                                      "-", max(depth_to, na.rm = T))) %>% 
          group_by(month, lon, lat, depth_to_range) %>%
          summarise(z = round(mean(z, na.rm = T), 2))
      }
      colnames(CTD)[5] <- input$Variable
      write.csv(CTD, file, row.names = F)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

