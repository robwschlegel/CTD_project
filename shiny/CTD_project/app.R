

# Prep --------------------------------------------------------------------

library(shiny)
library(maps)
library(ggplot2)
library(dplyr)
# setwd("shiny/CTD_project/") # For in-app testing
load("CTD_interp_tester.Rdata")
CTD_interp_tester <- CTD_interp_tester %>% 
  arrange(depth)
CTD_interp_tester <- as.data.frame(CTD_interp_tester)
# metadata <- CTD_interp_monthly
# type <- unique(CTD_interp_monthly$type)
type <- c("CTD", "Bongo", "Calvet")
dates <- unique(CTD_interp_tester$month)
depths <- unique(CTD_interp_tester$depth)
variables <- colnames(CTD_interp_tester)[5:7]

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
      selectInput(inputId = "Date",
                  label = "Choose one month",
                  choices = dates, multiple = F),
      selectInput(inputId = "Depth",
                  label = "Choose one depth",
                  choices = depths, multiple = F),
      selectInput(inputId = "Variable",
                  label = "Choose one variables",
                  choices = variables, multiple = F)),
    
    
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
                              different variables, and added some explanatory text."))),
        tabPanel("Map", plotOutput("map1")))
    )
  ),
  downloadButton("save_data", "Download")
)


# The server --------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {


# The subsetted CTD data --------------------------------------------------

  CTD <- reactive({
    CTD <- CTD_interp_tester
    dates <- input$Date
    CTD <- CTD[CTD$month == dates, ]
    depths <- input$Depth
    CTD <- CTD[CTD$depth == depths, ]
    CTD <- as.data.frame(CTD)
    CTD$z <- as.vector(CTD[,colnames(CTD) == input$Variable])
    CTD <- CTD[complete.cases(CTD$z),]
    CTD <- CTD %>% 
      select(-temp, -salinity, -oxygen)
  })
  

# The map -----------------------------------------------------------------

  output$map1 <- renderPlot({
    
    if(input$Variable == "temp") viri_col <- "A"
    if(input$Variable == "salinity") viri_col <- "D"
    if(input$Variable == "oxygen") viri_col <- "E"

    col_range <- c(min(CTD_interp_tester[,colnames(CTD_interp_tester) == input$Variable], na.rm = T),
                   max(CTD_interp_tester[,colnames(CTD_interp_tester) == input$Variable], na.rm = T))
    
    CTD <- CTD()
    # Testing...
    # CTD <- CTD_interp_tester %>%
    #   filter(depth == 10,
    #          month == "Jan") %>%
    #   # select(lon, lat, temp) %>%
    #   select(-depth, -month) %>%
    #   # mutate(z = temp) %>% 
    #   unique() %>%
    #   na.omit()
    # CTD$z <- as.vector(CTD[,colnames(CTD) == "temp])
    # The map
    ggplot(CTD, aes(x = lon, y = lat)) +
      borders(fill = "grey80", colour = "black") +
      # geom_raster(aes(fill = temp))
      geom_point(aes(colour = z), shape = 15) +
      scale_colour_viridis_c(input$Variable, option = viri_col) +
      coord_cartesian(xlim = c(13, 34), ylim = c(-26, -38))
  })  
  

# The saving --------------------------------------------------------------

  output$save_data <- downloadHandler(
    filename = "CTD.csv",
    content = function(file) {
      CTD <- CTD()
      colnames(CTD)[5] <- input$Variable
      # CTD$z <- NULL
      write.csv(CTD, file, row.names = F)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

