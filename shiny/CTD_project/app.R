

# Prep --------------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
# setwd("shiny/CTD_project/") # For in-app testing
load("CTD_interp_tester.Rdata")
CTD_interp_tester <- CTD_interp_tester %>% 
  arrange(depth)
# metadata <- CTD_interp_monthly
# type <- unique(CTD_interp_monthly$type)
type <- c("CTD", "Bongo", "Calvet")
years <- unique(CTD_interp_tester$year)
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
                  label = "Choose one year",
                  choices = years, multiple = F),
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
                 p("Hello and welcome to the CTD project.")),
        navbarMenu(title = "Information",
                   tabPanel("Data Selection",
                            p()),
                   tabPanel("Meta-data definitions",
                            p()),
                   tabPanel("Downloading Data",
                            p()),
                   tabPanel("Updates",
                            p(),
                            p("2018-06-14: The project goes live."))),
        tabPanel("Map", plotOutput("map1")))
    )
  )
)


# The server --------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {


# The subsetted CTD data --------------------------------------------------

  CTD <- reactive({
    CTD <- CTD_interp_tester
    years <- input$Date
    CTD <- CTD[CTD$year == years, ]
    depths <- input$Depth
    CTD <- CTD[CTD$depth == depths, ]
    z <- input$Variable
    CTD <- as.data.frame(CTD)
    CTD$z <- as.vector(CTD[,colnames(CTD) == z])
    CTD <- CTD[complete.cases(CTD$z),]
  })
  

# The map -----------------------------------------------------------------

  output$map1 <- renderPlot({
    
    CTD1 <- CTD()
    # CTD1 <- CTD_interp_tester %>% 
    #   filter(depth == 10,
    #          year == 1997) %>% 
    #   select(lon, lat, temp) %>% 
    #   unique() %>% 
    #   na.omit()
    # The map
    ggplot(CTD1, aes(x = lon, y = lat)) +
      # geom_raster(aes(fill = temp))
      geom_point(aes(colour = z)) +
      coord_cartesian(xlim = c(13, 34), ylim = c(-28, -38))
  })  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

