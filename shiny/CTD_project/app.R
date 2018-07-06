
# Prep --------------------------------------------------------------------

# Load libraries
library(shiny)
library(maps)
library(ggplot2)
library(dplyr)

# Load data
# setwd("shiny/CTD_project/") # For in-app testing
load("CTD_pre_interp.Rdata")
load("CTD_interp_clim.Rdata")
load("CTD_interp_yearly.Rdata")
load("CTD_interp_monthly.Rdata")

# The base map
sa_map <- ggplot() +
  borders(fill = "grey80", colour = "black") +
  coord_equal(xlim = c(13, 34), ylim = c(-26, -38)) +
  labs(y = "", x = "")

# The UI ------------------------------------------------------------------

ui <- fluidPage(
  
  # Application title
  titlePanel("CTD Project"),
  

  # The side bar filters ----------------------------------------------------
  
  sidebarLayout(
    sidebarPanel(
      # Eventually it would be good to allow the user to choose the type
      # of instrument(s) from which to create the data products.
      # This is not done yet due to the paucity of the raw data.
      # selectInput(inputId = "Type",
      #             label = "Choose instrument type(s)", 
      #             choices = type, multiple = T),
      radioButtons(inputId = "Base",
                   label = "Choose product",
                   choices = list("Clean", "Monthly", "Yearly", "Climatology"),
                   inline = TRUE,
                   selected = "Clean"),
      uiOutput("Year"),
      uiOutput("Month"),
      radioButtons(inputId = "Measure",
                  label = "Measure from",
                  choices = list("Top", "Bottom"),
                  inline = TRUE,
                  selected = "Top"),
      uiOutput("Depth"),
      selectInput(inputId = "Variable",
                  label = "Choose one variable",
                  choices = list("temp", "salinity", "oxygen"),
                  multiple = F),
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
                   would like to use the data for research purposes."),
                 p("The 'Information' tab above contains several sub-tabs that will explain in 
                   detail how this web app functions."),
                 p("The 'Map' tab above is where the data visualisation occurs."),
                 br(),
                 p("Please send any input you may have to Robert Schlegel at:"),
                 p("robwschlegel@gmail.com")),
        navbarMenu(title = "Information",
                   tabPanel("Data Selection",
                            p(),
                            p("Below please find an itemised explanation for how to use the filters found 
                              in the menu column on the left-hand side of the screen."),
                            p(strong("Choose product")),
                            p("In order to make a sub-setted selection of the data available please
                              first choose which base product you would like to work from by clicking
                              on the appropriate radio button. The descriptions of the base products 
                              available are as follows:"),
                            column(1), p(strong("Clean"), "- This product is only a cleaned up version of the 
                                         raw data. The data were first filtered for any obvious outliers
                                         before being binned into lon/lat values within 0.1 degrees of
                                         one another, depths of 10 metre intervals, and the dates were
                                         rounded from days to months. Any data points found to be 
                                         overlapping one another were meaned. All of the temperature,
                                         salinity, and oxygen values were then rounded to the second
                                         decimal values. All data deeper than 1000 metres were excluded."),
                            column(1), p(strong("Monthly"), "- This product takes the above product and
                                         performs a linear interpolation without otherwise binning any of 
                                         the data further. This product is most useful if a researcher is 
                                         interested in using these CTD data as a monthly time series but
                                         would like to fill in the large gaps as much as possible."),
                            column(1), p(strong("Yearly"), "- This product is the same as above but the 
                                         data are first binned into yearly groups before the linear 
                                         interpolation is performed. This provides a much more complete 
                                         product spatially, but looses out on temporal precision. This 
                                         product is of best use to a researcher that is interested in
                                         long-term (annual) trends or would like more spatial coverage than
                                         the monthly product provides."),
                            column(1), p(strong("Climatology"), "- This product bins all of the data into the
                                         months they were sampled, irrespective of the year, before then
                                         performing the linear interpolation. This product provides the best
                                         spatial coverage, but the worst temporal coverage. This product is best
                                         used for a research that would like to know, on average, what the abiotic
                                         values along the coast of South Africa are during a given month of the 
                                         year."),
                            p(strong("Choose year(s)/month(s)")),
                            p("After chosing the data product the next step is to decide how to filter it based
                              on the date values available. For the", strong("Clean"), "and", strong("Monthly"), 
                              "products one may choose to filter by both year and month values. Select as many of
                              either value as is desirable by clicking on the year and/or month value in the 
                              drop-down list. Please note that it will only be possible to choose year values for
                              the", strong("Yearly"), "product and only month values for the", 
                              strong("Climatology"), "product. Please note that if multiple years/months are
                              selected the visualisation panel will show all of the overlapping values meaned
                              together."),
                            p(strong("Measure from")),
                            p("Selected the radio button 'Top' if one would like to filter the data product based
                              on the distance from the sea surface. Select 'Bottom' to rather filter data based on 
                              their distance from the sea floor."),
                            p(strong("Choose depth(s)")),
                            p("Once the direction of measurement has been selected, one may then choose which depth 
                              layer(S) (rounded to the nearest 10 metres) to filter. Please note that if multiple
                              depths are selected the visualisation will show them meaned together."),
                            p(strong("Choose one variable")),
                            p("With all of the filters set, the last step is to select which abiotic variable one 
                              is interested in. The choices are 'temp' (temperature in °C), 'salinity' 
                              (parts per thousand; PPT), and 'oxygen' (miligrams per litre; mg/L)."),
                            p(strong("Filter Data")),
                            p("Click this button to apply the chosen filters. Please note that the visualisations
                              are not generated until this button is clicked.")),
                   tabPanel("Downloading Data",
                            p(),
                            p("In order to download data, plesae first make the selection as outlined in 
                              the 'Data Selection' sub-tab and then click on one of the download buttons in 
                              the bottom left corner of this app. The 'Download All' button will download all
                              of the filtered data whereas the 'Download Mean' button will bin together and
                              average all of the data if multiple different depth values were selected.")),
                   tabPanel("Meta-data definitions",
                            p(),
                            p("Presently the meta-data that exists 'behind' the data shown here are not
                              available with the products hosted here as they are an amalgamation of all
                              of the data presently available. If more raw data are to be made available
                              it may then be possible to create products that show the the accompanying 
                              meta-data upon download of the filtered product.")),
                   tabPanel("Updates",
                            p(),
                            p("2018-06-14: The project goes live."),
                            p("2018-06-14: Added land mask, changed colour palettes for 
                              different variables, and added some explanatory text."),
                            p("2018-06-22: Fixes to linearly interpolated monthly climatologies."),
                            p("2018-07-05: Bottom depth filter added as well as multiple products."),
                            p("2018-07-06: Filter button added and help text updated.")),
                  tabPanel("Acknowledgments",
                           p(),
                           p("The data found within this web app are an amalgamation of data that were
                             collected during DAFF and DEA research/survey cruises. The data were mainly
                             collected with CTDs though there are some BONGO and CALVET data blended in
                             for the upper mixed layer. The majority of the raw data from which the data
                             products in this app were created may be requested from SADCO. The links for
                             the sources referenced here may be found below:"),
                           column(1), p("DAFF - ", a(href = "www.daff.gov.za", "Department of Agriculture Forestry and Fisheries")),
                           column(1), p("DEA - ", a(href = "www.environment.gov.za", "Department of Environmental Affairs")),
                           column(1), p("SADCO - ", a(href = "www.sadco.csir.co.za", "Southern African Data Centre for Oceanography")))),
        tabPanel("Map", plotOutput("map1")))
    )
  ),
  downloadButton("save_data_all", "Download All"),
  downloadButton("save_data_mean", "Download Mean")
)


# The server --------------------------------------------------------------

server <- function(input, output) {


  # Choosing the base product -----------------------------------------------
  
  base <- reactive({
    if(input$Base == "Clean"){
      base <- CTD_pre_interp
    } else if(input$Base == "Climatology"){
      base <- CTD_interp_clim
    } else if(input$Base == "Yearly"){
      base <- CTD_interp_yearly
    } else if(input$Base == "Monthly"){
      base <- CTD_interp_monthly
    }
  })
  

  # Choosing year and month filters -----------------------------------------

  output$Year <-  renderUI({
    base <- base()
    if(input$Base %in% c("Clean", "Monthly", "Yearly")){
      selectInput('year2', 'Choose years(s)', 
                  choices = unique(base$year),
                  multiple = T)
    } else if(input$Base == "Climatology") {
    }
  })
  
  output$Month <-  renderUI({
    base <- base()
    if(input$Base %in% c("Clean", "Monthly", "Climatology")){
      selectInput('month2', 'Choose month(s)', 
                  choices = unique(base$month)[order(unique(base$month))],
                  multiple = T)
    } else if(input$Base == "Yearly") {
    }
  })

  
  # Choosing depths filters -------------------------------------------------

  output$Depth <- renderUI({
    base <- base()
    if(input$Measure == "Top"){
      selectInput('depth2', 'Choose depth(s)', choices = unique(base$depth),
                  multiple = T)
    } else if(input$Measure == "Bottom") {
      selectInput('depth2', 'Choose depth(s)',
                  choices = unique(base$depth_to)[order(unique(base$depth_to))],
                  multiple = T)
    }
  })
  

  # Filtering the final data product ----------------------------------------

  CTD <- eventReactive(input$Filter, {
      CTD <- base()
      if(input$Base %in% c("Clean", "Monthly")){
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
      CTD$var <- rep(as.character(input$Variable[1]), nrow(CTD))
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
    
    # To control the colour scale to be static based on the full range 
    # of values found within the base product requires a bit of wizardry
    # I've chosen for now not to do this, and to rather allow the colour 
    # scale to change based on the filtered value range.
    # The first step towards changing this is commented out below,
    # but quite a bit more work is necessary tofinish this issue.
    # base <- base()
    # 
    # col_range <- c(min(base[,colnames(base) == input$Variable], na.rm = T),
    #                max(base[,colnames(base) == input$Variable], na.rm = T))
    
    CTD <- CTD()
    if(CTD$var[1] == "temp") viri_col <- "A"
    if(CTD$var[1] == "salinity") viri_col <- "D"
    if(CTD$var[1] == "oxygen") viri_col <- "E"
    
    CTD2 <- CTD %>% 
      group_by(lon, lat) %>% 
      summarise(z = mean(z, na.rm = T)) %>% 
      ungroup()

    # The map
    sa_map +
      geom_raster(data = CTD2, aes(x = lon, y = lat, fill = z), stat = "identity") +
      scale_fill_viridis_c(CTD$var, option = viri_col)
  })
  

  # The saving --------------------------------------------------------------

  output$save_data_all <- downloadHandler(
    filename = "CTD_all.csv",
    content = function(file) {
      CTD <- CTD()
      CTD <- CTD %>% 
        select(-var) %>% 
        select(date, everything())
      colnames(CTD)[colnames(CTD) == "z"] <- input$Variable
      write.csv(CTD, file, row.names = F)
    }
  )
  
  output$save_data_mean <- downloadHandler(
    filename = "CTD_mean.csv",
    content = function(file) {
      CTD <- CTD()
      CTD <- CTD %>%
        group_by(date, lon, lat, depth_bot)
      if(input$Measure == "Top") {
        CTD <- CTD %>% 
          mutate(depth_range = paste0(min(depth, na.rm = T),
                                      "-", max(depth, na.rm = T))) %>% 
          group_by(date, lon, lat, depth_range, depth_bot) %>%
          summarise(z = round(mean(z, na.rm = T), 2))
      } else if(input$Measure == "Bottom"){
        CTD <- CTD %>% 
          mutate(depth_to_range = paste0(min(depth_to, na.rm = T),
                                      "-", max(depth_to, na.rm = T))) %>% 
          group_by(date, lon, lat, depth_to_range, depth_bot) %>%
          summarise(z = round(mean(z, na.rm = T), 2))
      }
      colnames(CTD)[colnames(CTD) == "z"] <- input$Variable
      write.csv(CTD, file, row.names = F)
    }
  )
  
}


# Run the application  ----------------------------------------------------

shinyApp(ui = ui, server = server)

