#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(shinydashboard)
require(tidyverse)
require(DT)
#require(stringr)
#require(scales)
#require(grid)  

options(scipen = 999) # To avoid scientific notation


dat <- read_csv("NRS - Data Visualisation - Local area migration - Dataset.csv")

area_type_options <- unique(dat$`Area Type`)
indicator_type_options <- unique(dat$`Indicator Type`)

# text <- data.frame("Indicator_type" = c("Flow", "Stocks", "NINo"),
#                    "Text" = c("Text for flow indicators",
#                               "Text for stock indicators",
#                               "Text for NINo"))

# ==============================================================================
# Define UI
# ==============================================================================
ui <- dashboardPage(
  title="Local area migration - National Records of Scotland", 
  
  dashboardHeader(title = tags$a(href='http://www.nrscotland.gov.uk',
                          tags$img(height="45", alt="NRS", src="logo.png"),
                          tags$script(
                            HTML(paste0('$(document).ready(function() {
                                         $("header").find("nav").append(\'<span class="myClass">',
                                         'Local area migration',
                                         '</span>\');
                                         })' 
                                        ))),
                          tags$head(tags$style(
                            HTML('.myClass {
                               font-size: 18px;
                               line-height: 50px;
                               text-align: left;
                               padding: 0 15px;
                               overflow: hidden;
                               color: white;
                               font-family: "Roboto", sans-serif !important; font-weight:400;
                               }')),
                            HTML('<link rel="stylesheet" href="http://fonts.googleapis.com/css?family=Roboto: 100,200,300,400,500,900">')))
                    ), #end of title
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Home", tabName = "tab_Home", icon = icon("th")),
                        menuItem("Select Data", tabName = "tab_indicator", icon = icon("line-chart")),
                        menuItem(HTML("Sources of Data"), tabName = "tab_DataSource", icon = icon("info")))
                      
                    ),

  # ------------------------------------------------------------------------------
  dashboardBody(
    #adding css file
    tags$head(
      
      includeCSS("style.css")
      
    ),
    
    tabItems(
      # - Tab 0 ----------------------------------------------------------------------
      tabItem(tabName = "tab_Home",
              fluidPage(
                titlePanel("Home"),
                
                br(),

                h4(em("This interactive visualisation provides access to a range of official migration statistics produced by", strong("NRS"), "and other organisations, which provide an indication of migration trends for each council and NHS Board area in Scotland.")),
                
                br(),
                
                h4(strong("This is a newly developed tool and we welcome any feedback to help inform future improvements.", 
                  a("Get in touch!", 
                    href="mailto:Denise.Patrick@nrscotland.gov.uk?cc=statisticscustomerservices@nrscotland.gov.uk&subject=Local%20Area%20Migration%20visualisation"))),
                
                br(),
                
                
                h4(strong("Indicator types")),
                
                h4("Data can be selected using the 'Select your data' tab. The indicators in this visualisation are divided into", strong("Flows, Stocks, NINo"), "and", strong("Other"),":"),
                br(),
                h4(strong("Flows:"), "The number of individuals entering and leaving each council area and health board to live from overseas, rest of the UK and within Scotland."),
                h4(strong("NINo:"), "The number of allocations for National Insurance Numbers (NINo) from different groups of countries (e.g. EU, EU2, EU8, EU14, Other Europe, Asia, Rest of the World)."),
                h4(strong("Stocks:"), "Measures of the number of people living in Scotland with non-British nationality and those born outside of the UK."),
                h4(strong("Other:"), "The number of school pupils from minority ethnic groups, the number of school pupils with English as an additional language land the number of births to non-UK mothers."),
                
                br(),
                
                br(),
                
             
                
                  h4(strong("Important:"), "Not all indicators are direct measures of migration and are therefore not all directly comparable."),
                  h4("For example, indicators are from different sources may show different trends as they may be measuring different things or use different definitions or relate to different time periods. This should be taken into consideration when comparing the data.")
                      
  
              )
      ),
      
      
      
      # - Tab 1 - Flows ------------------------------------------------------------------
      tabItem(tabName = "tab_indicator",
              fluidPage(
                titlePanel("Select data"),
                
                br(),
                
                h4(strong("Flows:"), "The number of individuals migrating to and from Scotland, its council areas and health boards (e.g. flows in year ending 30 June 2018)."),
                h4(strong("NINo:"), "The number of National Insurance Number allocations to overseas nationals entering the UK."),
                h4(strong("Stocks:"), "The number of non-British nationals and non-UK born living in Scotland (total numbers as at a particular point in time)."),
                h4(strong("Other:"), "The number of pupils from minority ethnic groups, the number of pupils who have English as an additional language and the number of births to non-UK mothers. "),
                
                br(),
                
                
                fluidRow(
                  
                  # - Side menu ----------------------------------------------------------
                  column(4,
                         wellPanel(
                           helpText("Select your choice of indicators below:") ,
                           
                           br(),
                           radioButtons(inputId = "indicator_type",
                                        label = "Indicator type:",
                                        choices = indicator_type_options,
                                        selected = "Flow"),
                           uiOutput("indicator_options_ui"),
                           
                           br(),
                           radioButtons(inputId = "area_type",
                                        label = "Area type:",
                                        choices = area_type_options,
                                        selected = "Council Area"),
                           uiOutput("area_options_ui"),
                           
                           br(),
                           
                           sliderInput(inputId = "year_range",
                                       label = "Year range:",
                                       min = 2004,
                                       max = 2018,
                                       value = c(2004,2018),
                                       step = NULL,
                                       sep = "", #This was to take out the comma in the year (eg. 2,004 = 2004)
                                      dragRange = TRUE),
                           
                           br(),
                        
                           conditionalPanel(
                             condition = "input.indicator_type != 'Stocks'", 
                             radioButtons(inputId = "value_type",
                                        label = "Show:",
                                        choices = c("Absolute values", "Rate per population*"),
                                        selected = "Absolute values")
                           ),
                           
                           h6("*Rate per population refers to the rate per 1,000 of the resident population for that area at the mid-year (as at 30 June) of the relevant year.")
                         )
                  ),
                  # - Plot ---------------------------------------------------------------
                  column(8,
                         wellPanel(
                           plotOutput("line_plot"),
                           br(),
                           DTOutput('tbl')
                           )
                  ))
              )),
    
      # - More information----------------------------------------------------------------
      tabItem(tabName = "tab_DataSource",
              fluidPage(
                titlePanel("Sources of Data"),
                
                fluidRow(
                  column(12, 
                         
                         h4("You can find key data sources as well as links to more information on the work of NRS and associated departments below."),
                         
                         br(),
                         
                         h4(em("Population estimates are from", strong("NRS' mid-year population estimates"),", and refer to all those usually resident in the area, whatever their nationality.")),
                         
                         br(),
                         
                         h4(em(strong("Information on  data sources.")),
                            
                        br(),
                         
                         
                         br(),
                         
                         h4(strong("International Passenger Survey (IPS)")),
                         h4("The IPS provides information on moves into and out of Scotland with an origin or destination of outside the UK. The IPS is used alongside information from the Home Office and the Labour Force Survey (LFS) to compile the official estimates of international migration into Scotland."),
                         h4("The IPS collects data on intentions which may or may not be realised, and so an adjustment is made to the data for visitor and migrant switchers - people who change their intentions and their migratory status."),
                         
                         br(),
                         
                         h4(strong("National Health Service Central Register (NHSCR)")),
                         h4("The NHSCR system records movement of patient records between NHS Board areas in the UK. Counts of these re-registrations are used as a proxy indicator for moves within the UK."),
                  
                         br(),
                         
                         h4(strong("Community Health Index (CHI)")),
                         h4("The CHI holds records of people registered with an NHS GP in Scotland. The CHI records contain the postcode of the patients address which enables migration to be estimated for Council areas by matching two extracts taken one year apart."),
                         
                         br(),
                         
                         h4(strong("National Insurance Number (NINo) allocations")),
                         h4("TNINo data is sourced from DWP and includes overseas nationals registering for a National Insurance number which is required to work or claim benefits/tax credits in the UK."),
                         h4("The location of the NINo registration reflects the applicants personal residence, not the employer's address."),
                         
                         br(),
                         
                         h4(strong("Annual Population Survey (APS)")),
                         h4("The APS is a household survey run by the Office for National Statistics. It measures the number of non-British nationals and non-UK born living in Scotland."), 
                         h4("The APS captures the total resident population, though as it is a household survey it does not cover most people living in communal establishments. As the APS is capturing the resident population it captures individuals who may have moved to Scotland many years ago."),
                         
                         br(),
                         
                         br(),
                         
                         
                         h4("More information about the data in this visualisation, their uses and their strengths/limitations can be found on the", 
                           a("Local Area Migration", 
                             href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/migration/migration-statistics/local-area-migration",
                             target="_blank"), 
                           "page on the NRS website."))),
                         
                         br(),
                         
                         fluidRow(
                                  
                           column(4,   
                                  br(),
                                  h6("Follow us on Twitter - ", 
                                     a("@NatRecordsScot", 
                                       href="https://twitter.com/NatRecordsScot",
                                       target="_blank")),
                                  h6("See more ", 
                                     a("Infographics & Visualisations", 
                                       href="http://www.nrscotland.gov.uk/statistics-and-data/statistics/stats-at-a-glance/infographics-and-visualisations", 
                                       target="_blank"))),
                           column(4, 
                                  br(),
                                  h6(a("National Records of Scotland", href="http://www.nrscotland.gov.uk", target="_blank")),
                                  h6("© Crown Copyright 2019 - ",
                                     a("Copyright conditions", 
                                       href="http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/",
                                       target="_blank")))
                         ),
                         br(), 
                         
                         p("Any feedback about this visualisation?", 
                           a("Get in touch!", 
                             href="mailto:Denise.Patrick@nrscotland.gov.uk?cc=statisticscustomerservices@nrscotland.gov.uk&subject=Local%20Area%20Migration%20visualisation")) 
                  ) #end column
                ) #end fluidRow
              ) #end fluidPage
      ) #tab4 end
      
      
      
    ) #tabItems
  ) #dashboardBody
                    
  
  






# ==============================================================================
# Define server
# ==============================================================================
server <- function(input, output, session) {

  ## REACTIVE CONTROLS
  output$indicator_options_ui <- renderUI({
    indicator_options <- unique(dat$Indicator[dat$`Indicator Type` == input$indicator_type])
    
    selectInput(inputId = "indicator", 
                label = "Indicator", 
                choices = indicator_options,
                multiple = TRUE,
                selected = indicator_options[8])
  })
  
  output$area_options_ui <- renderUI({
    area_options <- unique(dat$Area[dat$`Area Type` == input$area_type])
    
    selectInput(inputId = "area", 
                label = "Area", 
                choices = area_options,
                multiple = TRUE,
                selected = area_options[1])
  })
  
  output$year_options_ui <- renderUI({
    year_options <- unique(dat$Year[dat$`Year Range`== input$year_options])
    selectInput(inputId = "year_range",
                label = "Year:",
                choices = year_options)
  })
  

  ## DATA SELECTIONS
  select_data <- reactive({ 
    dat %>%
      group_by(Area, `Area Type`, Indicator, Year) %>%
      filter(Area %in% input$area &
               `Area Type` %in% input$area_type &
               Indicator %in% input$indicator &
               Year >= input$year_range[1] &
               Year <= input$year_range[2]) %>%
      droplevels()
  })
  
  
  
  ## PLOT
  output$line_plot <- renderPlot({ 
    
    if(input$value_type == "Absolute values" | input$indicator_type == "Stocks") {
      ggplot(data = select_data(),
             mapping = aes(x = Year, 
                           y = Value, 
                           colour = Area, 
                           linetype = Indicator)) +
        geom_line(size = 1.5) +
        geom_point(size = 4.5) +
        theme_minimal() +
        scale_x_continuous(breaks = 2004:2018) + ### This has to be changed manually
        #scale_y_continuous(label = comma) +
        expand_limits(y = 0) +
        theme(legend.position = "right",
              panel.grid.minor = element_blank(),
              legend.key.width = unit(5, "line"))
    } else {
      ggplot(data = select_data(),
             mapping = aes(x = Year, 
                           y = Rate, 
                           colour = Area, 
                           linetype = Indicator)) +
        geom_line(size = 1.5) +
        geom_point(size = 4.5) +
        theme_minimal() +
        scale_x_continuous(breaks = 2004:2018) + ### This has to be changed manually
        #scale_y_continuous(label = comma) +
        expand_limits(y = 0) +
        theme(legend.position = "right",
              panel.grid.minor = element_blank())    }
    
    
    })
  
  output$tbl = renderDT(
    select_data(),
    extensions = 'Buttons',
    options = list(pageLength = 250,
                   dom = 'Bfrtip',
                   buttons = c('copy', 'csv', 'pdf', 'print')
    )
  )
  
}

# ==============================================================================
# Run the application 
# ==============================================================================
shinyApp(ui = ui, server = server)

