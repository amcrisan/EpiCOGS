library(shiny)
library(shinydashboard)
library(leaflet)

# Define UI for application that draws a histogram
dashboardPage(skin = "black",
  dashboardHeader(titleWidth = "267px",title=tagList(shiny::icon("gears"), "EpiCOGs")),
  dashboardSidebar(
    width = "310px",
    h3("Load and View Data",align = "center"),
     sidebarMenu(id="tabs",
        menuItem("Load Data",tabName="dataIn",selected=TRUE),
        menuItemOutput("allPatientMenu"),
        menuItemOutput("selectedPatientMenu"),
        menuItem("Analysis Modes",tabName="analysis_modes"),
        menuItem("Reports",tabName = "reports"),
        menuItem("Feedback",href = "https://docs.google.com/forms/d/13qMJNf3luskv_GOs2PkDj6ZIuymbFo9yDZXcfWA3fBQ/viewform?usp=send_form",icon=icon("commenting-o"),selected=FALSE)
      ),

    h3("Filter Patients",align = "center"),
    uiOutput("filterMenu")
    
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tabItems(
        tabItem(tabName="dataIn",
                selected=TRUE,
                tabsetPanel(type="pill",
                  tabPanel("Input Data",
                      br(),
                      box(width=12,
                          height="250px",
                          solidHeader = TRUE,
                          h4("Load a New File"),
                          
                          fileInput('file1', 'Choose Input File (CSV of RDS)'),
#                                     accept=c('text/csv', 
#                                              'text/comma-separated-values,text/plain', 
#                                              '.csv'))
                          radioButtons("inType","New file, or previously formatted dataset?",
                                        choices=c("New","Formatted"),selected="Formatted")
                      ),
                        fluidRow(column(1),
                                 column(2,
                                        actionButton("done",label="Done")),
                                 column(9,
                                        conditionalPanel("input.done",
                                                         h3("Please verify in Mapping Panel"))
                                 )
                        ),
                      uiOutput("inputChoices")
                  ),
                  tabPanel("Mapping",
                           br(),
                           actionButton("verified","Verfiy mapping is correct"),
                           p("Note : Variables of category type 'Ignore', will be ignored"),
                           br(),
                           br(),
                           tableOutput("mapOverView"))
                )
          
        ),
        tabItem("all_patients",
                tabsetPanel(
                  tabPanel("Spatial",
                      leafletOutput("map"),
                      DT::dataTableOutput("linelist")
                  ),
                  tabPanel("Overview",
                              plotOutput("caseOverTime")
                    
                  )
                )
        ),
        tabItem("selected_patients",
                tabsetPanel(id="selectedTabs",
                            type="pills",
                            
                            tabPanel(tagList(shiny::icon("table"), "Line List"),
                                     br(),
                                     DT::dataTableOutput("selectedPatients"),
                                     box(tagList(shiny::icon("car"),"Get optimal driving route to visit patients"),
                                       width=12,
                                        p("It make take a few minutes to calculate driving directions"),
                                       br(),
                                        actionButton("drivingDirections","CALCULATE ROUTE!"),
                                        br(),
                                        conditionalPanel("input.drivingDirections",
                                                         htmlOutput("routeURL")
                                        )
                                     ),
                                     br(),
                                     box(tagList(shiny::icon("user"),"View Selected Patient's Details"),
                                         width=12,
                                         collapsible = TRUE,
                                         collapsed = TRUE,
                                         h3("Still to be implemented"))
                            )
                )
        ),tabItem("analysis_modes",
                p("Welcome to Analysis Modes!"),
                 #moduleUI("test")
                uiOutput("availableAnalysis"),
                analysisModeUI("analysisSelected")
        ), tabItem("reports",
                   p("Future Home of Reports"))
    )
  )
)
