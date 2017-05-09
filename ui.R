library(shiny)
library(shinydashboard)

### Title:

header <- dashboardHeader(title = "LA Art Project")

### SideBar:
sidebar <- dashboardSidebar(
  sidebarMenu(
   menuItem("Demographics", tabName = "demographics", icon = icon("fa fa-circle")),
   menuItem("Location and Time Analysis", tabName = "data", icon = icon("fa fa-circle")),
   menuItem("City Data Analysis", tabName = "about", icon = icon("fa fa-info-circle"))
  )
 )


### Dashboard:
body <- dashboardBody(



  ### Tabintes:

  tabItems(

   ### TAB 1 = dashboard:
   tabItem(tabName = "demographics",

   fluidRow(

    # Sample size slider
    box(width = 3, title = "Zip Code",
        solidHeader = TRUE, status = "primary",

   # sliderInput(inputId = "sample",
             #       label = "Sample size",
              #      value = 50, min = 10, max = 100),
    selectInput("zipcode", label = h5("Choose the zip code"), 
                choices = list("Choice 1" = 1, "Choice 2" = 2,
                               "Choice 3" = 3), selected = 1),
   tableOutput('table')
   # sliderInput(inputId = "slope",
          #     label = "Regression slope",
           #    value = .25, min = -2, max = 2,step = .25), 
    # Sd slider:
   # sliderInput(inputId = "SD",
          #     label = "Standard deviation",
            #   value = 3, min = 0, max = 50), 
  #  actionButton(inputId = "refresh", label = "Simulate New Data" , 
        #         icon = icon("fa fa-refresh"))   ##refresh = simulate new data button
    ),

    mainPanel(

   #  box(width = 25,
    #     title = "Race Demographics",
    #     solidHeader = TRUE, status = "primary",
    #     plotOutput(outputId = "reg")),   ##The regression chart


     box(width = 25,title = "Demographics",
         solidHeader = TRUE, status = "primary",
     tabsetPanel(type = "tabs",
                 tabPanel("Age", plotOutput("total")),
                 tabPanel("Race", plotOutput("regression")),
                 tabPanel("Income Distribution", plotOutput("error"))
                 

     )
     )
    )
 
   )),

   # TAB 2 = dashboard:

   tabItem(tabName = "data",
           fluidRow(
             box(width = 4, title = "Zip Code",
                 solidHeader = TRUE, status = "primary",
                 
                 # sliderInput(inputId = "sample",
                 #       label = "Sample size",
                 #      value = 50, min = 10, max = 100),
                 selectInput("year", label = h5("Choose the Year"), 
                             choices = list("Choice 1" = 1, "Choice 2" = 2,
                                            "Choice 3" = 3), selected = 1),
                 selectInput("projectarea", label = h5("Choose the Area"), 
                             choices = list("Choice 1" = 1, "Choice 2" = 2,
                                            "Choice 3" = 3), selected = 1)
                 
                 ),
                 
             mainPanel(
               
               #  box(width = 25,
               #     title = "Race Demographics",
               #     solidHeader = TRUE, status = "primary",
               #     plotOutput(outputId = "reg")),   ##The regression chart
               
               
               box(width = 25,title = "Location Analysis",
                   solidHeader = TRUE, status = "primary",
                   tabsetPanel(type = "tabs",
                               tabPanel("Is the Artist Local", plotOutput("data")),
                               tabPanel("Project Location", plotOutput("data1"))

                   )
               )
             ),
            box(width = 6, solidHeader = TRUE, status = "primary",
                title = "Project Locations by Year",
                plotOutput(outputId = "histogram1")
                #actionButton(inputId = "refresh2", label = "Simulate New Data" , 
                            # icon = icon("fa fa-refresh"))
            ),
            box(width = 6, solidHeader = TRUE, status = "primary",
                title = "Number of Projects by Year",
                plotOutput(outputId = "histogram2")),
            box(width = 6, solidHeader = TRUE, status = "primary",
                title = "Where are the Projects Located (Overall)",
                plotOutput(outputId = "histogram3")),
            box(width = 6, solidHeader = TRUE, status = "primary",
                title = "Is the Artist Local (Overall)",
                plotOutput(outputId = "histogram4"))

            )
            ),

   # TAB 3 = About
  tabItem(tabName = "about",
          
          fluidRow(
            
            # Sample size slider
            box(width = 3, title = "Zip Code",
                solidHeader = TRUE, status = "primary",
                
                # sliderInput(inputId = "sample",
                #       label = "Sample size",
                #      value = 50, min = 10, max = 100),
                selectInput("zipcode1", label = h5("Choose the zip code"), 
                            choices = list("Choice 1" = 1, "Choice 2" = 2,
                                           "Choice 3" = 3), selected = 1)
              
                # sliderInput(inputId = "slope",
                #     label = "Regression slope",
                #    value = .25, min = -2, max = 2,step = .25), 
                # Sd slider:
                # sliderInput(inputId = "SD",
                #     label = "Standard deviation",
                #   value = 3, min = 0, max = 50), 
                #  actionButton(inputId = "refresh", label = "Simulate New Data" , 
                #         icon = icon("fa fa-refresh"))   ##refresh = simulate new data button
            ),
            
            mainPanel(
              
              #  box(width = 25,
              #     title = "Race Demographics",
              #     solidHeader = TRUE, status = "primary",
              #     plotOutput(outputId = "reg")),   ##The regression chart
              
              
              box(width = 25,title = "City Data Analysis",
                  solidHeader = TRUE, status = "primary",
                  tabsetPanel(type = "tabs",
                              tabPanel("Crime", plotOutput("crime")),
                              tabPanel("Housing", plotOutput("housing")),
                              tabPanel("New Businesses", plotOutput("business")),
                              tabPanel("New Projects", plotOutput("projects"))
                              
                              
                  )
              )
            )
            
          ))
           )
   )

ui <- dashboardPage(header, sidebar, body)


