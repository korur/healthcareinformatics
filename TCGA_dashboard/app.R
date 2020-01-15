library(shiny)
library(shinydashboard)
mean_mutations <- 107
mean_altered <- 0.23
patientcount <- 48008
studycount <- 145
# Shiny dashboard App
header <- dashboardHeader(
    dropdownMenu(type = "notifications", 
                 notificationItem(
                     text = "Build by www.dataatomic.com", 
                     icon = shiny::icon("atom"),
                     status = "success",
                     href = "https://www.dataatomic.com"))
)
sidebar <- dashboardSidebar(
    sliderInput("rateThreshold", "Warn when rate exceeds",
                min = 0, max = 50, value = 3, step = 0.1
    ),
    sidebarMenu(
        menuItem("Data", 
                 tabName = "data"
        ),
        menuItem("Dashboard",
                 tabName = "dashboard"
        ) 
    )
)




body <- dashboardBody(
    
    fluidRow(
        # Add a value box for mean mutaion count
        valueBox(
            value = mean_mutations,
            subtitle = "Average mutations", 
            icon = icon("dna"), color = 'yellow', width = 3
        ),
        valueBox(
            value = mean_altered,
            subtitle = "Average genome alterations", 
            icon = icon("percent"), color = 'yellow',width = 3
        ),
        valueBox(
            value = studycount,
            subtitle = "Studies", 
            icon = icon("folder-open"),color = 'aqua',width = 3
        ),
        valueBox(
            value = patientcount,
            subtitle = "Patients", 
            icon = icon("users"),color = 'aqua',width = 3
        )
        ),
    tabItems(
        tabItem(tabName = "data", tabBox(
            title = "Dataatomic",
            tabPanel("Tab1", "Tab1 content"),
            tabPanel("Tab2", "Tab2 content")
        )),
        tabItem(tabName = "dashboard", 
                tabBox(
                    title = "testbox",
                    tabPanel("Weight", "Tab1 content"),
                    tabPanel("Height", "Tab2 content")
                ))
    ),
    
    
    
    
)


ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    body <- body
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
