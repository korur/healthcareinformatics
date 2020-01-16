library(shiny)
library(shinydashboard)
data <- readRDS('mycalldataforTcgaApp_with_cancerclass.rds')
data$type = as.factor(data$type)
data$study = as.factor(data$study)

# Shiny dashboard App

header <- dashboardHeader(
    title= "TCGA DATA Analysis",
    dropdownMenu(type = "notifications", 
                 notificationItem(
                     text = "Build by www.dataatomic.com", 
                     icon = shiny::icon("atom"),
                     status = "success",
                     href = "https://www.dataatomic.com"))
)
sidebar <- dashboardSidebar(
    selectInput("c_type", "Cancer type",
                choices = c("All", levels(data$type))
    ),
    selectInput("s_type", "Study",
                choices = levels(data$study)
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

frow1 <- fluidRow(
    valueBoxOutput("mean_mutations")
    ,valueBoxOutput("mean_alt")
    ,valueBoxOutput("study")
    ,valueBoxOutput("patient")
    )
frow2 <- fluidRow( 
    box(
        title = "Mutation vs % Fraction of Genome alterations"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("revenuebyPrd", height = "300px")
    )
    ,box(
        title = "Workflow"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("revenuebyRegion", height = "300px")
    ) 
)
# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='purple')


# create the server functions for the dashboard  
server <- function(input, output) { 
    #some data manipulation to derive the values of KPI boxes
    
    #creating the valueBoxOutput content
    output$mean_mutations <- renderValueBox({
        valueBox(
            data %>% filter(type == input$c_type) %>% summarise(n = mean(MUTATION_COUNT)),10,
            ,icon = icon("dna")
            ,color = "yellow")  
    })
    output$mean_alt <- renderValueBox({
        valueBox(
            data %>% filter(type == input$c_type) %>% summarise(n = mean(FRACTION_GENOME_ALTERED)),1,
            
            ,icon = icon('percent')
            ,color = "orange")  
    })
    output$study <- renderValueBox({
        valueBox(
        data %>% filter(type == input$c_type) %>% count(),0.1,
            ,icon = icon("folder-open")
            ,color = "blue")  
    })
    output$patient <- renderValueBox({
        valueBox(
            data %>% filter(type == input$c_type) %>% summarise(n = mean(FRACTION_GENOME_ALTERED)), "PATIENTS",
            ,icon = icon("users")
            ,color = "blue")  
    })
}
shinyApp(ui = ui, server = server)
