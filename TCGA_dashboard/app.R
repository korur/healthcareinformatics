library(shiny)
library(shinydashboard)
library(ggrepel)
library(tidyverse)
library(DT)
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
                choices = c("All", levels(data$type)), selected = "All"
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

# combine the two fluid rows to make the body
body <- dashboardBody( 
fluidRow(
    valueBoxOutput("mean_mutations", width = 3)
    ,valueBoxOutput("mean_alt", width = 3)
    ,valueBoxOutput("study", width = 3)
    ,valueBoxOutput("patient", width = 3)
    ),
fluidRow( 
    box(
        title = "Mutation vs % Altered genome"
        ,width = "8"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("mut_genome", height = "500px")
    ),
    
    box(
        title = "Mutations & % Altered genome per study"
        ,width = "4"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,column(width = 12,
                DT::dataTableOutput("mut_table"),style = "height:300px; overflow-y: scroll;overflow-x: scroll;"
        )
    )

)  
) #dash


#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin= "purple")


# create the server functions for the dashboard  
server <- function(input, output) { 
    #some data manipulation to derive the values of KPI boxes
    sum_frac_mut <- data %>% group_by(ID, study) %>% summarise(frac = round(mean(FRACTION_GENOME_ALTERED),3), mutations = round(mean(MUTATION_COUNT),0)) 
    #creating the valueBoxOutput content
    output$mean_mutations <- renderValueBox({
        valueBox(
            ifelse(input$c_type == "All",  data %>% summarise(n = mean(MUTATION_COUNT)) %>% round(0),
            data %>% filter(type == input$c_type) %>% summarise(n = mean(MUTATION_COUNT)) %>% round(0)
            ), "AVG. MUTATION COUNT"
            ,icon = icon("dna")
            ,color = "red")  
    })
    output$mean_alt <- renderValueBox({
        valueBox(
            ifelse(input$c_type == "All",  data %>% summarise(n = mean(FRACTION_GENOME_ALTERED)) %>% round(2),
            data %>% filter(type == input$c_type) %>% summarise(n = mean(FRACTION_GENOME_ALTERED)) %>% round(2)
            ), "% GENOME ALTERED"
            ,icon = icon('percent')
            ,color = "green")  
    })
    output$study <- renderValueBox({
        valueBox(
            ifelse(input$c_type == "All",  data %>% distinct(study) %>% count(),
                   data %>% filter(type == input$c_type) %>% distinct(study) %>% count()
            ), "STUDIES"
            ,icon = icon("users")
            ,color = "yellow")  
    })
   
        output$patient <- renderValueBox({
            valueBox(
                ifelse(input$c_type == "All",  nrow(data),
                       data %>% filter(type == input$c_type) %>% nrow()
                ), "PATIENTS"
                ,icon = icon("users")
                ,color = "blue")  
        })
        output$mut_genome <- renderPlot({
             
            # Plot
            sum_frac_mut %>% ggplot(aes(frac, log(mutations), color = study)) + geom_point() + geom_label_repel(aes(label=ID, size =0.1)) + theme(legend.position = "none") # all studies
        })
        output$mut_table <- renderDataTable({
            
            datatable(sum_frac_mut, options = list(paging = FALSE))
            
        })
}
shinyApp(ui = ui, server = server)
