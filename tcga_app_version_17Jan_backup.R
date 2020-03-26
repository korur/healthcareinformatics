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
                 text = "Build by Serdar Korur | www.dataatomic.com", 
                 icon = shiny::icon("atom"),
                 status = "success",
                 href = "https://www.dataatomic.com"))
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard",
             tabName = "dashboard"
    ) ,
    menuItem("Workflow", 
             tabName = "workflow"
    ),
    menuItem("Rawdata", 
             tabName = "Rawdata"
    )
    
  )
)

# combine the two fluid rows to make the body
body <- dashboardBody( 
  tabItems(
    tabItem("dashboard",
            
            fluidRow(
              valueBoxOutput("mean_mutations", width = 4)
              ,valueBoxOutput("mean_alt", width = 4)
              ,valueBoxOutput("study", width = 2)
              ,valueBoxOutput("patient", width = 2)
            ),
            
            fluidRow( 
              box(
                title = h3("Genome Alteration Landscape of Cancers")
                ,width = "8"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("mut_genome", height = "500px"),
                
                fluidRow(
                  box(
                    selectInput("c_type", "Cancer type",
                                choices = c("All", levels(data$type)), selected = "All"
                    ), width = 5),
                  box(
                    selectInput("radio", "Label type", choices = c("ID","type","study"), selected = "ID"), width = 5
                  ),
                  box(
                    numericInput("pointsize", "Point size", 1), width = 2))
              ),
              
              box(
                title = "Data"
                ,width = "4", height = "700px"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,column(width = 12,
                        DT::dataTableOutput("mut_table"),style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
                )
              )
            )  # fluid row
    ), #tabitem1
    tabItem("Rawdata",
            numericInput("maxrows", "Rows to show", 15),
            verbatimTextOutput("rawtable"),
            downloadButton("downloadCsv", "Download as CSV"),
            tableOutput("raw_table")
    ), #tabitem2
    tabItem("workflow", width =6,
            verbatimTextOutput("Workflow"),
            imageOutput("wflow", width = "200%")
    ) 
  ) #tabitems
) #dash


#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin= "blue")


# create the server functions for the dashboard  
server <- function(input, output) { 
  #some data manipulation to derive the values of KPI boxes
  sum_frac_mut <- data %>% group_by(ID, study, type) %>% summarise(altered_gen = round(mean(FRACTION_GENOME_ALTERED),3), mutations = round(mean(MUTATION_COUNT),0)) 
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
      ifelse(input$c_type == "All",  data %>% summarise(n = 100 * mean(FRACTION_GENOME_ALTERED)) %>% round(1),
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
      ,icon = icon("folder-open")
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
    ifelse(input$c_type == "All", sum_frac_mut, sum_frac_mut <- sum_frac_mut %>% filter(type == input$c_type))
    
    if (input$radio == "type") {
      ggplot(sum_frac_mut, aes(altered_gen, log(mutations))) + geom_point(size = input$pointsize) + geom_text_repel(aes(label = type)) + 
        theme(text = element_text(size=18), legend.position = "none") + xlim(0,0.84) + ylim(0,7.4) + xlab('Percent Genome altered') + ylab("Number of Mutations (Log)") + 
        scale_x_continuous(labels = function(x) paste0(100*x, "%")) 
    } else if (input$radio == "study"){
      ggplot(sum_frac_mut, aes(altered_gen, log(mutations))) + geom_point(size = input$pointsize) + geom_text_repel(aes(label = study)) + 
        theme(text = element_text(size=18), legend.position = "none")+ xlim(0,0.84) + ylim(0,7.4) + xlab('Percent Genome altered') + ylab("Number of Mutations (Log)") +
        scale_x_continuous(labels = function(x) paste0(100*x, "%")) 
    } else {
      ggplot(sum_frac_mut, aes(altered_gen, log(mutations))) + geom_point(size = input$pointsize) + geom_text_repel(aes(label = ID)) + 
        theme(text = element_text(size=18), legend.position = "none")+ xlim(0,0.84) + ylim(0,7.4) + xlab('Percent Genome altered') + ylab("Number of Mutations (Log)") + 
        scale_x_continuous(labels = function(x) paste0(100*x, "%")) 
    }
  })
  
  output$mut_table <- renderDataTable({
    
    ifelse(input$c_type == "All", sum_frac_mut, sum_frac_mut <- sum_frac_mut %>% filter(type == input$c_type))
    datatable(sum_frac_mut[, c(1,2,4,5)], options = list(paging = FALSE))
    
  })
  
  output$raw_table <- renderTable({
    
    data[1:input$maxrows,]
    
    
  })
  
  output$wflow <- renderImage({
    return(list(src = "workflow_tcga.png",contentType = "image/png",alt = "Alignment"))
  }, deleteFile = FALSE) #where the src is wherever you have the picture
  
  
  
  
  
  output$downloadCsv <- downloadHandler(
    filename = "tcgadata.csv",
    content = function(file) {
      write.csv(data, file)
    },
    contentType = "text/csv"
  )
}
shinyApp(ui = ui, server = server)
