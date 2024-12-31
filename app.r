library(shiny)
library(ggplot2)
library(dplyr)

insurance <- read.csv("insurance.csv")


#1 

# UI
ui <- fluidPage(
  titlePanel("Insurance Charges by Group"),
  sidebarLayout(
    sidebarPanel(
      selectInput("group_by", "Group by:",
                  choices = c("sex", "smoker", "children")),
      # Add a checkbox for showing points
      checkboxInput("show_points", "Show Data Points", value = FALSE)
    ),
    mainPanel(
      plotOutput("boxplot"),
      verbatimTextOutput("summary")
    )
  )
)

# server
server <- function(input, output) {
  
  output$boxplot <- renderPlot({
    # Convert children to factor if it's not already. Important for boxplots
    if (input$group_by == "children") {
      insurance$children <- as.factor(insurance$children)
    }
    
    p <- ggplot(insurance, aes_string(x = input$group_by, y = "charges", fill = input$group_by)) +
      geom_boxplot() +
      labs(x = input$group_by, y = "Charges", fill = input$group_by) +
      theme_bw()
    
    # Conditionally add jittered points
    if (input$show_points) {
      p <- p + geom_jitter(width = 0.2, alpha = 0.6) 
    }
    
    print(p)
  })
  
  output$summary <- renderPrint({
    summary_data <- insurance %>% group_by(!!sym(input$group_by)) %>% summarise(
      mean_charges = mean(charges),
      median_charges = median(charges),
      sd_charges = sd(charges),
      n = n()
    )
    print(summary_data)
  })
}

# Run the application
#shinyApp(ui = ui, server = server)
