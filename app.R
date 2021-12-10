rm(list = ls())
library(dplyr)
library(shinydashboard)
library(rhandsontable)

df <- readxl::read_excel("FC_data.xlsx")
df <- mutate(df, days = as.integer(days))
# Questions asked by enumerator
col_names <- c("How many days over the last 7 days,
did all or most of of your HH members eat?", "# days consumed the\nfood group",
               "Weights for the\nfood groups", "FCS")

ui <- dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  dashboardBody(
    splitLayout(cellWidths = c("33.3%", "33.3%", "33.3%"),
                valueBox("Poor", width = "100%", subtitle = "0-21", color = "red"),
                valueBox("Borderline", width = "100%", subtitle = "21.5-35", color = "orange"),
                valueBox("Acceptable", width = "100%", subtitle = "35+", color = "green")
                ),
    splitLayout(cellWidths = c("52%", "1%", "47%"),
                list(
                  rHandsontableOutput("table"),
                  actionButton("calculate", "Calculate FCS")
                  # actionButton("reset", "Reset")
                ),
                NULL,
                # includeMarkdown("description.Rmd")
                includeMarkdown(WrapRmd::str_rmd_wrap("description.Rmd"))
                ),
    tags$hr(),
    textOutput("fcs_score"),
    tags$hr(),
    tags$hr()
  )
)

server <- function(input, output){
  
  output$table <- renderRHandsontable({
    rhandsontable(df[, 1:3], colHeaders = col_names, stretchH = "all") %>% 
      hot_col(c(col_names[1], col_names[3]), readOnly = TRUE)
  })

  observeEvent(input$calculate, {
    final_data <- hot_to_r(input$table)
    final_data <- mutate(final_data, FCS = final_data$days * final_data$weight)
    writexl::write_xlsx(final_data, "new_data.xlsx")
    # output$fcs_score <- renderText({
    #   as.character(sum(final_data$FCS))
    # })
  })
  
  observeEvent(input$calculate, {
    df_new <- readxl::read_excel("new_data.xlsx")
    score <- sum(df_new$FCS)
    output$fcs_score <- renderText({
      
      status <- if(score <= 21 ) {
        "poor"
      } else if (score <= 35) {
        "borderline"
      } else {
        "acceptable"
      }
      
      glue::glue("The Food Consumption Score (FCS) is {score} 
                 which is considered as {status} based on the World Food Program (WFP) guideline.")
      
    })
  })
  
}

shinyApp(ui, server)



