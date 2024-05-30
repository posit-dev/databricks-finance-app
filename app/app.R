# install.packages("remotes")
# remotes::install_github("rstudio/shiny") # for version 1.8.1.9001
library(shiny)
library(tibble)
library(readr)

source("authenticate.R")
source("cards.R")
source("helpers.R")

rates <- connect_to_lending_club_data_on_databricks()
model <- read_rds("vetiver-model.RDS")

ui <- bslib::page_navbar(
  title = "Predicted Interest Rate Calculator",
  theme = bs_theme(bootswatch = "flatly", success = "#4682b4"),
  bg = "#082D46",
  underline = FALSE,
  nav_panel(
    title = " ",
    layout_columns(
      
      # Row 1
      card(
        helpText("Please fill out the fields below. Then click Predict Rate."),
        layout_columns(
          cards[[1]], 
          cards[[2]], 
          cards[[3]], 
          cards[[4]], 
          cards[[5]],
          width = 1/5, 
          height = 170
        ),
        actionButton(
          "predict", "Predict Rate", width = 200, 
           style="color: #fff; background-color: #4682b4;"
        )
      ),
      
      # Row 2
      layout_columns(
        conditionalPanel(condition = "input.predict > 0", vbs),
        conditionalPanel(condition = "input.predict > 0", plot),
        height = 500, col_widths = c(3, 9)
      ),
      col_widths = c(12, 12)
    ),
    card_footer(foot)
  )
)

server <- function(input, output, session) {
  
  all_util <- 
    reactive(input$all_balance / input$all_limit)
  
  bc_util <- 
    reactive(input$bc_balance / input$bc_limit)
  
  bc_open_to_buy <-
    reactive(input$bc_limit - input$bc_balance)
  
  predictions_df <- 
    reactive({
      
      pred_tibble <-
        tibble(term = as.numeric(input$term),
               all_util = all_util(),
               bc_util = bc_util(),
               bc_open_to_buy = bc_open_to_buy())
      
      predict(model, pred_tibble)
    }) |> 
    bindCache(input$term, 
              input$all_balance, 
              input$all_limit, 
              input$bc_balance, 
              input$bc_limit) |> 
    bindEvent(input$predict)
  
  output$pred_int <- 
    renderText({
      predictions_df()$.pred |> round(2)
    })
  
  rate_distribution <- 
    reactive({
      rates |> 
        find_rates_for_similar_applicants(input$term, 
                                          all_util(), 
                                          bc_util(), 
                                          bc_open_to_buy())
    })  |> 
    bindCache(input$term, 
              input$all_balance, 
              input$all_limit, 
              input$bc_balance, 
              input$bc_limit) |> 
    bindEvent(input$predict)
  
  output$plot <-
    renderPlot({
      plot_rate_distribution(rate_distribution())
    })
  
}

shinyApp(ui, server)