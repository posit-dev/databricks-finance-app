# cards.R

# install.packages("remotes")
# remotes::install_github("rstudio/bslib") # for version 0.7.1
library(bslib)
library(bsicons)

cards <- list(
  card(
    card_body(
      selectInput(
        inputId = "term",
        choices = list("36 months" = 36, "60 months" = 60),
        selected = 36,
        label = tooltip(
          trigger = list("Term of loan", bs_icon("info-circle")),
          "How soon would you like to pay off the loan?"
        )
      )
    )
  ),
  
  card(
    card_body(
      numericInput(
        inputId = "all_balance",
        value = 5000,
        min = 0,
        max = 500000,
        step = 1000,
        label = tooltip(
          trigger = list("Credit Balance", bs_icon("info-circle")),
          "How much credit, in dollars, do you currently have withdrawn from all sources of credit, including bank cards?"
        )
      )
    )
  ),
  
  card(
    card_body(
      numericInput(
        inputId = "all_limit",
        value = 10000,
        min = 0,
        max = 500000,
        step = 1000,
        label = tooltip(
          trigger = list("Credit Limit", bs_icon("info-circle")),
          "What is your total credit limit, in dollars, for all sources of credit, including bank cards?"
        )
      )
    )
  ),
  
  card(
    card_body(
      numericInput(
        inputId = "bc_balance",
        value = 5000,
        min = 0,
        max = 500000,
        step = 1000,
        label = tooltip(
          trigger = list("Bank Card Balance", bs_icon("info-circle")),
          "How much credit, in dollars, do you currently have withdrawn from only bank cards?"
        )
      )
    )
  ),
  
  card(
    card_body(
      numericInput(
        inputId = "bc_limit",
        value = 10000,
        min = 0,
        max = 500000,
        step = 1000,
        label = tooltip(
          trigger = list("Bank Card Limit", bs_icon("info-circle")),
          "What is your total credit limit, in dollars, for only bank cards?"
        )
      )
    )
  )
)

vbs <-
  value_box(
    title = "Predicted interest rate",
    value = textOutput("pred_int"),
    style = "background-color: #082D46!important; color: #FFFFFF!important",
    showcase = bsicons::bs_icon("bank", fill = '#4682b4 !important;'),
    showcase_layout = "top right",
    height = 500
  )

foot <-
  tags$div(
    style = "background-color: #FFFFFF; padding: 0px; text-align: center; bottom: 0; width: 100%;",
    HTML(
      "Powered by <a href='https://posit.co'><img src='https://www.rstudio.com/assets/img/posit-logo-fullcolor-TM.svg' alt='Posit Logo' style='width:55px;'></a> | Integrated with <a href='https://www.databricks.com'><img src='https://cdn.cookielaw.org/logos/29b588c5-ce77-40e2-8f89-41c4fa03c155/bc546ffe-d1b7-43af-9c0b-9fcf4b9f6e58/1e538bec-8640-4ae9-a0ca-44240b0c1a20/databricks-logo.png' alt='Databricks Logo' style='width:85px;'></a>. For more details, see our <a href='https://posit.co/blog/databricks-and-posit-announce-new-integrations/' target='_blank'>blog post</a> announcing the partnership."
    )
  )

plot <-
  card(full_screen = TRUE,
       card_header(HTML("Applicants like you have received interest rates in this range")),
       card_body(
         plotOutput("plot")
       ))