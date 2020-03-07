library(shiny)
library(googlesheets4)
library(DT)
library(dplyr)
source("global.R")
# 
# # designate project-specific cache
# options(gargle_oauth_cache = ".secrets")
# 
# # check the value of the option, if you like
# gargle::gargle_oauth_cache()
# 
# # trigger auth on purpose --> store a token in the specified cache
# sheets_auth()
# 
# # see your token file in the cache, if you like
# list.files(".secrets/")



options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

payment_url <- "https://docs.google.com/spreadsheets/d/1_KxXTywIfc3uySg9eIGZ97YWTNUqtiam2HsrND7W9fU/edit#gid=0"
players_url <- "https://docs.google.com/spreadsheets/d/1qFj1TMgxHL37P-pT5UyTU3-ei5tEwrSFkBIp7-XB50s/edit#gid=0"

ui <- fluidPage(
  
  strong(Sys.Date()),
  
  fluidRow(
    
    splitLayout(
      
      checkboxGroupInput("players",
                         "Players",
                         choices = player_list),
      
      checkboxGroupInput("payg",
                         "PAYG?",
                         choices = player_list),
      
      uiOutput("paid")
      
    ),
    
    actionButton("confirm", "Confirm"),
    actionButton("submit_players", "Submit Players"),
    actionButton("submit_payment", "Submit Payment"),
    
    
    tableOutput("players"),
    tableOutput("payment")
  )
)

server <- function(input, output, session) {
  
  # dynamically create payment textbox
  output$paid <- renderUI({
    
    textbox_list <- lapply(input$players, function(i) {
      
      text_name <- sub(" ", "_", i)
      
      numericInput(text_name, label = i, value = 0, step = 0.5)
      
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, textbox_list)
    
  })
  
  info <- eventReactive(input$confirm, {
    
    paid_df <- data.frame(Name = input$players,
                          paid = purrr::imap_dbl(sub(" ", "_", input$players), ~input[[.x]]))
    
    
    data.frame(Name = input$players) %>%
      mutate(payg = ifelse(Name %in% input$payg, 1, 0),
             type = ifelse(payg == 0, "paym", "payp")) %>%
      left_join(paid_df, by = "Name")
    
    
  })
  
  
  add_players <- reactive({
    
    data.frame(Date = as.character(Sys.Date()),
               Name = info()$Name,
               is_payp = as.numeric(info()$payg))
    
  })
  
  add_payment <- reactive({
    
    data.frame(Date = as.character(Sys.Date()),
               Name = info()$Name,
               Payment = as.numeric(info()$paid),
               Method = NA_character_,
               Type = info()$type) %>%
      filter(Payment > 0)
    
  })
  
  output$players <- renderTable({
    add_players()
  })
  
  output$payment <- renderTable({
    add_payment()
  })
  
  # update google sheet
  observeEvent(input$submit_players, {
    
    sheets_append(add_players(), players_url)
    
  })
  
  observeEvent(input$submit_payment, {
    
    sheets_append(add_payment(), payment_url)
    
  })
  
}

shinyApp(ui, server)