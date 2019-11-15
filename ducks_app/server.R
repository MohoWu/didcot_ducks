library(shiny)
library(dplyr)
library(lubridate)
library(plotly)
# library(markdown)

which_week <- function(the_date) {
  
  weeks_from_start <- floor((the_date - as_date("2019-11-11"))/7) + 1
  
  if (weeks_from_start > 4) {
    
    no_week <- weeks_from_start %% 4
    
  } else no_week <- weeks_from_start
  
  as.numeric(no_week)
  
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # read payment sheet
  payment <- read.csv(url("https://docs.google.com/spreadsheets/d/1_KxXTywIfc3uySg9eIGZ97YWTNUqtiam2HsrND7W9fU/export?usp=sharing&format=csv")) %>%
    mutate(Date = as_date(Date))
  
  # get bills
  bills <- payment %>%
    filter(Name == "UTC")
  periods <- unique(c(as_date("2019-01-01"), bills$Date, Sys.Date()))
  ## cut bills with periods
  bills <- bills %>%
    mutate(period = cut(Date, periods, right = TRUE, labels = (1:(length(periods) - 1))))
  
  # calc pot
  pot <- payment %>%
    filter(Type == "paym") %>%
    mutate(period = cut(Date, periods, right = TRUE, labels = 1:(length(periods) - 1)),
           Name = as.character(Name)) %>%
    group_by(period, Name) %>%
    summarise(pot = sum(Payment)) %>%
    group_nest() %>%
    left_join(bills[c("period", "Payment")], by = "period") %>%
    mutate(Payment = tidyr::replace_na(Payment, 0)) 
  
  current_pot <- purrr::reduce2(pot$data, pot$Payment, calc_pot, .init = old_pot)
  
  output$no_week <- renderText(
    paste0("WEEK ", which_week(Sys.Date()), " OF 4")
  )

  output$pot <- renderPlotly({
    
    plot_ly(current_pot) %>%
      add_pie(values = ~pot, labels = ~Name, textinfo = "text",
              text = ~paste0(Name, ": ", "£", pot),
              hoverinfo = "text",
              hole = 0.3,
              title = ~paste0("£", sum(pot))) %>%
      config(displayModeBar = FALSE)
    
  })

})

