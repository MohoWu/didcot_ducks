library(shiny)
library(dplyr)
library(lubridate)
library(plotly)
# library(markdown)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  current_cycle <- which_week(Sys.Date(), cycle = TRUE)

# calculate pot -----------------------------------------------------------

  # read payment sheet
  payment <- reactiveFileReader(1000, session,
                                payment_sheet, 
                                readr::read_csv)  
  
  current_pot <- reactive({
    
    
    # get bills
    bills <- payment() %>%
      filter(Name == "UTC")
    periods <- unique(c(as_date("2019-01-01"), 
                        bills$Date, 
                        Sys.Date(), 
                        as_date("2100-01-01")))
    ## cut bills with periods
    bills <- bills %>%
      mutate(period = cut(Date, periods, right = TRUE, labels = (1:(length(periods) - 1))))
    
    # calc pot
    pot <- payment() %>%
      filter(Type %in% c("paym", "payp")) %>%
      mutate(period = cut(Date, periods, right = TRUE, labels = 1:(length(periods) - 1)),
             Name = as.character(Name)) %>%
      filter(which_week(Date, cycle = TRUE) <= current_cycle) %>% # filter up to current cycle
      group_by(period, Name, Type, add = TRUE) %>% 
      summarise(pot = sum(Payment)) %>% # summarise the pot for each period
      group_by(period) %>%
      group_nest() %>% 
      left_join(bills[c("period", "Payment")], by = "period") %>%
      mutate(Payment = tidyr::replace_na(Payment, 0))
    
    current_pot <- purrr::reduce2(pot$data, pot$Payment, calc_pot, .init = old_pot)
    
    current_pot
    
  })

# calculate debtors -------------------------------------------------------
  
  # read play log
  players <- reactiveFileReader(1000, session,
                                player_sheet,
                                readr::read_csv)
  # calculate debt
  debt <- reactive({
    
    players <- players() %>%
      mutate(cycle = purrr::map_dbl(Date, which_week, cycle = TRUE)) %>%
      filter(cycle == current_cycle,
             is_payp == 0 | is.na(is_payp)) %>% # filter current cycle and not payp plays
      group_by(cycle) %>%
      count(Name, name = "times") %>%
      ungroup()
    
    if (nrow(players) != 0) {
      
      # players with £10 in the pot or all equal when payments had been taken
      if (length(unique(current_pot()$pot)) == 1) {
        
        players_pot <- current_pot()$Name
        
      } else {
        
        players_pot <- current_pot() %>%
          filter(pot == 10) %>%
          pull(Name)
        
      }
      
      # players owing money
      players_owe <- players %>%
        filter(Name %in% setdiff(players$Name, players_pot))
      
      # # calc debt table
      debt <- players_owe %>%
        left_join(current_pot(), by = "Name") %>%
        mutate(pot = tidyr::replace_na(pot, 0),
               # PAYP = times * 2.5,
               # debt = 10 - pot
               debt = times * 2.5)
      
    } else {
      
      debt <- data.frame()
      
    }
    
    # get PAYP people
    payp_players <- payment() %>%
      filter(Type == "payp") %>%
      group_by(Name) %>%
      summarise(paid = sum(Payment))
    
    # PAYP plays
    payp_plays <- players() %>%
      filter(is_payp == 1) %>%
      count(Name, name = "times") %>%
      mutate(owe = times * 2.5)
    
    # payp table
    payp_tbl <- payp_plays%>%
      left_join(payp_players, by = "Name") %>%
      mutate(paid = tidyr::replace_na(paid, 0),
             debt = owe - paid) %>%
      filter(debt != 0)

    debt <- bind_rows(debt, payp_tbl) %>%
      select(Name, debt) %>%
      left_join(current_pot(), by = "Name") %>%
      mutate(pot = ifelse(is.na(pot), 0, pot),
             `Pay by month` = 10 - pot,
             `Pay by game` = debt)
    
    debt
    
    
  })
 

# plotting ----------------------------------------------------------------

  output$no_week <- renderText(
    paste0("  WEEK ", which_week(Sys.Date()))
  )

  output$pot <- renderPlotly({
    
    plot_ly(current_pot()) %>%
      add_pie(values = ~pot, labels = ~Name, textinfo = "text",
              text = ~paste0(Name, ": ", "£", pot),
              hoverinfo = "text",
              hole = 0.3,
              title = ~paste0("£", sum(pot))) %>%
      config(displayModeBar = FALSE)
    
  })
  
  output$debit <- renderTable({
    
    debt() %>%
      filter(debt > 0) %>%
      select(Name, `Pay by game`, `Pay by month`)
    
  })
  
  output$credit <- renderTable({
    
    debt() %>%
      filter(debt < 0) %>%
      mutate(debt = debt * (-1)) %>%
      select(Name, Credit = debt)
    
  })

})

