# functions
first_mondays <- function(year) {
  
  first_day <- paste0(year, "-01-01")
  last_day <- paste0(year, "12-31")
  
  x <- seq(lubridate::ymd(first_day), ymd(last_day), by="1 day")
  
  x[lubridate::wday(x, label = TRUE) == "Mon" & lubridate::day(x) <= 7]
}


which_week <- function(the_date, cycle = FALSE) {
  
  cycles <- c(as.Date(c("2019-10-07", "2019-11-04", "2019-12-02")), 
              first_mondays(2020))
  
  cycle_start_date <- purrr::map(the_date, ~cycles[max(which(.x >= cycles))])
  
  # cycle_start_date <- cycle[max(which(the_date >= cycle))]
  
  no_week <- purrr::map2_int(the_date, cycle_start_date,
                             ~as.integer(floor((.x - .y)/7) + 1))
  
  no_cycle <- purrr::map_dbl(the_date, ~max(which(.x >= cycles)))
  
  # return cycle number or week number
  if (cycle) no_cycle else as.numeric(no_week)
  
}
round_05 <- function(x) {
  digit <- x - floor(x)

    if (digit >= 0.5) {
    return(floor(x) + 0.5)
  }
  
  if (digit < 0.5) {
    return(floor(x))
  }
}

calc_pot <- function(old, current, to_pay = -50) {
  
  # update pot money
  data <- current %>% 
    full_join(old, by = "Name", suffix = c("_c", "_o")) %>%
    mutate(pot = purrr::map2_dbl(pot_o, pot_c, 
                                 sum, na.rm = TRUE)) 
  # calculate remaining money to divide
  remain <- sum(data$pot) + to_pay
  
  # allocate remaining to people who paid £10 in the pot
  if (to_pay != 0) {
    
    pot <- data %>%
      filter(pot >= 10,
             Type == "paym") %>%
      mutate(dividor = length(unique(Name)),
             pot = purrr::map_dbl(remain/dividor,
                                  round_05)) %>%
      select(Name, pot)
    
  } else {
    
    pot <- data %>%
      # filter(Type == "paym") %>%
      select(Name, pot)
  }
  
  pot
  
}

old_pot <- data.frame(Name = character(),
                      pot = numeric(0),
                      stringsAsFactors = FALSE)

payment_sheet <- "https://docs.google.com/spreadsheets/d/1_KxXTywIfc3uySg9eIGZ97YWTNUqtiam2HsrND7W9fU/export?usp=sharing&format=csv"

player_sheet <- "https://docs.google.com/spreadsheets/d/1qFj1TMgxHL37P-pT5UyTU3-ei5tEwrSFkBIp7-XB50s/export?usp=sharing&format=csv"