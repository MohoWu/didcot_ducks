library(shiny)
library(shinymaterial)
library(plotly)

shinyUI(material_page(
  title = "Didcot Ducks",
  nav_bar_color = "yellow lighten-1",
  font_color = "deep-purple",
  background_color = "yellow accent-1",
  # define tabs
  material_tabs(
    tabs = c(
      "Books" = "books",
      "About" = "about"
    ),
    color = "pink"
  ),
  
  # books tab
  material_tab_content(
    tab_id = "books",
    # week number
    span(tags$h4(textOutput("no_week")), style="color:#e65100"),
    # donut chart
    material_card(
      title = span("MONEY IN POT", style="color:#9c27b0"),
      color = "amber accent-1",
      depth = 5,
      plotlyOutput("pot")
    ),
    # debt
    material_card(
      title = span("PLAYERS BOOK", style="color:#9c27b0"),
      color = "amber accent-2",
      depth = 5,
      span("In Debit", style="color:#9c27b0"),
      tableOutput("debtit"),
      span("In Credit", style="color:#9c27b0"),
      tableOutput("credit"),
      includeHTML("payment.html")
    )
  ),
  
  # about tab
  material_tab_content(
    tab_id = "about",
    includeHTML("about.html")
  )
)
)
