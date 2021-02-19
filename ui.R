# ui.R

# dependencies
library(shinydashboard)
library(leaflet)
library(DT)

# header
header <- dashboardHeader(
  title = "brwAAAnalytics"
  )

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Gelijktijdigheid", tabName = "gelijktijdigheid", icon = icon("clock-o")
    )
  )
)

# body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "gelijktijdigheid",
            fluidRow(
              column(width = 12,
                     box(width = 6,
                         solidHeader = TRUE,
                         title = "Zoek naar gelijktijdige incidenten tijdens incidenten met de volgende parameters:",
                         box(width = 4, background = "blue", height = "200px",
                             uiOutput("items_vanprio")
                         ),
                         box(width = 4, background = "blue", height = "200px",
                             uiOutput("items_vangebied")
                         ),
                         box(width = 4, background = "blue", height = "200px",
                             uiOutput("items_vantype")
                         )
                     ),
                     box(width = 6,
                         solidHeader = TRUE,
                         title = "Waarbij de gelijktijdige incidenten deze parameters hebben:",
                         box(width = 4, background = "orange", height = "200px",
                             uiOutput("items_naarprio")
                         ),
                         box(width = 4, background = "orange", height = "200px",
                             uiOutput("items_naargebied")
                         ),
                         box(width = 4, background = "orange", height = "105px",
                             uiOutput("items_naartype")
                         ),
                         box(width = 4, background = "green", height = "75px",
                             actionButton("bereken", "Start"),
                             checkboxInput("debug", "debug", FALSE))
                     )
              )
            ),
            fluidRow(
              column(width = 12,
                     box(width = 12,
                         div(DT::dataTableOutput("gelijkdata"), style = "font-size:75%")
                         )
                     )
            ),
            conditionalPanel(
              condition = "(typeof input.gelijkdata_rows_selected !== 'undefined' && input.gelijkdata_rows_selected.length > 0)",
              fluidRow(
                column(width = 12,
                       box(width = 12,
                           div(DT::dataTableOutput("gelijkdatasub"), style = "font-size:75%"),
                           leafletOutput("kaart", width = "100%", height = 800)
                       )
                )
              )
            )
      )
    
  )
)

dashboardPage(
  header,
  sidebar,
  body,
  title = "Brandweer Amsterdam-Amstelland Analytics",
  skin = "red"
)
