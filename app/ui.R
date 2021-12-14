library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem(HTML("&beta; plots"), tabName = "beta", icon=icon("image"), selected = TRUE),
              menuItem(HTML("&alpha;<sup>2</sup> plots"), tabName = "alpha2", icon=icon("image")),
              menuItem("About", tabName = "about", icon = icon("info"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "beta",
            fluidRow(
              box(width = 3, title="Input",
                     fileInput("beta_csv", "Choose a beta CSV file",
                               multiple = FALSE,
                               accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")),
                     selectInput(inputId = "hvar",
                                 label = "Horizontal variable",
                                 choices = list("chi1", "chi2"),
                                 selected = "chi1"),
                     selectInput(inputId = "hfacet",
                                 label = "Horizontal facet",
                                 choices = list("none", "load_comb", "rho", "chi1", "chi2"),
                                 selected = "load_comb"),
                     selectInput(inputId = "vfacet",
                                 label = "Vertical facet",
                                 choices = list("none", "load_comb", "rho", "chi1", "chi2"),
                                 selected = "rho"),
                     selectInput(inputId = "color",
                                 label = "Color",
                                 choices = list("none", "load_comb", "rho", "chi1", "chi2"),
                                 selected = "chi2"),
                      selectInput(inputId = "f_cck",
                                  label = "f_cck [MPa]:",
                                  choices = list(30, 60, 80),
                                  selected = 30),
                     sliderInput(inputId = "d",
                                 label = "d [mm]:",
                                 min = 100,
                                 max = 130,
                                 step = 10,
                                 value = 100),
                     downloadButton('download_beta_plot', 'Download Plot')
              ),
              fluidRow(
                box(width = 8, title="Reliability index plot",
                       plotOutput(outputId = "beta_plot")
                )
              )
            )
    ),
    tabItem(tabName = "alpha2",
            fluidRow(
              box(width = 3, title="Input",
                     fileInput("alpha_csv", "Choose an alpha CSV file",
                               multiple = FALSE,
                               accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")),
                    selectInput(inputId = "f_cck_alpha",
                                label = "f_cck [MPa]:",
                                choices = list(30, 60, 80),
                                selected = 30),
                    sliderInput(inputId = "d_alpha",
                                label = "d [mm]:",
                                min = 100,
                                max = 130,
                                step = 10,
                                value = 100),   
                     sliderInput(inputId = "rho_alpha",
                                 label = "rho_s [-]:",
                                 min = 0.005,
                                 max = 0.015,
                                 step = 0.005,
                                 value = 0.01),
                     selectInput(inputId = "load_comb_alpha",
                                 label = "load combination:",
                                 choices = list('traffic', 'snow_wind', 'snow_imposed', 'wind_imposed'),
                                 selected = 'wind_imposed'),  
                     sliderInput(inputId = "chi2_alpha",
                                 label = "chi2 [-]:",
                                 min = 0.0,
                                 max = 0.9,
                                 step = 0.1,
                                 value = 0.3),
                     downloadButton('download_alphachi1_plot', 'Download Plot 1'),      
                     sliderInput(inputId = "chi1_alpha",
                                 label = "chi1 [-]:",
                                 min = 0.1,
                                 max = 0.9,
                                 step = 0.1,
                                 value = 0.3),      
                     downloadButton('download_alphachi2_plot', 'Download Plot 2')
              ),
              fluidRow(
                box(width = 8, title="Squared sensitivity factor plots",
                       HTML("<h3>&alpha;<sup>2</sup> - &chi;<sub>1</sub> plot (Plot 1)<h3>"),
                       plotOutput(outputId = "alphachi1_plot"),
                       HTML("<h3>&alpha;<sup>2</sup> - &chi;<sub>2</sub> plot (Plot 2)<h3>"),
                       plotOutput(outputId = "alphachi2_plot")
                )
              )
            )
    ),
    tabItem(tabName = "about",
            includeMarkdown("about.Rmd")
    )
  )
)

dashboardPage(
  dashboardHeader(title = "Explore calibration results"),
  sidebar,
  body,
  skin = "blue"
)