library(shiny)
library(shinydashboard)
library(markdown)

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem(HTML("&beta; plots"), tabName = "beta", icon=icon("image"), selected = TRUE),
              menuItem(HTML("&alpha;<sup>2</sup> plots"), tabName = "alpha2", icon=icon("image")),
              menuItem("Data", tabName = "data", icon = icon("database")),
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
                     h5(HTML("Select <a href='ttps://cfss.uchicago.edu/notes/grammar-of-graphics/' target='_blank'>aesthetics</a>:")),
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
                  
                     h5("Select design scenario parameters:"),
                     # dynamically generated UI component, see the reactive endpoints of `server.R` 
                     uiOutput("beta_parameters_input"),
                  
                     checkboxInput(inputId="plot_title_beta", label=HTML("<b>Show plot title.</b>"), value=TRUE),
                     checkboxInput(inputId="beta_plot_settings", label=HTML("<b>Settings for downloading plots.</b>"), value=FALSE),
                     conditionalPanel(
                       condition = "input.beta_plot_settings == 1",
                       sliderInput(inputId = "width_beta", label = "Width [cm]", min = 10, max=60, value = 30, step = 5),
                       sliderInput(inputId = "aspect_ratio_beta", label = "Width to height ratio", min = 0.5, max=6, value = 3, step = 0.5),
                       sliderInput(inputId = "dpi_beta", label = "DPI", min = 100, max=1000, value = 400, step = 100),
                     ),
                     downloadButton('download_beta_plot', HTML('Download <i>&beta;</i> plot'))
              ),
              fluidRow(
                box(width = 8, title=HTML("Reliability index (<i>&beta;</i>) plot"),
                       plotOutput(outputId = "beta_plot"),
                       helpText(HTML("<ul>
                                       <li>Dashed black line: target reliability (<i>&beta;</i><sub>target</sub>).</li>
                                       <li>Empty circle: zero weight.</li>
                                     </ul>"))  
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
                     checkboxInput(inputId="combine_to_e_r", label=HTML("<b>Combine RVs into <i>E</i> and <i>R</i>.</b>"), value=FALSE),
                     h5("Select design scenario parameters:"),
                     # dynamically generated UI component, see the reactive endpoints of `server.R`
                     uiOutput("alpha_parameters_input"),

                     checkboxInput(inputId="plot_title_alpha", label=HTML("<b>Show plot title.</b>"), value=TRUE),
                     checkboxInput(inputId="alpha2_plot_settings", label=HTML("<b>Settings for downloading plots.</b>"), value=FALSE),
                     conditionalPanel(
                       condition = "input.alpha2_plot_settings == 1",
                       sliderInput(inputId = "width_alpha2", label = "Width [cm]", min = 10, max=60, value = 30, step = 5),
                       sliderInput(inputId = "aspect_ratio_alpha2", label = "Width to height ratio", min = 0.5, max=6, value = 3, step = 0.5),
                       sliderInput(inputId = "dpi_alpha2", label = "DPI", min = 100, max=1000, value = 400, step = 100),
                     ),
                     downloadButton('download_alphachi1_plot', HTML('Download <i>&alpha;</i><sup>2</sup> - <i>&chi;</i><sub>1</sub> plot')),
                     downloadButton('download_alphachi2_plot', HTML('Download <i>&alpha;</i><sup>2</sup> - <i>&chi;</i><sub>2</sub> plot'))
              ),
              fluidRow(
                box(width = 8, title=HTML("Squared sensitivity factor (<i>&alpha;</i><sup>2</sup>) plots"),
                       HTML("<h3><i>&alpha;</i><sup>2</sup> - <i>&chi;</i><sub>1</sub> plot<h3>"),
                       plotOutput(outputId = "alphachi1_plot"),
                       HTML("<h3><i>&alpha;</i><sup>2</sup> - <i>&chi;</i><sub>2</sub> plot<h3>"),
                       plotOutput(outputId = "alphachi2_plot")
                )
              )
            )
    ),
    tabItem(tabName = "data",
            includeMarkdown("data.Rmd")
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