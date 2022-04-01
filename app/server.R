library(shiny)
library(dplyr)
library(magrittr)
library(tibble)
library(ggplot2)
library(latex2exp)
library(RColorBrewer)

source("dynamic_ui_utils.R")
source("plotting_utils.R")

# An illustrative example
ID = ""

beta_t = 4.7
data_dir = "data"

shinyServer(function(input, output, session) {
    # ==================================================================== 
    # REACTIVE EXPRESSION(S) - reactive conductor(s)
    # ====================================================================

    # get the beta csv file for plotting
    df_beta <- reactive({
        if (is.null(input$beta_csv)) {
            fpath = file.path(data_dir, paste("beta", ID, ".csv", sep = ""))
            df = read.csv(fpath)
            df = as_tibble(df)

        } else {
            df = read.csv(input$beta_csv$datapath)
            df = as_tibble(df)
        }
        
        df$load_comb = as.factor(df$load_comb)
        df$f_cck = as.factor(df$f_cck)
        df$rho = as.factor(df$rho)
        df$d = as.factor(df$d)
        df$d_lower = as.factor(df$d_lower)
        df$a_to_d_ratio = as.factor(df$a_to_d_ratio)
        df$weight_logic = df$weight > 0

        return(df)
    })


    beta_plot_gg <- reactive({
        hvar = input$hvar
        hfacet = input$hfacet
        vfacet = input$vfacet
        color = input$color

        f_cck_ii = input$f_cck
        d_ii = input$d
        d_lower_ii = input$d_lower
        a_to_d_ratio_ii = input$a_to_d_ratio
        
        show_title = input$plot_title_beta
        
        # handle the initialization of the dynamically generated UI (avoid 
        # displaying errors)
        if (is.null(f_cck_ii)){
          g = NULL
        } else {
          df_b = df_beta()
          
          g = beta_ggplot(
            df_beta=df_b, hvar=hvar, hfacet=hfacet, vfacet=vfacet,
            color=color, f_cck_ii=f_cck_ii, d_ii=d_ii, rho_ii=NULL,
            d_lower_ii=d_lower_ii, a_to_d_ratio_ii=a_to_d_ratio_ii,
            show_title=show_title)
        }

        return(g)
    })


    # get the alpha csv file for plotting
    df_alpha <- reactive({
        if (is.null(input$alpha_csv)) {
            fpath = file.path(data_dir, paste("alpha2", ID, ".csv", sep = ""))
            df = read.csv(fpath)
            df = as_tibble(df)
        } else {
            df = read.csv(input$alpha_csv$datapath)
            df = as_tibble(df)
        }

        df$load_comb_r = as.factor(df$load_comb_r)
        df$f_cck_r = as.factor(df$f_cck_r)
        df$rho_r = as.factor(df$rho_r)
        df$d_r = as.factor(df$d_r)
        df$weight_logic_r = df$weight_r > 0
        df$chi1_r = as.factor(df$chi1_r)
        df$chi2_r = as.factor(df$chi2_r)

        return(df)
    })


    alphachi1_plot_gg <- reactive({
        f_cck_ii = input$f_cck_alpha
        d_ii = input$d_alpha
        rho_ii = input$rho_alpha
        d_lower_ii = input$d_lower_alpha
        a_to_d_ratio_ii = input$a_to_d_ratio_alpha
        load_comb_ii = input$load_comb_alpha
        chi2_ii = input$chi2_alpha
        
        combine_to_e_r = input$combine_to_e_r
        show_title = input$plot_title_alpha

        # handle the initialization of the dynamically generated UI (avoid 
        # displaying errors)
        if (is.null(f_cck_ii)){
          g = NULL
        } else {
          g = alpha_chi1_ggplot(
            df_alpha = df_alpha(),
            f_cck_ii = f_cck_ii,
            d_ii = d_ii,
            rho_ii = rho_ii,
            d_lower_ii = d_lower_ii,
            a_to_d_ratio_ii = a_to_d_ratio_ii,
            load_comb_ii = load_comb_ii,
            chi2_ii = chi2_ii,
            combine_pos_neg = combine_to_e_r,
            show_title = show_title
          )
        }

        return(g)
    })


    alphachi2_plot_gg <- reactive({
        f_cck_ii = input$f_cck_alpha
        d_ii = input$d_alpha
        rho_ii = input$rho_alpha
        d_lower_ii = input$d_lower_alpha
        a_to_d_ratio_ii = input$a_to_d_ratio_alpha
        load_comb_ii = input$load_comb_alpha
        chi1_ii = input$chi1_alpha

        combine_to_e_r = input$combine_to_e_r
        show_title = input$plot_title_alpha
        
        # handle the initialization of the dynamically generated UI (avoid 
        # displaying errors)
        if (is.null(f_cck_ii)){
          g = NULL
        } else {
          g = alpha_chi2_ggplot(
            df_alpha = df_alpha(),
            f_cck_ii = f_cck_ii,
            d_ii = d_ii,
            rho_ii = rho_ii,
            d_lower_ii = d_lower_ii,
            a_to_d_ratio_ii = a_to_d_ratio_ii,
            load_comb_ii = load_comb_ii,
            chi1_ii = chi1_ii,
            combine_pos_neg = combine_to_e_r,
            show_title = show_title
          )
        }

        return(g)
    })


    # ==================================================================== 
    # OBSERVER(S) - reactive endpoint(s)
    # ====================================================================

    # Render beta plot
    output$beta_plot <- renderPlot({
      validate(
        need(!is.null(beta_plot_gg()), 'Initializing...')
      )
      
      beta_plot_gg()
    })

    # Download beta plot
    output$download_beta_plot <- downloadHandler(
        filename = function() {
          paste("beta_plot_", format(Sys.time(), "%Y-%b-%d_%H.%M.%S"), ".png", sep="")
        }, 
        content = function(file) {
          units = "cm"
          width = input$width_beta
          height = width / input$aspect_ratio_beta
          dpi = input$dpi_beta
          
          ggsave(
            file, plot = beta_plot_gg(), device = "png", dpi = dpi,
            width = width, height = height, units = "cm")
    })

    # Render alpha-chi1 plot
    output$alphachi1_plot <- renderPlot({
      validate(
        need(!is.null(alphachi1_plot_gg()), 'Initializing...')
      )
      
      alphachi1_plot_gg()
    })

    # Download alpha-chi1 plot
    output$download_alphachi1_plot <- downloadHandler(
        filename = function() {
          paste("alpha1_chi2_plot_", format(Sys.time(), "%Y-%b-%d_%H.%M.%S"), ".png", sep="")
        },
        content = function(file) {
          units = "cm"
          width = input$width_alpha2
          height = width / input$aspect_ratio_alpha2
          dpi = input$dpi_alpha2
          
          ggsave(
            file, plot = alphachi1_plot_gg(), device = "png", dpi = dpi,
            width = width, height = height, units = "cm")
        })

    # Render alpha-chi2 plot
    output$alphachi2_plot <- renderPlot({
      validate(
        need(!is.null(alphachi2_plot_gg()), 'Initializing...')
      )
      
      alphachi2_plot_gg()
    })

    # Download alpha-chi2 plot
    output$download_alphachi2_plot <- downloadHandler(
        filename = function () {
          paste("alpha2_chi2_plot_", format(Sys.time(), "%Y-%b-%d_%H.%M.%S"), ".png", sep="")
        },
        content = function(file) {
            units = "cm"
            width = input$width_alpha2
            height = width / input$aspect_ratio_alpha2
            dpi = input$dpi_alpha2
            
            ggsave(
              file, plot = alphachi2_plot_gg(), device = "png", dpi = dpi,
              width = width, height = height, units = "cm")
        })
    
    
    # .................................................................
    # Dynamic UI
    # .................................................................
    
    # inputs to select design scenarios - beta tab
    output$beta_parameters_input <- renderUI(
        expr={
          df = df_beta()
          d_unique = unique(df$d)
          f_cck_unique = unique(df$f_cck)
          d_lower_unique = unique(df$d_lower)
          a_to_d_ratio_unique = unique(df$a_to_d_ratio)
          
          s_d = parameter_input(x=d_unique, inputId="d", label=HTML("<i>d</i> [mm]:"))
          s_f_cck = parameter_input(
            x=f_cck_unique, 
            inputId="f_cck", 
            label=HTML("<i>f</i><sub>ck</sub> [MPa]:")
          )
          s_d_lower = parameter_input(x=d_lower_unique, inputId="d_lower", label=HTML("<i>d</i><sub>g</sub> [mm]:"))
          s_a_to_d_ratio = parameter_input(x=a_to_d_ratio_unique, inputId="a_to_d_ratio", label=HTML("<i>a/d</i> [-]:"))
          
          list(s_f_cck, s_d, s_d_lower, s_a_to_d_ratio)
        }
    )
    
    # inputs to select design scenarios - alpha^2 tab
    output$alpha_parameters_input <- renderUI(
      expr={
        df = df_alpha()
        
        load_comb_unique = unique(df$load_comb_r)
        chi1_unique = unique(df$chi1_r)
        chi2_unique = unique(df$chi2_r)
        f_cck_unique = unique(df$f_cck_r)
        d_unique = unique(df$d_r)
        rho_unique = unique(df$rho_r)
        d_lower_unique = unique(df$d_lower_r)
        a_to_d_ratio_unique = unique(df$a_to_d_ratio_r)
        
        
        selected = load_comb_unique[round(length(load_comb_unique)/2 + 0.5)]
        s_load_comb = selectInput(inputId="load_comb_alpha", label="load combination:", choices=load_comb_unique, selected=selected)
        s_chi1 = parameter_input(x=chi1_unique, inputId="chi1_alpha", label=HTML("&chi;<sub>1</sub> [-]:"))
        
        # chi2 should only be available when two actions are present
        s_chi2 = parameter_input(x=chi2_unique, inputId="chi2_alpha", label=HTML("&chi;<sub>2</sub> [-]:"))
        cs_chi2 = conditionalPanel(
          condition = "input.load_comb_alpha != 'traffic'",
          s_chi2
        )
        
        s_f_cck = parameter_input(x=f_cck_unique, inputId="f_cck_alpha", label=HTML("<i>f</i><sub>ck</sub> [MPa]:"))
        s_d = parameter_input(x=d_unique, inputId="d_alpha", label=HTML("<i>d</i> [mm]:"))
        s_rho = parameter_input(x=rho_unique, inputId="rho_alpha", label=HTML("<i>&rho;</i><sub>l</sub> [-]:"))
        s_d_lower = parameter_input(x=d_lower_unique, inputId="d_lower_alpha", label=HTML("<i>d</i><sub>g</sub> [mm]:"))
        s_a_to_d_ratio = parameter_input(x=a_to_d_ratio_unique, inputId="a_to_d_ratio_alpha", label=HTML("<i>a/d</i> [-]:"))
        
        list(s_load_comb, s_chi1, cs_chi2, s_f_cck, s_d, s_rho, s_d_lower, s_a_to_d_ratio)
      }
    )
    

})
