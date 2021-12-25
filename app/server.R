library(shiny)
library(dplyr)
library(magrittr)
library(tibble)
library(ggplot2)
library(latex2exp)


# An illustrative example
ID = ""

beta_t = 4.7

data_dir = "data/"
# variables = c('load_comb', 'rho', 'chi1', 'chi2') fpath = paste('beta', ID, '.csv',
# sep = '') dfb = read.csv(fpath) dfb = as.tibble(dfb)


shinyServer(function(input, output, session) {
    # ==================================================================== REACTIVE
    # EXPRESSION(S) - reactive conductor(s)
    # ====================================================================

    # get the beta csv file for plotting
    df_beta <- reactive({
        if (is.null(input$beta_csv)) {
            fpath = paste(data_dir, "beta", ID, ".csv", sep = "")
            df = read.csv(fpath)
            df = as.tibble(df)

        } else {
            df = read.csv(input$beta_csv$datapath)
            df = as.tibble(df)
        }

        df$load_comb = as.factor(df$load_comb)
        df$f_cck = as.factor(df$f_cck)
        df$rho = as.factor(df$rho)
        df$d = as.factor(df$d)
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

        df_b = df_beta()

        df = data.frame(beta = df_b$beta, weight = df_b$weight, weight_logic = df_b$weight_logic)
        df$hvar = df_b[[hvar]]

        if (hfacet == "none") {
            df$hfacet = "none"
        } else {
            df$hfacet = as.factor(df_b[[hfacet]])
        }

        if (vfacet == "none") {
            df$vfacet = "none"
        } else {
            df$vfacet = as.factor(df_b[[vfacet]])
        }

        if (color == "none") {
            df$color = "none"
        } else {
            df$color = as.factor(df_b[[color]])
        }

        df$f_cck = as.factor(df_b$f_cck)
        df$d = as.factor(df_b$d)

        df = df %>%
            filter(f_cck == f_cck_ii & d == d_ii)

        # for overlay plot - indicate weight range (not the best solution)
        dfw = df
        idx = df$weight_logic
        dfw = dfw[!idx, ]


        g = ggplot(df, aes(x = hvar, y = beta, color = color))
        g = g + geom_point(mapping = aes(size = weight), shape = 16, alpha = 0.7)
        g = g + geom_path()
        g = g + geom_hline(yintercept = beta_t)
        g = g + facet_grid(vfacet ~ hfacet, labeller = label_both)
        g = g + geom_point(data = dfw, color = "white", fill = "white", size = 1, stroke = 0)
        g = g + scale_color_discrete(name = color)
        g = g + scale_size_continuous(range = c(1.5, 4))

        g = g + ggtitle(paste("f_cck = ", f_cck_ii, "; d = ", d_ii, sep = ""))

        g = g + theme_bw()
        g = g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        g = g + ylab(TeX("$\\beta$"))
        g = g + xlab(hvar)
        g = g + xlim(c(0, 1))


        return(g)
    })




    # get the alpha csv file for plotting
    df_alpha <- reactive({
        if (is.null(input$alpha_csv)) {
            fpath = paste(data_dir, "alpha2", ID, ".csv", sep = "")
            df = read.csv(fpath)
            df = as.tibble(df)
        } else {
            df = read.csv(input$alpha_csv$datapath)
            df = as.tibble(df)
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
        load_comb_ii = input$load_comb_alpha
        chi2_ii = input$chi2_alpha

        df_a = df_alpha()

        df_chi1 = df_a %>%
            filter(f_cck_r == f_cck_ii & d_r == d_ii & rho_r == rho_ii & chi2_r == chi2_ii &
                load_comb_r == load_comb_ii)


        df_chi1_new = data.frame(chi1_r = df_chi1$chi1_r, alpha2_r = df_chi1$alpha2_r, alpha_labels = df_chi1$alpha_labels)

        df_chi1_new$chi1_r = as.numeric(as.character(df_chi1_new$chi1_r))

        g = ggplot(df_chi1_new, aes(x = chi1_r, y = alpha2_r, fill = alpha_labels))
        g = g + geom_area(alpha = 0.6, size = 1, colour = "black")
        g = g + ggtitle(paste("f_cck = ", f_cck_ii, "; d = ", d_ii, "; rho = ", rho_ii, "; chi2 = ",
            chi2_ii, "; load_comb = ", load_comb_ii, sep = ""))
        g = g + xlab(TeX("$\\chi_1$"))
        g = g + ylab(TeX("$\\alpha^2$"))
        g = g + labs(fill = "RV's")
        g = g + scale_fill_brewer(palette = "Paired")
        g = g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        g = g + scale_x_continuous(breaks = seq(0.1, 0.9, by = 0.1), expand = c(0, 0))
        g = g + scale_y_continuous(limits = seq(0, 1), expand = c(0, 0))

        return(g)
    })


    alphachi2_plot_gg <- reactive({
        f_cck_ii = input$f_cck_alpha
        d_ii = input$d_alpha
        rho_ii = input$rho_alpha
        load_comb_ii = input$load_comb_alpha
        chi1_ii = input$chi1_alpha

        df_a = df_alpha()

        df_chi2 = df_a %>%
            filter(f_cck_r == f_cck_ii & d_r == d_ii & rho_r == rho_ii & chi1_r == chi1_ii &
                load_comb_r == load_comb_ii)


        df_chi2_new = data.frame(chi2_r = df_chi2$chi2_r, alpha2_r = df_chi2$alpha2_r, alpha_labels = df_chi2$alpha_labels)

        df_chi2_new$chi2_r = as.numeric(as.character(df_chi2_new$chi2_r))

        g = ggplot(df_chi2_new, aes(x = chi2_r, y = alpha2_r, fill = alpha_labels))
        g = g + geom_area(alpha = 0.6, size = 1, colour = "black")
        g = g + ggtitle(paste("f_cck = ", f_cck_ii, "; d = ", d_ii, "; rho = ", rho_ii, "; chi1 = ",
            chi1_ii, "; load_comb = ", load_comb_ii, sep = ""))
        g = g + xlab(TeX("$\\chi_2$"))
        g = g + ylab(TeX("$\\alpha^2$"))
        g = g + labs(fill = "RV's")
        g = g + scale_fill_brewer(palette = "Paired")
        g = g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        g = g + scale_x_continuous(breaks = seq(0.1, 0.9, by = 0.1), expand = c(0, 0))
        g = g + scale_y_continuous(limits = seq(0, 1), expand = c(0, 0))

        return(g)
    })


    # ==================================================================== OBSERVER(S)
    # - reactive endpoint(s)
    # ====================================================================

    # observe({ hfacet = input$hfacet vfacet = input$vfacet choice1 = c('none',
    # setdiff(variables, c(vfacet))) choice2 = c('none', setdiff(variables, c(hfacet)))
    # updateSelectInput(session, 'hfacet', choices = choice1, selected = hfacet)
    # updateSelectInput(session, 'vfacet', choices = choice2, selected = vfacet) })

    # Render beta plot
    output$beta_plot <- renderPlot({
        beta_plot_gg()
    })

    # Download beta plot
    output$download_beta_plot <- downloadHandler(filename = "my_beta_plot.png", content = function(file) {
        ggsave(file, plot = beta_plot_gg(), device = "png", dpi = 600)
    })

    # Render alpha-chi1 plot
    output$alphachi1_plot <- renderPlot({
        alphachi1_plot_gg()
    })

    # Download alpha-chi1 plot
    output$download_alphachi1_plot <- downloadHandler(filename = "my_alphachi1_plot.png",
        content = function(file) {
            ggsave(file, plot = alphachi1_plot_gg(), device = "png", dpi = 600)
        })

    # Render alpha-chi2 plot
    output$alphachi2_plot <- renderPlot({
        alphachi2_plot_gg()
    })

    # Download alpha-chi2 plot
    output$download_alphachi2_plot <- downloadHandler(filename = "my_alphachi2_plot.png",
        content = function(file) {
            ggsave(file, plot = alphachi2_plot_gg(), device = "png", dpi = 600)
        })

})
