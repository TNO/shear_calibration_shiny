library(dplyr)
library(magrittr)
library(tibble)
library(ggplot2)
library(latex2exp)
library(RColorBrewer)


# --------------------------------------------------------------
# Constants and settings for plots
# --------------------------------------------------------------

beta_target = 4.7
axis_label_text_size = 14

# order!
effect_resistance_df_labels = c("effect (alpha > 0)", "resistance (alpha < 0)")
effect_resistance_legend_labels = c(TeX("$E$ ($\\alpha > 0$)"), TeX("$R$ ($\\alpha \\leq 0$)"))

# must be harmonized with how Matlab saves the results! (`save_results_for_visu.m`)
rv_df_labels = rev(c('theta_R', 'f_c', 'd', 'b', 'A_sl', 'G', 'theta_G', 'Q1', 'theta_Q1', 'Q2', 'theta_Q2', 'theta_E'))
rv_legend_labels = rev(c(
  TeX("$\\theta_{R}$"), TeX("$f_{c}$"), TeX("$d$"), 
  TeX("$b_{w}$"), TeX("$A_{sl}$"), TeX("$V_{G}$"),
  TeX("$\\theta_G$"), TeX("$V_{Q1}$"), TeX("$\\theta_{Q1}$"),
  TeX("$V_{Q2}$"), TeX("$\\theta_{Q2}$"), TeX("$\\theta_{E}$")
  ))

rv_label_map <- function(x){
  names(rv_legend_labels) <- rv_df_labels
  return(unlist(rv_legend_labels[x]))
}


# aesthetic mapping
italic = TRUE
aes_df_labels = c("beta", "chi1", "chi2", "rho", "weight", "load_comb")
aes_legend_labels = c(
  TeX("$\\beta$", italic=italic), TeX("$\\chi_1$", italic=italic),
  TeX("$\\chi_2$", italic=italic), TeX("$\\rho$", italic=italic),
  TeX("$w$", italic=italic), "load comb.")

# --------------------------------------------------------------
# Supporting functions
# --------------------------------------------------------------

aes_label_map <- function(x){
  names(aes_legend_labels) <- aes_df_labels
  return(unlist(aes_legend_labels[x]))
}

# facet labellers
facet_label_map <- function(facet_column_name, value){
  if (facet_column_name == "load_comb"){
    lc = gsub("_", "-", value)
    facet_label = bquote("load comb.:"~.(lc))
  } else if (facet_column_name == "chi1"){
    value = signif(as.numeric(value), 2)
    facet_label = bquote(italic(chi)*""["1"]*": "*.(value))
  } else if (facet_column_name == "chi2"){
    value = signif(as.numeric(value), 2)
    facet_label = bquote(italic(chi)*""["2"]*": "*.(value))
  } else if (facet_column_name == "rho"){
    value = signif(as.numeric(value), 2)
    facet_label = bquote(italic(rho[l])~": "~.(value))
  } else stop("Unknown facet_column_name.")
  
  return(facet_label)
}

facet_labeller <- function(facet_column_name, values){
  if (is.factor(values)){
    values = levels(values)
  }
  facet_labels = lapply(values, function(x) facet_label_map(facet_column_name, x))
  return(facet_labels)
}

filter_if_not_null <- function(df_to_filter, filtering_column, filter_value){
  if (is.null(filter_value) == TRUE) {
    df = df_to_filter
  } else {
    df = df_to_filter%>% filter(
      .data[[filtering_column]] == filter_value
    )
  }
  return(df)
}

# ==============================================================
# Functions
# ==============================================================

# ..............................................................
# Beta facet plot
# ..............................................................
prepare_aes <- function(df_target, df_source, aes_var_name, column_to_aes){
  if (column_to_aes == "none") {
    df_target[[aes_var_name]] = "none"
  } else {
    df_target[[aes_var_name]] = as.factor(df_source[[column_to_aes]])
  }
  
  return(df_target)
}

beta_ggplot <- function(
    df_beta, hvar, hfacet, vfacet, color, f_cck_ii, d_ii, rho_ii,
    d_lower_ii, a_to_d_ratio_ii, show_title = TRUE
    ){
  ## Plot reliability indices (betas).
  ##
  ## If you want to facet along a variable that is also input parameter then you
  ## can do that by proving it with `NULL` value). For example:
  ##   * This will create vertical faceting but will have only a single `rho` 
  ##     value (0.01):
  ##       `beta_ggplot(...,vfacet="rho", rho_ii=0.01)`
  ##   * This will create vertical faceting but with `rho` values in the 
  ##     dataframe:
  ##       `beta_ggplot(...,vfacet="rho", rho_ii=NULL)`
  ##   * This will create not create vertical faceting:
  ##       `beta_ggplot(...,vfacet="none", rho_ii=0.01)`
  
  # variable to group by: to properly connect the points (not perfect)
  if (hvar == "chi1"){
    group_col = "chi2"
  } else if (hvar == "chi2"){
    group_col = "chi1"
  } else {
    group_col = "none"
  }
  
  # ...........................................
  # Create the to-be-visualized dataframe
  # ...........................................
  df = tibble(beta = df_beta$beta, weight = df_beta$weight, weight_logic = df_beta$weight_logic, group = df_beta[[group_col]])
  
  # aesthetics
  df$hvar = df_beta[[hvar]]
  df = prepare_aes(df, df_beta, aes_var_name="hfacet", column_to_aes=hfacet)
  df = prepare_aes(df, df_beta, aes_var_name="vfacet", column_to_aes=vfacet)
  df = prepare_aes(df, df_beta, aes_var_name="color", column_to_aes=color)

  
  df$f_cck = as.factor(df_beta$f_cck)
  df$d = as.factor(df_beta$d)
  df$rho = as.factor(df_beta$rho)
  df$d_lower = as.factor(df_beta$d_lower)
  df$a_to_d_ratio = as.factor(df_beta$a_to_d_ratio)
  
  df = filter_if_not_null(df, "f_cck", f_cck_ii)
  df = filter_if_not_null(df, "d", d_ii)
  df = filter_if_not_null(df, "rho", rho_ii)
  df = filter_if_not_null(df, "d_lower", d_lower_ii)
  df = filter_if_not_null(df, "a_to_d_ratio", a_to_d_ratio_ii)

  
  # Prepare the labels to be plotted
  if (vfacet == "none"){
    facet_rows = NULL
  } else {
    facet_rows = ggplot2::vars(vfacet)
    vfacet_levels = unique(df$vfacet)
    
    if (is.factor(vfacet_levels)){
      vfacet_levels = levels(vfacet_levels) 
    }
    df$vfacet <- factor(df$vfacet,
                        levels=vfacet_levels,
                        labels=facet_labeller(vfacet, vfacet_levels)
                        )
    
  }
  
  if (hfacet == "none"){
    facet_cols = NULL 
  } else {
    facet_cols = ggplot2::vars(hfacet)
    hfacet_levels = unique(df$hfacet)
    
    if (is.factor(hfacet_levels)){
      hfacet_levels = levels(hfacet_levels) 
    }
    df$hfacet <- factor(df$hfacet,
                        levels=hfacet_levels,
                        labels=facet_labeller(hfacet, hfacet_levels)
    )
  }
  
  # for overlay plot - indicate weight range (not the best solution)
  dfw = df
  idx = df$weight_logic
  dfw = dfw[!idx, ]
  
  # ...........................................
  # Visualize
  # ...........................................
  g = ggplot(df, aes(x = hvar, y = beta, color = color, group = group))
  g = g + geom_point(mapping = aes(size = weight), shape = 16, alpha = 0.7)
  g = g + geom_path(size=1)
  g = g + geom_hline(yintercept = beta_target, linetype="dashed")
  g = g + facet_grid(cols=facet_cols, rows=facet_rows, labeller = label_parsed)
  g = g + geom_point(data = dfw, color = "white", fill = "white", size = 1, stroke = 0)
  g = g + scale_color_brewer(palette = "RdYlGn", name = aes_label_map(color))
  g = g + scale_size_continuous(range = c(1.5, 4), name=aes_label_map("weight"))
  
  if (show_title == TRUE){
    g = g + ggtitle(paste(
      "f_cck = ", f_cck_ii, "; d = ", d_ii, 
      "; d_g = ", d_lower_ii, "; a/d = ", a_to_d_ratio_ii, sep = ""
      ))
  }
  
  g = g + theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    
  )
  g = g + ylab(TeX("$\\beta$"))
  g = g + xlab(aes_label_map(hvar))
  g = g + xlim(c(0, 1))
  
  return(g)
}


# ..............................................................
# Alpha^2 - Chi plots
# ..............................................................

alpha_chi1_ggplot <- function(
    df_alpha, f_cck_ii, d_ii, rho_ii, d_lower_ii, a_to_d_ratio_ii,
    load_comb_ii, chi2_ii, combine_pos_neg = TRUE, show_title = TRUE
    ){
  
  # load combination
  load_1_name = strsplit(load_comb_ii, "_")[[1]][1]
  if (load_1_name == "traffic"){
    chi2_ii = 0
  }
  
  df_chi1 = df_alpha %>%
    filter(f_cck_r == f_cck_ii & d_r == d_ii & rho_r == rho_ii &
             chi2_r == chi2_ii & load_comb_r == load_comb_ii &
             d_lower_r == d_lower_ii & a_to_d_ratio_r == a_to_d_ratio_ii)
  
  if (combine_pos_neg == TRUE){
    idx_pos = df_chi1$alpha_r >= 0
    idx_neg = !idx_pos
    
    df_chi1$alpha_labels[idx_pos] = effect_resistance_df_labels[1]
    df_chi1$alpha_labels[idx_neg] = effect_resistance_df_labels[2]
    
    df_chi1 = df_chi1 %>% 
      group_by(chi1_r, alpha_labels) %>% 
      summarise(alpha2_r = sum(alpha2_r), .groups="keep")

    df_chi1$alpha_labels = as.factor(df_chi1$alpha_labels)
    
    fill_df_labels = effect_resistance_df_labels
    fill_legend_labels = effect_resistance_legend_labels
    
  } else {
    # TODO: improve, allow for a subset of RVs
    fill_df_labels = rv_df_labels
    fill_legend_labels = rv_legend_labels
  }
  
  df_chi1_new = data.frame(chi1_r = df_chi1$chi1_r, alpha2_r = df_chi1$alpha2_r, alpha_labels = df_chi1$alpha_labels)
  df_chi1_new$chi1_r = as.numeric(as.character(df_chi1_new$chi1_r))
  
  
  # ensure the desired order
  df_chi1_new$alpha_labels <- factor(df_chi1_new$alpha_labels, levels=fill_df_labels)
  
  # extend color palette beyond the default maximum number of distinct colors
  num_rv = length(unique(df_chi1_new$alpha_labels))
  rv_colors = colorRampPalette(brewer.pal(12, "Paired"))(num_rv)
  
  g = ggplot(df_chi1_new, aes(x = chi1_r, y = alpha2_r, fill = alpha_labels))
  g = g + geom_area(alpha = 0.6, size = 1, colour = "black")
  
  if (show_title) {
    g = g + ggtitle(paste("f_ck = ", f_cck_ii, "; d = ", d_ii,
                          "; rho = ", rho_ii,  "; d_g = ", d_lower_ii, 
                          "; a/d = ", a_to_d_ratio_ii,
                          "; chi2 = ", chi2_ii, "; load_comb = ", load_comb_ii,
                          sep = "")
                    )
  }
  
  if (combine_pos_neg == TRUE){
    g = g + geom_hline(yintercept=0.8^2, linetype="dashed")
    g = g + annotate("text", x=0.1+0.05, y=0.8^2+0.04,
                     label=TeX("$\\alpha_{R,EC0}^{2} = 0.8^2$"), hjust=0)
  }
  g = g + scale_fill_manual(values=rv_colors, labels=fill_legend_labels)
  # g = g + scale_fill_manual(values=rv_colors)
  
  g = g + xlab(TeX(paste("$\\chi_{1,", load_1_name, "}$", sep="")))
  g = g + ylab(TeX("$\\alpha^2$"))
  g = g + labs(fill = "RVs")
  
  g = g + theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_text(size = axis_label_text_size),
    axis.title.y = element_text(size = axis_label_text_size),
    panel.grid.minor = element_blank(),
    legend.margin = margin(t = 0, unit='cm'),
    legend.text.align = 0,
  )
  g = g + scale_x_continuous(breaks = seq(0.1, 0.9, by = 0.1), expand = c(0, 0))
  g = g + scale_y_continuous(limits = c(-0.001, 1.001), expand = c(0, 0))
  
  return(g)
}



alpha_chi2_ggplot <- function(
    df_alpha, f_cck_ii, d_ii, rho_ii, d_lower_ii, a_to_d_ratio_ii,
    load_comb_ii, chi1_ii, combine_pos_neg = TRUE, show_title = TRUE
    ){
  
  # load combination
  load_2_name = strsplit(load_comb_ii, "_")[[1]][2]
  
  df_chi2 = df_alpha %>%
    filter(f_cck_r == f_cck_ii & d_r == d_ii & rho_r == rho_ii & 
             chi1_r == chi1_ii & load_comb_r == load_comb_ii &
             d_lower_r == d_lower_ii & a_to_d_ratio_r == a_to_d_ratio_ii)

  if (combine_pos_neg == TRUE){
    idx_pos = df_chi2$alpha_r >= 0
    idx_neg = !idx_pos
    
    df_chi2$alpha_labels[idx_pos] = effect_resistance_df_labels[1]
    df_chi2$alpha_labels[idx_neg] = effect_resistance_df_labels[2]
    
    df_chi2 = df_chi2 %>% 
      group_by(chi2_r, alpha_labels) %>% 
      summarise(alpha2_r = sum(alpha2_r), .groups="keep")
    
    df_chi2$alpha_labels = as.factor(df_chi2$alpha_labels)
    
    fill_df_labels = effect_resistance_df_labels
    fill_legend_labels = effect_resistance_legend_labels
  } else {
    # TODO: improve, allow for a subset of RVs
    fill_df_labels = rv_df_labels
    fill_legend_labels = rv_legend_labels
  }
  
  df_chi2_new = data.frame(chi2_r = df_chi2$chi2_r, alpha2_r = df_chi2$alpha2_r, alpha_labels = df_chi2$alpha_labels)
  df_chi2_new$chi2_r = as.numeric(as.character(df_chi2_new$chi2_r))
  
  # ensure the desired order
  df_chi2_new$alpha_labels <- factor(df_chi2_new$alpha_labels, levels=fill_df_labels)
  
  # extend color palette beyond the default maximum number of distinct colors
  num_rv = length(unique(df_chi2_new$alpha_labels))
  rv_colors = colorRampPalette(brewer.pal(12, "Paired"))(num_rv)
  
  g = ggplot(df_chi2_new, aes(x = chi2_r, y = alpha2_r, fill = alpha_labels))
  g = g + geom_area(alpha = 0.6, size = 1, colour = "black")
  
  if (show_title) {
    g = g + ggtitle(paste("f_ck = ", f_cck_ii, "; d = ", d_ii,
                          "; rho = ", rho_ii,  "; d_g = ", d_lower_ii, 
                          "; a/d = ", a_to_d_ratio_ii,
                          "; chi1 = ", chi1_ii, "; load_comb = ", load_comb_ii,
                          sep = "")
    )
  }
  if (combine_pos_neg == TRUE){
    g = g + geom_hline(yintercept=0.8^2, linetype="dashed")
    g = g + annotate("text", x=0.1+0.05, y=0.8^2+0.04, label=TeX("$\\alpha_{R,EC0}^{2} = 0.8^2$"))
  }
  g = g + scale_fill_manual(values=rv_colors, labels=fill_legend_labels)
  
  g = g + xlab(TeX(paste("$\\chi_{2,", load_2_name, "}$", sep="")))
  g = g + ylab(TeX("$\\alpha^2$"))
  g = g + labs(fill = "RVs")

  g = g + theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_text(size = axis_label_text_size),
    axis.title.y = element_text(size = axis_label_text_size),
    panel.grid.minor = element_blank(),
    legend.text.align = 0,
  )
  g = g + scale_x_continuous(breaks = seq(0.1, 0.9, by = 0.1), expand = c(0, 0))
  g = g + scale_y_continuous(limits = c(-0.001, 1.001), expand = c(0, 0))
  
  return(g)
}
