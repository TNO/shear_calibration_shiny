# not part of the shiny app, used to save plot for the paper
rm(list=ls(all=TRUE))

library(dplyr)
library(magrittr)
library(tibble)
library(ggplot2)
library(latex2exp)
library(ggh4x)
library(ggisoband)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("plotting_utils.R")


# =================================================================
# Input and options
# =================================================================
# ID = "_calibration_run_2022-Apr-02_16.02.38"
# ID = "_calibration_run_2022-Apr-12_15.50.14"
ID = "_calibration_run_2022-Apr-13_20.37.00"

show_title = FALSE
dpi = 400
data_dir = "data"
results_dir = "results"

# =================================================================
# Pre-process
# =================================================================

fpath = file.path(data_dir, paste("beta", ID, ".csv", sep = ""))
df_beta = read.csv(fpath)
df_beta = as_tibble(df_beta)
df_beta$weight_logic = df_beta$weight > 0

fpath = fpath = file.path(data_dir, paste("alpha2", ID, ".csv", sep = ""))
df_alpha = read.csv(fpath)
df_alpha = as_tibble(df_alpha)

# =================================================================
# Visualize
# =================================================================

# -----------------------------------------------------------------
# beta facet plot
# -----------------------------------------------------------------
# idx = 100

# f_cck_ii = df_beta$f_cck[idx]
# d_ii = df_beta$d[idx]
# rho_ii = df_beta$rho[idx]
# # rho_ii = NULL
# d_lower_ii = df_beta$d_lower[idx]
# a_to_d_ratio_ii = df_beta$a_to_d_ratio[idx]

f_cck_ii = 40
d_ii = 300
rho_ii = 0.01
d_lower_ii = df_beta$d_lower[1]
a_to_d_ratio_ii = df_beta$a_to_d_ratio[1]

hvar = "chi1"
color = "chi2"
# hvar = "chi2"
# color = "chi1"
hfacet = "load_comb"
vfacet = "none"


g = beta_ggplot(
  df_beta=df_beta, hvar=hvar, hfacet=hfacet, vfacet=vfacet,
  color=color, f_cck_ii=f_cck_ii, d_ii=d_ii, rho_ii=rho_ii, d_lower_ii=d_lower_ii,
  a_to_d_ratio_ii=a_to_d_ratio_ii, show_title=show_title)

plot(g)

# Save
width = 20
height = 0.5 * width
file = file.path(results_dir, paste("beta_plot", ID, ".png", sep=""))
ggsave(
  file, plot = g, device = "png", dpi = dpi,
  width = width, height = height, units = "cm"
)

# -----------------------------------------------------------------
# beta facet plot - mc2010 odd behavior
# -----------------------------------------------------------------

d_ii = 300
rho_ii = 0.01
d_lower_ii = df_beta$d_lower[1]
a_to_d_ratio_ii = df_beta$a_to_d_ratio[1]

hvar = "chi1"
color = "chi2"
hfacet = "load_comb"
vfacet = "f_cck"


g = beta_ggplot(
  df_beta=df_beta, hvar=hvar, hfacet=hfacet, vfacet=vfacet,
  color=color, f_cck_ii=NULL, d_ii=d_ii, rho_ii=rho_ii, d_lower_ii=d_lower_ii,
  a_to_d_ratio_ii=a_to_d_ratio_ii, show_title=show_title)

plot(g)

# Save
width = 20
height = 1.2 * width
file = file.path(results_dir, paste("beta_plot_fc_facet", ID, ".png", sep=""))
ggsave(
  file, plot = g, device = "png", dpi = dpi,
  width = width, height = height, units = "cm"
)

# -----------------------------------------------------------------
# chi - alpha2 plot
# -----------------------------------------------------------------
# idx = 100
# 
# f_cck_ii = df_alpha$f_cck_r[idx]
# d_ii = df_alpha$d_r[idx]
# rho_ii = df_alpha$rho_r[idx]
# load_comb_ii = df_alpha$load_comb_r[idx]
# chi2_ii = df_alpha$chi2_r[idx]
# d_lower_ii = df_alpha$d_lower_r[idx]
# a_to_d_ratio_ii = df_alpha$a_to_d_ratio_r[idx]

# figure 4
f_cck_ii = 40
d_ii = 300
rho_ii = 0.01
load_comb_ii = "traffic"
chi2_ii = 0
d_lower_ii = df_alpha$d_lower_r[1]
a_to_d_ratio_ii = df_alpha$a_to_d_ratio_r[1]

# figure 5
f_cck_ii = 40
d_ii = 300
rho_ii = 0.01
load_comb_ii = "wind_imposed"
chi2_ii = 0.1
d_lower_ii = df_alpha$d_lower_r[1]
a_to_d_ratio_ii = df_alpha$a_to_d_ratio_r[1]


combine_pos_negs = c(TRUE, FALSE)

for (combine_pos_neg in combine_pos_negs){
  g = alpha_chi1_ggplot(
    df_alpha, f_cck_ii, d_ii, rho_ii, load_comb_ii, chi2_ii,
    d_lower_ii=d_lower_ii, a_to_d_ratio_ii=a_to_d_ratio_ii,
    combine_pos_neg = combine_pos_neg, show_title = show_title)
  
  plot(g)
  
  
  # Save
  width = 10
  height = 0.9 * width
  file = file.path(results_dir, paste(
    "alpha2_chi1_plot", ID, "_combine_pos_neg=", as.character(combine_pos_neg), ".png", sep="")
  )
  
  # fix the size of the plotting panel
  gs = g + force_panelsizes(
    rows = unit(height*0.8, "cm"),
    cols = unit(width*0.6, "cm")
  )
  
  ggsave(
    file, plot = gs, device = "png", dpi = dpi,
    width = width, height = height, units = "cm"
  )
}

# -----------------------------------------------------------------
# alpha2 - chi1 - ch2 surface plot
# -----------------------------------------------------------------
# fragily and too specific code, TODO: clean up and generalize
rv_name = "theta_R"
# rv_name = "R"

alpha2_breaks = seq(0.25, 0.80, 0.05)
# alpha2_breaks = seq(0.2, 0.7, 0.05)
# alpha2_breaks = NULL 
chi_breaks = seq(0.1, 0.9, 0.1)

# ..............................................
if (rv_name == "R" | rv_name == "E"){
  group_by_cols = c(
    "alpha_labels", "load_comb_r", "d_r", "f_cck_r", "chi1_r",
    "chi2_r", "rho_r", "d_lower_r", "a_to_d_ratio_r")
  select_cols = append("alpha_r", group_by_cols)
  
  # Combined alpha_R and alpha_E
  df_combined_alpha = df_alpha %>% select_at(select_cols)
  
  idx_pos = df_alpha$alpha_r >= 0
  idx_neg = !idx_pos
  
  df_combined_alpha$alpha_labels[idx_pos] = "E"
  df_combined_alpha$alpha_labels[idx_neg] = "R"
  
  
  df_combined_alpha = df_combined_alpha %>% 
    group_by_at(group_by_cols) %>% 
    summarise(alpha2_r = sum(alpha_r^2), .groups="keep")

  rv_label = bquote(italic(.(rv_name)))
} else {
  df_combined_alpha = df_alpha
  rv_label = rv_label_map(rv_name)
}

df_aggr = df_combined_alpha %>% 
  filter(alpha_labels==rv_name & chi2_r != 0) %>%
  group_by(chi1_r, chi2_r) %>% 
  summarize(
    mean = mean(alpha2_r), median = mean(alpha2_r),
    min = min(alpha2_r), max = max(alpha2_r))



alpha2_isoline = 0.8^2
idx = which(df_aggr$median > alpha2_isoline)
y_isoline = max(df_aggr$chi2_r[idx])


g = ggplot(df_aggr, aes(x=chi1_r, y=chi2_r, z=median))
g = g + geom_isobands(
  aes(fill = median),
  color = NA,
  bins=7,
  # breaks=alpha2_breaks,
  alpha=0.7,
  polygon_outline = FALSE)

g = g + scale_fill_viridis_c(
  name=TeX("median $\\alpha^2$"),
  guide = guide_legend(reverse = TRUE),
  limits=c(min(alpha2_breaks), max(alpha2_breaks)),
  breaks=alpha2_breaks,
)

if (rv_name == "R"){
  g = g + stat_contour(breaks=c(0.8^2), color="black", linetype="dashed")
  
  g = g + annotate(
    "text",
    x=min(chi_breaks)+0.05,
    y=y_isoline+0.1,
    label=TeX("$\\alpha_{R,EC0}^{2} = 0.8^2$"),
    hjust=0,
    vjust=1
  )
}

g = g + annotate(
  "text", x=min(chi_breaks)+0.05, y=max(chi_breaks)-0.05,
  label=rv_label, hjust=0, vjust=1)

g = g + coord_fixed(ratio = 1, expand = FALSE)
g = g + scale_x_continuous(name=TeX("$\\chi_1$"), breaks=chi_breaks)
g = g + scale_y_continuous(name=TeX("$\\chi_2$"), breaks=chi_breaks)
g = g + theme_bw()
g = g + theme(
  panel.grid.minor = element_blank()
)

plot(g)

# Save
width = 12
height = 0.8 * width
file = file.path(results_dir, paste("alpha_contour", ID, "_", rv_name, ".png", sep=""))
ggsave(
  file, plot = g, device = "png", dpi = dpi,
  width = width, height = height, units = "cm"
)
