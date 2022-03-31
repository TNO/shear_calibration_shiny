# not part of the shiny app, used to save plot for the paper
rm(list=ls(all=TRUE))

library(dplyr)
library(magrittr)
library(tibble)
library(ggplot2)
library(latex2exp)
library(ggh4x)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("plotting_utils.R")


# =================================================================
# Input and options
# =================================================================
ID = ""

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
idx = 100

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

# hvar = "chi1"
# color = "chi2"
hvar = "chi2"
color = "chi1"
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
load_comb_ii = "snow_wind"
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
