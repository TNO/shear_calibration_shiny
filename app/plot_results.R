# not part of the shiny app, used to save plot for the paper
rm(list=ls(all=TRUE))

library(dplyr)
library(magrittr)
library(tibble)
library(ggplot2)
library(latex2exp)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("plotting_utils.R")


# =================================================================
# Input and options
# =================================================================
ID = ""

show_title = FALSE
dpi = 400

# =================================================================
# Pre-process
# =================================================================

fpath = paste("beta", ID, ".csv", sep = "")
df_beta = read.csv(fpath)
df_beta = as_tibble(df_beta)
df_beta$weight_logic = df_beta$weight > 0

fpath = paste("alpha2", ID, ".csv", sep = "")
df_alpha = read.csv(fpath)
df_alpha = as_tibble(df_alpha)

# =================================================================
# Visualize
# =================================================================

# ................................................................
# beta facet plot
# ................................................................
idx = 100

f_cck_ii = df_beta$f_cck[idx]
d_ii = df_beta$d[idx]
hvar = "chi1"
hfacet = "load_comb"
vfacet = "rho"
color = "chi2"

g = beta_ggplot(
  df_beta=df_beta, hvar=hvar, hfacet=hfacet, vfacet=vfacet,
  color=color, f_cck_ii=f_cck_ii, d_ii=d_ii)

plot(g)

# ................................................................
# chi - alpha2 plot
# ................................................................
idx = 100

f_cck_ii = df_alpha$f_cck_r[idx]
d_ii = df_alpha$d_r[idx]
rho_ii = df_alpha$rho_r[idx]
load_comb_ii = df_alpha$load_comb_r[idx]
chi2_ii = df_alpha$chi2_r[idx]

combine_pos_neg = FALSE

g = alpha_chi1_ggplot(df_alpha, f_cck_ii, d_ii, rho_ii, load_comb_ii, chi2_ii,
                      combine_pos_neg = combine_pos_neg, show_title = show_title)

plot(g)

