# Main regression ==========================

res_1 <- felm(
  admission_total_share ~ temp_cut_morning + morning_precipitation_mm + morning_snowfall_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_2 <- felm(
  admission_total_share ~ temp_cut_morning + morning_precipitation_mm + morning_cum_snow_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_3 <- felm(
  admission_total_share ~ temp_cut_exam + exam_precipitation_mm + exam_snowfall_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_4 <- felm(
  admission_total_share ~ temp_cut_exam + exam_precipitation_mm + exam_cum_snow_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_5 <- felm(
  admission_total_share ~ 
    temp_cut_morning + morning_precipitation_mm + morning_snowfall_cm +
    temp_cut_exam + exam_precipitation_mm + exam_snowfall_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_6 <- felm(
  admission_total_share ~ 
    temp_cut_morning + morning_precipitation_mm + morning_cum_snow_cm +
    temp_cut_exam + exam_precipitation_mm + exam_cum_snow_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res_1, res_2, res_3, res_4, res_5, res_6) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
    covariate.labels = c(
      "Temperature (\\degree C) $\\le$ -3 (morning)",
      "Temperature (\\degree C) -3-0 (morning)",
      "Temperature (\\degree C) 3-6 (morning)",
      "Temperature (\\degree C) $>$ 6 (morning)",
      "Rainfall (mm) (morning)",
      "Snowfall (cm) (morning)",
      "Cumulated snow (cm) (morning)",
      "Temperature (\\degree C) $\\le$ -3 (during exam)",
      "Temperature (\\degree C) -3-0 (during exam)",
      "Temperature (\\degree C) 3-6 (during exam)",
      "Temperature (\\degree C) $>$ 6 (during exam)",
      "Rainfall (mm) (during exam)",
      "Snowfall (cm) (during exam)",
      "Cumulated snow (cm) (during exam)"
    ),
    title = "",
    add.lines = list(
      c("Prefecture FE", rep("Yes", 6)),
      c("Year FE", rep("Yes", 6))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/morning_exam_reg.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )

# # Regression table for beamer
# list(res_2, res_3, res_4) %>% 
#   stargazer(
#     dep.var.labels = "Matriculation share (\\%)",
#     keep = c(
#       "^[^temp_cut]"
#     ),
#     covariate.labels = c(
#       "Rainfall (mm)",
#       "Snowfall (cm)",
#       "Cumulated snow (cm)",
#       "Cumulated snow $>$ 10 cm"
#     ),
#     title = "",
#     add.lines = list(
#       c("Temperature bins", rep("Yes", 3)),
#       c("Prefecture FE", rep("Yes", 3)),
#       c("Year FE", rep("Yes", 3))
#     ),
#     type = "latex",
#     out = file.path(git_dir, "Output/tex/during_exam_reg_beamer.tex"),
#     omit.stat = c("adj.rsq", "ser", "rsq"),
#     digits = 2,
#     float = FALSE
#   )

# Non-linear regression figures ============

# Function for morning temperature 
make_coef_figure_morning <- function(res, temp_cut_str = "^temp_cut", ylim_lb = -0.40, ylim_ub = 0.15) {

  temp_cut_names <- grepl(temp_cut_str, rownames(res$coefficients))

  coef <- res %>%
    summary %>%
    .$coef %>%
    .[temp_cut_names, "Estimate"]
  coef <- c(coef[1:2], 0, coef[3:4])
  se <- res %>%
    summary %>%
    .$coef %>%
    .[temp_cut_names, "Cluster s.e."]
  se <- c(se[1:2], 0, se[3:4])

  b <- as.numeric(coef)
  lb <- as.numeric(coef - 1.645 * se)
  ub <- as.numeric(coef + 1.645 * se)
  x_label = c(
    "<= -3", 
    "-3-0",
    "0-3",
    "3-6",
    "> 6"
    )
  df <- tibble(x_label = x_label, b = b, ymin = lb, ymax = ub)

  fig <- ggplot(df, aes(x = factor(x_label, levels = x_label), y = b)) + 
    geom_point() +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .2) +
    geom_hline(yintercept = 0, alpha = 0.4) +
    theme_classic() +
    ylab("Coefficients") +
    xlab("Temperature bins (Celsius)") +
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 15)) +
    scale_x_discrete(
      labels = expression(
        "" <= -3,
        "(-3, 0]",
        "(0, 3]",
        "(3, 6]",
        "" > 6
      )
    ) +
    ylim(ylim_lb, ylim_ub)

  return(fig)
}

ylim_lb = -0.40
ylim_ub = 0.15

walk2(
  list(res_3, res_4),
  c(3, 4), 
  ~ ggsave(
    filename = file.path(git_dir, str_interp("Output/images/morning_exam_reg_${.y}.pdf")),
    plot = make_coef_figure(.x, ylim_lb = ylim_lb, ylim_ub = ylim_ub),
    height = 6,
    width = 6
    )
  )

walk2(
  list(res_1, res_2),
  c(1, 2),
  ~ ggsave(
    filename = file.path(git_dir, str_interp("Output/images/morning_exam_reg_${.y}.pdf")),
    plot = make_coef_figure_morning(.x, ylim_lb = ylim_lb, ylim_ub = ylim_ub),
    height = 6,
    width = 6
    )
)

walk2(
  list(res_5, res_6),
  c(5, 6),
  ~ ggsave(
    filename = file.path(git_dir, str_interp("Output/images/morning_exam_reg_${.y}_exam.pdf")),
    plot = make_coef_figure(.x, temp_cut_str = "^temp_cut_exam", ylim_lb = ylim_lb, ylim_ub = ylim_ub),
    height = 6,
    width = 6
    )
)

walk2(
  list(res_5, res_6),
  c(5, 6),
  ~ ggsave(
    filename = file.path(git_dir, str_interp("Output/images/morning_exam_reg_${.y}_morning.pdf")),
    plot = make_coef_figure_morning(.x, temp_cut_str = "^temp_cut_morning", ylim_lb = ylim_lb, ylim_ub = ylim_ub),
    height = 6,
    width = 6
    )
)

