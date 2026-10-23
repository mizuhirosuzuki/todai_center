# Function to make estimates coefficient figures

make_coef_figure <- function(res, temp_cut_str = "^temp_cut", ylim_lb = -0.40, ylim_ub = 0.40) {

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
    "<= 0", 
    "0-3",
    "3-6",
    "6-9",
    "> 9"
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
        "" <= 0,
        "(0, 3]",
        "(3, 6]",
        "(6, 9]",
        "" > 9
      )
    ) +
    ylim(ylim_lb, ylim_ub)
  
  return(fig)
}


