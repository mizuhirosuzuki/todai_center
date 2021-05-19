# Regression with matriculation per population (million) ==========================

res_1 <- felm(
  admission_per_pop ~ temp_cut + daytime_precipitation_mm + daytime_snowfall_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_2 <- felm(
  admission_per_pop ~ temp_cut + daytime_precipitation_mm + daytime_cum_snow_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_3 <- felm(
  admission_per_pop_15_19 ~ temp_cut + daytime_precipitation_mm + daytime_snowfall_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_4 <- felm(
  admission_per_pop_15_19 ~ temp_cut + daytime_precipitation_mm + daytime_cum_snow_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res_1, res_2, res_3, res_4) %>% 
  stargazer(
    dep.var.labels = "Matriculation per population (million)",
    column.labels = c(rep("Total", 2), rep("15-19", 2)),
    covariate.labels = c(
      "Temperature (\\degree C) $\\le$ 0",
      "Temperature (\\degree C) 0-3",
      "Temperature (\\degree C) 6-9",
      "Temperature (\\degree C) $>$ 9",
      "Rainfall (mm)",
      "Snowfall (cm)",
      "Cumulated snow (cm)",
      "Cumulated snow $>$ 10 cm"
    ),
    title = "",
    add.lines = list(
      c("Prefecture FE", rep("Yes", 8)),
      c("Year FE", rep("Yes", 8))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/per_pop_reg.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )

# Non-linear regression figures ============
walk2(
  list(res_1, res_2),
  c(1, 2), 
  ~ ggsave(
    filename = file.path(git_dir, str_interp("Output/images/per_pop_reg_${.y}.pdf")),
    plot = make_coef_figure(.x, ylim_lb = -10, ylim_ub = 10),
    height = 6,
    width = 6
    )
  )

walk2(
  list(res_3, res_4),
  c(3, 4), 
  ~ ggsave(
    filename = file.path(git_dir, str_interp("Output/images/per_pop_reg_${.y}.pdf")),
    plot = make_coef_figure(.x, ylim_lb = -100, ylim_ub = 120),
    height = 6,
    width = 6
    )
  )

# Non-linear regression figures (previous 10 days) ============
res_1 <- felm(
  admission_per_pop ~ temp_cut_pre10 + precipitation_sum_10 + snowfall_sum_10 | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_2 <- felm(
  admission_per_pop ~ temp_cut_pre10 + precipitation_sum_10 + snowfall_sum_10 | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

walk2(
  list(res_1, res_2),
  c(1, 2), 
  ~ ggsave(
    filename = file.path(git_dir, str_interp("Output/images/per_pop_reg_${.y}_pre10.pdf")),
    plot = make_coef_figure(.x, ylim_lb = -5, ylim_ub = 4),
    height = 6,
    width = 6
    )
  )

# ==========================================================================

ggsave(
  filename = file.path(git_dir, str_interp("Output/images/per_pop_reg_t.pdf")),
  plot = make_coef_figure(
    felm(
      admission_per_pop ~ 
        temp_cut + daytime_precipitation_mm + daytime_snowfall_cm +
        f1_temp_cut + f1_precipitation_mm + f1_snowfall_cm | 
        prefecture + year | 0 | prefecture, 
      data = reg_df
    ),
    temp_cut_str = "temp_cut",
    ylim_lb = -5, ylim_ub = 5.5
    ),
  height = 6,
  width = 6
  )

ggsave(
  filename = file.path(git_dir, str_interp("Output/images/per_pop_reg_f1.pdf")),
  plot = make_coef_figure(
    felm(
      admission_per_pop ~ 
        temp_cut + daytime_precipitation_mm + daytime_snowfall_cm +
        f1_temp_cut + f1_precipitation_mm + f1_snowfall_cm | 
        prefecture + year | 0 | prefecture, 
      data = reg_df
    ),
    temp_cut_str = "^f1_temp_cut",
    ylim_lb = -5, ylim_ub = 5.5
    ),
  height = 6,
  width = 6
  )

ggsave(
  filename = file.path(git_dir, str_interp("Output/images/pop_reg_t.pdf")),
  plot = make_coef_figure(
    felm(
      total_pop ~ 
        temp_cut + daytime_precipitation_mm + daytime_snowfall_cm +
        f1_temp_cut + f1_precipitation_mm + f1_snowfall_cm | 
        prefecture + year | 0 | prefecture, 
      data = reg_df
    ),
    temp_cut_str = "temp_cut",
    ylim_lb = -0.1, ylim_ub = 0.1
    ),
  height = 6,
  width = 6
  )

ggsave(
  filename = file.path(git_dir, str_interp("Output/images/pop_reg_f1.pdf")),
  plot = make_coef_figure(
    felm(
      total_pop ~ 
        temp_cut + daytime_precipitation_mm + daytime_snowfall_cm +
        f1_temp_cut + f1_precipitation_mm + f1_snowfall_cm | 
        prefecture + year | 0 | prefecture, 
      data = reg_df
    ),
    temp_cut_str = "^f1_temp_cut",
    ylim_lb = -0.1, ylim_ub = 0.1
    ),
  height = 6,
  width = 6
  )

# ==========================================================================

make_coef_figure(
    felm(
      total_pop ~ 
        temp_cut_pre10 + precipitation_sum_10 + snowfall_sum_10 |
        # temp_cut + daytime_precipitation_mm + daytime_snowfall_cm |
        # f1_temp_cut + f1_precipitation_mm + f1_snowfall_cm | 
        prefecture + year | 0 | prefecture, 
      data = reg_df
    ),
    temp_cut_str = "^temp_cut",
    ylim_lb = -0.1, ylim_ub = 0.1
    )

make_coef_figure(
    felm(
      pop_15_19 ~  
        # temp_cut_pre10 + precipitation_sum_10 + snowfall_sum_10 |
        # temp_cut + daytime_precipitation_mm + daytime_snowfall_cm |
        # f1_temp_cut + f1_precipitation_mm + f1_snowfall_cm |
        f1_temp_cut + f1_precipitation_mm + f1_snowfall_cm |
        prefecture + year | 0 | prefecture, 
      data = reg_df
    ),
    temp_cut_str = "^(f1_)?temp_cut",
    ylim_lb = -0.01, ylim_ub = 0.01
    )

make_coef_figure(
    felm(
      admission_per_pop ~ 
        # temp_cut_pre10 + precipitation_sum_10 + snowfall_sum_10 |
        temp_cut + daytime_precipitation_mm + daytime_snowfall_cm |
        # f1_temp_cut + f1_precipitation_mm + f1_snowfall_cm |
        prefecture + year | 0 | prefecture, 
      data = reg_df
      # data = reg_df %>%
      #   filter(prefecture != "東京")
    ),
    temp_cut_str = "^(f1_)?temp_cut",
    # temp_cut_str = "^temp_cut",
    ylim_lb = -10, ylim_ub = 10
    )

make_coef_figure(
    felm(
      admission_per_pop_15_19 ~ 
        # temp_cut + daytime_precipitation_mm + daytime_snowfall_cm |
        f1_temp_cut + f1_precipitation_mm + f1_snowfall_cm |
        # temp_cut_pre10 + precipitation_sum_10 + snowfall_sum_10 |
        prefecture + year | 0 | prefecture, 
      data = reg_df %>%
        filter(prefecture != "東京")
      # data = reg_df
    ),
    temp_cut_str = "^(f1_)?temp_cut",
    ylim_lb = -110, ylim_ub = 110
    )

make_coef_figure(
    felm(
      admission_per_avg_pop_15_19 ~ 
        # temp_cut + daytime_precipitation_mm + daytime_snowfall_cm |
        f1_temp_cut + f1_precipitation_mm + f1_snowfall_cm |
        # temp_cut_pre10 + precipitation_sum_10 + snowfall_sum_10 |
        prefecture + year | 0 | prefecture, 
      # data = reg_df %>%
      #   filter(prefecture != "東京")
      data = reg_df
    ),
    temp_cut_str = "^(f1_)?temp_cut",
    ylim_lb = -110, ylim_ub = 110
    )

make_coef_figure(
    felm(
      admission_total ~ 
        temp_cut + daytime_precipitation_mm + daytime_snowfall_cm |
        # f1_temp_cut + f1_precipitation_mm + f1_snowfall_cm |
        prefecture + year | 0 | prefecture, 
      data = reg_df
    ),
    temp_cut_str = "^(f1_)?temp_cut",
    # temp_cut_str = "^temp_cut",
    ylim_lb = -15, ylim_ub = 15
    )

make_coef_figure(
    felm(
      aa ~ 
        # temp_cut + daytime_precipitation_mm + daytime_snowfall_cm |
        f1_temp_cut + f1_precipitation_mm + f1_snowfall_cm |
        prefecture + year | 0 | prefecture, 
      data = reg_df %>% 
        group_by(prefecture) %>% 
        # mutate(aa = (admission_total_share - mean(admission_total_share)) / sd(admission_total_share))
        # mutate(aa = (admission_total_share - mean(admission_total_share)) / sd(admission_total_share))
        # mutate(aa = (admission_per_pop - mean(admission_per_pop)) / sd(admission_per_pop))
        # mutate(aa = (admission_per_pop_15_19 - mean(admission_per_pop_15_19)) / sd(admission_per_pop_15_19))
        # mutate(aa = (total_pop - mean(total_pop)) / sd(total_pop))
        # mutate(aa = admission_total_share)
        # mutate(aa = admission_per_pop)
        mutate(aa = log(admission_per_pop))
    ),
    temp_cut_str = "^(f1_)?temp_cut",
    # temp_cut_str = "^f1_temp_cut",
    ylim_lb = -3, ylim_ub = 3
    )

make_coef_figure(
    felm(
      admission_total_share ~ 
        # temp_cut + daytime_precipitation_mm + daytime_snowfall_cm |
        f1_temp_cut + f1_precipitation_mm + f1_snowfall_cm |
        prefecture + year | 0 | prefecture, 
      # data = reg_df %>%
      #   filter(prefecture != "東京")
      data = reg_df
    ),
    temp_cut_str = "^(f1_)?temp_cut",
    # temp_cut_str = "^temp_cut",
    ylim_lb = -0.4, ylim_ub = 0.4
    )

reg_df %>% 
  group_by(prefecture) %>% 
  summarise_at(vars(temperature_pre10_avg), mean, na.rm = TRUE) %>% 
  arrange(temperature_pre10_avg) %>% 
  View

felm(
  pop_15_19 ~ 
    # temp_cut_pre10 + precipitation_sum_10 + snowfall_sum_10 |
    # temp_cut + daytime_precipitation_mm + daytime_snowfall_cm |
    f1_temp_cut + f1_precipitation_mm + f1_snowfall_cm |
    # f1_temperature_degree + f1_precipitation_mm + f1_snowfall_cm |
    prefecture + year | 0 | prefecture, 
  data = reg_df
  ) %>% 
  summary


felm(
  aa ~ 
    # temp_cut + daytime_precipitation_mm + daytime_cum_snow_m |
    temp_cut + daytime_precipitation_mm + daytime_cum_snow_m +
    f1_temp_cut + f1_precipitation_mm + f1_cum_snow_m |
    prefecture + year | 0 | prefecture, 
  data = reg_df %>% 
    # filter(prefecture != "東京") %>%
    # filter(prefecture != "神奈川") %>%
    # filter(prefecture != "兵庫") %>%
    # filter(prefecture != "愛知") %>%
    # filter(prefecture != "千葉") %>%
    group_by(prefecture) %>%
    # mutate(aa = (admission_total - mean(admission_total)) / sd(admission_total))
    # mutate(aa = (admission_total_share - mean(admission_total_share)) / sd(admission_total_share))
    # mutate(aa = (admission_total_share - mean(admission_total_share)) / sd(admission_total_share))
    # mutate(aa = (admission_per_pop - mean(admission_per_pop)) / sd(admission_per_pop))
    # mutate(aa = (admission_per_pop_15_19 - mean(admission_per_pop_15_19)) / sd(admission_per_pop_15_19))
    # mutate(aa = (admission_per_avg_pop_15_19 - mean(admission_per_avg_pop_15_19)) / sd(admission_per_avg_pop_15_19))
    # mutate(aa = admission_total_share)
    mutate(aa = log(admission_total_share))
    # mutate(aa = admission_per_avg_pop_15_19)
    # mutate(aa = total_pop)
  ) %>% 
  summary

felm(daytime_temperature_degree ~ f1_temperature_degree | prefecture + year | 0 | prefecture, data = reg_df) %>% summary

reg_df %>% 
  filter(prefecture != "東京") %>%
  filter(prefecture != "神奈川") %>%
  ggplot(aes(x = log(admission_total))) +
  geom_histogram()

admission_df_all %>% 
  # filter(prefecture %in% aa[1:10]) %>% 
  group_by(prefecture_eng) %>% 
  summarise_at(vars(admission_total), mean) %>% 
  ggplot(aes(x = prefecture_eng, y = admission_total)) +
  scale_x_discrete(limits = prefecture_order) +
  geom_point() +
  coord_flip()

prefecture_order <- admission_df_all %>% 
  group_by(prefecture_eng) %>% 
  summarise_at(vars(admission_total), mean) %>% 
  arrange(admission_total) %>% 
  .$prefecture_eng %>% 
  as.vector

felm(
  daytime_temperature_degree ~ f1_temperature_degree | prefecture + year | 0 | prefecture, 
  data = reg_df %>% 
    filter(prefecture != "東京") %>%
    filter(prefecture != "神奈川")
    ) %>% 
  summary

reg_df %>% 
  filter(f1_temp_cut == "(0,3]") %>% 
  select(prefecture) %>% 
  unique

reg_df %>% 
  filter(temp_cut == "(0,3]") %>% 
  select(prefecture) %>% 
  unique

reg_df %>% select(f1_temp_cut)

reg_df %>% 
  mutate(
    temp_cut = cut(daytime_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
    f1_temp_cut = cut(f1_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
    a = daytime_temperature_degree > f1_temperature_degree
  ) %>% 
  # select(temp_cut, f1_temp_cut) %>% 
  select(temp_cut, f1_temp_cut) %>% 
  # select(temp_cut) %>%
  # select(f1_temp_cut) %>%
  table

stata_cmd <- str_interp("
  egen pref_group = group(prefecture)
  gen aa = (daytime_temperature_degree < 3)
  twowayfeweights admission_total pref_group year temp_cut, type(feTR)
  ")

stata_cmd <- str_interp("
  egen pref_group = group(prefecture)
  gen aa = (f1_temperature_degree < 3)
  did_multiplegt admission_total pref_group year aa, breps(100)
  ")

options("RStata.StataVersion" = 16)
options("RStata.StataPath" = "/Applications/Stata/StataSE.app/Contents/MacOS/stata-se")
stata(stata_cmd, data.in = reg_df)


ggsave(
  filename = file.path(git_dir, str_interp("Output/images/how_i_fail_reg_log.pdf")),
  plot = make_coef_figure(
    felm(
      outcome ~ 
        temp_cut + daytime_precipitation_mm + daytime_cum_snow_m +
        f1_temp_cut + f1_precipitation_mm + f1_cum_snow_m |
        prefecture + year | 0 | prefecture, 
      data = reg_df %>% 
        # filter(prefecture != "東京") %>%
        # filter(prefecture != "神奈川") %>%
        mutate(outcome = log(admission_total))
      ), 
    temp_cut_str = "^temp_cut",
    ylim_lb = -0.5, ylim_ub = 0.5),
  height = 6,
  width = 6
  )

ggsave(
  filename = file.path(git_dir, str_interp("Output/images/how_i_fail_reg_log_f1.pdf")),
  plot = make_coef_figure(
    felm(
      outcome ~ 
        temp_cut + daytime_precipitation_mm + daytime_cum_snow_m +
        f1_temp_cut + f1_precipitation_mm + f1_cum_snow_m |
        prefecture + year | 0 | prefecture, 
      data = reg_df %>% 
        # filter(prefecture != "東京") %>%
        # filter(prefecture != "神奈川") %>%
        mutate(outcome = log(admission_total))
      ), 
    temp_cut_str = "^f1_temp_cut",
    ylim_lb = -0.5, ylim_ub = 0.5),
  height = 6,
  width = 6
  )

ggsave(
  filename = file.path(git_dir, str_interp("Output/images/how_i_fail_reg_nobig.pdf")),
  plot = make_coef_figure(
    felm(
      outcome ~ 
        temp_cut + daytime_precipitation_mm + daytime_cum_snow_m +
        f1_temp_cut + f1_precipitation_mm + f1_cum_snow_m |
        prefecture + year | 0 | prefecture, 
      data = reg_df %>% 
        filter(prefecture != "東京") %>%
        filter(prefecture != "神奈川") %>%
        mutate(outcome = admission_total_share)
      ), 
    temp_cut_str = "^temp_cut",
    ylim_lb = -0.5, ylim_ub = 0.5),
  height = 6,
  width = 6
  )

ggsave(
  filename = file.path(git_dir, str_interp("Output/images/how_i_fail_reg_nobig_f1.pdf")),
  plot = make_coef_figure(
    felm(
      outcome ~ 
        temp_cut + daytime_precipitation_mm + daytime_cum_snow_m +
        f1_temp_cut + f1_precipitation_mm + f1_cum_snow_m |
        prefecture + year | 0 | prefecture, 
      data = reg_df %>% 
        filter(prefecture != "東京") %>%
        filter(prefecture != "神奈川") %>%
        mutate(outcome = admission_total_share)
      ), 
    temp_cut_str = "^f1_temp_cut",
    ylim_lb = -0.5, ylim_ub = 0.5),
  height = 6,
  width = 6
  )

reg_df %>% 
  group_by(prefecture) %>% 
  summarise_at(vars(admission_total_share), mean)