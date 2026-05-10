create_temp_pres_plot <- function(model, DT) {
  # plot
  p <- model %>%
    epred_draws(
      expand_grid(
        type = c('Control', 'Vegetated'),
        doy = 228,
        tod = c('day', 'night')
      ),
      re_formula = NA
    ) %>%
    ggplot(aes(x = .epred, fill = tod)) +
    stat_halfeye(alpha = 0.7) +
    scale_fill_manual(
      values = c("#CFA35E", "#45A291"),
      labels = c('Day', 'Night')
    ) +
    theme_classic() +
    theme(
      legend.position = "top",
      strip.text.y = element_text(angle = 0),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 16),
      axis.title = element_text(size = 16),
      strip.text = element_text(size = 16)
    ) +
    labs(y = "", color = "", fill = "") +
    facet_wrap(~type, ncol = 1)
  #facet_grid(rows = vars(factor(PastLandUse, levels=c('Industrial', 'Agricultural', 'Forested'))))

  # get axis breaks
  atx <- c(as.numeric(na.omit(layer_scales(p)$x$break_positions())))

  # unscale x axis
  f <- t +
    scale_x_continuous(
      name = "Temperature (\u00B0C)",
      breaks = atx,
      labels = round(atx * sd(DT$tem) + mean(DT$temp_C_s), 1)
    )

  ggsave(
    'graphics/plu_temp_total_plots.png',
    f,
    width = 12,
    height = 10,
    units = 'in'
  )

  return(f)
}
