data(olives, package = "classifly")
View(olives)

ggplot(olives, aes(palmitoleic, palmitic, color = Area)) + 
  geom_point(data = ~select(., -Area), color = "grey") +
  geom_point() + 
  facet_wrap(~Area) + 
  scale_color_viridis_d() + 
  guides(color = "none") +
  labs(tag = "(C)")


paste0("Age ", data_socsec_age$age)
# -------------------------------------------------------------------------

# 
# ggplot(mutate(df_health, health = fct_rev(health)), aes(x = age, y = ..count..)) +
#   geom_density_line(data = select(df_health, -health), aes(fill = "all people surveyed   "), color = "transparent") +
#   geom_density_line(aes(fill = "highlighted group"), color = "transparent") +
#   facet_wrap(~health, nrow = 1) +
#   scale_x_continuous(name = "age (years)", limits = c(15, 98), expand = c(0, 0)) +
#   scale_y_continuous(name = "count", expand = c(0, 0)) +
#   scale_fill_manual(
#     values = c("#b3b3b3a0", "#2b8cbed0"),
#     name = NULL,
#     guide = guide_legend(direction = "horizontal")
#   ) +
#   coord_cartesian(clip = "off") +
#   theme_dviz_hgrid() +
#   theme(
#     axis.line.x = element_blank(),
#     strip.text = element_text(size = 14, margin = margin(0, 0, 0.2, 0, "cm")),
#     legend.position = "bottom",
#     legend.justification = "right",
#     legend.margin = margin(4.5, 0, 1.5, 0, "pt"),
#     legend.spacing.x = grid::unit(4.5, "pt"),
#     legend.spacing.y = grid::unit(0, "pt"),
#     legend.box.spacing = grid::unit(0, "cm")
#   )

