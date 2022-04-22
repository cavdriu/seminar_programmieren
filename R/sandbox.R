data(olives, package = "classifly")
View(olives)

ggplot(olives, aes(palmitoleic, palmitic, color = Area)) + 
  geom_point(data = ~select(., -Area), color = "grey") +
  geom_point() + 
  facet_wrap(~Area) + 
  scale_color_viridis_d() + 
  guides(color = "none") +
  labs(tag = "(C)")

View(data_socsec_age)

test <- data_socsec_age %>%
  filter(age != "Total" & service != "Total", service == "ALV")

glimpse(test)
#https://r-graphics.org/recipe-line-graph-area
ggplot(test, aes(x = as.numeric(year), y = value, fill = age)) + # wieso ist as.numeric notwendig?
  geom_area(data = ~select(., -age), fill = "grey", color = "grey", group = "grey") +
  geom_area() +
  facet_wrap(~age)

test %>% 
  ggplot(aes(x = year, y = value, fill)) + 
  geom_area()
