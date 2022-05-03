########################
### dashboard graphs ###
########################
# source: https://clauswilke.com/dataviz/
# source: https://blog.datawrapper.de/colors-for-data-vis-style-guides/
# source: https://r-graphics.org/
# source: https://github.com/emitanaka/wearerladies/blob/master/day3.md

# setup -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# plots: ueber zeit gruppen (gender/alter) --------------------------------------------------------

# die kategorien gender, pop_group und age sind nur im verhältniss zu service gruppiert
# untereinander stehen sie nicht im verhältnis

## data
glimpse(socsec_rate)

data_socsec_rate <- socsec_rate %>% 
  mutate(year = year) %>% 
  filter(unit == "n")#,
         #service != "Total")
# gender
data_socsec_gender <- data_socsec_rate %>% 
  filter(pop_group == "Total" & age == "Total" & gender != "Total") %>% 
  pivot_wider(names_from = gender, values_from = value) %>%
  rename_with(tolower) %>% 
  mutate(diff = m - f) %>% 
  pivot_longer(cols = c(m, f), names_to = "gender", values_to = "value") %>% 
  select(!c("pop_group","age")) 

stats_gender <- data_socsec_gender %>% 
  group_by(gender, service) %>% 
  summarize(mean = mean(value),
            se = sd(value)) %>% 
  mutate(mean_pos = mean + (1 * se),
         mean_neg = mean - (1 * se)) %>% 
  ungroup()


# pop
data_socsec_pop <- data_socsec_rate %>% 
  filter(gender == "Total" & age == "Total" & pop_group != "Total") %>% 
  pivot_wider(names_from = pop_group, values_from = value) %>%
  rename_with(tolower) %>% 
  mutate(diff = ch - ausl) %>% 
  pivot_longer(cols = c(ch, ausl), names_to = "pop_group", values_to = "value") %>% 
  select(!c("gender","age"))

# age
data_socsec_age <- data_socsec_rate %>% 
  filter(gender == "Total" & pop_group == "Total") %>% 
  select(-c(gender:pop_group, unit))

data_socsec_age_total <- data_socsec_age %>%
  mutate(value_total = if_else(age == "Total",
                               value,
                               1)) %>%
  select(-value) %>% 
  pivot_wider(names_from = age,
              values_from = value_total) %>% 
  mutate("18-24" = Total, "25-39" = Total, "40-54" = Total, "55-65" = Total) %>%  # einfacherer weg möglich? zb. across()
  pivot_longer(!year:service,
               names_to = "age",
               values_to = "value_total")

data_socsec_age <- left_join(data_socsec_age, data_socsec_age_total)

## plot male vs. female (overview)
data_socsec_gender %>% 
  filter(gender != "Total" & service != "Total") %>% 
ggplot(aes(x = year, y = value, fill = gender)) +
  geom_col(position = "fill") + 
  geom_hline(aes(yintercept = 0.5), linetype = "dotted") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~service) +
  theme_minimal() +
  theme(
    #axis.text.x = element_text(angle = 45),
    #legend.position = "none",
    plot.title = element_text(size = 14))

## zooming
# https://r-graph-gallery.com/web-extended-dumbbell-plot-ggplot2.html
# https://r-graph-gallery.com/web-lollipop-plot-with-r-mario-kart-64-world-records.html
data_socsec_m <- filter(data_socsec_gender, gender == "m") %>% 
  filter(service == "Total")
data_socsec_f <- filter(data_socsec_gender, gender == "f") %>% 
  filter(service == "Total")
stats_m <- filter(stats_gender, gender == "m") %>% 
  filter(service == "Total")
stats_f <- filter(stats_gender, gender == "f") %>% 
  filter(service == "Total")
diff_gender <- data_socsec_gender %>% 
  filter(gender == "m") %>% 
  mutate(x_pos = value - (diff/2)) %>% 
  filter(service == "Total")


data_socsec_gender %>% 
  filter(service == "Total") %>% 
ggplot() +
  
  # add mean and standard deviation for groups
  geom_rect(aes(xmin = stats_m$mean_neg, xmax = stats_m$mean_pos, ymin = 2019.5, ymax = 2009.5), 
            fill = "#762a83", alpha = 0.03) +
  geom_vline(xintercept = stats_m$mean, linetype = "solid", size = 0.5, alpha = 0.8, color = "#762a83") +
  geom_rect(aes(xmin = stats_f$mean_neg, xmax = stats_f$mean_pos, ymin = 2019.5, ymax = 2009.5), 
            fill = "#009688", alpha = 0.03) +  
  geom_vline(xintercept = stats_f$mean, color = "#009688", linetype = "solid",  size = 0.5, alpha = 0.8) +
  
  # add range between groups
  geom_segment(data = data_socsec_m,
               aes(x = value, y = year,
                   xend = data_socsec_f$value, yend = data_socsec_f$year),
               color = "#aeb6bf",
               size = 4.5, # segment muss zu den punkten passen
               alpha = 0.8) +
  geom_point(aes(x = value, y = year, color = gender), 
             size = 4) +
  # color points
  scale_color_manual(values = c("#009688","#762a83"))+
  geom_text(data = diff_gender,
            aes(label = paste("Diff. ", diff), x = x_pos, y = year),
            #fill = "white",
            #family = "Times New Roman"
            color = "#4a4e4d",
            size = 2.5) +
  #add annotations for mean and standard deviations
  geom_text(x = stats_m$mean - 1000, y = 2019.3, label = "MEAN", angle = 90, size = 2.5, color = "#762a83") + #family = "Segoe UI"
  geom_text(x = stats_m$mean_pos - 1000, y = 2019.3, label = "STDEV", angle = 90, size = 2.5, color = "#762a83") +  #family = "Segoe UI""
  
  # adjust panel
  scale_y_continuous(expand = c(0, 0),
                     breaks = data_socsec_gender$year,
                     labels = data_socsec_gender$year) +
  #scale_x_continuous(breaks = scales::pretty_breaks()) +
  theme(#panel.grid.major.x = element_line(color = "#e3e2e2", linetype = "dashed"),
        #panel.grid.minor.x = element_line(color = "#e3e2e2", linetype = "dashed"),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "#e3e2e2"),
        axis.line.x = element_line(color = "#e3e2e2"),
        text = element_text(color = "#4a4e4d"), #family = "Segoe UI"
        strip.text.y.left  = element_text(angle = 0),
        panel.background = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "black", color = "white"),
        strip.text = element_text(color = "#4a4e4d"),#, family = "Segoe UI"
        plot.background = element_rect(fill = "white", color = "white"),
        panel.spacing = unit(0, "lines"),
        plot.margin = margin(1,1,0.5,1, "cm"))

  
## plot grouped by age

###wieso ist as.numeric() notwendig ???###
data_socsec_age %>%
  filter(age != "Total" & service != "Total", service == "ALV") %>% # service anpassen
  mutate(age = paste0("Age: ", .$age)) %>% 
ggplot(aes(x = as.numeric(year), y = value, fill = age)) +
  geom_area(aes(x = as.numeric(year), y = value_total), fill = "grey",  alpha = 0.8, color = "grey") + 
  geom_area() +
  geom_line(color = "black") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(
    labels = number,
    expand = c(0, 0)
  ) +
  facet_wrap(~age) +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size = 14))

# # vergleich zu stacked
# data_socsec_age %>%
#   filter(age != "Total" & service != "Total", service == "ALV") %>%
#   ggplot(aes(x = as.numeric(year), y = value, fill = age)) +
#   geom_area() +
#   scale_fill_brewer(palette = "Dark2") +
#   theme_minimal() +
#   theme(
#     legend.position="none",
#     plot.title = element_text(size = 14))




# plots: ueber zeit und gruppen (gender/alter) ???--------------------------------------------------------------
# was ist status / gleicher Monat?

## data
glimpse(socsec_dedu)

-data_socsec_dedu <- socsec_dedu %>% 
  mutate(year = as.character(year)) %>% 
  filter(unit == "n")#,
         #service %in% c("ALV", "IV", "SH"))
         #service != "Total")

## plot
data_socsec_dedu %>% 
  ggplot(aes(x = year, y = value, group = service, color = service)) +
  geom_line() +
  scale_x_discrete(
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    #limits = c(0, max(data_socsec$value) + 20000),
    labels = number,
    expand = c(0, 0),
    sec.axis = dup_axis(
      breaks = data_socsec_last$value,
      labels = data_socsec_last$service,
      name = NULL)
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)) +
  guides(group = "none", color = "none") +
  theme_minimal()


