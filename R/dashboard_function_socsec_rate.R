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
  filter(unit == "n")

# gender
data_socsec_gender <- data_socsec_rate %>% 
  filter(pop_group == "Total" & age == "Total" & gender != "Total") %>% 
  pivot_wider(names_from = gender, values_from = value) %>%
  rename_with(tolower) %>% 
  mutate(diff_a = m - f,
         diff_r = round((100 / (m + f) * diff_a), digits = 1)) %>% 
  pivot_longer(cols = c(m, f), names_to = "gender", values_to = "value") %>% 
  select(!c("pop_group","age")) 

# stats_gender <- data_socsec_gender %>% 
#   group_by(gender, service) %>% 
#   summarize(mean = mean(value),
#             se = sd(value)) %>% 
#   mutate(mean_pos = mean + (1 * se),
#          mean_neg = mean - (1 * se)) %>% 
#   ungroup()


# pop
data_socsec_pop <- data_socsec_rate %>% 
  filter(gender == "Total" & age == "Total" & pop_group != "Total") %>% 
  pivot_wider(names_from = pop_group, values_from = value) %>%
  rename_with(tolower) %>% 
  mutate(diff_a = ch - ausl,
         diff_r = round((100 / (ch + ausl) * diff_a), digits = 1)) %>%
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



# gender ------------------------------------------------------------------

## overview ratio
plot_gender_overview_ratio <- function(data, years, services) {
  data %>% 
    filter(gender != "Total" & service != "Total") %>% 
    filter(year %in% years, service %in% services) %>% 
  ggplot(aes(x = year, y = value, fill = gender)) +
    geom_col(position = "fill") + 
    geom_hline(aes(yintercept = 0.5), linetype = "dotted") +
    scale_x_continuous(breaks = c(1900:2200)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_brewer(palette = "Dark2") +
    facet_wrap(~service) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
      plot.title = element_text(size = 14))
}

# test
plot_gender_overview_ratio(data_socsec_gender, 2010:2019, c("IV"))

## overview absolute
plot_gender_overview_absolute <- function(data, years, services) {
  data %>% 
    filter(gender != "Total" & service != "Total") %>% 
    filter(year %in% years, service %in% services) %>% 
    ggplot(aes(x = year, y = value, fill = gender)) +
    geom_col() + 
    geom_hline(aes(yintercept = mean(value)), linetype = "dotted") + #funktioniert das immer?
    scale_x_continuous(breaks = c(1900:2200)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_brewer(palette = "Dark2") +
    facet_wrap(~service) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
      plot.title = element_text(size = 14))
}

# test
plot_gender_overview_absolute(data_socsec_gender, 2010:2019, c("IV"))


## zooming in
# https://r-graph-gallery.com/web-extended-dumbbell-plot-ggplot2.html
# https://r-graph-gallery.com/web-lollipop-plot-with-r-mario-kart-64-world-records.html

plot_gender_zooming <- function(data, years, services) {
  
  ## data for plot
  list <- list()
  list$data_plot <- data %>% 
    filter(year %in% years, service %in% services) 
   
  ## stat for plot
  list$stats_gender <- list$data_plot %>% 
    group_by(gender) %>% 
    summarize(mean = mean(value),
              se = sd(value)) %>% 
    mutate(mean_pos = mean + (1 * se),
           mean_neg = mean - (1 * se)) %>% 
    ungroup()
  
  list$data_socsec_m <- filter(list$data_plot, gender == "m") 
  list$data_socsec_f <- filter(list$data_plot, gender == "f")
  
  list$stats_m <- filter(list$stats_gender, gender == "m")
  list$stats_f <- filter(list$stats_gender, gender == "f") 
  
  list$diff_gender <- list$data_plot %>% 
    filter(gender == "m") %>% 
    mutate(x_pos = value - (diff_a/2)) 
  
  #return(list)
  
  ## plot
  plot <- ggplot(data = list$data_plot) + 

  # add mean and standard deviation for groups
  geom_rect(aes(xmin = list$stats_m$mean_neg, xmax = list$stats_m$mean_pos,
                ymin = min(list$data_plot$year) - 0.5, ymax = max(list$data_plot$year) + 0.5),
            fill = "#D95F02", alpha = 0.03) +
  geom_vline(xintercept = list$stats_m$mean, linetype = "solid", size = 0.5, alpha = 0.8, color = "#D95F02") +
  geom_rect(aes(xmin = list$stats_f$mean_neg, xmax = list$stats_f$mean_pos,
                ymin = min(list$data_plot$year) - 0.5, ymax = max(list$data_plot$year) + 0.5),
            fill = "#1B9E77", alpha = 0.03) +
  geom_vline(xintercept = list$stats_f$mean, color = "#1B9E77", linetype = "solid",  size = 0.5, alpha = 0.8) +

  # add range between groups
  geom_segment(data = list$data_socsec_m,
               aes(x = value, y = year,
                   xend = list$data_socsec_f$value, yend = list$data_socsec_f$year),
               color = "#aeb6bf",
               size = 4.5, # segment muss zu den punkten passen
               alpha = 0.8) +
  geom_point(aes(x = value, y = year, color = gender),
             size = 4) +
    
  # color points
  scale_color_manual(values = c("#1B9E77","#D95F02")) +
  geom_text(data = list$diff_gender,
            aes(label = paste("n = ", diff_a, "  /  ", diff_r, "%", sep = ""), x = x_pos, y = year),
            color = "#4a4e4d",
            angle = 90,
            size = 2.5) +

  #add annotations for mean and standard deviations
  geom_text(x = list$stats_m$mean - 1300, y = max(list$data_plot$year) + 0.3,
            label = "MEAN", angle = 90, size = 2.5, color = "#D95F02") +
  geom_text(x = list$stats_m$mean_pos - 1300, y = max(list$data_plot$year) + 0.3,
            label = "STDEV", angle = 90, size = 2.5, color = "#D95F02") +

  # adjust panel
  scale_y_continuous(breaks = list$data_plot$year,
                     labels = list$data_plot$year,
                     expand = c(0, 0)) +
    
  coord_flip() +

  # layout
  theme(axis.ticks.y = element_line(color = "#e3e2e2"),
        axis.ticks.x = element_line(color = "#e3e2e2"),
        axis.line.x = element_blank(),
        text = element_text(color = "#4a4e4d"),
        strip.text.y.left  = element_text(angle = 0),
        panel.background = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "black", color = "white"),
        strip.text = element_text(color = "#4a4e4d"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.spacing = unit(0, "lines"),
        plot.margin = margin(1,1,0.5,1, "cm"))
  
  return(plot)

}

#test
plot_gender_zooming(data_socsec_gender, 2010:2019, "IV")


# population --------------------------------------------------------------

plot_pop_zooming <- function(data, years, services) {
  
  ## data for plot
  list <- list()
  list$data_plot <- data %>% 
    filter(year %in% years, service %in% services) 
  
  ## stat for plot
  list$stats_pop <- list$data_plot %>% 
    group_by(pop_group) %>% 
    summarize(mean = mean(value),
              se = sd(value)) %>% 
    mutate(mean_pos = mean + (1 * se),
           mean_neg = mean - (1 * se)) %>% 
    ungroup()
  
  list$data_socsec_ch <- filter(list$data_plot, pop_group == "ch") 
  list$data_socsec_ausl <- filter(list$data_plot, pop_group == "ausl")
  
  list$stats_ch <- filter(list$stats_pop, pop_group == "ch")
  list$stats_ausl <- filter(list$stats_pop, pop_group == "ausl") 
  
  list$diff_pop <- list$data_plot %>% 
    filter(pop_group == "ch") %>% 
    mutate(x_pos = value - (diff_a/2)) 
  
  #return(list)
  
  ## plot
  plot <- ggplot(data = list$data_plot) + 
    
    # add mean and standard deviation for groups
    geom_rect(aes(xmin = list$stats_ch$mean_neg, xmax = list$stats_ch$mean_pos,
                  ymin = min(list$data_plot$year) - 0.5, ymax = max(list$data_plot$year) + 0.5),
              fill = "#D95F02", alpha = 0.03) +
    geom_vline(xintercept = list$stats_ch$mean, linetype = "solid", size = 0.5, alpha = 0.8, color = "#D95F02") +
    geom_rect(aes(xmin = list$stats_ausl$mean_neg, xmax = list$stats_ausl$mean_pos,
                  ymin = min(list$data_plot$year) - 0.5, ymax = max(list$data_plot$year) + 0.5),
              fill = "#1B9E77", alpha = 0.03) +
    geom_vline(xintercept = list$stats_ausl$mean, color = "#1B9E77", linetype = "solid",  size = 0.5, alpha = 0.8) +
    
    # add range between groups
    geom_segment(data = list$data_socsec_ch,
                 aes(x = value, y = year,
                     xend = list$data_socsec_ausl$value, yend = list$data_socsec_ausl$year),
                 color = "#aeb6bf",
                 size = 4.5, # segment muss zu den punkten passen
                 alpha = 0.8) +
    geom_point(aes(x = value, y = year, color = pop_group),
               size = 4) +
    
    # color points
    scale_color_manual(values = c("#1B9E77","#D95F02")) +
    geom_text(data = list$diff_pop,
              aes(label = paste("Diff. = ", diff_a, "  /  ", diff_r, "%", sep = ""), x = x_pos, y = year),
              color = "#4a4e4d",
              angle = 90,
              size = 2.5) +
    
    #add annotations for mean and standard deviations
    geom_text(x = list$stats_ch$mean - 1300, y = max(list$data_plot$year) + 0.3,
              label = "MEAN", angle = 90, size = 2.5, color = "#D95F02") +
    geom_text(x = list$stats_ch$mean_pos - 1300, y = max(list$data_plot$year) + 0.3,
              label = "STDEV", angle = 90, size = 2.5, color = "#D95F02") +
    
    # adjust panel
    scale_y_continuous(breaks = list$data_plot$year,
                       labels = list$data_plot$year,
                       expand = c(0, 0)) +
    
    coord_flip() +
    
    # layout
    theme(axis.ticks.y = element_line(color = "#e3e2e2"),
          axis.ticks.x = element_line(color = "#e3e2e2"),
          axis.line.x = element_blank(),
          text = element_text(color = "#4a4e4d"),
          strip.text.y.left  = element_text(angle = 0),
          panel.background = element_rect(fill = "white", color = "white"),
          strip.background = element_rect(fill = "black", color = "white"),
          strip.text = element_text(color = "#4a4e4d"),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.spacing = unit(0, "lines"),
          plot.margin = margin(1,1,0.5,1, "cm"))
  
  return(plot)
  
}

#test
plot_pop_zooming(data_socsec_pop, 2010:2019, "IV")





# test --------------------------------------------------------------------


## plot grouped by age

###wieso geht factor / as.character nicht?###
data_socsec_age %>%
  filter(age != "Total" & service != "Total", service == "ALV") %>% # service anpassen
  mutate(age = paste0("Age: ", .$age)) %>% 
ggplot(aes(x = year, y = value, fill = age)) +
  geom_area(aes(x = year, y = value_total), fill = "grey",  alpha = 0.8, color = "grey") + 
  geom_area() +
  geom_line(color = "black") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(
    breaks = scales::pretty_breaks(),
    labels = scales::number,
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = c(1900:2200)
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


