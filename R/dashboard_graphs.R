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
library(scales)


# plots: ueber zeit pro service ------------------------------------------------------------

#https://wilkelab.org/SDS375/slides/redundant-coding.html#16

### absolut: linie plot

## data
glimpse(socsec)

data_socsec <- socsec %>% 
  mutate(year = as.character(year)) %>% 
  filter(unit == "n")#,
         #service %in% c("ALV", "IV", "SH"))
         #service != "Total")

data_socsec_last <- socsec %>% 
  mutate(year = as.character(year)) %>% 
  filter(unit == "n",
         #service %in% c("ALV", "IV", "SH"),
         #service != "Total",
         year == 2019)

## plot
data_socsec %>% 
  filter(service != "Total") %>% 
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
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14))


### relativ:  stacket barplot

## data
data_socsec_stack_bplot <- data_socsec %>% filter(service != "Total")
data_socsec_stack_bplot$service <- factor(data_socsec_stack_bplot$service, 
                                  levels = c("ALV", "IV", "SH", "ALV+IV", "ALV+SH", "SH+IV", "ALV+SH+IV"))
## plot
data_socsec_stack_bplot %>% 
ggplot(aes(x = year, y = value, fill = service)) + 
  geom_col(position = "fill") +
  scale_fill_brewer(palette = "Dark2"#,
                    #limits = c("ALV", "IV", "SH", "ALV+IV", "ALV+SH", "SH+IV", "ALV+SH+IV")
                    ) +
  theme_minimal() +
  theme(
    #legend.position="none",
    plot.title = element_text(size=14))

### relativ: dodge barplot

## data
data_socsec_dodge_bplot <- data_socsec %>% filter(service != "Total")
data_socsec_dodge_bplot$service <- factor(data_socsec_dodge_bplot$service, 
                                          levels = c("ALV+SH+IV", "ALV+IV", "SH+IV", "ALV+SH", "SH", "IV", "ALV"))
## plot
data_socsec_dodge_bplot %>% 
ggplot(aes(x = year, y = value, fill = service)) + 
  geom_col(position = "dodge") +
  scale_y_continuous(
    #limits = c(0, max(data_socsec_d$value) + 20000),
    labels = number,
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Dark2"#,
                    #limits = c("ALV", "IV", "SH")
                    ) +
  theme_minimal() +
  theme(
    #legend.position="none",
    plot.title = element_text(size = 14)) 



# plots: ueber zeit gruppen (gender/alter) --------------------------------------------------------

# die kategorien gender, pop_group und age sind nur im verhältniss zu service gruppiert
# untereinander stehen sie nicht im verhältnis

## data
glimpse(socsec_rate)

data_socsec_rate <- socsec_rate %>% 
  mutate(year = as.character(year)) %>% 
  filter(unit == "n")#,
         #service != "Total")
# gender
data_socsec_gender <- data_socsec_rate %>% 
  filter(pop_group == "Total" & age == "Total")

# pop
data_socsec_pop <- data_socsec_rate %>% 
  filter(gender == "Total" & age == "Total")

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

## plot male vs. female
data_socsec_gender %>% 
  filter(gender != "Total" & service != "Total") %>% 
ggplot(aes(x = year, y = value, fill = gender)) +
  geom_col(position = "fill") + 
  geom_hline(aes(yintercept = 0.5), linetype = "dotted") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~service) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14))
  
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


