########################
### dashboard graphs ###
########################
# source: https://clauswilke.com/dataviz/

# setup -------------------------------------------------------------------

library(dplyr)
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
  theme(
    legend.position="none",
    plot.title = element_text(size=14)) +
  theme_minimal()

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
  theme(
    legend.position="none",
    plot.title = element_text(size = 14)) +
  theme_minimal()



# plots: ueber zeit gruppen (gender/alter) --------------------------------------------------------

# >> daten nochmals überdenken (normalisieren? total rausbringen?)

## data
glimpse(socsec_rate)

data_socsec_rate <- socsec_rate %>% 
  mutate(year = as.character(year)) %>% 
  filter(unit == "n",
         gender != "Total"
         #service != "Total"
         )

data_socsec_rate_total <- data_socsec_rate %>% 
  filter(service == "Total", gender == "Total", pop_group == "Total", age == "Total")

## plot
data_socsec_rate %>% 
  ggplot(aes(x = age, y = value, fill = gender)) +
  geom_col() + 
  facet_wrap(~service)




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




# plots: ueber zeit und gruppen (gender/alter) ???--------------------------------------------------------------
# was ist status / gleicher Monat?

## data
glimpse(socsec_dedu)

data_socsec_dedu <- socsec_dedu %>% 
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


