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


# plots: ueber zeit pro service ------------------------------------------------------------
# https://wilkelab.org/SDS375/slides/redundant-coding.html#16

### absolut: linie plot

## data
glimpse(socsec)

data_socsec <- socsec %>% 
  #mutate(year = as.character(year)) %>% 
  mutate(year = factor(year)) %>% 
  filter(unit == "n")#,
         #service %in% c("ALV", "IV", "SH"))
         #service != "Total")

data_socsec_last <- socsec %>% 
  mutate(year = factor(year)) %>% 
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
    #limits = c(0, max(data_socsec$value) + 2000),
    breaks = scales::pretty_breaks(),
    labels = scales::number,
    #expand = c(0, 0),
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

### absolut: dodge barplot

## data
data_socsec_dodge_bplot <- data_socsec %>% filter(service != "Total")
data_socsec_dodge_bplot$service <- factor(data_socsec_dodge_bplot$service, 
                                          levels = c("ALV+SH+IV", "ALV+IV", "SH+IV", "ALV+SH", "SH", "IV", "ALV"))
## plot
data_socsec_dodge_bplot %>% 
  ggplot(aes(x = year, y = value, fill = service)) + 
  geom_col(position = "dodge") +
  scale_y_continuous(
    breaks = scales::pretty_breaks(),
    labels = scales::number
  ) +
  scale_fill_brewer(palette = "Dark2"#,
                    #limits = c("ALV", "IV", "SH")
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14)) 

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
  theme(plot.title = element_text(size=14))




