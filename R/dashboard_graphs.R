########################
### dashboard graphs ###
########################


# setup -------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(scales)


# plots: soz.-versicherung ueber zeit (socsec) ------------------------------------------------------------

#https://wilkelab.org/SDS375/slides/redundant-coding.html#16

### data absolut

glimpse(socsec)

data_socsec <- socsec %>% 
  mutate(year = as.character(year)) %>% 
  filter(unit == "n",
         service %in% c("ALV", "IV", "SH"))
         #service != "Total")

data_socsec_last <- socsec %>% 
  mutate(year = as.character(year)) %>% 
  filter(unit == "n",
         service %in% c("ALV", "IV", "SH"),
         #service != "Total",
         year == 2019)

### plot absolut

data_socsec %>% 
  ggplot(aes(x = year, y = value, group = service, color = service)) +
  geom_line() +
  scale_x_discrete(
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, max(data_socsec$value) + 20000),
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

### data relativ a

data_socsec %>% 
  ggplot(aes(x = year, y = value, fill = service)) + 
  geom_col(position = "fill") +
  scale_fill_brewer(palette = "Dark2",
                    limits = c("ALV", "IV", "SH")) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)) +
  theme_minimal()

### data relativ b
data_socsec_d <- data_socsec
data_socsec_d$service <- factor(data_socsec_d$service, levels = c("SH", "IV", "ALV"))

data_socsec_d %>% 
  ggplot(aes(x = year, y = value, fill = service)) + 
  geom_col(position = "dodge") +
  scale_y_continuous(
    limits = c(0, max(data_socsec_d$value) + 20000),
    labels = number,
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Dark2",
                    limits = c("ALV", "IV", "SH")) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)) +
  theme_minimal()


# plots: ??? --------------------------------------------------------------



