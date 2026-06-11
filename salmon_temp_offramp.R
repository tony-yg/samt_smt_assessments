#Stolen from BDO-Science/samt_summary_new/temp_off_ramp.R
# Code to create the temperature offramp plot for the salmon assessment

library(tidyverse)
library(CDECRetrieve)
library(knitr)
library(kableExtra)
library(patchwork)
library(gridExtra)

startDate <- paste0(year(Sys.Date()),'-06-01')

mossdale <- cdec_query('MSD',
                       '25',
                       'H',
                       startDate,
                       Sys.Date()) %>%
  select(station = location_id, 
         date = datetime, 
         temp = parameter_value) %>%
  mutate(date = as.Date(date)) %>%
  group_by(station, date) %>%
  summarize(temp = mean(temp, na.rm = TRUE)) %>%
  mutate(temp = (temp -32)/1.8)

prisoners <- cdec_query('PPT',
                        '25',
                        'H',
                        startDate,
                        Sys.Date()) %>%
  select(station = location_id, 
         date = datetime, 
         temp = parameter_value) %>%
  mutate(date = as.Date(date)) %>%
  group_by(station, date) %>%
  summarize(temp = mean(temp, na.rm = TRUE)) %>%
  mutate(temp = (temp -32)/1.8)

all_temps <- bind_rows(mossdale, prisoners) %>%
  mutate(trigger = if_else(temp >= 22.2, 'YES', 'NO')) %>%
  na.omit() %>%
  mutate(station = factor(station, levels = c('MSD', 'PPT'), 
                          labels = c('Mossdale', 'Prisoners Point')))

# --- Threshold Exceedance Table Data ---
yes_triggers <- all_temps %>%
  filter(trigger == "YES") %>%
  group_by(station) %>%
  summarize(n_yes = n(), .groups = "drop") %>%
  complete(station = unique(all_temps$station), fill = list(n_yes = 0))

exceedance_dates <- all_temps %>%
  filter(trigger == "YES") %>%
  group_by(station) %>%
  summarize(
    dates_exceeding = paste(format(date, "%b %d"), collapse = ", "),
    .groups = "drop"
  ) %>%
  complete(station = unique(all_temps$station), 
           fill = list(dates_exceeding = "—"))

exceedance_table <- yes_triggers %>%
  left_join(exceedance_dates, by = "station") %>%
  mutate(
    threshold = "72°F / 22°C",
    status = if_else(n_yes > 0, "Threshold Exceeded", "Below Threshold")
  ) %>%
  select(
    Location             = station,
    `Threshold (°F/°C)`  = threshold,
    `Days Exceeding (n)` = n_yes,
    `Dates Exceeding`    = dates_exceeding,
    Status               = status
  )

# --- Convert Table to Grob ---
table_grob <- tableGrob(
  exceedance_table,
  rows = NULL,
  theme = ttheme_minimal(
    core = list(
      fg_params = list(hjust = 0, x = 0.05, fontsize = 11),
      bg_params = list(fill = c("white", "#fff0f0"), col = NA)  # alternating row fill; red tint for exceeded row
    ),
    colhead = list(
      fg_params = list(hjust = 0, x = 0.05, fontsize = 11, fontface = "bold"),
      bg_params = list(fill = "grey92", col = NA)
    )
  )
)

# --- Plot ---
temp_offramp <- ggplot(all_temps, mapping = aes(x = date, y = temp)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  geom_point(aes(fill = trigger), size = 4, shape = 21) +
  geom_label(yes_triggers, mapping = aes(x = max(all_temps$date) - 1,
                                         y = 19,
                                         label = paste0('n = ', n_yes)),
             size = 5) +
  geom_hline(yintercept = 22.2, linetype = 'dashed', color = 'red', linewidth = 1) +
  labs(x = 'Date', y = 'Water Temperature (C)') +
  ylim(c(18, 25)) +
  scale_fill_manual(values = c('#33cc33', 'red')) +
  facet_wrap(~station, ncol = 1) +
  theme_bw() +
  theme(legend.position = 'none',
        plot.margin = margin(0.2, 0.5, 0.2, 0.2, unit = 'cm'),
        axis.title.y = element_text(margin = margin(r = 15), size = 15),
        axis.title.x = element_text(margin = margin(t = 15), size = 15),
        strip.text = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13))

# --- Combine Table + Plot ---
table_patch <- wrap_elements(table_grob)

combined <- table_patch / temp_offramp +
  plot_layout(heights = c(1, 3)) +
  plot_annotation(
    #title   = "2.8 End of Entrainment Management for Salmonids",
    #caption = "Dashed line indicates 22°C threshold. Red points indicate exceedance days.",
    theme   = theme(
      plot.title   = element_text(size = 14, face = "bold", margin = margin(b = 10)),
      plot.caption = element_text(size = 10, color = "grey40", margin = margin(t = 10))
    )
  )

combined

ggsave(combined, file = 'outputs/temp_offramp_with_table.png', height = 10, width = 9)
