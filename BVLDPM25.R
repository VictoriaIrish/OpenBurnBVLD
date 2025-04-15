library(envair)
library(tidyverse)
library(ggplot2)
library(openair)

#retrieve raw data
SMITHERS_RAW_PM <- importBC_data(parameter_or_station = "pm25", years = 2020:2025)

RAW_PM <- SMITHERS_RAW_PM %>%
  filter(STATION_NAME %in% c("Smithers Muheim Memorial", "Burns Lake Fire Centre", "Houston Firehall")) %>%
  rename(date = DATE_PST)

#calculate rolling average
ROLLING_24_AVG <- RAW_PM %>%
  group_by(STATION_NAME) %>% 
  mutate(ROLLING_24_VALUE = zoo::rollapply(RAW_VALUE, width = 24, FUN = mean, fill = NA, align = "right"),
         is_sept_nov = month(date) %in% c(9, 10, 11)
         )

#plot 24 averaged hourly data 
ROLLING_24_AVG %>%
  ggplot(aes(x = date, y = ROLLING_24_VALUE, color = STATION_NAME)) +
  geom_line() +
  geom_hline(yintercept = 25, col = "red", linetype = "dotted") +
  ylim(0, 100) +
  labs(title = "Timeseries of Rolling 24-hr average PM2.5", 
       x = "Date", 
       y = "Rolling 24-hr average") +
  facet_wrap(~ STATION_NAME, ncol = 1) +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%b %Y")

# Plot with highlighted Sept-Nov regions
ROLLING_24_AVG %>%
  ggplot(aes(x = date, y = ROLLING_24_VALUE, color = STATION_NAME)) +
  # Add shaded regions for Sept-Nov
  geom_rect(data = ROLLING_24_AVG %>% filter(is_sept_nov),
            aes(xmin = date, xmax = date + days(1), ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "bisque3", alpha = 0.1) +
  geom_line() +
  geom_hline(yintercept = 25, col = "red", linetype = "dotted") +
  ylim(0, 100) +
  labs(x = "Date", 
       y = expression(paste("Rolling 24-hr average ", PM[2.5])),
       color = "") +
  facet_wrap(~ STATION_NAME, ncol = 1) +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_bw() + 
  theme(legend.position = "right",
                      axis.text = element_text(size = 12),
                      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                      axis.title = element_text(size = 16),
                      strip.text.x = element_text(size = 14),
                      legend.text = element_text(size = 12),
                      panel.grid = element_blank())
