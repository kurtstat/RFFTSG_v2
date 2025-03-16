
# Case Study No. 2 - ED fullness minute by minute -------------------------


# Step 0 - load the packages ----------------------------------------------

library(openxlsx)
library(tidyverse)
library(ggpattern)


# Step 1 - import the data ------------------------------------------------

df_ed_day <-
  read.xlsx("https://www.kurtosis.co.uk/data/01_ed_duration_extract.xlsx") |> 
  mutate(new_arrival_datetime = convertToDateTime(arrival_datetime)) |> 
  mutate(new_departure_datetime = convertToDateTime(departure_datetime)) |>filter(new_arrival_datetime <= as.POSIXct('2015-07-09 23:59',
                                        tz = "UTC")) |> 
  filter(new_departure_datetime >= as.POSIXct('2015-07-09 00:00',
                                          tz = "UTC"))


# Step 2 - create a dataframe with 1441 minutes ---------------------------

df_1441 <-
  data_frame(date_hour_minute = seq(as.POSIXct('2015-07-09 00:00', 
                                               tz = "UTC-1"),
                                    as.POSIXct('2015-07-10 00:00', 
                                               tz = "UTC-1"),
                                    by = "1 min"))


# Step 3 - do the non-equi join -------------------------------------------

df_1441_a <-
  df_1441 |> 
  left_join(df_ed_day,
            join_by(date_hour_minute >= new_arrival_datetime,
                    date_hour_minute < new_departure_datetime)) |> 
  group_by(date_hour_minute) |> 
  summarize(ed_fullness = n())


# Step 4 - calculate the minimum and maximum fullness --------------------

min_fullness <- 
  df_1441_a |> 
  arrange(ed_fullness) |> 
  mutate(seq_number = row_number()) |> 
  filter(seq_number == 1) |> 
  select(date_hour_minute)

max_fullness <- 
  df_1441_a |> 
  arrange(desc(ed_fullness)) |> 
  mutate(seq_number = row_number()) |> 
  filter(seq_number == 1) |> 
  select(date_hour_minute)


# Step 5 - draw a graph ---------------------------------------------------

ggplot(data = df_1441_a) +
  aes(x = date_hour_minute,
      y = ed_fullness) +
  geom_area_pattern(pattern = "gradient",
                    pattern_fill = "#e5f5e0",
                    pattern_fill2 = "#31a354") +
  geom_line(colour = "#31a354") +
  geom_segment(x = as.numeric(min_fullness),
               y = 0,
               xend = as.numeric(min_fullness),
               yend = 16,
               colour = "black",
               linetype = "dashed") +
  geom_segment(x = as.numeric(max_fullness),
               y = 0,
               xend = as.numeric(max_fullness),
               yend = 25,
               colour = "black",
               linetype = "dashed") +
  scale_x_datetime(date_labels = "%H:%M") +
  scale_y_continuous(limits = c(0, 30),
                     breaks = seq(0, 30, 5)) +
  annotate("text", 
            x = as.POSIXct('2015-07-09 06:56'),
            y = 15, 
            label = "Minimum (1 patient in at 06:56)",
            size = 2.5,
            hjust = 1.03,
            colour = "black") +
  annotate("text", 
           x = as.POSIXct('2015-07-09 12:45'),
           y = 24, 
           label = "Maximum (20 patients in at 12:45)",
           size = 2.5,
           hjust = -0.03,
           colour = "black") +
  labs(title = "100% compliance | 148 attendances",
       subtitle = "Minute-by-minute fullness snapshots: Thu 9 July 2015",
       x = "",
       y = "") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5),"cm"))

