
# Case Study No. 1 - The Barbershop Theory of ED Delays -------------------


# Step 0 - load the packages ----------------------------------------------

library(readxl)
library(tidyverse)


# Step 1 - import the data ------------------------------------------------

df_ed_duration_extract <-
  read_xlsx("data/01_ed_duration_extract.xlsx")


# Step 2 - create a dataframe of unique arrival_datetimes -----------------

df_unique_arrival_times <-
  df_ed_duration_extract |> 
  filter(arrival_datetime >= as.POSIXct('2014-09-29 00:00',
                                            tz = "UTC")) |> 
  group_by(arrival_datetime) |> 
  summarize(no_of_attendances = n()) |> 
  rename(unique_arrival_datetime = arrival_datetime) |> 
  select(unique_arrival_datetime) |> 
  arrange(unique_arrival_datetime)


# Step 3 - find the ED fullness values for each unique arrival time -------

df_ed_fullness_values <-
  df_unique_arrival_times |> 
  left_join(df_ed_duration_extract,
            join_by(unique_arrival_datetime >= arrival_datetime,
                    unique_arrival_datetime < departure_datetime)) |> 
  group_by(unique_arrival_datetime) |> 
  summarize(ed_fullness = n())


# Step 4 - find the mean ED los_mins values for each unique arrival time --

df_ed_mean_los_mins <-
  df_ed_duration_extract |> 
  filter(arrival_datetime >= as.POSIXct('2014-09-29 00:00',
                                        tz = "UTC")) |> 
  group_by(arrival_datetime) |> 
  summarize(mean_ed_los_mins = mean(los_mins))


# Step 5 - join the ED los_mins values to the ED_fullness values ----------

df_final <-
  df_ed_fullness_values |> 
  left_join(df_ed_mean_los_mins,
            join_by(unique_arrival_datetime == arrival_datetime))


# Step 6 - draw a massive scatterplot -------------------------------------

ggplot(data = df_final) +
  aes(x = ed_fullness,
      y = mean_ed_los_mins) +
  geom_point(alpha = 0.1) +
  scale_y_continuous(limits = c(0, 1440),
                     breaks = seq(0, 1440, 180))


# Step 7 - summarize the df_final data ------------------------------------

df_final_x <-
  df_final |> 
  group_by(ed_fullness) |> 
  summarize(mean_ed_los_mins = mean(mean_ed_los_mins),
            no_of_arrivals = n())


# Step 8 - draw a bubbleplot using the summarized data --------------------

ggplot(data = df_final_x) +
  aes(x = ed_fullness,
      y = mean_ed_los_mins,
      size = no_of_arrivals) +
  geom_point(pch = 21,
             fill = "yellow",
             colour = "black",
             alpha = 0.7) +
  geom_smooth(data = df_final_x |> 
              filter(ed_fullness < 36),
                method = lm, 
              se = FALSE, 
              colour ="blue",
              linetype = "dashed",
              linewidth = 0.9) +
  scale_x_continuous(limits = c(0, 60),
                     breaks = seq(0, 60, 10)) +
  scale_y_continuous(limits = c(120, 240),
                     breaks = seq(120, 240, 15)) +
  labs(title = "The Barbershop Theory of ED Delay",
       subtitle = "The fuller the ED on arrival, the longer the ED length of stay",
       x = "No. of patients in ED on arrival",
       y = "Length of stay in ED (mins)") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
