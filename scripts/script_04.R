# Case Study No. 4 - draw an annotated chart of the weekly AMU in-reach data


# Step 0 - load the packages ----------------------------------------------

library(tidyverse)
library(openxlsx)
library(scales)


# Step 1 - import the data ------------------------------------------------

df_01 <-
  read.xlsx("https://www.kurtosis.co.uk/data/inreach_weekly_data.xlsx") |> 
  mutate(new_discharge_week = convertToDate(discharge_week)) |> 
  select(new_discharge_week,
         transferred_onwards,
         discharged_home,
         total)


# Step 2 - calculate the weekly percentages -------------------------------

df_01a <-
  df_01 |> 
  drop_na(new_discharge_week) |> 
  mutate(weekly_percentage = discharged_home / total)


# Step 3 - split into autumn 2014 and summer 2015 ------------------------

autumn <-
  df_01a |> 
  filter(new_discharge_week <= '2014-12-22')

summer <- 
  df_01a |> 
  filter(new_discharge_week >= '2015-06-29')


# Step 4 - calculate the control chart values ----------------------------

first_mean <- mean(autumn$weekly_percentage)
first_amr <- mean(abs(diff(autumn$weekly_percentage)))
first_lcl <- first_mean - (2.66 * first_amr)
first_ucl <- first_mean + (2.66 * first_amr)

second_mean <- mean(summer$weekly_percentage)
second_amr <- mean(abs(diff(summer$weekly_percentage)))
second_lcl <- second_mean - (2.66 * second_amr)
second_ucl <- second_mean + (2.66 * second_amr)


# Step 5 - add the constants to the dataframe -----------------------------

df_01b <-
  df_01a |> 
  mutate(first_mean = if_else(new_discharge_week <= '2014-12-22',
                              first_mean,
                              NA)) |> 
  mutate(first_lcl = if_else(new_discharge_week <= '2014-12-22',
                              first_lcl,
                              NA)) |> 
  mutate(first_ucl = if_else(new_discharge_week <= '2014-12-22',
                              first_ucl,
                              NA)) |> 
  mutate(second_mean = if_else(new_discharge_week >= '2015-06-29',
                              second_mean,
                              NA)) |> 
  mutate(second_lcl = if_else(new_discharge_week >= '2015-06-29',
                             second_lcl,
                             NA)) |> 
  mutate(second_ucl = if_else(new_discharge_week >= '2015-06-29',
                             second_ucl,
                             NA))


# Step 6 - draw the chart -------------------------------------------------

ggplot(data = df_01b) +
  annotate("rect", 
           xmin = as.Date('2014-09-27'), 
           xmax = as.Date('2014-12-22'), 
           ymin = first_lcl, 
           ymax = first_ucl,
           fill = "darkorange",
           alpha = 0.05) +
  annotate("rect", 
           xmin = as.Date('2015-06-29'), 
           xmax = as.Date('2015-09-21'), 
           ymin = second_lcl, 
           ymax = second_ucl,
           fill = "blue",
           alpha = 0.05) +
  aes(x = new_discharge_week) +
  geom_line(aes(y = weekly_percentage)) +
  geom_point(aes(y = weekly_percentage)) +
  geom_line(aes(y = first_mean),
            colour = "darkorange",
            linewidth = 0.5) +
  geom_line(aes(y = first_lcl),
            linetype = "dashed",
            colour = "black") +
  geom_line(aes(y = first_ucl),
            linetype = "dashed",
            colour = "black") +
  geom_line(aes(y = second_mean),
            colour = "blue",
            linewidth = 0.5) +
  geom_line(aes(y = second_lcl),
            linetype = "dashed",
            colour = "black") +
  geom_line(aes(y = second_ucl),
            linetype = "dashed",
            colour = "black") +
  annotate("text", 
           x = as.Date('2014-09-29'),
           y = 0.06, 
           label = "Autumn 2014 = 28%",
           size = 2.8,
           hjust = 0,
           colour = "darkorange") +
  annotate("text", 
           x = as.Date('2015-06-29'),
           y = 0.49, 
           label = "Summer 2015 = 34%",
           size = 2.8,
           hjust = 0,
           colour = "blue") +
  scale_y_continuous(limits = c(0, 0.5),
                     breaks = seq(0, 0.5, 0.1),
                     labels = percent) +
  labs(title = "More patients went home from the Acute Medical Unit",
       subtitle = "Weekly percentage of direct discharges: 29 Sep 2014 to 27 Sep 2015",
       x = "",
       y = "") +
  theme_classic() +
  theme(plot.margin = unit(c(0.5,0.5,0,0), "cm"))
  
