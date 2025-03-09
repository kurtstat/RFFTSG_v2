
# Case Study No. 3 - AMU ALoS Dumbbell Chart (more conventional method) ---


# Step 0 - load the packages ----------------------------------------------

library(readxl)
library(tidyverse)
library(ggtext)


# Step 1 - import the data ------------------------------------------------

df_amu_stays <-
  read_xlsx("data/03_amu_stays.xlsx")


# Step 2 - delete the improbably long stays -------------------------------

df_amu_stays_a <-
  df_amu_stays |> 
  filter(amu_los_hours <= (7*24)) |> 
  arrange(desc(amu_los_hours))


# Step 3 - append a quarter variable --------------------------------5-----

df_amu_stays_b <-
  df_amu_stays_a |> 
  mutate(amu_end_quarter = case_when(amu_end_datetime <= '2014-12-28 23:59' ~ "q1",
                                     amu_end_datetime > '2014-12-28 23:59' 
                                     & amu_end_datetime <= '2015-03-29 23:59' ~ "q2",
                                     amu_end_datetime > '2015-03-29 23:59' 
                                     & amu_end_datetime <= '2015-06-28 23:59' ~ "q3",
                                     amu_end_datetime > '2014-12-28 23:59' ~ "q4")) |> 
  arrange(amu_end_datetime)


# Step 4 - check what happened to AMU ALoS q1 to q4 -----------------------

summary_amu_stays_q <-
  df_amu_stays_b |> 
  group_by(amu_end_quarter) |> 
  summarize(no_of_stays = n(),
            amu_alos = mean(amu_los_hours))


# Step 5 - delete the q2 and q4 stays -------------------------------------

df_amu_stays_c <-
  df_amu_stays_b |> 
  filter(amu_end_quarter == "q1" | amu_end_quarter == "q4")


# Step 6 - create a summary anon_cons_code league table -------------------

summary_amu_stays_cons <-
  df_amu_stays_c |> 
  group_by(anon_cons_code) |> 
  summarize(q1_stays = sum(if_else(amu_end_quarter == "q1", 1, 0)),
            q4_stays = sum(if_else(amu_end_quarter == "q4", 1, 0)),
            include = if_else(q1_stays > 50 & q4_stays > 50,"include", "exclude"))


# Step 7 - append the include variable to df_amu_stays_c ------------------

df_amu_stays_d <-
  df_amu_stays_c |> 
  left_join(summary_amu_stays_cons,
            join_by(anon_cons_code == anon_cons_code)) |> 
  filter(include == "include") |> 
  select(anon_id,
         anon_cons_code,
         amu_los_hours,
         amu_end_quarter) |> 
  filter(anon_cons_code != "Consultant 147")


# Step 8a - prepare the q1 summary table for plotting ------------------------

summary_amu_stays_plot_q1 <-
  df_amu_stays_d |> 
  filter(amu_end_quarter == "q1") |> 
  group_by(anon_cons_code) |> 
  summarize(alos_q1 = mean(amu_los_hours))
  

# Step 8b - prepare the q4 summary table for plotting ------------------------

summary_amu_stays_plot_q4 <-
  df_amu_stays_d |> 
  filter(amu_end_quarter == "q4") |> 
  group_by(anon_cons_code) |> 
  summarize(alos_q4 = mean(amu_los_hours))


# Step 9 - join the two summary tables together ---------------------------

summary_amu_stays_plot <-
  summary_amu_stays_plot_q1 |> 
  left_join(summary_amu_stays_plot_q4,
            join_by(anon_cons_code == anon_cons_code))

# Step 10 - draw the plot -------------------------------------------------
ggplot(data = summary_amu_stays_plot) +
  aes(y = reorder(anon_cons_code,
                  alos_q1)) +
  geom_segment(aes(x = alos_q1, xend = alos_q4,
                   y = anon_cons_code, yend = anon_cons_code),
               color = "#E7E7E7", 
               linewidth = 3.5) +
  geom_point(aes(x = alos_q1),
             size = 3,
             colour = "#436685") +
  geom_point(aes(x = alos_q4),
             size = 3,
             colour = "#BF2F24") +
  scale_x_continuous(limits = c(18, 42),
                     breaks = seq(18, 42, 6)) +
  labs(title = "AMU stays were shorter in <span style='color: #BF2F24;'>Q4</span> than in <span style='color: #436685;'>Q1</span>...",
       subtitle = "...but some consultants achieved bigger reductions than others",
       x = "Average length of stay in AMU (hours)",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        plot.title = element_markdown(size=14, face = "bold"),
        plot.subtitle = element_text(size=10),
        axis.title=element_text(size=9),
        axis.text = element_text(size = 9),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "lightgrey",
                                          size = 0.5,
                                          linetype = 3),
        panel.grid.minor.x = element_blank())
