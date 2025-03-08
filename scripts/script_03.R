
# Case Study No. 3 - AMU ALoS Dumbbell Chart ------------------------------


# Step 0 - load the packages ----------------------------------------------

library(readxl)
library(tidyverse)
library(patchwork)
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
         amu_end_quarter)


# Step 8 - prepare the summary table for plotting -------------------------

summary_amu_stays_plot <-
  df_amu_stays_d |> 
  group_by(anon_cons_code,
           amu_end_quarter) |> 
  summarize(no_of_stays = n(),
            alos = mean(amu_los_hours),
            sd = sd(amu_los_hours)) |> 
  filter(anon_cons_code != "cons_147")


# Step 9 - draw the plot --------------------------------------------------

summary_amu_stays_plot %>% 
  ggplot(aes(x = alos,
             y = anon_cons_code)) +
  geom_line(aes(group = anon_cons_code), 
            colour = "#E7E7E7", 
            linewidth = 3.5) +
  geom_point(aes(colour = amu_end_quarter), 
             size = 3) +
  scale_colour_manual(values=c("#436685", "#BF2F24")) +
  scale_x_continuous(limits = c(12, 48),
                     breaks = seq(12, 48, 6)) +
  labs(title = "AMU length of stay was lower in <span style='color: #BF2F24;'>Q4</span> than in <span style='color: #436685;'>Q1</span>",
       subtitle = "13 consultant physicians at Anytown General Hospital: 2014-15",
       x = "Average length of stay in AMU (hours)",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_markdown(size=14, face = "bold"),
        plot.subtitle = element_text(size=10),
        axis.text = element_text(colour = "black",
                                 size = 9),
        axis.title = element_text(size = 9),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
