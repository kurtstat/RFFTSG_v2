
# Join Practice -----------------------------------------------------------


# load packages -----------------------------------------------------------

library(readxl)
library(tidyverse)


# import data -------------------------------------------------------------

table_01 <-
  read_xlsx("data/join_table_examples.xlsx",
            "table_01")

table_02 <-
  read_xlsx("data/join_table_examples.xlsx",
            "table_02")


# left_join() -------------------------------------------------------------

df_left <-
  table_01 |> 
  left_join(table_02,
            join_by(x$seq_no == y$seq_no))


# right_join() ------------------------------------------------------------

df_right <-
  table_02 |> 
  left_join(table_01,
            join_by(y$seq_no == x$seq_no))


# inner_join() ------------------------------------------------------------

df_inner <-
  df_inner <-
  table_01 |> 
  inner_join(table_02,
            join_by(x$seq_no == y$seq_no))


# anti_join() -------------------------------------------------------------

df_anti <-
  table_01 |> 
  anti_join(table_02,
             join_by(x$seq_no == y$seq_no))


# full_join() -------------------------------------------------------------

df_full <-
  table_01 |> 
  full_join(table_02,
            join_by(x$seq_no == y$seq_no))
