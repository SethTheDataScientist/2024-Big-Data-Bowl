FullTeamTable <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/Full_TeamTable.csv")

OffTeamTable <- FullTeamTable %>% 
  mutate(KeepAway = (
    percent_rank(MeanPlayO) * 2 +
    percent_rank(BreakThroughO)
                            )/3,
    Voronoi_Sum = MeanPlayO,
    BreakThrough = BreakThroughO * 100
    ) %>% 
  select(possessionTeam, Voronoi_Sum, BreakThrough, KeepAway) %>% 
  arrange(desc(KeepAway))

DefTeamTable <- FullTeamTable %>% 
  mutate(KeepAway = (
    (1 - percent_rank(MeanPlayD)) * 2 +
      (1 - percent_rank(BreakThroughD))
  )/3,
  Voronoi_Sum = MeanPlayD,
  BreakThrough = BreakThroughD * 100
  ) %>% 
  select(possessionTeam, Voronoi_Sum, BreakThrough, KeepAway) %>% 
  arrange(desc(KeepAway))

# Define the color palette
color_palette <- c("#e15759", "#edc948", "#59a14f")

table = gt(DefTeamTable) %>% 
  data_color(
    columns = c("Voronoi_Sum", "BreakThrough"),
    colors = scales::col_numeric(
      palette = color_palette,
      domain = NULL,
      reverse = T  # Set this to TRUE for color reversal
    )) %>% 
  data_color(
    columns = c("KeepAway"),
    colors = scales::col_numeric(
      palette = color_palette,
      domain = NULL,
      reverse = F  # Set this to TRUE for color reversal
    )) %>% 
  fmt_number(columns = c("Voronoi_Sum", "BreakThrough","KeepAway",
                         ), decimals = 2)%>% 
  opt_align_table_header(align = "center") %>% 
  cols_align("center") %>% 
  opt_row_striping() %>% 
  gt_theme_espn() %>% 
  tab_header(
    title = md("Defensive Team Total Values"),
    subtitle = "Voronoi Weighted Sum of Area, Breakthrough Percentage, and KeepAway (Weighted Percentile of Voronoi and Breakthrough)") %>% 
  tab_source_note("Table: @SethDataScience | 2024 Big Data Bowl | Data: 2022 Weeks 1-9") %>% 
gt_nfl_wordmarks(column ="possessionTeam", height = 30, locations = NULL)



gtsave(table, "Defensive Team Keepaway.png")



# Offense and Defensive Players -------------------------------------------

Full_Offense <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/Full_Offense.csv")

OffTable <- Full_Offense %>% 
  group_by(nflId) %>% 
  slice_tail(n = 1) %>% 
  ungroup() %>% 
  filter(position == "RB") %>% 
  mutate(
  Voronoi_Sum = MeanPlay,
  BreakThrough = UnCoveredPerc * 100,
  KeepAway = KEEPAWAY,
  TopBottom = if_else(KeepAway >= 0.72, "Top 10", "Bottom 10"),
  ) %>% 
  group_by(TopBottom) %>% 
  select(displayName, club, Voronoi_Sum, BreakThrough, KeepAway) %>% 
  arrange(desc(KeepAway)) %>% 
  distinct()

top_and_bottom_10 <- rbind(head(OffTable, 10), tail(OffTable, 10))

table = gt(top_and_bottom_10) %>% 
  data_color(
    columns = c("Voronoi_Sum", "BreakThrough"),
    colors = scales::col_numeric(
      palette = color_palette,
      domain = NULL,
      reverse = F  # Set this to TRUE for color reversal
    )) %>% 
  data_color(
    columns = c("KeepAway"),
    colors = scales::col_numeric(
      palette = color_palette,
      domain = NULL,
      reverse = F  # Set this to TRUE for color reversal
    )) %>% 
  fmt_number(columns = c("Voronoi_Sum", "BreakThrough","KeepAway",
  ), decimals = 2)%>% 
  opt_align_table_header(align = "center") %>% 
  cols_align("center") %>% 
  opt_row_striping() %>% 
  gt_theme_espn() %>% 
  tab_header(
    title = md("Running Back Total Values"),
    subtitle = "Voronoi Weighted Sum of Area, Breakthrough Percentage, and KeepAway (Weighted Percentile of Voronoi and Breakthrough)") %>% 
  tab_source_note("Table: @SethDataScience | 2024 Big Data Bowl | Data: 2022 Weeks 1-9") %>% 
  gt_nfl_wordmarks(column ="club", height = 30, locations = NULL)



gtsave(table, "RB Keepaway.png")




Full_Defense <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/Full_Defense.csv")

DefTable <- Full_Defense %>% 
  group_by(nflId) %>% 
  slice_tail(n = 1) %>% 
  ungroup() %>% 
  filter(Alt_Position == "LB") %>% 
  mutate(
    Voronoi_Sum = ClosingPlay,
    BreakThrough = CoveringPerc * 100,
    KeepAway = KEEPAWAY,
    TopBottom = if_else(KeepAway >= 0.48, "Top 10", "Bottom 10"),
  ) %>% 
  group_by(TopBottom) %>% 
  select(displayName, club, Voronoi_Sum, BreakThrough, KeepAway) %>% 
  arrange(desc(KeepAway)) %>% 
  distinct()

top_and_bottom_10 <- rbind(head(DefTable, 10), tail(DefTable, 10))

table = gt(top_and_bottom_10) %>% 
  data_color(
    columns = c("Voronoi_Sum"),
    colors = scales::col_numeric(
      palette = color_palette,
      domain = NULL,
      reverse = T  # Set this to TRUE for color reversal
    )) %>% 
  data_color(
    columns = c("KeepAway", "BreakThrough"),
    colors = scales::col_numeric(
      palette = color_palette,
      domain = NULL,
      reverse = F  # Set this to TRUE for color reversal
    )) %>% 
  fmt_number(columns = c("Voronoi_Sum", "BreakThrough","KeepAway",
  ), decimals = 2)%>% 
  opt_align_table_header(align = "center") %>% 
  cols_align("center") %>% 
  opt_row_striping() %>% 
  gt_theme_espn() %>% 
  tab_header(
    title = md("Linebacker Total Values"),
    subtitle = "Voronoi Weighted Sum of Area, Breakthrough Percentage, and KeepAway (Weighted Percentile of Voronoi and Breakthrough)") %>% 
  tab_source_note("Table: @SethDataScience | 2024 Big Data Bowl | Data: 2022 Weeks 1-9") %>% 
  gt_nfl_wordmarks(column ="club", height = 30, locations = NULL)



gtsave(table, "LB Keepaway.png")


