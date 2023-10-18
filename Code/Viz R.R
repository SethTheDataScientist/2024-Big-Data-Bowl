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
  BreakThrough = BreakThroughD * 100,
  possessionTeam2 = possessionTeam
  ) %>% 
  select(possessionTeam,possessionTeam2, Voronoi_Sum, BreakThrough, KeepAway) %>% 
  arrange(desc(KeepAway))

# Define the color palette
color_palette <- c("#e15759", "#edc948", "#59a14f")

gt(DefTeamTable) %>% 
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
gt_nfl_wordmarks(column ="possessionTeam2", height = 30, locations = NULL)%>% 
gt_nfl_logos(column ="possessionTeam", height = 30, locations = NULL)






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



#DATA VIZ ----------------------------------------------------------------
# Gravity Plot ------------------------------------------------------------
library(deldir)
library(magick)
library(ggforce)
library(gganimate)
library(transformr)

AllGamesYAC <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/AllGamesYAC.csv")
select_plays <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/viz_play.csv")

# Field stuff -------------------------------------------------------------
ScalingFactor = 2

SelectPlay <- select_plays %>%
  filter(club != "football",
         frameId > 7) %>% 
  mutate(x = if_else(is.na(x) == 1, 0, x * ScalingFactor),
         y = if_else(is.na(y) == 1, 0, y * ScalingFactor),
         HasBall = if_else(frameId >= 2,
                           1,
                           0.000000000000000001),
         ballCarrier = if_else(nflId == 43454,
                               HasBall * 100,
                               0.000000000000001)) %>% 
  group_by(frameId)
  # mutate(
  #   x_max = max(x)+1,
  #   x_min = min(x)-1,
  #   y_max = max(y)+1,
  #   y_min = min(y)-1,
  #   bound = cbind(c(y_min, y_max, y_max), c(x_min, x_min, x_max, x_max)))


hash_right <- 38.35 * ScalingFactor
hash_left <- 12 * ScalingFactor
hash_width <- 3.3 * ScalingFactor
# Define the range of coordinates
xmin <- 0 * ScalingFactor
xmax <- 53.3 * ScalingFactor
ymin <- max(round(min(SelectPlay$x, 
                      na.rm = TRUE) - (5 * ScalingFactor), -1), 0 * ScalingFactor)
ymax <- min(round(max(SelectPlay$x, 
                      na.rm = TRUE) + (5 * ScalingFactor), -1), 120 * ScalingFactor)

df_hash <- 
  expand.grid(x = c(0, 23.36667 * ScalingFactor, 29.96667 * ScalingFactor, xmax -  1 * ScalingFactor), 
              y = (seq((10 * ScalingFactor), (110 * ScalingFactor), by = ScalingFactor))) %>% 
  filter(!(floor(y %% (5 * ScalingFactor)) == 0), y < ymax, y > ymin)

field_base <- ggplot() +
  annotate("segment", x = df_hash$x[df_hash$x < 55/2 * ScalingFactor],
           y = df_hash$y[df_hash$x < 55/2 * ScalingFactor],
           xend = df_hash$x[df_hash$x < 55/2 * ScalingFactor] + 1 * ScalingFactor,
           yend = df_hash$y[df_hash$x < 55/2 * ScalingFactor]) +
  
  annotate("segment", x = df_hash$x[df_hash$x > 55/2 * ScalingFactor],
           y = df_hash$y[df_hash$x > 55/2 * ScalingFactor],
           xend = df_hash$x[df_hash$x > 55/2 * ScalingFactor] + 1 * ScalingFactor,
           yend = df_hash$y[df_hash$x > 55/2 * ScalingFactor]) +
  
  annotate("segment", x = xmin, y = seq(max(10 * ScalingFactor, ymin), min(ymax, 110 * ScalingFactor), by = 5 * ScalingFactor),
           xend =  xmax, yend = seq(max(10 * ScalingFactor, ymin), min(ymax, 110 * ScalingFactor), by = 5 * ScalingFactor)) +
  
  annotate("text", x = rep(hash_left, 11), y = seq(10 * ScalingFactor, 110 * ScalingFactor, by = 10 * ScalingFactor),
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 5 * ScalingFactor) +
  
  annotate("text", x = rep((xmax - hash_left), 11), y = seq(10 * ScalingFactor, 110 * ScalingFactor, by = 10 * ScalingFactor),
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),
           angle = 90, size = 5 * ScalingFactor) +
  
  annotate("segment", x = c(xmin, xmin, xmax, xmax), y = c(ymin, ymax, ymax, ymin),
           xend = c(xmin, xmax, xmax, xmin), yend = c(ymax, ymax, ymin, ymin), color = "black")


# Animated VIZ ---- 
# 
# triangle <- cbind(c(0, 53.3 * ScalingFactor, 53.3 * ScalingFactor, 0), c(60 * ScalingFactor, 60 * ScalingFactor, 90 * ScalingFactor, 90 * ScalingFactor))


triangle <- cbind(c(xmin, xmax, xmax, xmin), c(ymin, ymin, ymax, ymax))

play_animationVIZ = field_base+
  geom_voronoi_tile(data = SelectPlay,
                    aes(x = (xmax - y), y = x,
                        fill = "#0e8326", group = -1,
                        alpha = ballCarrier),
                        bound = triangle)+
  geom_voronoi_segment(data = SelectPlay,
                    aes(x = (xmax - y), y = x),
                    color = "black")+
  geom_point(data = SelectPlay, 
             aes(x = (xmax - y), y = x,
                 group = nflId, 
                 fill = primary,
                 color = primary,
                 ),
             size = 7 * ScalingFactor) +
  geom_text(data = SelectPlay,
            aes(x = (xmax-y), y = x, label = jerseyNumber),
            color = "white",
                size = 3.5 * ScalingFactor,
            vjust = 0.36 * ScalingFactor) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_identity(aesthetics = c("fill", "color")) +
  ylim(ymin, ymax)+
  cowplot::theme_nothing() + theme(plot.title = element_text(),
                                   plot.subtitle = element_text(),
                                   plot.caption = element_text()) +
  labs(caption = "@SethDataScience | Data: GameId = 2022102311, PlayId = 159")+
  transition_time(SelectPlay$frameId) +
  ease_aes('linear') + NULL


ex_play_lengthVIZ <- length(unique(SelectPlay$frameId))


animate(play_animationVIZ, fps = 25, nframe = ex_play_lengthVIZ, height = 1200, width = 1000)

