
# Team Rankings -----------------------------------------------------------


FullTeamTable <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/Full_TeamTable.csv")
# 
# OffTeamTable <- FullTeamTable %>% 
#   mutate(KeepAway = (
#     percent_rank(MeanPlayO) * 2 +
#     percent_rank(BreakThroughO)
#                             )/3,
#     Voronoi_Sum = MeanPlayO,
#     BreakThrough = BreakThroughO * 100
#     ) %>% 
#   select(possessionTeam, Voronoi_Sum, BreakThrough, KeepAway) %>% 
#   arrange(desc(KeepAway))

DefTeamTable <- FullTeamTable %>%
  group_by() %>% 
  mutate(PassPercentRankPUR = 1 - percent_rank(PassVoroni),
         PassPercentRankSU = percent_rank(PassShielding),
         PassPercentRankIT =  1-percent_rank(PassRootMeanSquaredDeviation),
         PassPURSUIT = (PassPercentRankPUR +
                          PassPercentRankSU + 
                          PassPercentRankIT) / 3,
         RushPercentRankPUR = 1 - percent_rank(RushVoronoi),
         RushPercentRankSU = percent_rank(RushShielding),
         RushPercentRankIT =  1-percent_rank(RushRootMeanSquaredDeviation),
         RushPURSUIT = (RushPercentRankPUR +
                          RushPercentRankSU + 
                          RushPercentRankIT) / 3,
         TotalPercentRankPUR = 1 - percent_rank(TotalVoronoi),
         TotalPercentRankSU = percent_rank(TotalShielding),
         TotalPercentRankIT =  percent_rank(TotalRootMeanSquaredDeviation),
         TotalPURSUIT = (TotalPercentRankPUR +
                           TotalPercentRankSU + 
                           TotalPercentRankIT) / 3,
         Team = defensiveTeam
  ) %>% 
  ungroup() %>%  
  mutate(
    Voronoi_Reduction = TotalVoronoi,
    Shielding_Upfield = TotalShielding * 100,
    Ideal_Angle = TotalRootMeanSquaredDeviation,
    PURSUIT = TotalPURSUIT,
    Team_Logo = Team,
  ) %>% 
  select(Team, Team_Logo, Voronoi_Reduction, Shielding_Upfield, Ideal_Angle, PURSUIT) %>% 
  arrange(desc(PURSUIT)) %>% 
  distinct()



# Define the color palette
color_palette <- c("#e15759", "#edc948", "#59a14f")

table = gt(DefTeamTable) %>% 
  data_color(
    columns = c("Voronoi_Reduction", "Ideal_Angle"),
    colors = scales::col_numeric(
      palette = color_palette,
      domain = NULL,
      reverse = T  # Set this to TRUE for color reversal
    )) %>% 
  data_color(
    columns = c("Shielding_Upfield", "PURSUIT"),
    colors = scales::col_numeric(
      palette = color_palette,
      domain = NULL,
      reverse = F  # Set this to TRUE for color reversal
    )) %>% 
  fmt_number(columns = c("Voronoi_Reduction", "Shielding_Upfield","Ideal_Angle", "PURSUIT"
  ), decimals = 2)%>% 
  opt_align_table_header(align = "center") %>% 
  cols_align("center") %>% 
  opt_row_striping() %>% 
  gt_theme_espn() %>% 
  tab_header(
    title = md("Defensive Team Total Values"),
    subtitle = "Voronoi Area Sum Allowed, Shielding  Percentage, and Deviation from Ideal Angle (Weighted Percentile of Voronoi, Shielding, and Ideal Angle)") %>% 
  tab_source_note("Table: @SethDataScience | 2024 Big Data Bowl | Data: 2022 Weeks 1-9 | All Plays") %>% 
  gt_nfl_wordmarks(column ="Team", height = 30, locations = NULL)%>% 
gt_nfl_logos(column ="Team_Logo", height = 30, locations = NULL)






gtsave(table, "Defensive Team Total PURSUIT.png")



# Offense and Defensive Players -------------------------------------------
# 
# Full_Offense <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/Full_Offense.csv")
# 
# OffTable <- Full_Offense %>% 
#   group_by(nflId) %>% 
#   slice_tail(n = 1) %>% 
#   ungroup() %>% 
#   filter(position == "RB") %>% 
#   mutate(
#   Voronoi_Sum = MeanPlay,
#   BreakThrough = UnCoveredPerc * 100,
#   KeepAway = KEEPAWAY,
#   TopBottom = if_else(KeepAway >= 0.72, "Top 10", "Bottom 10"),
#   ) %>% 
#   group_by(TopBottom) %>% 
#   select(displayName, club, Voronoi_Sum, BreakThrough, KeepAway) %>% 
#   arrange(desc(KeepAway)) %>% 
#   distinct()
# 
# top_and_bottom_10 <- rbind(head(OffTable, 10), tail(OffTable, 10))
# 
# table = gt(top_and_bottom_10) %>% 
#   data_color(
#     columns = c("Voronoi_Sum", "BreakThrough"),
#     colors = scales::col_numeric(
#       palette = color_palette,
#       domain = NULL,
#       reverse = F  # Set this to TRUE for color reversal
#     )) %>% 
#   data_color(
#     columns = c("KeepAway"),
#     colors = scales::col_numeric(
#       palette = color_palette,
#       domain = NULL,
#       reverse = F  # Set this to TRUE for color reversal
#     )) %>% 
#   fmt_number(columns = c("Voronoi_Sum", "BreakThrough","KeepAway",
#   ), decimals = 2)%>% 
#   opt_align_table_header(align = "center") %>% 
#   cols_align("center") %>% 
#   opt_row_striping() %>% 
#   gt_theme_espn() %>% 
#   tab_header(
#     title = md("Running Back Total Values"),
#     subtitle = "Voronoi Weighted Sum of Area, Breakthrough Percentage, and KeepAway (Weighted Percentile of Voronoi and Breakthrough)") %>% 
#   tab_source_note("Table: @SethDataScience | 2024 Big Data Bowl | Data: 2022 Weeks 1-9") %>% 
#   gt_nfl_wordmarks(column ="club", height = 30, locations = NULL)
# 
# 
# 
# gtsave(table, "RB Keepaway.png")
# 
# 


Full_Defense <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/Full_Defense.csv")


DefTable <- Full_Defense %>%
  group_by(Alt_Position) %>% 
  mutate(PassPercentRankPUR = 1 - percent_rank(PassVoronoi),
         PassPercentRankSU = percent_rank(PassShielding),
         PassPercentRankIT =  1-percent_rank(PassRootMeanSquaredDeviation),
         PassPURSUIT = (PassPercentRankPUR +
                      PassPercentRankSU + 
                      PassPercentRankIT) / 3,
         RushPercentRankPUR = 1 - percent_rank(RushVoronoi),
         RushPercentRankSU = percent_rank(RushShielding),
         RushPercentRankIT =  1-percent_rank(RushRootMeanSquaredDeviation),
         RushPURSUIT = (RushPercentRankPUR +
                          RushPercentRankSU + 
                          RushPercentRankIT) / 3,
         TotalPercentRankPUR = 1 - percent_rank(TotalVoronoi),
         TotalPercentRankSU = percent_rank(TotalShielding),
         TotalPercentRankIT =  percent_rank(TotalRootMeanSquaredDeviation),
         TotalPURSUIT = (TotalPercentRankPUR +
                          TotalPercentRankSU + 
                          TotalPercentRankIT) / 3,
         Team = club_x
  ) %>% 
  ungroup() %>% 
  filter(Alt_Position == "DB") %>% 
  mutate(
    Player = displayName,
    Voronoi_Reduction = RushVoronoi,
    Shielding_Upfield = RushShielding * 100,
    Ideal_Angle = RushRootMeanSquaredDeviation,
    PURSUIT = RushPURSUIT,
    TopBottom = if_else(PURSUIT >= 0.5, "Top 10", "Bottom 10"),
  ) %>% 
  group_by(TopBottom) %>% 
  select(Player, Team, Voronoi_Reduction, Shielding_Upfield, Ideal_Angle, PURSUIT) %>% 
  arrange(desc(PURSUIT)) %>% 
  distinct()

top_and_bottom_10 <- rbind(head(DefTable, 10), tail(DefTable, 10))


table = gt(top_and_bottom_10) %>% 
  data_color(
    columns = c("Voronoi_Reduction", "Ideal_Angle"),
    colors = scales::col_numeric(
      palette = color_palette,
      domain = NULL,
      reverse = T  # Set this to TRUE for color reversal
    )) %>% 
  data_color(
    columns = c("Shielding_Upfield", "PURSUIT"),
    colors = scales::col_numeric(
      palette = color_palette,
      domain = NULL,
      reverse = F  # Set this to TRUE for color reversal
    )) %>% 
  fmt_number(columns = c("Voronoi_Reduction", "Shielding_Upfield","Ideal_Angle", "PURSUIT"
  ), decimals = 2)%>% 
  opt_align_table_header(align = "center") %>% 
  cols_align("center") %>% 
  opt_row_striping() %>% 
  gt_theme_espn() %>% 
  tab_header(
    title = md("Defensive Back Rush Values"),
    subtitle = "Voronoi Area Sum Allowed, Shielding  Percentage, and Deviation from Ideal Angle (Weighted Percentile of Voronoi, Shielding, and Ideal Angle)") %>% 
  tab_source_note("Table: @SethDataScience | 2024 Big Data Bowl | Data: 2022 Weeks 1-9 | Min. 25 Total Snaps") %>% 
  gt_nfl_wordmarks(column ="Team", height = 30, locations = NULL)



gtsave(table, "DB Rush PURSUIT.png")



# Gravity Plot ------------------------------------------------------------
library(deldir)
library(magick)
library(ggforce)
library(gganimate)
library(transformr)

AllGamesYAC <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/AllGamesYAC.csv")
select_plays <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/viz_play.csv")

select_plays_values <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/viz_play_values.csv")


# Line Plot ---------------------------------------------------------------

ggplot(select_plays_values, aes(x = Time, y = area))+
  geom_line(aes(color = primary))+
  geom_line(aes(y = a, color = "Acceleration (Divided by 10)")) +
  geom_line(aes(y = area, color = "Voronoi Area")) +
  geom_line(aes(y = adjusted_change, color = "Adjusted Voronoi Area")) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_manual(values = c("Acceleration (Divided by 10)" = "blue", "Voronoi Area" = "darkgreen", "Adjusted Voronoi Area" = "darkred"))+
  scale_x_continuous(breaks = seq(1,300, 1))+
  labs(
    title = "Regular and Adjusted Voronoi Area During Play",
    y = "Voronoi Area"
  )+
  theme_reach()


animation = ggplot(select_plays_values, aes(x = Time)) +
  #geom_line(aes(y = a, color = "Acceleration")) +
  #geom_line(aes(y = area, color = "Voronoi Area")) +
  #geom_line(aes(y = adjusted_change, color = "Adjusted Voronoi Area")) +
  geom_line(aes(y = ScalingFactor, color = "Scaling Factor (Defenders Nearby)")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(
    "Acceleration" = "blue",
    "Voronoi Area" = "darkgreen",
    "Adjusted Voronoi Area" = "darkred",
    "Scaling Factor (Defenders Nearby)" = "purple")) +
  scale_x_continuous(breaks = seq(1, 300, 1))+
  labs(
    #title = "Components of Voronoi Area During Play",
    y = "Scaling Factor (Defenders Nearby)",
    caption = "@SethDataScience | Data: GameId = 2022110604, PlayId = 221",
    color = "LegendTitle"
  ) +
  theme_reach() + 
  theme(legend.position = "top")+
  transition_reveal(select_plays_values$Time) +
  ease_aes('linear') + NULL


lengthVIZ <- length(unique(select_plays_values$Time))


animate(animation, fps = 25, nframe = lengthVIZ,
        height = 400, width = 700)



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
         ballCarrier = if_else(nflId == 46116,
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
  labs(caption = "@SethDataScience | Data: GameId = 2022110604, PlayId = 221")+
  transition_time(SelectPlay$frameId) +
  ease_aes('linear') + NULL


ex_play_lengthVIZ <- length(unique(SelectPlay$frameId))


animate(play_animationVIZ, fps = 25, nframe = ex_play_lengthVIZ, height = 1200, width = 1000)



# Validation Random Plots -------------------------------------------------

AllGamesBreakthrough <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/AllGamesBreakthrough.csv")

AllGamesYAC <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/AllGamesYAC.csv")

AllGamesDeviation <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/AllGamesDeviation.csv")

Voronoi_plot_data = AllGamesDeviation %>% 
  group_by(gameId, playId) %>% 
  summarise(HomeWP = head(preSnapHomeTeamWinProbability,1),
            Down = head(down, 1),
            Yardline = head(absoluteYardlineNumber, 1),
            TimeRemaining = head(gameClock, 1),
            YardsRemaining = head(yardsToGo, 1),
            Deviation = mean(Deviation)
            #CoveringPerc = mean(Covering)
            #adjusted_change = mean(adjusted_change)
            )

ggplot(Voronoi_plot_data,
       aes(x = HomeWP, y = Deviation))+
  geom_point()+
  theme_reach()+
  labs(
    title = "Ideal Angle Deviation by Home Win Probability",
    subtitle = "Taking the average of the Ideal Angle Deviation by Home Win Probability",
    y = "Ideal Angle Deviation",
    x = "Home Win Probability",
    caption = "@SethDataScience"
  )

# Aggression vs Tackles ---------------------------------------------------
tackles <- read_csv("DataBowl/2024-Big-Data-Bowl/Non_Games_Data/tackles.csv")

Full_Defense <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/Full_Defense.csv")

tackle_player = tackles %>% 
  mutate(TackleAssist = tackle + assist) %>% 
  group_by(nflId) %>% 
  summarise(TacklePerc = mean(tackle),
            TackleAssistPerc = mean(TackleAssist),
            ForcedFumblePerc = mean(forcedFumble),
            MissedTacklePerc = mean(pff_missedTackle)
            )

DefTable <- Full_Defense %>%
  group_by(Alt_Position) %>% 
  mutate(PassPercentRankPUR = 1 - percent_rank(PassVoronoi),
         PassPercentRankSU = percent_rank(PassShielding),
         PassPercentRankIT =  1-percent_rank(PassRootMeanSquaredDeviation),
         PassPURSUIT = (PassPercentRankPUR +
                          PassPercentRankSU + 
                          PassPercentRankIT) / 3,
         RushPercentRankPUR = 1 - percent_rank(RushVoronoi),
         RushPercentRankSU = percent_rank(RushShielding),
         RushPercentRankIT =  1-percent_rank(RushRootMeanSquaredDeviation),
         RushPURSUIT = (RushPercentRankPUR +
                          RushPercentRankSU + 
                          RushPercentRankIT) / 3,
         TotalPercentRankPUR = 1 - percent_rank(TotalVoronoi),
         TotalPercentRankSU = percent_rank(TotalShielding),
         TotalPercentRankIT =  percent_rank(TotalRootMeanSquaredDeviation),
         TotalPURSUIT = (TotalPercentRankPUR +
                           TotalPercentRankSU + 
                           TotalPercentRankIT) / 3,
         Team = club_x
  ) %>% 
  left_join(tackle_player, by = "nflId")

ggplot(DefTable,
       aes(x = TotalPercentRankPUR, y = MissedTacklePerc))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Alt_Position)+
  theme_reach()+
  labs(
    title = "Voronoi Area Reduction vs Missed Tackle Percentage By Position Group",
    subtitle = "Missed Tackle Percentage is the percent of plays in the dataset that were recorded as a Missed Tackle for that player",
    y = "Missed Tackle Percentage",
    x = "Voronoi Area Reduction",
    caption = "@SethDataScience"
  )
