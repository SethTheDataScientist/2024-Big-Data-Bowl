
# Team Rankings -----------------------------------------------------------


FullTeamTable <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/Full_TeamTable.csv")

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
         TotalPercentRankIT = 1- percent_rank(TotalRootMeanSquaredDeviation),
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
    TopBottom = if_else(PURSUIT > 0.59, "Top 10", "Bottom 10")
  ) %>% 
  select(Team, Team_Logo, Voronoi_Reduction, Shielding_Upfield, Ideal_Angle, PURSUIT, TopBottom) %>% 
  arrange(desc(PURSUIT)) %>% 
  distinct() %>%
  group_by(TopBottom) %>% 
  slice_tail(n = 10)%>%
arrange(desc(PURSUIT))  



# Define the color palette
color_palette <- c("#e15759", "#edc948", "#59a14f")

gt(DefTeamTable) %>% 
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
    subtitle = "Voronoi Area Sum Allowed, Shielding  Percentage, Deviation from Ideal Angle, and PURSUIT") %>% 
  tab_source_note("Table: @SethDataScience | 2024 Big Data Bowl | Data: 2022 Weeks 1-9 | All Plays") %>% 
  gt_nfl_wordmarks(column ="Team", height = 30, locations = NULL)%>% 
gt_nfl_logos(column ="Team_Logo", height = 30, locations = NULL)






gtsave(table, "Defensive Team Total PURSUIT.png", expand = 25)



# Defensive Players -------------------------------------------


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
         TotalPercentRankIT =  1- percent_rank(TotalRootMeanSquaredDeviation),
         TotalPURSUIT = (TotalPercentRankPUR +
                          TotalPercentRankSU + 
                          TotalPercentRankIT) / 3,
         Team = club_x
  ) %>% 
  ungroup() %>% 
  filter(Alt_Position == "DB") %>% 
  mutate(
    Player = displayName,
    Voronoi_Reduction = TotalVoronoi,
    Shielding_Upfield = TotalShielding * 100,
    Ideal_Angle = TotalRootMeanSquaredDeviation,
    PURSUIT = TotalPURSUIT,
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
    title = md("Defensive Back Total Values"),
    subtitle = "Voronoi Area Sum Allowed, Shielding  Percentage, and Deviation from Ideal Angle (Weighted Percentile of Voronoi, Shielding, and Ideal Angle)") %>% 
  tab_source_note("Table: @SethDataScience | 2024 Big Data Bowl | Data: 2022 Weeks 1-9 | Min. 25 Total Snaps") %>% 
  gt_nfl_wordmarks(column ="Team", height = 30, locations = NULL)



gtsave(table, "DB Total PURSUIT.png", expand = 25)



# Field Plot Inputs------------------------------------------------------------
library(deldir)
library(magick)
library(ggforce)
library(gganimate)
library(transformr)
library(magick)
library(av)
library(ggrounded)
library(ggchicklet)

AllGamesYAC <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/AllGamesYAC.csv")

AllGamesBreakThrough <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/AllGamesBreakthrough.csv")

select_plays <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/viz_play.csv") %>% 
  mutate(outline = if_else(
    nflId == 46173 | nflId == 52546,
    1, -1
  ))

select_plays_values <- read_csv("DataBowl/2024-Big-Data-Bowl/Created_DF/viz_play_values.csv")  %>%   
  mutate(PR = -1*(PR),
         )   
  group_by(nflId) %>% 
  mutate(Time = seq(1, n()),
         Shielding = cummean(1-Covering),
         Line1 = if_else(Time >= 10, 1, 0),
         Line2 = if_else(Time >= 20, 1, 0)) %>% 
  filter(nflId == 52546)

select_plays_values_frame <- select_plays_values %>% 
  group_by(frameId) %>%
  summarise(FrameVor = head(PR, 1),
            FrameShielding = mean(Covering, na.rm = T),
         FrameDeviation = mean(180-Deviation, na.rm = T)) %>% 
  group_by() %>% 
  mutate(Time = seq(1, n()),
         Line1 = if_else(Time >= 10, 1, 0),
         Line2 = if_else(Time >= 20, 1, 0))



# Line Plot ---------------------------------------------------------------

ggplot(select_plays_values, aes(x = Time)) +
  geom_line(aes(y = FrameVor, color = "Percentile Voronoi Area")) +
  geom_line(aes(y = FrameDeviation, color = "Mean Angle Deviation")) +
  geom_line(aes(y = FrameShielding*100, color = "Total Shielding Percent")) +
  geom_vline(aes(xintercept = 10, alpha = Line1))+
  geom_vline(aes(xintercept = 20, alpha = Line2))+
  scale_color_manual(values = c(
    "Percentile Voronoi Area" = "blue",
    "Mean Angle Deviation" = "darkgreen",
    "Total Shielding Percent" = "darkred")) +
  scale_x_continuous(breaks = seq(1, 300, 1))+
  scale_alpha(guide = "none")+
  ylim(0,120)+
  labs(
    title = "Components of PURSUIT During Play",
    y = "Percentile / Percent / Angle Deviation",
    # caption = "@SethDataScience | Data: GameId = 2022092502, PlayId = 664",
    color = "LegendTitle"
  ) +
  theme_reach() + 
  theme(legend.position = "top")


animation = ggplot(select_plays_values_frame, aes(x = Time)) +
  geom_line(aes(y = FrameVor, color = "Percentile Voronoi Area")) +
  geom_line(aes(y = FrameDeviation, color = "Mean Angle Deviation")) +
  geom_line(aes(y = FrameShielding*100, color = "Total Shielding Percent")) +
  geom_vline(aes(xintercept = 10, alpha = Line1))+
  geom_vline(aes(xintercept = 20, alpha = Line2))+
  scale_color_manual(values = c(
    "Percentile Voronoi Area" = "blue",
    "Mean Angle Deviation" = "darkgreen",
    "Total Shielding Percent" = "darkred")) +
  scale_x_continuous(breaks = seq(1, 300, 1))+
  scale_alpha(guide = "none")+
  ylim(0,120)+
  labs(
    title = "Components of PURSUIT During Play",
    y = "Voronoi Area Percentile / Shielding Percent / Angle Deviation",
    # caption = "@SethDataScience | Data: GameId = 2022092502, PlayId = 664",
    color = "LegendTitle"
  ) +
  theme_reach() + 
  theme(legend.position = "top")+
  transition_reveal(select_plays_values_frame$Time) +
  ease_aes('linear') + NULL


lengthVIZ <- length(unique(select_plays_values_frame$Time))


animate(animation, fps = 25, nframe = lengthVIZ,
        height = 600, width = 700)


#lJarius Sneed

ggplot(select_plays_values, aes(x = Time)) +
  geom_line(aes(y = PR, color = "Percentile Voronoi Area")) +
  geom_line(aes(y = Deviation, color = "Mean Angle Deviation")) +
  geom_line(aes(y = Shielding*100, color = "Total Shielding Percent")) +
  geom_vline(aes(xintercept = 10, alpha = Line1))+
  geom_vline(aes(xintercept = 20, alpha = Line2))+
  scale_color_manual(values = c(
    "Percentile Voronoi Area" = "blue",
    "Mean Angle Deviation" = "darkgreen",
    "Total Shielding Percent" = "darkred")) +
  scale_x_continuous(breaks = seq(1, 300, 1))+
  ylim(0,120)+
  labs(
    title = "L'Jarius Sneed PURSUIT",
    y = "Percentile / Percent / Angle Deviation",
    # caption = "@SethDataScience | Data: GameId = 2022092502, PlayId = 664"
  ) +
  theme_reach()


animation = ggplot(select_plays_values, aes(x = Time)) +
  geom_line(aes(y = PR, color = "Percentile Voronoi Area")) +
  geom_line(aes(y = Deviation, color = "Mean Angle Deviation")) +
  geom_line(aes(y = Shielding*100, color = "Total Shielding Percent")) +
  geom_vline(aes(xintercept = 10, alpha = Line1))+
  geom_vline(aes(xintercept = 20, alpha = Line2))+
  scale_color_manual(values = c(
    "Percentile Voronoi Area" = "blue",
    "Mean Angle Deviation" = "darkgreen",
    "Total Shielding Percent" = "darkred")) +
  scale_x_continuous(breaks = seq(1, 300, 1))+
  ylim(0,120)+
  labs(
    title = "L'Jarius Sneed PURSUIT",
    y = "Voronoi Area Percentile / Shielding Percent / Angle Deviation",    caption = "@SethDataScience | Data: GameId = 2022092502, PlayId = 664"
  ) +
  theme_reach() + 
  transition_reveal(select_plays_values$Time) +
  ease_aes('linear') + NULL


lengthVIZ <- length(unique(select_plays_values$Time))


animate(animation, fps = 25, nframe = lengthVIZ,
        height = 600, width = 700)
#


# Field stuff -------------------------------------------------------------


SelectPlay <- select_plays %>%
  filter(frameId == 5, club != "football") %>% 
  mutate(x = if_else(is.na(x) == 1, 0, x ),
         y = if_else(is.na(y) == 1, 0, y ),
         ) %>% 
  group_by(nflId) %>% 
  mutate(
    Time = seq(1, n()))


hash_right <- 38.35 
hash_left <- 12 
hash_width <- 3.3 
# Define the range of coordinates
xmin <- 0 
xmax <- 53.3 
ymin <- max(round(min(SelectPlay$x, 
                      na.rm = TRUE) - (5 ), -1), 0 )
ymax <- min(round(max(SelectPlay$x, 
                      na.rm = TRUE) + (5 ), -1), 120 )
rect_height = 5

df_hash <- 
  expand.grid(x = c(0,
                    23.36667 ,
                    29.96667 ,
                    xmax -  1 ), 
              y = (seq((10 ), (110 ),
                       ))) %>% 
  filter(!(floor(y %% (5 )) == 0),
         y < ymax, y > ymin)

field_base <- ggplot() +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = seq(ymin, ymax - rect_height, by = rect_height), ymax = seq(ymin + rect_height, ymax, by = rect_height)),
    fill = rep(c("#7CA590", "#5A826D"), length.out = length(seq(ymin, ymax - rect_height, by = rect_height)))
    )+
  annotate("segment", x = df_hash$x[df_hash$x == 0 ]+1,
           y = df_hash$y[df_hash$x == 0 ],
           xend = df_hash$x[df_hash$x == 0 ] + 3,
           yend = df_hash$y[df_hash$x == 0 ], color = "#F3F3F4", size = 1.25) +
  
  annotate("segment", x = df_hash$x[df_hash$x == 23.36667] - 1,
           y = df_hash$y[df_hash$x == 23.36667],
           xend = df_hash$x[df_hash$x == 23.36667] + 1,
           yend = df_hash$y[df_hash$x == 23.36667], color = "#F3F3F4", size = 1.25) +
  
  
  annotate("segment", x = df_hash$x[df_hash$x == 29.96667]-1,
           y = df_hash$y[df_hash$x == 29.96667],
           xend = df_hash$x[df_hash$x == 29.96667] + 1,
           yend = df_hash$y[df_hash$x == 29.96667], color = "#F3F3F4", size = 1.25) +
  
  annotate("segment", x = df_hash$x[df_hash$x == 52.30000]-2,
           y = df_hash$y[df_hash$x == 52.30000],
           xend = df_hash$x[df_hash$x == 52.30000],
           yend = df_hash$y[df_hash$x == 52.30000], color = "#F3F3F4", size = 1.25) +
  
  
  annotate("segment", x = xmin, y = seq(max(10 , ymin), min(ymax, 110 ), by = 5 ),
           xend =  xmax, yend = seq(max(10 , ymin), min(ymax, 110 ), by = 5 ), color = "#F3F3F4", size = 1.25) +
  
  annotate("text", x = rep(hash_left, 11), y = seq(10 , 110 , by = 10 ),
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 5 * 2.5, color = "#F3F3F4") +
  
  annotate("text", x = rep((xmax - hash_left), 11), y = seq(10 , 110 , by = 10 ),
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),
           angle = 90, size = 5 * 2.5, color = "#F3F3F4") +
  
  annotate("segment", x = c(xmin, xmin, xmax, xmax), y = c(ymin, ymax, ymax, ymin),
           xend = c(xmin, xmax, xmax, xmin), yend = c(ymax, ymax, ymin, ymin), color = "#F3F3F4", size = 1.25)

field_base

# KPI full play -----------------------------------------------------------

play_animationVIZ = field_base+
  geom_point(data = select_plays,
             aes(x = (xmax - y), y = x,
                 fill = "gold",
                 color = "gold"
             ),
                 alpha = select_plays$outline,
             size = 9 * 2.5) +
  geom_point(data = select_plays, 
             aes(x = (xmax - y), y = x,
                 group = nflId, 
                 fill = primary,
                 color = primary,
             ),
             size = 7 * 2.5) +
  geom_text(data = select_plays,
            aes(x = (xmax-y), y = x, label = jerseyNumber),
            color = "white",
            size = 3.5 * 2.5,
            vjust = 0.36) +
  scale_shape_manual(values = c(21, 16, 21), guide = "none") +
  scale_fill_identity(aesthetics = c("fill", "color")) +
  scale_alpha_identity()+
  ylim(ymin+5, ymax-5)+
  theme_void()+
  guides(alpha = "none")+
  theme(
        plot.title = element_text(),
        plot.subtitle = element_text(),
        plot.caption = element_text(),
        plot.margin = margin(0,0,0,0, "pt"),
        panel.background = element_rect(
          fill = "#F0F0F0")) +
  transition_time(select_plays$frameId) +
  ease_aes('linear') + NULL


ex_play_lengthVIZ <- length(unique(select_plays$frameId))

animate(play_animationVIZ, fps = 10, nframe = ex_play_lengthVIZ, height = 1200, width = 1000)



# KPI Box -----------------------------------------------------------------


# Create animated KPI box
animated_kp_box = ggplot(select_plays_values_frame, aes(x = 1, y = 1.15, )) +
  
  # ggchicklet:::geom_rrect(
  #   aes(xmin = 0.6, xmax = 1.5, ymin = -0.05, ymax = 0.95), alpha = 0.4,
  #   fill = "black", r = grid::unit(10, "pt")
  # )+
  # ggchicklet:::geom_rrect(
  #   aes(xmin = 1.7, xmax = 2.325, ymin = 1.05, ymax = 1.475), alpha = 0.4,
  #   fill = "black", r = unit(0.125, "npc")
  # )+
  # ggchicklet:::geom_rrect(
  #   aes(xmin = 2.7, xmax = 3.325, ymin = 1.05, ymax = 1.475), alpha = 0.4,
  #   fill = "black", r = unit(0.125, "npc")
  # )+

geom_rect(xmin = -1, xmax = 4, ymin = -1, ymax = 4, fill = "#F0F0F0")+
  
  ggchicklet:::geom_rrect(
    aes(xmin = 0.7, xmax = 1.325, ymin = 1.05, ymax = 1.475), alpha = 0.4,
        fill = "black", r = unit(0.125, "npc")
  )+
  ggchicklet:::geom_rrect(
    aes(xmin = 1.7, xmax = 2.325, ymin = 1.05, ymax = 1.475), alpha = 0.4,
        fill = "black", r = unit(0.125, "npc")
  )+
  ggchicklet:::geom_rrect(
    aes(xmin = 2.7, xmax = 3.325, ymin = 1.05, ymax = 1.475), alpha = 0.4,
        fill = "black", r = unit(0.125, "npc")
  )+
  
  
  ggchicklet:::geom_rrect(
    aes(xmin = 0.675, xmax = 1.3, ymin = 1.075, ymax = 1.525),
        fill = "gray", r = unit(0.125, "npc")
  )+
  ggchicklet:::geom_rrect(
    aes(xmin = 1.675, xmax = 2.3, ymin = 1.075, ymax = 1.525),
        fill = "gray", r = unit(0.125, "npc")
  )+
  ggchicklet:::geom_rrect(
    aes(xmin = 2.675, xmax = 3.3, ymin = 1.075, ymax = 1.525),
        fill = "gray", r = unit(0.125, "npc")
  )+
  
  geom_text(aes(x = 1, y = 1.175, color = "black", label = sprintf("%.2f",round(FrameVor, 2))), size = 10)+
  geom_text(aes(x = 1, y = 1.375, 
                      label = "Voronoi Area \nPercentile", color = "black"), size = 10)+
  geom_col_rounded(aes(y = 1, x = 1), radius =  grid::unit(10, "pt"))+
  geom_col(aes(y = (FrameVor)/100, x = 1, fill = (FrameVor)/100))+
  
  
  geom_text(aes(x = 2, y = 1.175, color = "black", label = paste0(" ",sprintf("%.2f",round(FrameShielding*100, 2)),"%")), size = 10)+
  geom_text(aes(x = 2, y = 1.375, 
                      label = "Shielding \nUpfield", color = "black"), size = 10)+
  geom_col_rounded(aes(y = 1, x = 2), radius =  grid::unit(10, "pt"))+
  geom_col(aes(y = (FrameShielding*100)/100, x = 2, fill = (FrameShielding*100)/100))+
  
  
  geom_text(aes(x = 3, y = 1.175, color = "black", label = sprintf("%.2f",round(FrameDeviation/180*100, 2))), size = 10)+
  geom_text(aes(x = 3, y = 1.375, 
                      label = "Mean Angle \nDeviation", color = "black"), size = 10)+
  geom_col_rounded(aes(y = 1, x = 3), radius =  grid::unit(10, "pt"))+
  geom_col(aes(y = (FrameDeviation/180), x = 3, fill = (FrameDeviation/180)))+
  
  # 
  # annotate("segment", x = 0.55,
  #          xend = 1.45,
  #          y = 0.25, yend = 0.25,
  #          color = "#F0F0F0", size = 0.75,
  #          linetype = "dashed")+
  # 
  # annotate("segment", x = 0.55,
  #          xend = 1.45,
  #          y = 0.5, yend = 0.5,
  #          color = "#F0F0F0", size = 0.75,
  #          linetype = "dashed")+
  # 
  # annotate("segment", x = 0.55,
  #          xend = 1.45,
  #          y = 0.75, yend = 0.75,
  #          color = "#F0F0F0", size = 0.75,
  #          linetype = "dashed")+
  # 
  # annotate("segment", x = 1.55,
  #          xend = 2.45,
  #          y = 0.25, yend = 0.25,
  #          color = "#F0F0F0", size = 0.75,
  #          linetype = "dashed")+
  # 
  # annotate("segment", x = 1.55,
  #          xend = 2.45,
  #          y = 0.5, yend = 0.5,
  #          color = "#F0F0F0", size = 0.75,
  #          linetype = "dashed")+
  # 
  # annotate("segment", x = 1.55,
  #          xend = 2.45,
  #          y = 0.75, yend = 0.75,
  #          color = "#F0F0F0", size = 0.75,
  #          linetype = "dashed")+
  
  # 
  # annotate("segment", x = 2.55,
  #          xend = 3.45,
  #          y = 0.25, yend = 0.25,
  #          color = "#F0F0F0", size = 0.75,
  #          linetype = "dashed")+
  # 
  # annotate("segment", x = 2.55,
  #          xend = 3.45,
  #          y = 0.5, yend = 0.5,
  #          color = "#F0F0F0", size = 0.75,
  #          linetype = "dashed")+
  # 
  # annotate("segment", x = 2.55,
  #          xend = 3.45,
  #          y = 0.75, yend = 0.75,
  #          color = "#F0F0F0", size = 0.75,
  #          linetype = "dashed")+
  
  scale_color_identity(aesthetics = c("color"))+
  scale_fill_gradient(low = "#e15759", high = "#59a14f") +
  transition_states(Time, transition_length = 0, state_length = 0.1) +
  # ylim(0,1.55)+
  ease_aes('linear') +
  theme_void() +
  # coord_fixed() +
  theme(legend.position="none",
        panel.background = element_rect(fill = "#F0F0F0"),
        plot.background = element_rect(
          color = "#F0F0F0"),
        plot.margin = margin(0,0,0,0,"pt")
        )

lengthVIZ <- length(unique(select_plays_values_frame$Time))


animate(animated_kp_box, fps = 10, nframe = lengthVIZ,
        height = 600, width = 1000)


# Gif Stitcher ------------------------------------------------------------
  
  # List of paths to your GIFs
  gif_paths <- image_read("DataBowl/2024-Big-Data-Bowl/Plots/Sub Plots/KPI.gif")
  
  play <- image_read("DataBowl/2024-Big-Data-Bowl/Plots/Sub Plots/Full Play Real.gif")
  
  
  play <- image_scale(play, '1000x600!')



new_gif <- image_append(c(gif_paths[1], play[1]), stack = T)
for(i in 2:32){
  combined <- image_append(c(gif_paths[i], play[i]), stack = T)
  new_gif <- c(new_gif, combined)
}
new_gif


gif_2 <- image_read("DataBowl/2024-Big-Data-Bowl/Plots/Sub Plots/Full Play Dots cropped.gif")

gif_2 <- image_scale(gif_2, '1000x1200!')


gif_app <- image_read("DataBowl/2024-Big-Data-Bowl/Plots/Sub Plots/Play Stacked.gif")

gif_app <- image_scale(gif_app, '1000x1200!')


  new_gif <- image_append(c(gif_2[1], gif_app[1]), stack = F)
  for(i in 2:32){
    combined <- image_append(c(gif_2[i], gif_app[i]), stack = F)
    new_gif <- c(new_gif, combined)
  }
  new_gif
  
#
# Stability ---------------------------------------------------------------

FirstHalf <- SeasonNFL %>% 
  group_by() %>% 
  arrange(desc(week)) %>% 
  mutate(index = seq(1, n())) %>% 
  filter(index <= 2518)%>% 
  group_by(player_id) %>% 
  summarise(Voronoi_Area = mean(Voronoi_Area),
            Shielding = mean(Shielding),
            Deviation = mean(Deviation),
            Pursuit = mean(Pursuit)
            )

SecondHalf <- SeasonNFL %>%
  group_by() %>% 
  arrange(desc(week)) %>% 
  mutate(index = seq(1, n())) %>% 
  filter(index > 2518) %>% 
  group_by(player_id) %>% 
  summarise(Voronoi_Area = mean(Voronoi_Area),
            Shielding = mean(Shielding),
            Deviation = mean(Deviation),
            Pursuit = mean(Pursuit)
            )

Stability <- FirstHalf %>% 
  inner_join(SecondHalf, by = c("player_id")) %>% 
  summarise(Voronoi_Area_Cor = cor(Voronoi_Area.x, Voronoi_Area.y),
            Voronoi_Area_p_value = t.test(Voronoi_Area.x, Voronoi_Area.y)$p.value,
            Shielding_Cor = cor(Shielding.x, Shielding.y),
            Shielding_p_value = t.test(Shielding.x, Shielding.y)$p.value,
            Deviation_Cor = cor(Deviation.x, Deviation.y),
            Deviation_p_value = t.test(Deviation.x, Deviation.y)$p.value,
            Pursuit_Cor = cor(Pursuit.x, Pursuit.y),
            Pursuit_p_value = t.test(Pursuit.x, Pursuit.y)$p.value
            )

StabilityMetrics <- read_excel("DataBowl/2024-Big-Data-Bowl/Created_DF/StabilityMetrics.xlsx") %>% 
  arrange(desc(Cor))


# Define the color palette
color_palette <- c("#e15759", "#edc948", "#59a14f")

table = gt(StabilityMetrics) %>%
  fmt_number(columns = c("Cor", "P_Value"), decimals = 2)%>% 
  opt_align_table_header(align = "center") %>% 
  cols_align("center") %>% 
  opt_row_striping() %>%
  gt_theme_espn()  %>%
  tab_options(
    
    column_labels.background.color = "white",
    heading.background.color = "white",
    table.background.color = "#7CA590",
    row.striping.background_color = "#5A826D",
    source_notes.background.color = "white",
  ) %>% 
  tab_header(
    title = md("PURSUIT Component Stability"),
    subtitle = "First to Second half Correlation and T-Test P_value") %>% 
  tab_source_note("Table: @SethDataScience | 2024 Big Data Bowl")






gtsave(table, "Stability Metrics_updated.png")



# Feature Importance ------------------------------------------------------

Feature_Imp <- read_excel("DataBowl/2024-Big-Data-Bowl/Created_DF/Feature Importance.xlsx")

metrics = c("Deviation", "Shielding", "Voronoi")

ggplot(Feature_Imp, aes(x = `F-Score`,
            y = reorder(Feature, `F-Score`)))+
  geom_col(aes(
    # fill = if_else(Feature %in% metrics, "#59a14f", "#e15759")
    fill = if_else(`F-Score` > 15, "#59a14f", "#e15759")
  ))+
  scale_color_identity(aesthetics = c("color", "fill"))+
  labs(
    title = "Feature Importance",
    subtitle = "These are the features that were the most important to an XGBoost model trained to predict Yards After Catch",
    y = "Feature",
    caption = "@SethDataScience"
  )+
  theme_reach()

