AllGamesRoutesVoronoi <- read_csv("C:/Users/sethl/OneDrive/Important Stuff/R/R files/NFL/DataBowl/2024-Big-Data-Bowl/Created_DF/AllGamesRoutesVoronoiManZone.csv")

ggplot(AllGamesRoutesVoronoi, aes(x = Play_Gravity))+
  geom_density()+
  xlim((-0.1272 - 1.5 * 0.30507),(0.1777 + 1.5 * 0.30507))+
  facet_wrap(~pff_passCoverageType_Man)


