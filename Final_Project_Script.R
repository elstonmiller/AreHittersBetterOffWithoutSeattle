# Setwd
setwd("C:/Users/eemil/OneDrive - University of South Florida/Fall 2025/LIS 4273 - Adv Stat & R/Final Project")

# 1. Data Preperation:
## Import datasets
mariners <- read.csv("2010_2025_o100PA_Mariners.csv", header = TRUE) # >= 100 PA in a season, 2010-2025, regular season 
league <- read.csv("2010_2025_o100PA_League.csv", header = TRUE) # >= 100 PA in a season, 2010-2025, regular season 


## Prepare datasets
mariners_names <- mariners$Player.additional #Collect all mariners players names 
league_ms <- league[league$Player.additional %in% mariners_names,] # Filter df so it contains individual season of any player who has played on the Mariners since 2010
ms_on_other_team <- league_ms[!grepl("SEA", league_ms$Team), ] # Filter out the seasons spent with the Mariners  

### Create helper function to calculate K% and BB%
per_ab <- function(stat, PA){
  stat/PA
}

## Create K% ("KP") stat in both tables
ms_on_other_team$KP <- per_ab(ms_on_other_team$SO,ms_on_other_team$PA)
mariners$KP <- per_ab(mariners$SO,mariners$PA)

## Create BB% ("BBP") stat in both tables
ms_on_other_team$BBP <- per_ab(ms_on_other_team$BB,ms_on_other_team$PA)
mariners$BBP <- per_ab(mariners$BB,mariners$PA)

## Create df containing only the necessary statistics 
other_clean <- data.frame(OPS_Plus=ms_on_other_team$OPS., 
                          OBP=ms_on_other_team$OBP, 
                          SLG=ms_on_other_team$SLG, 
                          KP=ms_on_other_team$KP, 
                          BBP=ms_on_other_team$BBP)
mariners_clean <- data.frame(OPS_Plus=mariners$OPS., 
                             OBP=mariners$OBP, 
                             SLG=mariners$SLG, 
                             KP=mariners$KP, 
                             BBP=mariners$BBP)

# 2. Perform Analysis:
## Perform Welch t-tests (Variances are not identical) 
t.test(mariners_clean$OPS_Plus, other_clean$OPS_Plus) #P-value 0.9709 - not significant, mean is slightly better on mariners
t.test(mariners_clean$OBP, other_clean$OBP) #P-value = 1.111e-06 - very significant
t.test(mariners_clean$SLG, other_clean$SLG) #P-value = 0.08867 - close, but not significant 
t.test(other_clean$KP, mariners_clean$KP) #P-value = 1.329e-07 - very significant (order arguments switched, because the goal is to have a low K%)
t.test(mariners_clean$BBP, other_clean$BBP) #P-value = 0.08614 - close, but not significant

# 3. Visualize results with ggplot2
## install.packages("ggplot2")
library(ggplot2)

## Format data for visualization 
### Create team columns in the dfs 
mariners_clean$Team <- "Mariners"
other_clean$Team <- "Other"

### Combine into one df
cleaned_fullset <- rbind(mariners_clean, other_clean)

## Graph OPS_Plus
OPS_Plus_Plot <-ggplot(cleaned_fullset, aes(Team, OPS_Plus, fill = Team))+ 
geom_boxplot()+
scale_fill_manual(values = c("Mariners" = "#27aeb9", "Other" = "#bf0d3e"))+
labs(title = "Mariners vs. Ex & Future Mariners: OPS+", subtitle = "2010 - 2025 Regular Season; 
Season Long Statistics, 100 PA Min.", y = "OPS+ (league avg = 100)")+
theme_bw()+
stat_summary(fun = mean, geom = "point", color = "#c4ced4")

## Graph OBP
OBP_Plot <- ggplot(cleaned_fullset, aes(Team, OBP, fill = Team))+ 
  geom_boxplot()+
  scale_fill_manual(values = c("Mariners" = "#27aeb9", "Other" = "#bf0d3e"))+
  labs(title = "Mariners vs. Ex & Future Mariners: OBP", subtitle = "2010 - 2025 Regular Season; 
Season Long Statistics, 100 PA Min.")+
  theme_bw()+
  stat_summary(fun = mean, geom = "point", color = "#c4ced4")

## Graph SLG 
SLG_Plot <- ggplot(cleaned_fullset, aes(Team, SLG, fill = Team))+ 
  geom_boxplot()+
  scale_fill_manual(values = c("Mariners" = "#27aeb9", "Other" = "#bf0d3e"))+
  labs(title = "Mariners vs. Ex & Future Mariners: SLG", subtitle = "2010 - 2025 Regular Season; 
Season Long Statistics, 100 PA Min.")+
  theme_bw()+
  stat_summary(fun = mean, geom = "point", color = "#c4ced4")

## Graph K%
KP_Plot <- ggplot(cleaned_fullset, aes(Team, KP, fill = Team))+ 
  geom_boxplot()+
  scale_fill_manual(values = c("Mariners" = "#27aeb9", "Other" = "#bf0d3e"))+
  labs(title = "Mariners vs. Ex & Future Mariners: K%", subtitle = "2010 - 2025 Regular Season; 
Season Long Statistics, 100 PA Min.", y = "Strikeout %")+
  theme_bw()+
  stat_summary(fun = mean, geom = "point", color = "#c4ced4")

## Graph BB
BBP_Plot <- ggplot(cleaned_fullset, aes(Team, BBP, fill = Team))+ 
  geom_boxplot()+
  scale_fill_manual(values = c("Mariners" = "#27aeb9", "Other" = "#bf0d3e"))+
  labs(title = "Mariners vs. Ex & Future Mariners: BB%", subtitle = "2010 - 2025 Regular Season; 
Season Long Statistics, 100 PA Min.", y = "Walk %")+
  theme_bw()+
  stat_summary(fun = mean, geom = "point", color = "#c4ced4")

## Export Plots 
ggsave(filename = "OpsPlusPlot.png", plot = OPS_Plus_Plot)
ggsave(filename = "ObpPlot.png", plot = OBP_Plot)
ggsave(filename = "SlgPlot.png", plot = SLG_Plot)
ggsave(filename = "KpPlot.png", plot = KP_Plot)
ggsave(filename = "BbpPlot.png", plot = BBP_Plot)
