library(dplyr)
library(mlbplotR)
library(ggplot2)

#Import Data
Fullyeardata <- read.csv("FullPitchbyPitchDataset2022.csv")
#IF YOU DON'T HAVE FULLYEAR DATASET CHECKOUT HOW TO SCRAPE:
#https://www.youtube.com/watch?v=swJr4u-HYr0
#S/O Robert Frey! Checkout his videos!

#clean FF and SL----- Switch back to  = mean for accurate data
#did this after the fact to find middling variance
data20221 <- Fullyeardata %>%
  select(player_name, p_throws, pitch_type, pitcher, release_speed, pfx_x, pfx_z, release_pos_x, release_pos_z) %>%
  group_by(pitch_type, player_name) %>%
  filter(pitch_type == "FF", p_throws == "R") %>%
  summarise(release_speed = mean(release_speed),
            pfx_x = mean(pfx_x),
            pfx_z = mean(pfx_z),
            release_pos_x = mean(release_pos_x),
            release_pos_z = mean(release_pos_z))


data20222 <- Fullyeardata %>%
  select(player_name, pitch_type, p_throws, pitcher, release_speed, pfx_x, pfx_z, release_pos_x, release_pos_z) %>%
  group_by(pitch_type, player_name, pitcher) %>%
  filter(pitch_type == "SL", p_throws == "R") %>%
  summarise(release_speed = mean(release_speed),
            pfx_x = mean(pfx_x),
            pfx_z = mean(pfx_z),
            release_pos_x = mean(release_pos_x),
            release_pos_z = mean(release_pos_z))

#merge FB and SL
FBandSL <- merge(data20221, data20222, by = "player_name")

write.csv(FBandSL, "FBandSL.csv")

#Get rid of nulls
FBandSL <- na.omit(FBandSL)

#Make Horz and Vert Brk in inches
FBandSL$release_pos_z.x <- FBandSL$release_pos_z.x * 12
FBandSL$release_pos_z.y <- FBandSL$release_pos_z.y * 12
FBandSL$release_pos_x.x <- FBandSL$release_pos_x.x * 12
FBandSL$release_pos_x.y <- FBandSL$release_pos_x.y * 12

FBandSL$pfx_x.x <- FBandSL$pfx_x.x * 12
FBandSL$pfx_x.y <- FBandSL$pfx_x.y * 12

FBandSL$pfx_z.x <- FBandSL$pfx_z.x * 12
FBandSL$pfx_z.y <- FBandSL$pfx_z.y * 12

#release_pos_z column
FBandSL$ReleaseZDiff <- FBandSL$release_pos_z.y - FBandSL$release_pos_z.x
FBandSL$ReleaseXDiff <- FBandSL$release_pos_x.y - FBandSL$release_pos_x.x

FBandSL <- FBandSL[FBandSL$player_name != "Roberts, Ethan",]
FBandSL <- FBandSL[FBandSL$player_name != "Murfee, Penn",]

#Head and Tail
Top10 <- arrange(FBandSL, pfx_x.y) %>% tail(10)
Top10$pfx_x.y <- round(Top10$pfx_x.y, digits = 2)


Top10$player_name<- gsub("Crick, Kyle", "Crick", Top10$player_name)
Top10$player_name<- gsub("Weissert, Greg", "Weissert", Top10$player_name)
Top10$player_name<- gsub("Espino, Paolo", "Espino", Top10$player_name)
Top10$player_name<- gsub("Knebel, Corey", "Knebel", Top10$player_name)
Top10$player_name<- gsub("Wesneski, Hayden", "Wesneski", Top10$player_name)
Top10$player_name<- gsub("Ottavino, Adam", "Ottavino", Top10$player_name)
Top10$player_name<- gsub("Maton, Phil", "Maton", Top10$player_name)
Top10$player_name<- gsub("Mills, Alec", "Mills", Top10$player_name)
Top10$player_name<- gsub("Gray, Sonny", "Gray", Top10$player_name)
Top10$player_name<- gsub("Trivino, Lou", "Trivino", Top10$player_name)

#Plot
Top10 %>%
  ggplot(aes(x = reorder(pfx_x.y, -pfx_x.y), y = ReleaseZDiff))+
  geom_col(width = 0.4, alpha = 0.75, color = "red", fill = "dodgerblue3")+
  mlbplotR::geom_mlb_headshots(aes(player_id = pitcher), height = 0.14, vjust = .9)+
  geom_text(aes(label = player_name), vjust = 6.0, size = 3, color = "black")+
  theme_classic()+
  ylim(-9.5, 0)+
  labs(title = "RHP Most Horizontal Breaking Sliders & Release Height Difference",
       subtitle = "Release Height Difference = Difference Between SL and FB Release Height",
       x = "Slider Horizontal Break",
       y = "Release Height Difference (Slider and Fastball)",
       caption = "Data: Baseball Savant | Riley Feltner")

#Clean FF and CB
data20223 <- Fullyeardata %>%
  select(player_name, player_name, p_throws, pitch_type, pitcher, release_speed, pfx_x, pfx_z, release_pos_x, release_pos_z) %>%
  group_by(pitch_type, player_name, pitcher) %>%
  filter(pitch_type == "CU", p_throws == "R") %>%
  summarise(release_speed = mean(release_speed),
            pfx_x = mean(pfx_x),
            pfx_z = mean(pfx_z),
            release_pos_x = mean(release_pos_x),
            release_pos_z = mean(release_pos_z))

#Merge CB and FB
FBandCB <- merge(data20221, data20223, by = "player_name")

#Get rid of nulls
FBandCB <- na.omit(FBandCB)

#Make Horz and Vert Brk in inches
FBandCB$release_pos_z.x <- FBandCB$release_pos_z.x* 12
FBandCB$release_pos_z.y <- FBandCB$release_pos_z.y* 12

FBandCB$pfx_x.x <- FBandCB$pfx_x.x* 12
FBandCB$pfx_x.y <- FBandCB$pfx_x.y* 12

FBandCB$pfx_z.x <- FBandCB$pfx_z.x* 12
FBandCB$pfx_z.y <- FBandCB$pfx_z.y* 12

#Rel Z Diff
FBandCB$ReleaseZDiff <- FBandCB$release_pos_z.x - FBandCB$release_pos_z.y

#Head and Tail
Top10.1 <- arrange(FBandCB, pfx_z.y) %>% head(10)
Top10.1$pfx_x.y <- round(Top10.1$pfx_x.y, digits = 2)

#Rename
Top10.1$player_name <- gsub("Coulombe, Danny", "Coulombe", Top10.1$player_name)
Top10.1$player_name <- gsub("Richards, Garrett", "Richards", Top10.1$player_name)
Top10.1$player_name <- gsub("Estrada, Jeremiah", "Estrada", Top10.1$player_name)
Top10.1$player_name <- gsub("Thompson, Zach", "Thompson", Top10.1$player_name)
Top10.1$player_name <- gsub("Thompson, Zack", "Thompson", Top10.1$player_name)
Top10.1$player_name <- gsub("Espino, Paolo", "Espino", Top10.1$player_name)
Top10.1$player_name <- gsub("Civale, Aaron", "Civale", Top10.1$player_name)
Top10.1$player_name <- gsub("Paddack, Chris", "Paddack", Top10.1$player_name)
Top10.1$player_name <- gsub("Pallante, Andre", "Pallante", Top10.1$player_name)
Top10.1$player_name <- gsub("Woodford, Jake", "Woodford", Top10.1$player_name)
Top10.1$player_name <- gsub("Martinez, Nick", "Martinez", Top10.1$player_name)

#plot CB and FF
  ggplot(data = Top10.1, aes(x = pfx_z.y, y = ReleaseZDiff))+
  geom_col(width = 0.1, alpha = 0.75, color = "red", fill = "dodgerblue3")+
  mlbplotR::geom_mlb_headshots(aes(player_id = pitcher), height = 0.09, vjust = -.2)+
  geom_text(aes(label = player_name), vjust = -0.3, size = 3, color = "black")+
  theme_classic()+
  labs(title = "CB Most Vertical Break and Release Height Difference",
       subtitle = "Release Height Difference = Difference Between FB and CB Release Height",
       x = "Curveball Horizontal Break",
       y = "Release Height Difference (Curveball and Fastball)",
       caption = "Data: Baseball Savant | Riley Feltner")
  

#plot SL and FF RelX Diff
ggplot(data = Top10, aes(x = ReleaseXDiff, y = reorder(pfx_x.y, -pfx_x.y)))+
  geom_col(width = 0.1, alpha = 0.75, color = "red", fill = "dodgerblue3")+
  mlbplotR::geom_mlb_headshots(aes(player_id = pitcher), height = 0.09)+
  geom_text(aes(label = player_name), vjust = -1.3, size = 3, color = "black")+
  theme_classic()+
  labs(title = "RHP Most Horizontal Breaking Sliders & Release X Difference",
       subtitle = "Release X Difference = Difference Between FB and SL Release Side",
       y = "Slider Horizontal Break",
       x = "Release X Difference (Fastball and Slider)",
       caption = "Data: Baseball Savant | Riley Feltner")

#JOSE URQUIDY
rm(Jose)

Jose <- filter(Fullyeardata, player_name == "Urquidy, José")
Jose <- filter(Jose, pitch_type == "FF" | pitch_type == "SL")

Jose1 <- filter(Fullyeardata, player_name == "Urquidy, José")

Joseplot <- ggplot(data = Jose, mapping = aes(x = release_pos_x, y = release_pos_z, color = pitch_type))+
  xlim(-3, 3)+
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.1)+
  labs(title = "José Urquidy 2022 Fastball and Slider Release Positions",
       subtitle = "Catcher's Perspective",
       x = "Horizontal Release",
       y = "Vertical Release",
       caption = "Data: Baseball Savant | Riley Feltner")+
  theme_minimal()  

Joseplot + geom_point(alpha = 0.3)


#Max Scherzer
Max <- filter(Fullyeardata, player_name == "Scherzer, Max")
Max <- filter(Max, pitch_type == "FF" | pitch_type == "SL")


Maxplot <- ggplot(data = Max, mapping = aes(x = release_pos_x, y = release_pos_z, color = pitch_type))+
  xlim(-4, 4)+
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.1)+
  labs(title = "Max Scherzer 2022 Fastball and Slider Release Positions",
       subtitle = "Catcher's Perspective",
       x = "Horizontal Release",
       y = "Vertical Release",
       caption = "Data: Baseball Savant | Riley Feltner")+
  theme_minimal()  

Maxplot + geom_point(alpha = 0.3)

#Pete Fairbanks Small Variance
Pete <- filter(Fullyeardata, player_name == "Fairbanks, Pete")
Pete <- filter(Pete, pitch_type == "FF")


Peteplot <- ggplot(data = Pete, mapping = aes(x = release_pos_x, y = release_pos_z, color = pitch_type))+
  xlim(-3, 3)+
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.1)+
  labs(title = "Pete Fairbanks 2022 Fastball Release Positions",
       subtitle = "Catcher's Perspective",
       x = "Horizontal Release",
       y = "Vertical Release",
       caption = "Data: Baseball Savant | Riley Feltner")+
  theme_minimal()  

Peteplot + geom_point()


#Sandy Alcantara median variance
Sandy <- filter(Fullyeardata, player_name == "Alcantara, Sandy")
Sandy <- filter(Sandy, pitch_type == "FF")

Sandyplot <- ggplot(data = Sandy, mapping = aes(x = release_pos_x, y = release_pos_z, color = pitch_type))+
  xlim(-3, 3)+
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.1)+
  labs(title = "Sandy Alcantara 2022 Fastball Release Positions",
       subtitle = "Catcher's Perspective",
       x = "Horizontal Release",
       y = "Vertical Release",
       caption = "Data: Baseball Savant | Riley Feltner")+
  theme_minimal()  

Sandyplot + geom_point()


#Chris Bassit large variance
Chris <- filter(Fullyeardata, player_name == "Bassitt, Chris")
Chris <- filter(Chris, pitch_type == "FF")

Chrisplot<- ggplot(data = Chris, mapping = aes(x = release_pos_x, y = release_pos_z, color = pitch_type))+
  xlim(-3, 3)+
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.1)+
  labs(title = "Chris Bassit 2022 Fastball Release Positions",
       subtitle = "Catcher's Perspective",
       x = "Horizontal Release",
       y = "Vertical Release",
       caption = "Data: Baseball Savant | Riley Feltner")+
  theme_minimal()  

Chrisplot + geom_point()


#REL X
#Edward Cabrera High Variance
Edward <- filter(Fullyeardata, player_name == "Cabrera, Edward")
Edward <- filter(Edward, pitch_type == "FF")

Edwardplot<- ggplot(data = Edward, mapping = aes(x = release_pos_x, y = release_pos_z, color = pitch_type))+
  xlim(-3, 3)+
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.1)+
  labs(title = "Edward Cabrera 2022 Fastball Release Positions",
       subtitle = "Catcher's Perspective",
       x = "Horizontal Release",
       y = "Vertical Release",
       caption = "Data: Baseball Savant | Riley Feltner")+
  theme_minimal()  

Edwardplot + geom_point()

#David Bednar Low Variance
David <- filter(Fullyeardata, player_name == "Bednar, David")
David <- filter(David, pitch_type == "FF")

Davidplot<- ggplot(data = David, mapping = aes(x = release_pos_x, y = release_pos_z, color = pitch_type))+
  xlim(-3, 3)+
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.1)+
  labs(title = "David Bednar 2022 Fastball Release Positions",
       subtitle = "Catcher's Perspective",
       x = "Horizontal Release",
       y = "Vertical Release",
       caption = "Data: Baseball Savant | Riley Feltner")+
  theme_minimal()  

Davidplot + geom_point()

#Michael Wacha Median Variance
Michael <- filter(Fullyeardata, player_name == "Wacha, Michael")
Michael <- filter(Michael, pitch_type == "FF")

Michaelplot<- ggplot(data = Michael, mapping = aes(x = release_pos_x, y = release_pos_z, color = pitch_type))+
  xlim(-3, 3)+
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.1)+
  labs(title = "Michael Wacha 2022 Fastball Release Positions",
       subtitle = "Catcher's Perspective",
       x = "Horizontal Release",
       y = "Vertical Release",
       caption = "Data: Baseball Savant | Riley Feltner")+
  theme_minimal()  

Michaelplot + geom_point()

#Ohtani
Shohei <- filter(Fullyeardata, player_name == "Ohtani, Shohei")
Shohei <- filter(Shohei, pitch_type == "FF" | pitch_type == "CU")

Shoheiplot<- ggplot(data = Shohei, mapping = aes(x = release_pos_x, y = release_pos_z, color = pitch_type))+
  xlim(-3, 3)+
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.1)+
  labs(title = "Shohei Ohtani 2022 Fastball and Curveball Release Positions",
       subtitle = "Catcher's Perspective",
       x = "Horizontal Release",
       y = "Vertical Release",
       caption = "Data: Baseball Savant | Riley Feltner")+
  theme_minimal()  

Shoheiplot + geom_point(alpha = 0.5)


