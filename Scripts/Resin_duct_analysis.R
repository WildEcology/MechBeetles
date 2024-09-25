## ---------------------------
##
## Script name: 
##
## Author: Dr. Joan Dudney
##
## Date Created: 2021-12-06
##
## Copyright (c) Joan Dudney, 2021
## Email: jdudney@berkeley.edu
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


##packages
library(tidyverse)
library(ggpubr)
library(lme4)
library(sjPlot)
library(ggeffects)
library(patchwork)


select <- dplyr::select
rename <- dplyr::rename
group_by <- dplyr::group_by

##theme for plots
theme_set(
  theme_bw(base_size = 25)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))
)

##data
resin <- read_csv("Data/ResinDuctRoughCount_SEKI2021.csv")
resin <- read_csv("Data/ResinDuctRoughCount_SEKI2021.csv")[1:5] 
names(resin) <- tolower(names(resin)) 


plotdat <- read_csv("Data/Beetles_FieldNames_Tucson.csv")[c(1:6, 8, 16, 17)]
names(plotdat) <- tolower(names(plotdat))

colnames(combdat)

##combining data
combdat <- plotdat %>% 
  rename( core_id = "tucson_id") %>% 
  right_join(resin) %>% 
  rename(resin=resin_duct_count) %>% 
  filter(!is.na(resin)) %>% 
  filter(!species=="PIAL") %>% 
  filter(!species=="PICO") %>% 
  mutate(species1=ifelse(species=="PIMO", "Western white",
                         ifelse(species == "PILA", "Sugar pine", "Foxtail pine")))


uniquedat <- combdat %>% 
  distinct(elevation_m, species1, aspect)

##running a few models
mod1 <- lmer(resin ~ species +dbh+height+ elevation_m + aspect + (1|calendar_year), data=combdat)
mod1 <- lm(resin ~ species +dbh+height+ elevation_m + aspect + calendar_year, data=combdat)
mod1 <- lm(resin ~ species +dbh+height+ elevation_m + aspect + calendar_year, data=combdat)
mod1 <- glm(resin ~ species +dbh+height+ elevation_m + aspect + calendar_year, family="poisson", data=combdat)

tab_model(mod1, show.ci=F, show.se=T)

pred <- ggpredict(mod1)
plot(pred)

summary(aov(resin ~ species + elevation_m  + aspect + calendar_year, data=combdat))


pila <- combdat %>% 
  group_by(elevation_m, species, calendar_year, aspect) %>% 
  summarise(meanresin=mean(resin)) %>% 
  filter(aspect=="N") 
  #filter(species=="PILA")
pila

ggplot(pila, aes(x=elevation_m, y=meanresin, color=species))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(na.omit(combdat), aes(x=aspect, y=resin, fill=aspect))+
  geom_boxplot()

##figures
##creating a color palette
branded_colors <- list(
  "yellow"   = "#E6C87F",
  "red"    = "#D4787D",
  "green"  = "#718F94",
  "navy"   = "#C0C8DC",
  "purple"   = "#545775"
)


branded_pal <- function(
  primary = "purple",
  other = "yellow",
  direction = 1
) {
  stopifnot(primary %in% names(branded_colors))
  
function(n) {
    if (n > 6) warning("Branded Color Palette only has 6 colors.")
    
    if (n == 2) {
      other <- if (!other %in% names(branded_colors)) {
        other
      } else {
        branded_colors[other]
      }
      color_list <- c(other, branded_colors[primary])
    } else {
      color_list <- branded_colors[1:n]
    }
    
    color_list <- unname(unlist(color_list))
    if (direction >= 0) color_list else rev(color_list)
  }
}

##color
scale_colour_branded <- function(
  primary = "purple",
  other = "yellow",
  direction = 1,
  ...
) {
  ggplot2::discrete_scale(
    "colour", "branded",
    branded_pal(primary, other, direction),
    ...
  )
}

scale_color_branded <- scale_colour_branded

##fill

scale_fill_branded <- function(
  primary = "purple",
  other = "yellow",
  direction = 1,
  ...
) {
  ggplot2::discrete_scale(
    "fill", "branded",
    branded_pal(primary, other, direction),
    ...
  )
}

scale_fill_branded <- scale_fill_branded

hist(combdat$resin)

sp_plot <- ggplot(combdat, aes(x=factor(species1), y=resin, fill=species1, color="species1")) +
  #geom_jitter(alpha=.1, width= .4)+
  geom_boxplot(outlier.colour = "NA", width=.7) +
  scale_fill_branded(labels  = c("Foxtail pine",  "Sugar pine", "Western white")) +
  #scale_color_branded() +
  scale_color_manual(values = "black",
                     labels = c("Foxtail pine",  "Sugar pine", "Western white")) +
  guides(fill="none", color= "none") +
  ylim(0, 10) +
  ylab("Resin ducts (#/ring)") +
  xlab("")

sp_plot

aspect <- ggplot(filter(combdat,!is.na(aspect)), aes(x=aspect, y=resin, fill=aspect))+
  geom_boxplot(outlier.colour = "NA", width=.6) +
  scale_fill_branded() +
  #scale_color_branded() +
  scale_color_manual(values = "black") +
  guides(fill="none", color= "none") +
  ylab("Resin ducts (#/ring)") +
  xlab("Aspect")+
  ylim(0, 10)+
  stat_compare_means(aes(label = paste0("p = ", ..p.format..)), size=5)
                     
aspect
help("stat_compare_means")
sp_plot+aspect +
  plot_annotation(tag_levels = "A")


acrossyear <- ggplot(combdat, aes(x=calendar_year, y=resin, color=species1, fill=species1))+
  #geom_jitter(alpha=.5, color="grey")+
  geom_smooth( aes(x=calendar_year, y=resin, fill=species1), method="lm")+
  ylab("Resin ducts (#/ring)") +
  xlab("Year")+
  scale_fill_branded() +
  scale_color_branded() +
  scale_x_continuous(name="Year", breaks = scales::pretty_breaks(n = 16))+
  theme(legend.title = element_blank())

library(patchwork)
(sp_plot / aspect)  &
  plot_annotation(tag_levels="a") & theme(
      plot.tag = element_text(face = 'bold', size=15, family ="Helvetica"))
                                                                                                

ggplot(combdat, aes(x=elevation_m, y=resin, color=species1, fill=species1))+
  #geom_jitter(alpha=.5, color="grey")+
  geom_smooth( aes(x=elevation_m, y=resin, fill=species1), method="lm")+
  ylab("Resin ducts (#/ring)") +
  xlab("Elevation")+
  scale_fill_branded() +
  scale_color_branded() +
  scale_x_continuous(name="Elevation (m)", breaks = scales::pretty_breaks(n = 15))

ggplot(combdat, aes(x=elevation_m, y=resin))+
  #geom_jitter(alpha=.5, color="grey")+
  #geom_point()+
  geom_smooth( aes(x=elevation_m, y=resin), method="lm") +
  ylab("Resin ducts (#/ring)") +
  xlab("Elevation")+
  #scale_fill_branded() +
  #scale_color_branded() +
  scale_x_continuous(name="Elevation (m)", breaks = scales::pretty_breaks(n = 6))



dbh <- ggplot(filter(combdat,!is.na(dbh)), aes(x=dbh, y=resin, fill=species1, color=species1))+
  geom_smooth( aes(x=dbh, y=resin, fill=species1), method="lm")+
  ylab("Resin ducts (#/ring)") +
  xlab("DBH (cm)")+
  scale_fill_branded() +
  scale_color_branded() +
  scale_x_continuous(name="DBH (cm)", breaks = scales::pretty_breaks(n = 15))+
  theme(legend.title = element_blank())
dbh

height <- ggplot(combdat, aes(x = height, y=resin, fill=species1, color=species1))+
  geom_smooth( method="lm")+
  ylab("Resin ducts (#/ring)") +
  scale_fill_branded() +
  scale_color_branded() +
  scale_x_continuous(name="Height (m)", breaks = scales::pretty_breaks(n = 15))+
  theme(legend.title = element_blank())
height

