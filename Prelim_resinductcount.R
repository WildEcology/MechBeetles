
##=================================================================================================================
##                      
## Resin count differences across species  
## Very prelim data by Jenny Cribbs 
##    
##==================================================================================================================


##packages
library(tidyverse)

select <- dplyr::select
rename <- dplyr::rename
group_by <- dplyr::group_by

##theme for plots
theme_set(
  theme_bw(base_size = 12)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))
)


#data
resin <- read_csv("ResinDuctRoughCount.csv")[1:4] %>% 
  rename_all(tolower)

#summary plots
ggplot(resin, aes(x=species, y=resin_duct_count))+
  geom_boxplot()

ggplot(resin, aes(x=ring, y=resin_duct_count))+
  geom_point()+
  geom_smooth()

ggplot(resin, aes(x=ring, y=resin_duct_count, fill=species, color=species))+
  geom_smooth()+
  xlab("ring # (proxy for age)")


