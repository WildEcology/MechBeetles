
## ---------------------------
##
## Script name: Correlation between PIPO and PIIMO
##
## Author: Dr. Joan Dudney
##
## Date Created: 2021-06-23
##
## ---------------------------

##packages
library(tidyverse)
library(readxl)
library(hablar)


select <- dplyr::select
rename <- dplyr::rename
group_by <- dplyr::group_by

##theme for plots
theme_set(
  theme_bw(base_size = 12)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))
)

##=================================================================================================================
##     ##Reading in and cleaning Sharon's PIPO cores                 
##==================================================================================================================

df <- read.table("snf-pipo-rd.txt", header = FALSE, stringsAsFactors=F)

rnames <- as.vector(df[1,])

df2=df[-1,]
colnames(df2) = c("age_CE", "tree_ID", "status", "tda_mm2", "size_mm2", "duct_prod", "ringarea_mm2")

treerings <- df2 %>% 
  mutate(age_CE=as.numeric(age_CE), ringarea_mm2=as.numeric(ringarea_mm2)) %>% 
  group_by(age_CE) %>% 
  summarize(mean_area=mean(ringarea_mm2, na.rm=T)) %>% 
  mutate(rwi=mean_area/5) %>% 
  rename(year=age_CE) %>% 
  select(-mean_area) %>% 
  mutate(year=as.numeric(year))


##=================================================================================================================
##     ##Reading in and cleaning Jonny's sugar pine cores               
##==================================================================================================================

jcores <- read_excel("jonnydata_revised01.23.13_n104.xlsx", sheet = 2)

head(jcores)
##pivoting data to match sharon's

jcores_clean <- jcores %>% 
  retype() %>% ##change columns to numeric for calculations
  pivot_longer(-CoreID, names_to = "year") %>% 
  group_by(year) %>% 
  summarise(jrwi=mean(value, na.rm=T))%>%
  mutate(year=as.numeric(year)) %>% 
  full_join(treerings) %>% 
  na.omit()


##now estimating correlation between pipo and pila
cor(jcores_clean$rwi, jcores_clean$jrwi)


##=================================================================================================================
##   Figures                   
##==================================================================================================================

##correlation visualized
ggplot(jcores_clean, aes(x=rwi, y=jrwi))+
  geom_point()+
  geom_smooth()+
  xlab("PIPO RWI")+
  ylab("PILA RWI")

##differences across years
ggplot(jcores_clean, aes(x=year, y=jrwi, color="PILA"))+
  geom_point()+
  geom_line()+
  geom_point(aes(y=rwi, color="PIPO"))+
  geom_line(aes(y=rwi, color="PIPO"))+
  #guides(color=F)+
  ylab("RWI")+
  xlab("Year")

