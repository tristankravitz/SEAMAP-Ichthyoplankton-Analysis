library(vegan)
library(ggplot2)
library(dplyr)
library(ggdist)
library(tidyverse)
library(ISwR)
library(lme4)
library(AICcmodavg)
library(colorspace)
library(MetBrewer)
library(lubridate)
library(ggpubr)
library(gridExtra)
library(data.table)


#read in abundance data
plankton<-read.csv("abundancetotal.csv")

#convert to date format
plankton$Date<- as.Date(plankton$Date, format="%m/%d/%Y")

#omit NA's
plankton=na.omit(plankton)

#create separate column for years
plankton$Year=year(plankton$Date)
plankton$Month=month(plankton$Date)

#convert Year column to factor
plankton$Year=as.factor
plankton$Month=as.factor


#read in environmental and tow data
environment<-read.csv("environmenttotal.csv")

#convert to data frames
plankton_df=as.data.frame(plankton, stringsAsFactors = default.stringsAsFactors())

environment_df=as.data.frame(environment, stringsAsFactors = default.stringsAsFactors())


#left join data frames
list_df = list(plankton_df,environment_df)
joined_df <- list_df %>% reduce(left_join, by="TOWID")
joined_df

#remove extra date column if necessary
newjoin = select(joined_df, -Date.y)
#otherwise to run rest of code:
newjoin = joined_df

#create new column with month names called 'month'
months = c("Jan","Feb","Mar",
            "Apr","May","Jun",
            "Jul","Aug","Sep",
            "Oct","Nov","Dec")
newjoin$month = months[ newjoin$Month ]

newjoin$month=as.factor

summarise(newjoin$SCIENTIFICNAME)
dim(newjoin$SCIENTIFICNAME)
summary(newjoin$SCIENTIFICNAME)

#summary statistics

#species
summarystats = group_by(newjoin, SCIENTIFICNAME) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
summarystats

#species table and csv
species = data.table(summarystats)
write.csv(species, "/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/speciestable.csv", row.names=FALSE)

#year
summarystats2 = group_by(newjoin, Year) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
summarystats2


#year table and csv
year = data.table(summarystats2)
write.csv(year, "/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/yeartable.csv", row.names=FALSE)

#year line graph
yearline = ggplot(year, aes(x=Year, y = count))+
  geom_line()+
  geom_errorbar(aes(ymin=count-sd,ymax=count+sd), width=0.2)+theme(legend.position="none")+
  ylab("Count")+
  ggtitle("Total Count of Fish Larvae Sampled Per Year")
yearline

pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/yearline.pdf", height = 10, width=13)
plot(yearline)
dev.off()

#station
summarystats3 = group_by(newjoin, STATION.x) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
summarystats3

#bar graph of total larvae count per station
stationbar = ggplot(summarystats3, aes(x=STATION.x, y = count,fill=STATION.x))+
  geom_col()+
  scale_fill_manual(values=c("darkblue","slateblue","skyblue3"))+
  geom_errorbar(aes(ymin=count-sd,ymax=count+sd), width=0.2)+theme_bw()+theme(legend.position="none")+
  ylab("Total Count")+
  xlab("Station")+
  ggtitle("Total Count of Fish Larvae Sampled per Station")

stationbar

pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/stationbar.pdf", height = 10, width=13)
plot(stationbar)
dev.off()

#zeroes in data
sum(newjoin$TEMPERATURE==0)

#Six species-specific subsets
sub_flounder=subset(newjoin, SCIENTIFICNAME=="PARALICHTHYS DENTATUS")

sub_menhaden=subset(newjoin, SCIENTIFICNAME=="BREVOORTIA TYRANNUS")

sub_croaker=subset(newjoin, SCIENTIFICNAME=="MICROPOGONIAS UNDULATUS")

sub_eel=subset(newjoin, SCIENTIFICNAME=="ANGUILLA ROSTRATA")

sub_bay=subset(newjoin, SCIENTIFICNAME=="ANCHOA MITCHILLI")

sub_winter=subset(newjoin,SCIENTIFICNAME=="PSEUDOPLEURONECTES AMERICANUS")

#mean totals & species specific summary stats
mean(sub_flounder$TOTAL)
mean(sub_menhaden$TOTAL)
mean(sub_croaker$TOTAL)
mean(sub_bay$TOTAL)
mean(sub_eel$TOTAL)
mean(sub_winter$TOTAL)

#summer flounder totals by year
flounder_yr = group_by(sub_flounder, Year) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
flounder_yr

#bar graph of total summer flounder per year
floundercount_yr = ggplot(flounder_yr, aes(x=Year, y = count))+
  geom_col(fill="slateblue")+
  geom_errorbar(aes(ymin=count-sd,ymax=count+sd), width=0.2)+theme(legend.position="none")+
  ylab("Count")+
  ggtitle("Count of Summer Flounder per Year")

floundercount_yr

pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/summerflounderbar_year.pdf", height = 10, width=13)
plot(floundercount_yr)
dev.off()

#scatter plot of summer flounder per year
flounderyear_pt = ggplot(flounder_yr, aes(x=Year, y = count))+
  geom_point()+
  ggtitle("Count of Summer Flounder by Year")+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

flounderyear_pt

pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/flounderyear_pt.pdf", height = 10, width=13)
plot(flounderyear_pt)
dev.off()

#scatter plot of mean summer flounder count per year
floundermean_pt = ggplot(flounder_yr, aes(x=Year, y = mean))+
  geom_point()+
  ylab("Mean Count")+
  ggtitle("Mean Count of Summer Flounder by Year")+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")
floundermean_pt

pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/floundermean_pt.pdf", height = 10, width=13)
plot(floundermean_pt)
dev.off()

#summer flounder per station
flounder_st = group_by(sub_flounder, STATION.x) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
flounder_st

#summer flounder per station bar graph
flounder_station = ggplot(flounder_st, aes(x=STATION.x, y = count,fill=STATION.x))+
  geom_col()+
  scale_fill_manual(values=c("darkblue","slateblue","skyblue3"))+
  geom_errorbar(aes(ymin=count-sd,ymax=count+sd), width=0.2)+theme_bw()+theme(legend.position="none")+
  ylab("Total Count")+
  xlab("Station")+
  ggtitle("Count of Summer Flounder per Station")

flounder_station

pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/flounderstation_bar.pdf", height = 10, width=13)
plot(flounder_station)
dev.off()

#summer flounder count by temperature
flounder_temp = group_by(sub_flounder, TEMPERATURE) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
flounder_temp

#scatter plot of summer flounder count by temperature
q1 = ggplot(flounder_temp, aes(x=TEMPERATURE, y = count))+
  geom_point()+
  ggtitle("Count of Summer Flounder by Temperature")+
  ylab("Count")+
  xlab("Temperature (Degrees C)")+
  stat_regline_equation()+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

q1

#menahden count per year
menhaden_yr = group_by(sub_menhaden, Year) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
menhaden_yr

#bar graph of menhaden per year
menhadenyear_bar= ggplot(menhaden_yr, aes(x=Year, y = count))+
  geom_col(fill="slateblue")+
  geom_errorbar(aes(ymin=count-sd,ymax=count+sd), width=0.2)+theme(legend.position="none")+
  ylab("Count")+
  ggtitle("Count of Menhaden per Year")

menhadenyear_bar

pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/menhadenyear_bar.pdf", height = 10, width=13)
plot(menhadenyear_bar)
dev.off()

#scatter plot of menhaden per year
menhadenyear_pt = ggplot(menhaden_yr, aes(x=Year, y = count))+
  geom_point()+
  ggtitle("Count of Menhaden by Year")+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

menhadenyear_pt

pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/menhadenyear_pt.pdf", height = 10, width=13)
plot(menhadenyear_pt)
dev.off()

#mean menhaden count per year
menhadenmean_year = ggplot(menhaden_yr, aes(x=Year, y = mean))+
  geom_point()+
  ylab("Mean Count")+
  ggtitle("Mean Count of Nenhaden by Year")+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")
menhadenmean_year

pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/menhadenmean_yr.pdf", height = 10, width=13)
plot(menhadenmean_year)
dev.off()


#menhaden per station
menhaden_st = group_by(sub_menhaden, STATION.x) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
menhaden_st

#bar graph of menhaden per station
menhaden_station = ggplot(menhaden_st, aes(x=STATION.x, y = count,fill=STATION.x))+
  geom_col()+
  scale_fill_manual(values=c("darkblue","slateblue","skyblue3"))+
  geom_errorbar(aes(ymin=count-sd,ymax=count+sd), width=0.2)+theme_bw()+theme(legend.position="none")+
  ylab("Total Count")+
  xlab("Station")+
  ggtitle("Count of Atlantic Menhaden per Station")

menhaden_station

pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/menhadenstation_bar.pdf", height = 10, width=13)
plot(menhaden_station)
dev.off()


#menhaden by temperature
menhaden_temp = group_by(sub_menhaden, TEMPERATURE) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
menhaden_temp

#scatter plot of menhaden by temperature
q2 = ggplot(menhaden_temp, aes(x=TEMPERATURE, y = count))+
  geom_point()+
 theme(legend.position="none")+
  ylab("Count")+
  xlab("Temperature (Degrees C)")+
  ggtitle("Count of Menhaden by Temperature")+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

q2


#croaker per year
croaker_yr = group_by(sub_croaker, Year) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
croaker_yr

#bar graph of croaker per year
croakeryear_bar= ggplot(croaker_yr, aes(x=Year, y = count))+
  geom_col(fill="slateblue")+
  geom_errorbar(aes(ymin=count-sd,ymax=count+sd), width=0.2)+theme(legend.position="none")+
  ylab("Count")+
  ggtitle("Count of American Croaker per Year")

croakeryear_bar

pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/croakeryear_bar.pdf", height = 10, width=13)
plot(croakeryear_bar)
dev.off()

#scatter plot of croaker per year
croakeryear_pt= ggplot(croaker_yr, aes(x=Year, y = count))+
  geom_point()+
  ggtitle("Count of Croaker by Year")+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

croakeryear_pt

pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/croakeryear_pt.pdf", height = 10, width=13)
plot(croakeryear_pt)
dev.off()

#croaker per station
croaker_st = group_by(sub_croaker, STATION.x) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
croaker_st

#bar graph of croaker per station
croaker_station = ggplot(croaker_st, aes(x=STATION.x, y = count,fill=STATION.x))+
  geom_col()+
  scale_fill_manual(values=c("darkblue","slateblue","skyblue3"))+
  geom_errorbar(aes(ymin=count-sd,ymax=count+sd), width=0.2)+theme_bw()+theme(legend.position="none")+
  ylab("Total Count")+
  xlab("Station")+
  ggtitle("Count of Atlantic Croaker per Station")

croaker_station

pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/croakerstation_bar.pdf", height = 10, width=13)
plot(croaker_station)
dev.off()


#croaker by temperature
croaker_temp = group_by(sub_croaker, TEMPERATURE) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
croaker_temp

#scatter plot of croaker by temperature
q3 = ggplot(croaker_temp, aes(x=TEMPERATURE, y = count))+
  geom_point()+
  ggtitle("Count of Croaker by Temperature")+
  ylab("Count")+
  xlab("Temperature (Degrees C)")+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

q3

#american eel per year
eel_yr = group_by(sub_eel, Year) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
eel_yr

#bar graph of eel per year
eelyear_bar= ggplot(eel_yr, aes(x=Year, y = count))+
  geom_col(fill="slateblue")+
  geom_errorbar(aes(ymin=count-sd,ymax=count+sd), width=0.2)+theme(legend.position="none")+
  ylab("Count")+
  ggtitle("Count of American Eel per Year")

eelyear_bar

pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/eelyear_bar.pdf", height = 10, width=13)
plot(eelyear_bar)
dev.off()

#scatter plot of eel per year
eelyear_pt = ggplot(eel_yr, aes(x=Year, y = count))+
  geom_point()+
  ggtitle("Count of American Eel by Year")+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

eelyear_pt


pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/eelyear_pt.pdf", height = 10, width=13)
plot(eelyear_pt)
dev.off()

#eel per station
eel_st = group_by(sub_eel, STATION.x) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
eel_st

#bar graph of eel per station
eel_station = ggplot(eel_st, aes(x=STATION.x, y = count,fill=STATION.x))+
  geom_col()+
  scale_fill_manual(values=c("darkblue","slateblue","skyblue3"))+
  geom_errorbar(aes(ymin=count-sd,ymax=count+sd), width=0.2)+theme_bw()+theme(legend.position="none")+
  ylab("Total Count")+
  xlab("Station")+
  ggtitle("Count of American Eel per Station")

eel_station


pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/eel_station.pdf", height = 10, width=13)
plot(eel_station)
dev.off()

#eel by temperature
eel_temp = group_by(sub_eel, TEMPERATURE) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
eel_temp

#scatter plot of eel by temperature
q4 = ggplot(eel_temp, aes(x=TEMPERATURE, y = count))+
  geom_point()+
  ggtitle("Count of American Eel by Temperature")+
  ylab("Count")+
  xlab("Temperature (Degrees C)")+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

q4

#bay anchovy by year
bay_yr = group_by(sub_bay, Year) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
bay_yr

#bar graph of anchovy by year
bayyear_bar= ggplot(bay_yr, aes(x=Year, y = count))+
  geom_col(fill="slateblue")+
  geom_errorbar(aes(ymin=count-sd,ymax=count+sd), width=0.2)+theme(legend.position="none")+
  ylab("Count")+
  ggtitle("Count of Bay Anchovy per Year")

bayyear_bar

pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/anchovyyear_bar.pdf", height = 10, width=13)
plot(bayyear_bar)
dev.off()



#scatter plot of anchovy per year
bayyear_pt = ggplot(bay_yr, aes(x=Year, y = count))+
  geom_point()+
  ggtitle("Count of Bay Anchovy by Year")+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

bayyear_pt


pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/anchovyyear_pt.pdf", height = 10, width=13)
plot(bayyear_pt)
dev.off()

#anchovy per station
bay_st = group_by(sub_bay, STATION.x) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
bay_st

#bar graph of anchovy per station
bay_station = ggplot(bay_st, aes(x=STATION.x, y = count,fill=STATION.x))+
  geom_col()+
  scale_fill_manual(values=c("darkblue","slateblue","skyblue3"))+
  geom_errorbar(aes(ymin=count-sd,ymax=count+sd), width=0.2)+theme_bw()+theme(legend.position="none")+
  ylab("Total Count")+
  xlab("Station")+
  ggtitle("Count of Bay Anchovy per Station")

bay_station


pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/anchovy_station.pdf", height = 10, width=13)
plot(bay_station)
dev.off()

#anchovy by temperature
bay_temp = group_by(sub_bay, TEMPERATURE) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
bay_temp

#scatter plot of anchovy by temperature
q5 = ggplot(bay_temp, aes(x=TEMPERATURE, y = count))+
  geom_point()+
  ggtitle("Count of Bay Anchovy by Temperature")+
  ylab("Count")+
  xlab("Temperature (Degrees C)")+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

q5


#winter flounder per year
winter_yr = group_by(sub_winter, Year) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
winter_yr

#bar graph of winter flounder by year
winteryear_bar= ggplot(winter_yr, aes(x=Year, y = count))+
  geom_col(fill="slateblue")+
  geom_errorbar(aes(ymin=count-sd,ymax=count+sd), width=0.2)+theme(legend.position="none")+
  ylab("Count")+
  ggtitle("Count of Winter Flounder per Year")

winteryear_bar

pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/winteryear_bar.pdf", height = 10, width=13)
plot(winteryear_bar)
dev.off()

#scatter plot of winter flounder per year
winteryear_pt = ggplot(winter_yr, aes(x=Year, y = count))+
  geom_point()+
  ggtitle("Count of Winter Flounder by Year")+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

winteryear_pt


pdf("/Users/Trist/OneDrive/Desktop/Data Analysis/SEAMAP/winteryear_pt.pdf", height = 10, width=13)
plot(winteryear_pt)
dev.off()


#winter flounder by temperature
winter_temp = group_by(sub_winter, TEMPERATURE) %>%
  summarise(
    count = n(),
    mean = mean(TOTAL, na.rm = TRUE),
    sd = sd(TOTAL, na.rm = TRUE),
    median = median(TOTAL, na.rm = TRUE),
    IQR = IQR(TOTAL, na.rm = TRUE))
winter_temp

#scatter plot of winter flounder by temperature
q6 = ggplot(winter_temp, aes(x=TEMPERATURE, y = count))+
  geom_point()+
  ggtitle("Count of Winter Flounder by Temperature")+
  ylab("Count")+
  xlab("Temperature (Degrees C)")+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

q6


#arrange temperature scatter plots together with regression lines
grid.arrange(q1+theme_bw()+stat_regline_equation(),q2+theme_bw()+stat_regline_equation(),q3+theme_bw()+stat_regline_equation(),q4+theme_bw()+stat_regline_equation(),q5+theme_bw()+stat_regline_equation(),q6+theme_bw()+stat_regline_equation(),ncol=2)

#arrange station bar graphs together
grid.arrange(flounder_station,menhaden_station,croaker_station,eel_station,bay_station,ncol=2)

#boxplots of totals per year for each species

summerbox = boxplot(TOTAL~Year,data=sub_flounder, main = "Total Paralichthys dentatus Per Station", ylab = "Total", xlab = "Year", col="slateblue")
menhadenbox = boxplot(TOTAL~Year,data=sub_menhaden, main = "Total Brevoortia tyrannus Per Station", ylab = "Total", xlab = "Year", col="slateblue")
croakerbox = boxplot(TOTAL~Year,data=sub_croaker,  main = "Total Micropogonias undulatus Per Station", ylab = "Total", xlab = "Year", col="slateblue")
eelbox = boxplot(TOTAL~Year,data=sub_eel, main = "Total Anguilla rostrata Per Station", ylab = "Total", xlab = "Year", col="slateblue")
baybox = boxplot(TOTAL~Year,data=sub_bay, main = "Total Anchoa mitchilli Per Station", ylab = "Total", xlab = "Year", col="slateblue")
winterbox = boxplot(TOTAL~Year,data=sub_winter, main = "Total Pseudopleuronectes americanus Per Station", ylab = "Total", xlab = "Year", col="slateblue")



#Shapiro tests for normality, histograms of sample counts
shapiro.test(sub_flounder$TOTAL)
hist(sub_flounder$TOTAL,col="slateblue")
shapiro.test(sub_menhaden$TOTAL)
hist(sub_menhaden$TOTAL,col="slateblue")
shapiro.test(sub_croaker$TOTAL)
hist(sub_croaker$TOTAL,col="slateblue")
shapiro.test(sub_eel$TOTAL)
hist(sub_eel$TOTAL,col="slateblue")
shapiro.test(sub_bay$TOTAL)
hist(sub_bay$TOTAL,col="slateblue")
shapiro.test(sub_winter$TOTAL)
hist(sub_winter$TOTAL,col="slateblue")

par(mfrow=c(2,3))
hist(sub_flounder$TOTAL,col="slateblue",main=paste("Summer Flounder Counts"),xlab="Sample Count")
hist(sub_menhaden$TOTAL,col="slateblue",main=paste("Atlantic Menhaden Counts"),xlab="Sample Count")
hist(sub_croaker$TOTAL,col="slateblue",main=paste("Atlantic Croaker Counts"),xlab="Sample Count")
hist(sub_eel$TOTAL,col="slateblue",main=paste("American Eel Counts"),xlab="Sample Count")
hist(sub_bay$TOTAL,col="slateblue",main=paste("Bay Anchovy Counts"),xlab="Sample Count")
hist(sub_winter$TOTAL,col="slateblue",main=paste("Winter Flounder Counts"),xlab="Sample Count")
dev.off()

#Kruskal Wallis tests
kruskal.test(TOTAL ~ STATION.x, data = sub_flounder)
kruskal.test(TOTAL ~ TEMPERATURE, data = sub_flounder)
kruskal.test(TOTAL ~ Year, data = sub_flounder)


kruskal.test(TOTAL ~ STATION.x, data = sub_menhaden)
kruskal.test(TOTAL ~ TEMPERATURE, data = sub_menhaden)
kruskal.test(TOTAL ~ Year, data = sub_menhaden)


kruskal.test(TOTAL ~ STATION.x, data = sub_croaker)
kruskal.test(TOTAL ~ TEMPERATURE, data = sub_croaker)
kruskal.test(TOTAL ~ Year, data = sub_croaker)


kruskal.test(TOTAL ~ STATION.x, data = sub_eel)
kruskal.test(TOTAL ~ TEMPERATURE, data = sub_eel)
kruskal.test(TOTAL ~ Year, data = sub_eel)


kruskal.test(TOTAL ~ STATION.x, data = sub_bay)
kruskal.test(TOTAL ~ TEMPERATURE, data = sub_bay)
kruskal.test(TOTAL ~ Year, data = sub_bay)


kruskal.test(TOTAL ~ TEMPERATURE, data = sub_winter)
kruskal.test(TOTAL ~ Year, data = sub_winter)


#Pairwise wilcox tests
pairwise.wilcox.test(sub_flounder$TOTAL, sub_flounder$STATION.x,
                     p.adjust.method = "BH")
pairwise.wilcox.test(sub_flounder$TOTAL, sub_flounder$TEMPERATURE,
                     p.adjust.method = "BH")
pairwise.wilcox.test(sub_flounder$TOTAL, sub_flounder$Year,
                     p.adjust.method = "BH")


pairwise.wilcox.test(sub_menhaden$TOTAL, sub_menhaden$STATION.x,
                     p.adjust.method = "BH")
pairwise.wilcox.test(sub_menhaden$TOTAL, sub_menhaden$TEMPERATURE,
                     p.adjust.method = "BH")
pairwise.wilcox.test(sub_menhaden$TOTAL, sub_menhaden$Year,
                     p.adjust.method = "BH")


pairwise.wilcox.test(sub_croaker$TOTAL, sub_croaker$STATION.x,
                     p.adjust.method = "BH")
pairwise.wilcox.test(sub_croaker$TOTAL, sub_croaker$TEMPERATURE,
                     p.adjust.method = "BH")
pairwise.wilcox.test(sub_croaker$TOTAL, sub_croaker$Year,
                     p.adjust.method = "BH")


pairwise.wilcox.test(sub_eel$TOTAL, sub_eel$STATION.x,
                     p.adjust.method = "BH")
pairwise.wilcox.test(sub_eel$TOTAL, sub_eel$TEMPERATURE,
                     p.adjust.method = "BH")
pairwise.wilcox.test(sub_eel$TOTAL, sub_eel$Year,
                     p.adjust.method = "BH")


pairwise.wilcox.test(sub_bay$TOTAL, sub_bay$STATION.x,
                     p.adjust.method = "BH")
pairwise.wilcox.test(sub_bay$TOTAL, sub_bay$TEMPERATURE,
                     p.adjust.method = "BH")
pairwise.wilcox.test(sub_bay$TOTAL, sub_bay$Year,
                     p.adjust.method = "BH")


pairwise.wilcox.test(sub_winter$TOTAL, sub_winter$TEMPERATURE,
                     p.adjust.method = "BH")
pairwise.wilcox.test(sub_winter$TOTAL, sub_winter$Year,
                     p.adjust.method = "BH")






