# 30 Day Chart Challenge
# https://github.com/30DayChartChallenge/Edition2024

library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(forcats)

# Day 3 "waffle ----
# how much simple sugar (mono/disaccarides) do Belgians eat per day as a % of their energy intake
# https://www.cambridge.org/core/journals/british-journal-of-nutrition/article/energy-and-macronutrient-intakes-in-belgium-results-from-the-first-national-food-consumption-survey/841E8D7157D0E09A3BCA2FA1494C37E3
library(waffle)
sugar <- fread("X:/Datasets/30DayChartChallenge/Day03_BelgianSugar.csv",header=T)

#convert to waffle chart dataset
sugar$non_accounted_for <- 50 - sugar$SimpleSugar_cubes
sugar_waffle <- data.frame(GROUP=rep(sugar$Group,2),
                           AGE=rep(sugar$Age,2),
                           VALUE=c(sugar$SimpleSugar_cubes,sugar$non_accounted_for),
                           TYPE=rep(c("sugar","other"),each=15))
# convert to rounded squares
sugar_waffle$squares <- round(sugar_waffle$VALUE)
head(sugar_waffle)

ggplot(data=sugar_waffle[sugar_waffle$GROUP=="Men" & sugar_waffle$AGE == "AllAges",], 
       aes(fill=TYPE,values=VALUE))+
  geom_waffle(n_rows=6,size=0.33)+
  theme_void()

mw_sugar <- sugar_waffle[sugar_waffle$GROUP %in% c("Men","Women"),]
mw_sugar %>% group_by(GROUP)


# Day 5 "diverging" ----
# https://academic.oup.com/evolut/article/77/4/971/7023955?login=true

dogs <- fread("X:/Datasets/30DayChartChallenge/Day05_DomesticDog.csv",header=T)

y_offset <- 2
ggplot(dogs,aes(x=-Point_mya,y=Y+y_offset))+
  geom_ribbon(aes(ymin=0,ymax=Y+y_offset),
              fill="grey")+
  geom_segment(aes(xend=-NextPoint_x,yend=NextPoint_y+y_offset),
               color="red",size=1)+
  geom_point(size=0.8)+
  ylim(0,16)+
  theme_bw()+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid=element_blank(),panel.border = element_blank())+
  labs(y="",x="")+
  scale_x_continuous(breaks=c(-14,-12,-10,-8,-6,-4,-2,0),limits=c(-12.5,0))


# Day 7 - "hazard" ----
# https://www.hse.gov.uk/statistics/tables/index.htm

accidents <- read_xlsx("X:/Datasets/30DayChartChallenge/Day07_ridkind.xlsx",
                   sheet="Table 2",skip=7)
colnames(accidents) <- c("Year","Industry_section","Industry",
                     "Accident_kind","Total_number","Number_specific_injuries",
                     "Number_week",
                     "Percent_total_nonfatal","Percent_total_specific",
                     "Percent_total_week")

# Years are financial : 2014/15 is 1st April 2014 to 31st March 2015
# covid lockdown began in year 2020/2021
accidents$Year_rx <- substr(accidents$Year,0,7)
accidents$Year_Start <- as.numeric(substr(accidents$Year,0,4))
# slips <- slips[slips$Year_Start > 2016,]
slips <- subset(accidents,accidents$Accident_kind=="Slips, trips or falls on same level")
all_acc <- subset(accidents,accidents$Accident_kind=="All accident kinds")


#plot overall level of nonfatal accidents
ggplot(data=accidents[accidents$Industry_section=="A-U" & accidents$Accident_kind == "All accident kinds",])+
  geom_col(aes(x=Year_Start,y=Total_number),fill="red")+
  theme_bw()

# plot nonfatal accidents by type
ggplot(data=accidents[accidents$Industry_section=="A-U" & accidents$Accident_kind != "All accident kinds",])+
  geom_line(aes(x=Year_Start,y=Total_number,color=Accident_kind))+
  theme_bw()


# plot overall slips over time
ggplot(data=slips[slips$Industry_section != "A-U",],
       aes(x=Year_Start,y=Total_number))+
  geom_col()

# plot accidents by accident type
unique(accidents$Accident_kind)
temp_df <- accidents[accidents$Industry_section != "A-U" &
                       accidents$Accident_kind == unique(accidents$Accident_kind)[17],]
print(unique(temp_df$Accident_kind))
temp_df_plot <- aggregate(temp_df$Total_number,by=list(temp_df$Year_Start),FUN=sum)
ggplot(data=temp_df_plot,
       aes(x=Group.1,y=x))+
  geom_col()+
  geom_col(data = temp_df_plot[temp_df_plot$Group.1 == 2020,],
           aes(x=2020,y=x),fill="red")+
  theme_void()+
  labs(title=unique(temp_df$Accident_kind))

for(i in unique(accidents$Accident_kind)){
  yr_2020 <- accidents[accidents$Year_Start==2020 & accidents$Industry_section == "A-U" &
              accidents$Accident_kind==i,]$Total_number 
  yr_2019 <- accidents[accidents$Year_Start==2019 & accidents$Industry_section == "A-U" &
                accidents$Accident_kind==i,]$Total_number 
  yr_diff <- yr_2020/yr_2019
  print(paste(i,round(yr_diff,3)))
  # print(paste("Total in 2 years",sum(yr_2019,yr_2020)))
}

#plot accidents by industry
unique(accidents$Industry)
temp_df <- accidents[accidents$Accident_kind == "All accident kinds" &
                       accidents$Industry_section == unique(accidents$Industry_section)[15],]
print(unique(temp_df$Industry_section))
temp_df_plot <- aggregate(temp_df$Total_number,by=list(temp_df$Year_Start),FUN=sum)
ggplot(data=temp_df_plot,
       aes(x=Group.1,y=x))+
  geom_col()+
  geom_col(data = temp_df_plot[temp_df_plot$Group.1 == 2020,],
           aes(x=2020,y=x),fill="red")+
  theme_void()+
  labs(title=unique(temp_df$Industry))

for(i in unique(accidents$Industry_section)){
  yr_2020 <- accidents[accidents$Year_Start==2020 & accidents$Accident_kind == "All accident kinds" &
                         accidents$Industry_section==i,]$Total_number 
  yr_2019 <- accidents[accidents$Year_Start==2019 & accidents$Accident_kind == "All accident kinds" &
                         accidents$Industry_section==i,]$Total_number 
  yr_diff <- yr_2020/yr_2019
  print(paste(i,round(yr_diff,3)))
  # print(paste("Total in 2 years",sum(yr_2019,yr_2020)))
}



# Industry Codes
# A	Agriculture, forestry and fishing [Note 7]
# B	Mining and quarrying [Note 3]
# C	Manufacturing
# D	Electricity, gas, steam and air conditioning supply
# E	Water supply; sewerage, waste management and remediation activities
# F	Construction
# G	Wholesale and retail trade; repair of motor vehicles and motorcycles
# H	Transportation and storage [Note 2]
# I	Accommodation and food service activities
# J-N	Information and communication; financial and insurance activities; real estate activities; professional, scientific and technical activities; administrative and support service activities
# O	Public administration and defence; compulsory social security
# P	Education
# Q	Human health and social work activities
# R-U	Arts, entertainment and recreation; other service activities; activities of households as employers; undifferentiated goods-and services-producing activities of households for own use; activities of extraterritorial organisations and bodies



# Day 6 - OECD Data ----
# https://data-explorer.oecd.org/vis?df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_FISH_PROD%40DF_FISH_LAND&df[ag]=OECD.TAD.ARP&df[vs]=1.0&pd=2010%2C&dq=.A.LAND_MARINE._T.T&ly[rw]=REF_AREA&ly[cl]=TIME_PERIOD&to[TIME_PERIOD]=false&vw=tb
# Marine fish landings by country, attempt for map

library(giscoR)
library(countrycode)
library(sf)

# fishing data
marine <- fread("X:/Datasets/30DayChartChallenge/Day06_OECD_MarineLandings.csv",header=T)
marine$country <- marine$`Reference area`
marine <- marine[,c("country","REF_AREA","TIME_PERIOD","OBS_VALUE")]
colnames(marine)[4] <- "Marine_Tonnes"

inland  <- fread("X:/Datasets/30DayChartChallenge/Day06_OECD_InlandFisheries.csv",header=T)
inland$country <- inland$`Reference area`
inland <- inland[,c("country","REF_AREA","TIME_PERIOD","OBS_VALUE")]
colnames(inland)[4] <- "Inland_Tonnes"


world <- gisco_get_countries()
world$region <- countrycode(world$ISO3_CODE,
                            origin = "iso3c",
                            destination = "un.regionsub.name")
europe <- subset(world,region %in% c(
  "Southern Europe","Western Europe","Eastern Europe","Northern Europe",
  "Northern Africa","Western Asia"
))

# do some renaming
europe[europe$NAME_ENGL=="Turkey",]$NAME_ENGL <- "Türkiye"

# add fishery data
intersect(europe$NAME_ENGL,marine$country)
intersect(europe$ISO3_CODE,marine$REF_AREA)
europe <- merge(europe,
                marine[marine$TIME_PERIOD==2019,],
                all.x=T,by.x="ISO3_CODE",by.y="REF_AREA")
europe <- merge(europe,
                inland[inland$TIME_PERIOD==2019,],
                all.x=T,by.x="ISO3_CODE",by.y="REF_AREA")



# calculate ratio of inland to marine and vice versa
europe$inland_percent <- europe$Inland_Tonnes/(europe$Inland_Tonnes+europe$Marine_Tonnes)
europe$marine_percent <- europe$Marine_Tonnes/(europe$Inland_Tonnes+europe$Marine_Tonnes)

# tag for no data to change chart colour

ggplot(europe)+geom_sf(aes(fill=inland_percent), color="#d9c4a0")+
  xlim(-30,30)+ylim(-35,-72)+
  scale_fill_distiller(palette=3)+theme_void()+
  theme(panel.background = element_rect(fill = '#fcf5e5'))

median(europe$inland_percent,na.rm=T)
median(europe$Inland_Tonnes,na.rm=T)


# Day 8 - "circular" ----
# Persons killed in road traffic accidents per month in Germany, France, Netherlands, Spain, UK
# https://www.destatis.de/EN/Themes/Society-Environment/Traffic-Accidents/Tables/persons-killed-road-traffic.html

car_deaths <- fread("X:/Datasets/30DayChartChallenge/Day08_CarDeaths.csv",header=T)
car_deaths$Month <- factor(car_deaths$Month,
                           levels=c("January","February","March","April","May","June",
                                    "July","August","September","October","November","December"))
car_deaths$Month_num <- as.numeric(car_deaths$Month)

# car_deaths$average_scaled <- rowMeans(
#   car_deaths[,c("scaled2020","scaled2021","scaled2022","scaled2023")],
#   na.rm=T
# )

# need to use the below function so that we can connect the lines manually
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  # https://stackoverflow.com/questions/42562128/ggplot2-connecting-points-in-polar-coordinates-with-a-straight-line-2
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}
# in this version of R, geom_polygon is the only way of connecting up lines
# on a polar plot, but it does not behave as you would hope!
# it makes some weird circle going back the other way across the plot 
# see below for example
fake_df <- data.frame(day=c(1,2,3,4,5,6,7),value=c(2,2,3,4,2,2,4))
ggplot(fake_df,aes(day,value))+  ylim(0,5)+
  coord_polar()+
  geom_polygon(fill=NA,color="red")


ggplot(data=car_deaths[car_deaths$Country != "All Countries",],
       aes(x=Month,y=scaled2020,color=Country,group=Country,fill=Country))+
  ylim(0,0.5)+
  geom_line()+
  theme_bw()+
  coord_radar()+theme(legend.position="None")


ggplot(data=car_deaths[car_deaths$Country == "All Countries",],
       aes(x=Month,y=average_scaled,fill=Country,group=Country,color=Country))+
  geom_col()+
  coord_polar()+theme_bw()+theme(legend.position="None")+
  ylim(0,0.5)


# Day 9 - "major/minor" ----
# energy production by category and subtype in Germany in 2023
# https://energy-charts.info/charts/energy/chart.htm?l=en&c=DE&interval=month&source=total&month=-1&year=2023&legendItems=0011111111101111110
# Frauenhofer Institute for Solar Energy Systems ISE
energy <- read_xlsx("X:/Datasets/30DayChartChallenge/Day09_EnergyProduction.xlsx",sheet=1)
energy$Month <- factor(energy$Month,
                           levels=c("January","February","March","April","May","June",
                                    "July","August","September","October","November","December"))

# stack in specific order
energy$SourceName <- paste(energy$SourceCategory,energy$Source)
# get average values for ordering
aggregate(energy,by=list(energy$SourceName),FUN=mean)
unique(energy$SourceName)
# fuck it just do it manually, its not worth the effort
energy_names <- c("Nuclear Nuclear",
                  "Renewable Wind", "Renewable Solar","Renewable Biomass",
                  "Renewable Hydro","Renewable Waste","Renewable Geothermal",
                  "NonRenewable Coal","NonRenewable Gas","NonRenewable Waste", 
                  "NonRenewable Oil",
                  "Other Other")
# energy_names <- c("Other Other","Nuclear Nuclear",
#                   "NonRenewable Coal","NonRenewable Gas","NonRenewable Waste", 
#                   "NonRenewable Oil",
#                   "Renewable Wind", "Renewable Solar","Renewable Biomass",
#                   "Renewable Hydro","Renewable Waste","Renewable Geothermal")


energy$SourceName <- factor(energy$SourceName,
                            levels=rev(energy_names))

ggplot(data=energy,aes(x=Month,y=Production_GWh,
                       color=SourceName,group=SourceName,fill=SourceName))+
  geom_area(position="stack",fill=NA,size=1)+
  theme_bw()

# aggregate values for side bars
plot(energy[which(energy$Source=="Coal"),]$Production_GWh)

# Day 10 - "physical" ----
# Apartment prices in Paris by qm per arondissement 
library(giscoR)
# https://www.french-property.com/news/french_property_market/paris_prices_2020
# orig: French Chamber of Notaries

paris <- fread("X:/Datasets/30DayChartChallenge/Day10_ParisPrices.csv",header=T)

paris_map <- gisco_get_nuts(country="FRA",nuts_id="FR101")
ggplot(paris_map)+geom_sf()


# Day 11 "mobile friendly" ----
# market share of mobile phone OS over time, ridgeline?
library(ggridges)
library(lubridate)

os_list <- read.csv("X:/Datasets/30DayChartChallenge/Day11_mobile_friendly/os_names.csv",header=F)
os_list <- unique(os_list)

# data storage dataframe
mobile_df <- data.frame(
  "Date" = as.character(),
  "OS" = as.character(),
  "MarketShare" = as.numeric()
)
os_prefix <- "X:/Datasets/30DayChartChallenge/Day11_mobile_friendly/os_combined-eu-monthly-"

# read the data in
for(yr in 2009:2023){
  year_text <- paste(yr,"01-",yr,"12",sep="")
  this_year <- read.csv(paste(os_prefix,year_text,".csv",sep=""),header=T)
  this_year_long <- melt(this_year,id.vars="Date",variable.name="OS")
  colnames(this_year_long) <- c("Date","OS","MarketShare")
  mobile_df <- rbind(mobile_df,this_year_long)
}

 #give category supernames
mobile_df$category <- case_when(
  mobile_df$OS %in% c("Android","Samsung") ~ "Android",
  mobile_df$OS %in% c("iOS","OS.X") ~ "Apple",
  mobile_df$OS %in% c("SymbianOS") ~ "Symbian",
  mobile_df$OS %in% c("BlackBerry.OS") ~ "Blackberry",
  mobile_df$OS %in% c("Windows") ~ "Windows",
  mobile_df$OS %in% c("Sony.Ericsson") ~ "Sony Ericsson",
  mobile_df$OS %in% c("bada") ~ "Samsung",
  mobile_df$OS %in% c("Linux","LiMo","webOS","MeeGo") ~ "Linux",
  mobile_df$OS %in% c("Playstation") ~ "Playstation",
  mobile_df$OS %in% c("LG") ~ "LG",
  mobile_df$OS %in% c("Nokia.Unknown","Maemo.5","Series.40") ~ "Nokia",
  mobile_df$OS %in% c("Unknown","Other","JAVA") ~ "Other",
  mobile_df$OS %in% c("Nintendo","Nintendo.3DS") ~ "Nintendo"
)

overview <- aggregate(mobile_df$MarketShare,by=list(mobile_df$TIMEDATE,mobile_df$category),FUN=sum)
peak_share <- aggregate(mobile_df$MarketShare,by=list(mobile_df$category),FUN=max)
peak_share[order(-peak_share$x),]
find_max <- function(df,category_to_match){
  df <- subset(df,category==category_to_match)
  df <- df[order(-df$MarketShare),]
  df[1,]
}
find_max(mobile_full,"Nokia")


#convert to real date data
mobile_df$TIMEDATE <- as_date(mobile_df$Date,format="%Y-%m")
mobile_df$Year <- year(mobile_df$TIMEDATE)

# make a dataframe with a point for every value
mobile_full <- data.frame(DATE = rep(unique(mobile_df$TIMEDATE),13),
                          category = rep(unique(mobile_df$category),each=180))
mobile_full <- merge(mobile_full,overview,by.x=c("DATE","category"),by.y=c("Group.1","Group.2"),
                     all.x=T)
mobile_full <- mobile_full[order(mobile_full$category,mobile_full$DATE),]
colnames(mobile_full)[3] <- "MarketShare"

# scale values to their respective maxima
scale_01 <- function(x){x/max(x,na.rm=T)}
mobile_scaled <- mobile_full %>% group_by(category) %>% 
  mutate(scaledShare = scale_01(MarketShare))

# generate y offset
mobile_scaled$y_offset <- case_when(
  (mobile_scaled$category == "Android") ~ 1300,
  (mobile_scaled$category == "Apple") ~ 1200,
  (mobile_scaled$category == "Symbian") ~ 1100,
  (mobile_scaled$category == "Blackberry") ~ 1000,
  
  (mobile_scaled$category == "Sony Ericsson") ~ 800,
  (mobile_scaled$category == "Windows") ~ 700,
  (mobile_scaled$category == "Nokia") ~ 600,
  (mobile_scaled$category == "Playstation") ~ 500,
  (mobile_scaled$category == "Samsung") ~ 400,
  (mobile_scaled$category == "Nintendo") ~ 300,
  (mobile_scaled$category == "Linux") ~ 200,
  (mobile_scaled$category == "LG") ~ 100,
  (mobile_scaled$category == "Other") ~ 0,
)

mobile_scaled$bigsmall <- ifelse(
  mobile_scaled$category %in% c("Android","Apple","Symbian","Blackberry"),"big","small"
)

ggplot(data=mobile_scaled[mobile_scaled$bigsmall=="big",])+
  geom_ridgeline(aes(x=DATE,height=MarketShare,group=category,fill=category,
                     y=y_offset/3),
                 min_height=0.5)+
  theme_bw()+ theme(legend.position="None")+
  # geom_text(aes(x=as.Date("2010-01-01"),y=y_offset/10,label=category))+
  scale_x_date(breaks=as.IDate(paste(2009:2023,"-06-01",sep="")))

ggplot(data=mobile_scaled[mobile_scaled$bigsmall=="small" & mobile_scaled$category != "Other",])+
  geom_ridgeline(aes(x=DATE,height=MarketShare,group=category,fill=category,
                     y=y_offset/30),
                 min_height=0.05)+
  theme_bw()+ theme(legend.position="None")+
  # geom_text(aes(x=as.Date("2010-01-01"),y=y_offset/10,label=category))+
  scale_x_date(breaks=as.IDate(paste(2009:2023,"-06-01",sep="")))


ggplot(data=mobile_scaled[mobile_scaled$category != "Other",])+
  geom_ridgeline(aes(x=DATE,height=scaledShare,group=category,fill=category,
                     y=y_offset/150),
                 min_height=0)+
  theme_bw()+ theme(legend.position="None")+
  geom_text(aes(x=as.Date("2010-01-01"),y=y_offset/150,label=category))+
  scale_x_date(breaks=as.IDate(paste(2009:2023,"-06-01",sep="")))
# sorry but these just look relentlessly terrible


peak_yr <- mobile_full[year(mobile_full$DATE)==2019 &
                         !mobile_full$category %in% c("Other","Nintendo"),]
peak_share_yr <- aggregate(peak_yr$MarketShare,
                           by=list(peak_yr$category),FUN=mean)
peak_share_yr[order(-peak_share_yr$x),]
                 

# Day 13 "family" ----
# uk number of children and average age of mothers
# https://jokergoo.github.io/spiralize_vignettes/spiralize.html
library(spiralize)
babies <- read_xlsx("X:/Datasets/30DayChartChallenge/Day13_family/Day13_BirthStatistics.xlsx",sheet=1)

# get numbers of years for the plot
head(babies)
babies2 <- subset(babies, Year %in% seq(1940,2020,by=5))

# units
year_diff <- 2020-1940
unit_size <- 360/year_diff * 1 # multiplied by scaling factor
babies2$Year_X <- seq(0,1,length.out=length(babies2$Year))
circ_k <- 0.15 # circle_scaler

#generate plot
{
spiral_initialize(start=90,end=360*2+90,
                  clockwise=T,scale_by = "curve_length") # make base graphic
spiral_track(height=0.1) # add line
# actual kids per family
spiral_points(x=babies2$Year_X, y=0.5,
              size=unit(babies2$ChildrenFamily*circ_k/2,"npc"),
              gp=gpar(col="red",fill="red"))
# replacement circle
spiral_points(x=babies2$Year_X, y=0.5,
              size=unit(circ_k,"npc"),gp=gpar(col="black",lty=1))
# year text
spiral_text(x=babies2$Year_X, y=0.5,
            text=babies2$Year)

}

ggplot(data=babies2,aes(x=Year,y=MotherAge))+
  geom_point()+geom_line()+ylim(25,32)


# Day 14 "heatmap" ----
# in the file "WinterGolds_transform.R"


# Day 15 "historical" ----
founding <- fread("X:/Datasets/30DayChartChallenge/New folder/oldest-countries-2024.csv",header=T)
regions <- fread("X:/Datasets/30DayChartChallenge/New folder/regions.csv",header=T)
founding <- merge(founding,regions,by.x="country",by.y="Country",all.x=T)
# have manually fixed the regions file
# and have manually fixed the sovereignty years
# note that sovereignty != existence, e.g. Vatican City has sov since 1929, but existed since 1279

founding$`Sub-region` <- factor(founding$`Sub-region`,levels=c(
  "Middle East","North Africa","South Asia","East Asia","South East Asia","Central Asia",
  "Southern Europe","Western Europe","Central and Eastern Europe","Nordic countries",
  "East Africa","West Africa","Southern Africa","Central Africa",
  "North America","Caribbean","South America","Central America",
  "Australia and New Zealand","Pacific Islands"
))


ggplot(data=founding,
       aes(x=year,y=`Sub-region`))+
  theme_bw()+
  theme(legend.position = "None")+
  geom_segment(aes(xend=2024,yend=`Sub-region`))+
  xlim(-2600,2024)+
  geom_point(position=position_jitter(height=0.1),
             aes(color=Region))

sov_agg <- aggregate(founding$country, 
                     by=list(founding$Region,founding$`Sub-region`,founding$year),
                     FUN=length)
colnames(sov_agg) <- c("Region","Subregion","year","numco")


library(extrafont); font_import()

sov_agg$year_x <- sov_agg$year
sov_agg[sov_agg$year<0,]$year_x<- -50
sov_agg[sov_agg$year<0,]$numco<- 0

ggplot(data=sov_agg,
       aes(x=year_x,y=fct_rev(Subregion)))+
  geom_segment(aes(xend=2024,yend=Subregion,color=Region))+
  xlim(-50,2024)+
  geom_point(aes(color=Region,size=numco,alpha=0.1))+
  theme(legend.position="None",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_text(family="Verdana",face="italic",color="grey10",size=7),
        axis.text.x=element_text(family="Verdana",face="plain",color="grey10",size=8),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color="grey90"),
        panel.background = element_blank())+
  scale_color_viridis_d(option="H")+
  scale_y_discrete(position="right")



.




# Day 16 "weather" ----
lerwick <- fread("X:/Datasets/30DayChartChallenge/Day16_weather/lerwickdata.txt",skip=6,header=T,fill=T)
# year month maxtemp(C) mintemp(C) frost(days) rain(mm) sun(hours)
head(lerwick)
# remove asterisks and hashes
lerwick$rain <- as.numeric(gsub("[^0-9.-]", "", lerwick$rain)) # wow thats useful
lerwick$sun <- as.numeric(gsub("[^0-9.-]", "", lerwick$sun))
lerwick$yyyymm <- as_date(paste(lerwick$yyyy,lerwick$mm,sep="-"),format="%Y-%m")

lerwick$decade <- round(lerwick$yyyy / 10)*10


# get aggregate values
lw_agg <- aggregate(lerwick[,c("tmax","tmin")],by=list(lerwick$mm,lerwick$decade),FUN=mean)
lw_agg$diff <- lw_agg$tmax - lw_agg$tmin

ggplot(data=lerwick)+ # both min and max
  geom_line(aes(x=as.factor(mm),y=tmin,color=as.factor(decade),group=yyyy),alpha=0.5)+
  geom_line(aes(x=as.factor(mm),y=tmax,color=as.factor(decade),group=yyyy),alpha=0.5)+
  ylim(-5,20)+theme_bw()+
  scale_color_viridis_d(option="F")+
  theme(legend.position="None")

ggplot(data=lerwick)+ # temperature difference
  geom_line(aes(x=as.factor(mm),y=tmax-tmin,color=as.factor(decade),group=yyyy),alpha=0.5)+
  ylim(2,6)+theme_bw()+
  scale_color_viridis_d(option="F")+
  theme(legend.position="None")


a <- aggregate(lerwick[,c("tmax","tmin")],by=list(lerwick$decade),FUN=mean)
ggplot(data=a,aes(x=1:10,y=1,color=as.factor(Group.1)))+
  geom_point(size=10)+
  scale_color_viridis_d(option="F")+theme_void()+
  theme(legend.position="None")


# Day 17 "network" ----
# Netflix horror movie combinations
# https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html
library(purrr)
library(circlize)
netflix <- fread("X:/Datasets/30DayChartChallenge/Day17_network/netflix_titles.csv",header=)
netflix <- netflix[,c("show_id","type","title","director","release_year","listed_in")]
netflix <- subset(netflix,type=="Movie")
head(netflix)

# ignore some category labels
netflix$listed_in2 <- netflix$listed_in
# international
netflix$listed_in2 <- str_replace(netflix$listed_in2,"International Movies, ", "")
# independent
netflix$listed_in2 <- str_replace(netflix$listed_in2,"Independent Movies, ", "")
# "movies"
netflix$listed_in2 <- str_replace(netflix$listed_in2," Movies", "")
netflix$listed_in2 <- str_replace(netflix$listed_in2," Movies", "") # for some reason you need to do it twice?

# get genres
length(unique(netflix$listed_in))
genres <- unique(netflix$listed_in2)
genres <- unique(c(
  as.character(map(strsplit(netflix$listed_in2, split = ", "), 1)),
  as.character(map(strsplit(netflix$listed_in2, split = ", "), 2)),
  as.character(map(strsplit(netflix$listed_in2, split = ", "), 3))
)) # get actual list of all genres
#add to df
netflix$genres_0 <- as.character(map(strsplit(netflix$listed_in2, split = ", "), 1))
netflix$genres_1 <- as.character(map(strsplit(netflix$listed_in2, split = ", "), 2))
netflix$genres_2 <- as.character(map(strsplit(netflix$listed_in2, split = ", "), 3))

# identify horror films
netflix$is_horror <- ifelse(
  (netflix$genres_0 == "Horror") | (netflix$genres_1 == "Horror") |
    (netflix$genres_2 == "Horror"), 1,0
)

# remove nulls
netflix <- subset(netflix, genres_0 != "NULL")


# rearrange to make horror always first
for(net_row in 1:dim(netflix)[1]){
  if(netflix[net_row,]$is_horror){
    g0 <- netflix[net_row,]$genres_0; g1 <- netflix[net_row,]$genres_1
    if(g1 == "Horror"){ # swap g0 and g1
      netflix[net_row,]$genres_0 <- g1
      netflix[net_row,]$genres_1 <- g0
    }
  }
}


# aggregate
netflix[which(netflix$genres_1 %in% c("Independent","International")),]$genres_1 <-
  "NULL"
net_agg <- netflix %>% dplyr::count(genres_0, genres_1, sort = TRUE)
# net_agg_solo <- subset(net_agg, genres_1 == "NULL")
net_agg <- subset(net_agg, genres_1 != "NULL")


# plot the chord diagram
# set colours
grid.col = c(`Horror` = "red3",`Comedies`="grey",`Children & Family`="grey",
             `Dramas`="grey",`Documentaries`="grey",`Classic`="grey",
             `Action & Adventure`="grey",`Sci-Fi & Fantasy`="grey",`Anime Features`="grey",
             `Music & Musicals`="grey",`Cult`="grey",`LGBTQ`="grey",`Romantic`="grey",
             `Thrillers`="grey",`Sports`="grey",`Faith & Spirituality`="grey",
             `Stand-Up Comedy`="grey")

col_order <- c("Horror", "Anime Features","Children & Family","Music & Musicals",
               "Thrillers","Comedies","Action & Adventure","Sci-Fi & Fantasy",
               "Cult","Dramas","LGBTQ","Romantic","Documentaries","Classic","Sports",
               "Faith & Spirituality","Stand-Up Comedy")

circos.par(start.degree=100)
chordDiagram(net_agg, grid.col=grid.col,order=col_order,scale=T)
circos.clear()


# Day 18 Asian Development Bank ----
# https://data.adb.org/dataset/trade-balance-asia-and-pacific-asian-development-outlook

# trade balance
asia <- fread("X:/Datasets/30DayChartChallenge/Day18_ADB/ADO2020 - Trade Balance.csv",
              header=T,fill=T)
# remove region rows
asia <- subset(asia,`Country Code` != "")
#remove forecasts (2015 to 2019 only)
asia <- subset(asia, Year %in% c(2015,2016,2017,2018,2019))

# remove niue cos errrr new zealand
asia <- subset(asia, RegionalMember != "Niue")

# gross domestic product
gdp_2019 <- fread("X:/Datasets/30DayChartChallenge/Day18_ADB/basic-statistics-2020.csv",
                  header=T,fill=T)[,c("Regional Economy","Statistic","Year","Value")]
gdp_2018 <- fread("X:/Datasets/30DayChartChallenge/Day18_ADB/basic-statistics-2019.csv",
                  header=T,fill=T,sep=",")[,c("Regional Economy","Statistic","Year","Value")]
gdp_2017 <- fread("X:/Datasets/30DayChartChallenge/Day18_ADB/basic-statistics-2018.csv",
                  header=T,fill=T,sep=",")[,c("Regional Economy","Statistic","Year","Value")]
gdp_2016 <- fread("X:/Datasets/30DayChartChallenge/Day18_ADB/basic-statistics-2017.csv",
                  header=T,fill=T,sep=",")[,c("Regional Economy","Statistic","Year","Value")]
asia_gdps <- rbind(gdp_2019,gdp_2018,gdp_2017,gdp_2016)
asia_gdps <- subset(asia_gdps, `Regional Economy` != "Niue") # no Niue

#fix names
asia_gdps[which(asia_gdps$`Regional Economy` == "China, People's Republic of"),]$`Regional Economy` <- "People's Republic of China"
asia_gdps[which(asia_gdps$`Regional Economy` == "Micronesia, Federated States of"),]$`Regional Economy` <- "Federated States of Micronesia"
asia_gdps[which(asia_gdps$`Regional Economy` == "Korea, Republic of"),]$`Regional Economy` <- "Republic of Korea"


# subset to just GDP
asia_gdp <- asia_gdps[grep("Nominal Gross Domestic Product",asia_gdps$Statistic),]
asia_gdp$Value <- as.numeric(asia_gdp$Value)
# growth rate 
asia_growth <- asia_gdps[grep("Annual Growth Rate of ",asia_gdps$Statistic),]
asia_growth <- asia_growth[-grep("Person",asia_growth$Statistic),]


# will need to work backwards for gdp using annual growth rates
asia_gdp_extra <- data.frame("Economy"=character(),
                             "Statistic"=character(),
                             "Year"=numeric(), "Value"=numeric())
for(country in asia_gdp$`Regional Economy`){
  m_gdp_19 <- asia_gdp[asia_gdp$`Regional Economy` == country,]$Value
  e_gdp_18 <- m_gdp_19 / (1+as.numeric(subset(asia_growth, 
                                (`Regional Economy` == country) &
                                  (Year == 2018))$Value[1])/100)
  e_gdp_17 <- e_gdp_18 / (1+as.numeric(subset(asia_growth, 
                                              (`Regional Economy` == country) &
                                                (Year == 2017))$Value[1])/100)
  e_gdp_16 <- e_gdp_17 / (1+as.numeric(subset(asia_growth, 
                                              (`Regional Economy` == country) &
                                                (Year == 2016))$Value[1])/100)
  this_country <- data.frame("Economy"=rep(country,4),
                             "Statistic"="GDP",
                             "Year"=seq(2019,2016), 
                             "Value"=c(m_gdp_19,e_gdp_18,e_gdp_17,e_gdp_16))
  asia_gdp_extra <- rbind(asia_gdp_extra,this_country)
}


#merge
asia$Year <- as.numeric(asia$Year)
asia_trade <- merge(asia,asia_gdp_extra[,c("Economy","Value","Year")],
              by.x=c("RegionalMember","Year"),by.y=c("Economy","Year"),
              all.x=T)
asia_trade <- subset(asia_trade, Year != 2015)
#make trade into millions of USD
asia_trade$`Trade Balance` <- asia_trade$`Trade Balance` / 1000
# ignore the unit marker now
colnames(asia_trade)[7] <- "GDP"
colnames(asia_trade)[3] <- "Balance"


asia_trade$new_x <- asia_trade$GDP - (asia_trade$Balance * sqrt(2)/2) # neg right
asia_trade$new_y <- asia_trade$GDP + (asia_trade$Balance * sqrt(2)/2) # neg down

asia_trade$Bal_percent <- asia_trade$Balance / asia_trade$GDP

ggplot(asia_trade, aes(x=new_x,y=new_y,group=RegionalMember))+
  geom_abline(intercept=0,slope=1,linetype="dashed",color="red")+
  geom_point()+
  geom_line() + scale_x_log10() + scale_y_log10()+
  theme_bw()

# all that work with trig for nothing
ggplot(asia_trade, aes(y=GDP,x=Balance,group=RegionalMember,
                       color=Subregion))+
  geom_vline(xintercept=0,linetype="dashed",color="red")+
  geom_path(arrow=arrow(length=unit(0.1,"cm"),type="closed"))+
  # geom_point()+
  scale_y_log10()+theme_bw()

ggplot(asia_trade, aes(y=GDP,x=Bal_percent,group=RegionalMember,
                       color=Subregion))+
  geom_vline(xintercept=0,linetype="dashed",color="red")+
  geom_path(size=0.5)+
  # geom_path(arrow=arrow(length=unit(0.1,"cm"),type="open"),size=0.5)+
  # geom_point(data=asia_trade[asia_trade$Year != 2019,], 
  #            aes(y=GDP,x=Balance/GDP,group=RegionalMember,
  #                            color=Subregion))+
  geom_point()+
  scale_y_log10()+theme_bw()+
  scale_x_continuous(breaks=c(-1,-0.75,-0.5,-0.25,0,0.25,0.5),
                     limits=c(-1,0.5))+
  theme(legend.position = "None",
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())


aggregate(asia_trade[asia_trade$Year > 2017,]$Bal_percent,
          by=list(asia_trade[asia_trade$Year > 2017,]$Subregion),FUN=mean)



# Day 19 "dinosaur" ----
library(ggridges)
# https://www.kaggle.com/datasets/kjanjua/jurassic-park-the-exhaustive-dinosaur-dataset
# The Natural History Museum UK, via Kamran Janjua

dinos <- fread("X:/Datasets/30DayChartChallenge/Day19_dinosaur/data_dinosaur.csv",header=T)

{#make time variable more easily readable
dinos$Era <- (str_split_fixed(dinos$period," ",5)[,2]) # Jurassic, Cretaceous, Triassic
dinos$SubEra <- paste((str_split_fixed(dinos$period," ",5)[,1]),
                      (str_split_fixed(dinos$period," ",5)[,2]))
dinos$Years_Start <- str_split_fixed(str_split_fixed(dinos$period," ",5)[,3],"-",2)[,1]
dinos$Years_End <- str_split_fixed(str_split_fixed(dinos$period," ",5)[,3],"-",2)[,2]
dinos[which(dinos$Years_End==""),]$Years_End <- dinos[which(dinos$Years_End==""),]$Years_Start
dinos$Years_Start <- as.numeric(dinos$Years_Start)
dinos$Years_End <- as.numeric(dinos$Years_End)
}

# make length variable readable
dinos$total_length <- as.numeric(str_split_fixed(dinos$length,"m",2)[,1])

# change type to be more easily plotted
dinos$type_cat <- case_when(
  dinos$type %in% c("armoured dinosaur") ~ "Thyreophora",
  dinos$type %in% c("small theropod","large theropod") ~ "Theropoda",
  dinos$type %in% c("ceratopsian") ~ "Ceratopsia",
  dinos$type %in% c("sauropod") ~ "Sauropoda",
  dinos$type %in% c("euornithopod","small ornithiscian") ~ "Ornithopoda",
)

dinos$type_cat <- factor(dinos$type_cat,
                         levels=c("Sauropoda","Thyreophora","Theropoda","Ornithopoda","Ceratopsia"))

ggplot(dinos,aes(x=-Years_End,y=type_cat,size=total_length,color=type_cat))+
  geom_point(position=position_jitter(height=0.3),alpha=0.3)+
  theme_bw()


ggplot(data=dinos,aes(x=-Years_Start,y=type_cat))+
  geom_density_ridges(scale=2,aes(fill=type_cat))+
  geom_point(aes(x=-Years_End,y=as.numeric(type_cat)+0.5,
                 size=total_length,color=type_cat),
             position=position_jitter(height=0.3),alpha=0.3)+
  theme_bw()+
  scale_fill_manual(values=c(
    "Ceratopsia"="#b8336a",
    "Ornithopoda"="#c490d1",
    "Theropoda"="#acacde",
    "Thyreophora"="#abdafc",
    "Sauropoda"="#e5fcff"
  ))+
  scale_color_manual(values=c(
    "Ceratopsia"="#611937",
    "Ornithopoda"="#674d6e",
    "Theropoda"="#54546e",
    "Thyreophora"="#52697a",
    "Sauropoda"="#7e8b8c"
  ))
  

# make into how many dinos were alive per millions of year
living_dinos <- data.frame(mya=230:60, num_dinos=as.numeric(0),
                           num_cer=as.numeric(0),num_orn=as.numeric(0),num_sau=as.numeric(0),
                           num_the=as.numeric(0),num_thy=as.numeric(0))
for(mya in 230:60){
  these_dinos <- subset(dinos,Years_Start >= mya)
  these_dinos <- subset(these_dinos,Years_End <= mya)
  living_dinos[living_dinos$mya==mya,]$num_dinos <- dim(these_dinos)[1]
  #get numbers per type
  living_dinos[living_dinos$mya==mya,]$num_cer <- dim(subset(these_dinos,type_cat=="Ceratopsia"))[1]
  living_dinos[living_dinos$mya==mya,]$num_orn <- dim(subset(these_dinos,type_cat=="Ornithopoda"))[1]
  living_dinos[living_dinos$mya==mya,]$num_sau <- dim(subset(these_dinos,type_cat=="Sauropoda"))[1]
  living_dinos[living_dinos$mya==mya,]$num_the <- dim(subset(these_dinos,type_cat=="Theropoda"))[1]
  living_dinos[living_dinos$mya==mya,]$num_thy <- dim(subset(these_dinos,type_cat=="Thyreophora"))[1]
}

living_dinos_long <- data.frame(mya=rep(living_dinos$mya,times=5),
                                type=rep(c("Sauropoda","Thyreophora","Theropoda","Ornithopoda","Ceratopsia"),
                                         each=171),
                                number=c(living_dinos$num_sau,living_dinos$num_thy,living_dinos$num_the,
                                         living_dinos$num_orn,living_dinos$num_cer))
# fake this for geom_density_ridges
living_dinos_long2 <- data.frame(mya=as.numeric(),type=as.character(),number=as.numeric())
for(row in 1:dim(living_dinos_long)[1]){
  living_dinos_long2 <- rbind(living_dinos_long2,
                              living_dinos_long[rep(row,times=living_dinos_long[row,]$number),])
}
living_dinos_long2$type <- factor(living_dinos_long2$type,
                         levels=c("Sauropoda","Thyreophora","Theropoda","Ornithopoda","Ceratopsia"))


ggplot()+
  geom_rect(aes(xmin=-230,xmax=-201,ymin=0,ymax=5),fill="#FFC6D9",alpha=0.3)+
  geom_rect(aes(xmin=-201,xmax=-145,ymin=0,ymax=5),fill="#FFE1C6",alpha=0.3)+
  geom_rect(aes(xmin=-145,xmax=-065,ymin=0,ymax=5),fill="#FFF7AE",alpha=0.3)+
  geom_rect(aes(xmin=-065,xmax=-060,ymin=0,ymax=5),fill="#C9C5BA",alpha=0.3)+
  geom_density_ridges(data=living_dinos_long2,
                 aes(x=-mya,y=type,fill=type,height=..density..),
                 scale=0.9,stat="density",trim=F)+
  geom_point(data=dinos,
             aes(x=-Years_Start,y=as.numeric(type_cat)-0.2,
                 size=total_length,color=type_cat),
             position=position_jitter(height=0.1),alpha=0.6)+
  theme_ridges()+
  scale_fill_manual(values=c(
    "Ceratopsia"="#b8336a",
    "Ornithopoda"="#c490d1",
    "Theropoda"="#acacde",
    "Thyreophora"="#abdafc",
    "Sauropoda"="#e5fcff"
  ))+
  scale_color_manual(values=c(
    "Ceratopsia"="#701e40",
    "Ornithopoda"="#996fa3",
    "Theropoda"="#8585ab",
    "Thyreophora"="#7897ad",
    "Sauropoda"="#82acb0"
  ))+
  xlim(-230,-60)



# Day 20 "correlation" ----
# hours of sunshine vs precipitation in mm

sunshine <- fread("X:/Datasets/30DayChartChallenge/Day20_correlation/day20_sunshine.csv",header=T)
rainfall <- fread("X:/Datasets/30DayChartChallenge/Day20_correlation/day20_rainfall.csv",header=T)
temperature <- fread("X:/Datasets/30DayChartChallenge/Day20_correlation/day20_temperature.csv",header=T)
temperature <- temperature[seq(1,949,2),]

regions <- fread("X:/Datasets/30DayChartChallenge/Day20_correlation/regions2.txt",header=T,sep="\t")

sunshine$hours_sun <- as.numeric(sunshine$Year)
rainfall$rain_mm <- as.numeric(rainfall$Year)
temperature$mean_temp <- as.numeric(temperature$Year)

setdiff(sunshine$Country,regions$Country)
setdiff(rainfall$Country,regions$Country)

sun_rain <- merge(sunshine[,c("Country","City","hours_sun")],
                  rainfall[,c("Country","City","rain_mm")],
                  by=c("Country","City"))
sun_rain <- merge(sun_rain,
                  temperature[,c("Country","City","mean_temp")],
                  by=c("Country","City"))
sun_rain <- merge(sun_rain,regions,by.x="Country",by.y="Country",all.x=T)

ggplot(data=sun_rain,aes(x=hours_sun/365.25,y=rain_mm/365.25))+
  geom_smooth(method="glm")+
  geom_point(aes(color=mean_temp),size=2)+
  theme_bw()+
  scale_color_viridis_c(option="B",limits=c(-5,30))+
  scale_x_continuous(limits=c(2,11),breaks=c(2,4,6,8,10))+
  scale_y_continuous(limits=c(0,12),breaks=c(0,2,4,6,8,10,12))+
  theme(panel.grid.minor.y=element_blank())+
  geom_text(aes(label=City))


# Day 21 "green energy" ----
# inverted pyramid chart of who uses the most energy from green and non-green sources
# https://www.energyinst.org/statistical-review

energy_fuel <- read_xlsx("X:/Datasets/30DayChartChallenge/Day21_greenenergy/Statistical Review of World Energy Data.xlsx",sheet="Primary Energy - Cons by fuel",skip=2)
energy_capita <- read_xlsx("X:/Datasets/30DayChartChallenge/Day21_greenenergy/Statistical Review of World Energy Data.xlsx",sheet="Primary Energy - Cons Capita",skip=2)

# remove NA rows
energy_fuel <- na.omit(energy_fuel)
energy_capita <- na.omit(energy_capita)

# remove summary rows
energy_fuel <- energy_fuel[-grep("Total",energy_fuel$Exajoules),]
energy_fuel <- energy_fuel[-grep("Other",energy_fuel$Exajoules),]
energy_fuel <- energy_fuel[1:(dim(energy_fuel)[1]-3),]

energy_capita <- energy_capita[-grep("Total",energy_capita$`Gigajoule per capita`),]
energy_capita <- energy_capita[-grep("Other",energy_capita$`Gigajoule per capita`),]
energy_capita <- energy_capita[1:(dim(energy_capita)[1]-3),]

#merge into single data frame
setdiff(energy_capita$`Gigajoule per capita`,energy_fuel$Exajoules)
energy <- merge(energy_capita[,c("Gigajoule per capita","2022...59")],
                energy_fuel[,c(1,9:15)],
                by.x="Gigajoule per capita",by.y="Exajoules",all.y=T)
colnames(energy) <- c(
  "Country","PerCapita","Oil","NaturalGas","Coal","Nuclear","Hydro","OtherRenewable","Total"
)

# get values per green and non-green energy in percentage
# gigajoules is 10e9, exajoules is 10e18
energy$GreenEnergy <- rowSums(energy[,c("Hydro","OtherRenewable")])/energy$Total
energy$RedEnergy <- rowSums(energy[,c("Oil","NaturalGas","Coal")])/energy$Total
energy$BlueEnergy <- energy$Nuclear/energy$Total

# convert values to per capita in gigajoules
energy$GreenCapita <- energy$GreenEnergy*energy$PerCapita
energy$RedCapita <- energy$RedEnergy*energy$PerCapita
energy$BlueCapita <- energy$BlueEnergy*energy$PerCapita

# order by total energy use
energy <- merge(energy,regions,by="Country",all.x=T)
energy[which(energy$Country %in% c("South Africa","Morocco","Algeria")),]$Region <- 
  "Africa"
energy[which(energy$Country=="China Hong Kong SAR"),]$Region <- "Asia"
energy[which(energy$Country=="US"),]$Region <- "Americas"
energy[which(energy$Country=="Russian Federation"),]$Region <- "Europe"
energy[which(energy$Country=="Trinidad & Tobago"),]$Region <- "Americas"
table(energy$Region)
energy$Region <- factor(energy$Region,
                        levels=c("Africa","Americas","Asia","Europe",
                                 "Middle East and North Africa","Pacific"))

energy <- energy[order(energy$Region,energy$GreenCapita,decreasing=F),]
energy$Country <- factor(energy$Country,levels=energy$Country)

# make into long dataframe for a plot
energy_plot <- energy[,c("Country","GreenCapita","BlueCapita","RedCapita")]
energy_plot <- data.frame("Country"=rep(energy$Country,times=3),
                          "Type"=rep(c("Green","Blue","Red"),each=66),
                          "Consumption"=c(energy$GreenCapita,energy$BlueCapita,energy$RedCapita))

# mirrored versions
{
energy_plot_left <- subset(energy_plot,Type %in% c("Green","Blue"))
energy_plot_left$Consumption <- -energy_plot_left$Consumption
energy_plot_left[energy_plot_left$Type=="Blue",]$Consumption <- 
  energy_plot_left[energy_plot_left$Type=="Blue",]$Consumption/2
energy_plot_left$Type <- factor(energy_plot_left$Type,
                                levels=c("Red","Green","Blue"))

energy_plot_right <- subset(energy_plot,Type %in% c("Red","Blue"))
energy_plot_right[energy_plot_right$Type=="Blue",]$Consumption <- 
  energy_plot_right[energy_plot_right$Type=="Blue",]$Consumption/2
energy_plot_right$Type <- factor(energy_plot_right$Type,
                                levels=c("Red","Blue","Green"))
}

ggplot()+
  geom_col(data=energy_plot_left[energy_plot_left$Type=="Green",],
           aes(y=Country,x=Consumption,fill=Type))+
  geom_col(data=energy_plot_right[energy_plot_right$Type=="Red",],
           aes(y=Country,x=Consumption,fill=Type))+
  scale_fill_manual(values=c("Red"="red2","Blue"="skyblue","Green"="green2"))+
  theme_bw()


# Day 23 "tiles" ----
library(treemap)
# crufts winners by group and breed
crufts <- fread("X:/Datasets/30DayChartChallenge/Day23_tiles/CruftsWins.csv",header=T)

breed_wins <- as.data.frame(table(crufts$Breed))
breed_wins <- merge(breed_wins,crufts[,c("Breed","Group")],
                    by.x="Var1",by.y="Breed",all.x=T)
colnames(breed_wins) <- c("Breed","Wins","Group")

treemap(breed_wins,
        index=c("Group","Breed"),vSize="Wins",
        fontfamily.labels = "Verdana")


# Day 24 ILO Region for Africa ----
# foreign and native born working age population in west africa by gender
africa_immigrants <- fread("X:/Datasets/30DayChartChallenge/Day24_ILO_Africa/MST_FORP_SEX_CBR_NB_A-filtered-2024-04-23.csv",header=T)
africa <- fread("X:/Datasets/30DayChartChallenge/Day24_ILO_Africa/MST_XWAP_SEX_AGE_CBR_NB_A-filtered-2024-04-23.csv",header=T)

#reduce to just data of interest
africa_immigrants <- subset(africa_immigrants, classif1.label == "Country of birth: Total")
africa <- subset(africa, classif1.label=="Age (Aggregate bands): Total")


# get values of males and females by native or foreign born status per country
africa_agg <- aggregate(africa$obs_value,by=list(africa$ref_area.label,
                                                 africa$sex.label,
                                                 africa$classif2.label),
                        FUN=sum)
africa_agg <- subset(africa_agg, !Group.3=="Place of birth: Status unknown")
table(africa_agg$Group.1)

af_sex <- data.frame(Country=unique(africa_agg$Group.1),
                     Female_Total=as.numeric(NA),
                     Female_Native=as.numeric(NA),
                     Female_Foreign=as.numeric(NA),
                     Male_Total=as.numeric(NA),
                     Male_Native=as.numeric(NA),
                     Male_Foreign=as.numeric(NA))
for(cnt in 1:length(af_sex$Country)){
  thiscountry <- africa_agg[which(africa_agg$Group.1==af_sex[cnt,]$Country),]
  thisfemales <- thiscountry[which(thiscountry$Group.2 == "Sex: Female"),]
  thismales <- thiscountry[which(thiscountry$Group.2 == "Sex: Male"),]
  af_sex[cnt,]$Female_Total <- thisfemales[which(
      (thisfemales$Group.3 == "Place of birth: Total")),]$x
  af_sex[cnt,]$Female_Native <- thisfemales[which(
      (thisfemales$Group.3 == "Place of birth: Native-born")),]$x
  af_sex[cnt,]$Female_Foreign <- thisfemales[which(
      (thisfemales$Group.3 == "Place of birth: Foreign-born")),]$x
  af_sex[cnt,]$Male_Total <- thismales[which(
    (thismales$Group.3 == "Place of birth: Total")),]$x
  af_sex[cnt,]$Male_Native <- thismales[which(
    (thismales$Group.3 == "Place of birth: Native-born")),]$x
  af_sex[cnt,]$Male_Foreign <- thismales[which(
    (thismales$Group.3 == "Place of birth: Foreign-born")),]$x
}

af_sex$Female_Native_pc <- af_sex$Female_Native/af_sex$Female_Total*100
af_sex$Female_Foreign_pc <- af_sex$Female_Foreign/af_sex$Female_Total*100
af_sex$Male_Native_pc <- af_sex$Male_Native/af_sex$Male_Total*100
af_sex$Male_Foreign_pc <- af_sex$Male_Foreign/af_sex$Male_Total*100
af_sex$Female_Native_Ratio <- af_sex$Female_Native/af_sex$Male_Native
af_sex$Female_Foreign_Ratio <- af_sex$Female_Foreign/af_sex$Male_Foreign

# chloropleth of west africa with female native working age ratio as colour
# then overlay doughnut charts for foreign working age ratio in ppt
library(giscoR)

west_africa <- gisco_get_countries(region="Africa")
west_africa <- merge(west_africa,af_sex[,c("Country","Female_Native_Ratio","Female_Foreign_Ratio")],
                     by.x="NAME_ENGL",by.y="Country",all.x=T)
west_africa[west_africa$NAME_ENGL=="Côte D’Ivoire",]$Female_Native_Ratio <-
  af_sex[af_sex$Country == "Côte d’Ivoire",]$Female_Native_Ratio
west_africa[west_africa$NAME_ENGL=="Côte D’Ivoire",]$Female_Foreign_Ratio <-
  af_sex[af_sex$Country == "Côte d’Ivoire",]$Female_Foreign_Ratio
west_africa[west_africa$NAME_ENGL=="Cape Verde",]$Female_Native_Ratio <-
  af_sex[af_sex$Country == "Cabo Verde",]$Female_Native_Ratio
west_africa[west_africa$NAME_ENGL=="Cape Verde",]$Female_Foreign_Ratio <-
  af_sex[af_sex$Country == "Cabo Verde",]$Female_Foreign_Ratio

#native-born women
ggplot(west_africa)+
  geom_sf(aes(fill=Female_Native_Ratio),
          color="black")+
  xlim(-30,20)+ylim(0,30)+
  theme_void()+
  scale_fill_distiller(palette="PRGn",limits=c(0.7,1.3),na.value="grey70")+
  theme(legend.position = "None")

af_temp <- af_sex[order(af_sex$Female_Native_Ratio),]
af_temp$Country <- factor(af_temp$Country,levels=af_temp$Country)
ggplot(af_temp,
       aes(y=Country,x=Female_Native_Ratio,fill=Female_Native_Ratio))+
  geom_col()+
  scale_fill_distiller(palette="PRGn",limits=c(0.7,1.3),na.value="grey70")+
  theme(legend.position = "None")

#foreign-born women
ggplot(west_africa)+
  geom_sf(aes(fill=Female_Foreign_Ratio),
          color="black")+
  xlim(-30,20)+ylim(0,30)+
  theme_void()+
  scale_fill_distiller(palette="PRGn",limits=c(0.2,1.8),na.value="grey70")+
  theme(legend.position = "None")

ggplot(data=data.frame(value=c(0.7,0.8,0.9,1,1.1,1.2,1.3)))+
  geom_point(aes(x=1:7,y=1,color=value),size=10)+
  scale_color_distiller(palette="PrGn",limits=c(0.7,1.3),na.value="grey70")
  
ggplot(data=data.frame(value=c(0.2,0.5,0.8,1,1.3,1.5,1.8)))+
  geom_point(aes(x=1:7,y=1,color=value),size=10)+
  scale_color_distiller(palette="PRGn",limits=c(0.2,1.8),na.value="grey70")


# Day 25 "global change" ----
# when will dogs take over from babies?
pets <- fread("X:/Datasets/30DayChartChallenge/Day25_globalchange/pets_births.txt",header=T)

pets$dogs_per_person <- pets$Dogs/pets$`UK Population`
pets$babies_per_person <- pets$`Live Births`/pets$`UK Population`

# get line slopes & intercepts
dog_slop <- summary(glm(Dogs~Year,data=pets,family="gaussian"))$coef[2,1]
dog_inte <- summary(glm(Dogs~Year,data=pets,family="gaussian"))$coef[1,1]
baby_slop <- summary(glm(`Live Births`~Year,data=pets,family="gaussian"))$coef[2,1]
baby_inte <- summary(glm(`Live Births`~Year,data=pets,family="gaussian"))$coef[1,1]
human_slop <- summary(glm(`UK Population`~Year,data=pets,family="gaussian"))$coef[2,1]
human_inte <- summary(glm(`UK Population`~Year,data=pets,family="gaussian"))$coef[1,1]
rabbit_slop <- summary(glm(Rabbits~Year,data=pets,family="gaussian"))$coef[2,1]
rabbit_inte <- summary(glm(Rabbits~Year,data=pets,family="gaussian"))$coef[1,1]


projections <- data.frame(x=2021:2030)
projections$dog_y <- dog_inte + dog_slop*projections$x
projections$baby_y <- baby_inte + baby_slop*projections$x
projections$human_y <- human_inte + human_slop*projections$x
projections$rabbit_y <- rabbit_inte + rabbit_slop*projections$x

hist_df <- data.frame(Year=pets$Year,
                      rabbit_y = pets$Rabbits,
                      dog_y=pets$Dogs,
                      baby_y = pets$`Live Births`)
proj_df <- data.frame(Year=projections$x,
                      rabbit_y = projections$rabbit_y,
                      baby_y = projections$baby_y)
hist_proj <- rbind(hist_df,proj_df)
# proj_df <- rbind(hist_df[hist_df$Year==2021,],proj_df)

ggplot(data=hist_df,aes(x=Year))+
  # Rabbits
  stat_smooth(aes(y=rabbit_y),method="lm",fullrange=TRUE,
              color="brown4",size=1,fill="brown")+
  geom_point(aes(y=rabbit_y),
             color="brown4")+
  # Babies
  stat_smooth(aes(y=baby_y),method="lm",fullrange=TRUE,
              color="deepskyblue",size=1,fill="deepskyblue")+
  geom_point(aes(y=baby_y),
             color="deepskyblue")+
  theme_bw()+ylim(-1e6,20e5)+
  scale_x_continuous(breaks=seq(2012,2030),limits=c(2012,2030))


# Day 27 "good/bad" ----
# saturated vs unsaturated fats
# https://www.kaggle.com/datasets/trolukovich/nutritional-values-for-common-foods-and-products Aleksandr Antonov
nutrition <- fread("X:/Datasets/30DayChartChallenge/Day27_good_bad/nutrition.csv",header=T)
food_groups <- fread("X:/Datasets/30DayChartChallenge/Day27_good_bad/food_groups.csv",header=T)

# lets do some text mining for food types woo!
remove_food <- c("alcohol","beverage","babyfood","juice","fast food","infant","restaurant","school","snacks","spices","formula","candie")
nutrition <- nutrition[!grep(paste(remove_food,collapse="|"),
                             nutrition$name,ignore.case=T),]
brands <- c("applebee","mcdonald","kellogg","kashi","olive garden","glutino",
            "george weston","archway","austin","barbara dee","bear naked","burger king","campbell",
            "nestle","caramello","hershey","MARS","kit kat","mounds","goodbar",
            "reese","rolo","symphony","carraba","cracker barrel","denny","interstate",
            "keebler","little caesar","morningstar","murray","mother","pepperidge",
            "prego","uncle","valley","healthy","subway","taco bell","friday",
            "wendy","gardenburger","schiff")
nutrition <- nutrition[!grep(paste(brands,collapse="|"),
                             nutrition$name,ignore.case=T),]
unique(food_groups$Group)
# categorise by group
nutrition$group <- as.character(NA)
for(this_group in unique(food_groups$Group)){
  nutrition[grep(
    paste(food_groups[food_groups$Group%in%this_group,]$Food,collapse="|"),
    nutrition$name,ignore.case=TRUE),]$group <- this_group
}

nutrition[grep("margarine|butter|oil|dressing|pesto",nutrition$name,ignore.case=T),]$group <- "spreadsoils"
nutrition[grep("nut",nutrition$name,ignore.case=T),]$group <- "seedsnuts"
nutrition[grep("soymilk",nutrition$name,ignore.case=T),]$group <- "legumes"

#tables
table(nutrition$group)
# nutrition[nutrition$group=="processedmeat","name"]
nutrition[is.na(nutrition$group),"name"]
na_group <- nutrition[is.na(nutrition$group),]

# good fats, monounsaturated fats and polyunsaturated fats
# bad fats, trans fats
# less bad fats, saturated fats

{
fats <- nutrition[,c("name","group","calories","total_fat",
                     "saturated_fat","fatty_acids_total_trans",
                     "monounsaturated_fatty_acids","polyunsaturated_fatty_acids"
                     )]
fats$total_fat_g <- as.numeric(str_split_fixed(fats$total_fat,"g",2)[,1])
fats$saturated_fat_g <- as.numeric(str_split_fixed(fats$saturated_fat,"g",2)[,1])
fats$fatty_acids_total_trans_mg <- as.numeric(str_split_fixed(fats$fatty_acids_total_trans,"mg",2)[,1])
fats$monounsaturated_fatty_acids_g <- as.numeric(str_split_fixed(fats$monounsaturated_fatty_acids,"g",2)[,1])
fats$polyunsaturated_fatty_acids_g<- as.numeric(str_split_fixed(fats$polyunsaturated_fatty_acids,"g",2)[,1])
}

# remove pure fats 
fats <- fats[fats$total_fat_g < 100,]
# plot(fats$saturated_fat_g,(fats$polyunsaturated_fatty_acids_g + fats$monounsaturated_fatty_acids_g))

{
fats$good_fats <- fats$monounsaturated_fatty_acids_g +
                  fats$polyunsaturated_fatty_acids_g
fats$bad_fats <-  fats$saturated_fat_g
fats$good_fats_kcal <- fats$good_fats*9
fats$bad_fats_kcal <- fats$bad_fats*9
fats$good_fats_kcal_pc <- fats$good_fats_kcal/fats$calories
fats$bad_fats_kcal_pc <- fats$bad_fats_kcal/fats$calories
}

plot(fats$good_fats_kcal_pc,fats$bad_fats_kcal_pc)

# fats filtering
fats2 <- na.omit(fats[,c("name","group","total_fat_g",
                         "good_fats_kcal_pc","bad_fats_kcal_pc")])
fats2 <- fats2[fats2$total_fat_g > 0,]
fats2 <- fats2[fats2$good_fats_kcal_pc > 0,]

fats2$supergroup <- case_when(
  fats2$group %in% c("redmeat","whitemeat","processedmeat","fish") ~ "meat",
  fats2$group %in% c("leafygreen","legumes","fruit","vegetable") ~ "fruitsveg",
  fats2$group %in% c("bread","grains","processedgrains") ~ "grains",
  fats2$group %in% c("dairy") ~ "dairy",
  fats2$group %in% c("seedsnuts","spreadsoils") ~ "nutsoils"
)
length(is.na(fats2$supergroup))

fats2$ratio <- fats2$good_fats_kcal_pc/fats2$bad_fats_kcal_pc

fats2[grep("margarine|butter|oil|dressing|pesto",fats2$name,ignore.case=T),]$group <- "spreadsoils"
fats2[grep("nut",fats2$name,ignore.case=T),]$group <- "seedsnuts"
fats2[grep("soymilk",fats2$name,ignore.case=T),]$group <- "legumes"

fats2 <- fats2[!grep("soup|cracker|muffin|cerea|alaska|taq|brea|corn|roll",
                     fats2$name,ignore.case=T),]

ggplot(fats2,aes(x=good_fats_kcal_pc,y=bad_fats_kcal_pc,
                color=as.factor(group)))+
  geom_point(alpha=0.5)+
  theme_bw()

ggplot(fats2[fats2$group %in% c("redmeat","whitemeat","fish",
                               "dairy","egg",
                               "vegetable","legumes","leafygreen",
                               "seedsnuts")],
       aes(x=good_fats_kcal_pc,y=bad_fats_kcal_pc,
                 color=as.factor(group)))+
  geom_point(alpha=0.5)+
  theme_bw()+xlim(0,1)+ylim(0,1)+
  scale_color_manual(values=c("redmeat"="red","whitemeat"="pink","fish"="grey",
                              "fruit"="darkgreen","vegetable"="darkgreen",
                              "legumes"="darkgreen","leafygreens"="darkgreen",
                              "egg"="lightblue","dairy"="darkblue",
                              "seedsnuts"="coral"))+
  theme_void()

# bread check 2
# dairy bad
# fish good
# fruit check 6



ggplot(fats2[fats2$group %in% 
               c("bread","dairy","fish","redmeat","whitemeat","vegetable",
                 "legumes","fruit","leafygreen")],
       aes(x=total_fat_g,y=ratio,
                 color=as.factor(group)))+
  geom_point(alpha=0.5)+
  theme_bw()+
  # ylim(0,10)+scale_x_log10(limits=c(1,100))
  scale_y_log10(limits=c(0.5,10))+scale_x_log10(limits=c(1,100))


# Day 28 "trends" ----

euro <- fread("X:/Datasets/30DayChartChallenge/Day28_trends/song_data.csv",header=T)

table(euro$style)
euro_style <- as.data.frame(table(euro$year,euro$style))
ggplot(euro_style,aes(x=Var1,y=Freq,fill=Var2))+
  geom_col(position="fill")
ggplot(euro_style,aes(x=Var1,y=Freq,color=Var2,group=Var2))+
  geom_line(size=1.5)

ggplot(euro,aes(x=year,y=final_place))+
  geom_point(aes(color=style))


euro_winners <- euro[final_place==1,]
euro_winners[,c("year","style")]
table(euro$year,euro$style)


# Day 30 FiveThirtyEight theme ----
seats <- fread("X:/Datasets/30DayChartChallenge/Day30_FiveThirtyEight/seats_won.txt",header=T,sep=" ")
female_mps <- fread("X:/Datasets/30DayChartChallenge/Day30_FiveThirtyEight/female_mps.txt",header=T,sep=" ")
women_mps_year <- 1918 # women could be an MP
women_somevote_year <- 1918 # some women could vote (over 30, married to a man who owned property ~ 8.4 million women)
women_vote_year <- 1928 # all women over 21 could vote (equal to men)
voting_age_year <- 1969 # voting age lowered to 18
women_pms <- data.frame(year <- c(1979,2016,2022),
                        n_women <- c(8,67.5,88),
                        name=c("Thatcher","May","Truss"))

seats[seats$LD==0,]$LD <- "NA"
seats[seats$PC==0,]$PC <- "NA"
seats[seats$SNP==0,]$SNP <- "NA"

# female_mps[female_mps$CON==0,]$CON <- "NA"
# female_mps[female_mps$LAB==0,]$LAB <- "NA"
# female_mps[female_mps$LD==0,]$LD <- "NA"
# female_mps[female_mps$PC==0,]$PC <- "NA"
# female_mps[female_mps$SNP==0,]$SNP <- "NA"


ggplot(data=female_mps,aes(x=YEAR))+
  geom_vline(xintercept=women_vote_year,linetype="dotted")+
  geom_vline(xintercept=voting_age_year,linetype="dotted")+
  geom_line(aes(y=CON),color="#0087DC",size=1.5,linetype="solid")+
  geom_line(aes(y=LAB),color="#E4003B",size=1.5,linetype="solid")+
  geom_line(aes(y=LD),color="#FAA61A",size=1.5,linetype="solid")+
  geom_line(aes(y=PC),color="#005B54",size=1.5,linetype="solid")+
  geom_line(aes(y=SNP),color="#FDF38E",size=1.5,linetype="solid")+
  theme_bw()+labs(y="Seats held by Female MPs")+
  geom_point(data=women_pms,aes(x=year,y=n_women),size=3.4,
             fill="#0087DC",color="black",shape=21)+
  scale_x_continuous(limits=c(1918,2024),breaks=seq(1920,2020,10))+
  scale_y_continuous(limits=c(0,120),breaks=seq(0,120,20))+
  theme(panel.grid.minor=element_blank())
  
