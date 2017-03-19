library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(GGally)
library(regex)

elnino <- read_delim("C:/Users/cpickering/Syncplicity Folders/ChadSync/BS STATISTICS/STA141B/Project/elnino.txt", 
                     " ", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
colnames(elnino) <- c("Date", "TMIN", "TMAX", "Station", "Location", "TAVG", "PRCP", "Month", "Year", "ONI")

oni <- read_csv("C:/Users/cpickering/Syncplicity Folders/ChadSync/BS STATISTICS/STA141B/Project/oni.txt", 
                  col_names = FALSE)
colnames(oni) <- c("oni")

# Convert datetime from chr to date class
elnino$Date <- as.Date(paste(elnino$Date, "-01", sep=""))   
oni$Date <- unique(elnino$Date)

# Annual Precipitation Plots - Domestic 

ca_ak_hi <- annual_precip[annual_precip$Location %in% c("Sacramento,CA","LosAngeles,CA","Honolulu,HI","Juneau,AK"),]
or_wa_co_mo <- annual_precip[annual_precip$Location %in% c("Seattle,WA","Portland,OR","ColoradoSprings,CO","KansasCity,MO"),]
mn_ny_il_me <- annual_precip[annual_precip$Location %in% c("Minneapolis,MN","Buffalo,NY","Chicago,IL","Bangor,ME"),]
la_fl_tn_dc <- annual_precip[annual_precip$Location %in% c("NewOrleans,LA","Miami,FL","Nashville,TN","WashingtonD.C."),]


ggplot(ca_ak_hi, aes(Year,PRCP, color=factor(Location)))+
   geom_line()+
   facet_wrap(~Location, ncol=2)+
   scale_color_brewer(palette="Set1")+
   ylim(0,90)+
   stat_smooth(method="gam", formula=y~s(x,k=5), col="black", se=FALSE, size=1)

ggplot(or_wa_co_mo, aes(Year,PRCP, color=factor(Location)))+
   geom_line()+
   facet_wrap(~Location, ncol=2)+
   scale_color_brewer(palette="Set1")+
   ylim(0,90)+
   stat_smooth(method="gam", formula=y~s(x,k=5), col="black", se=FALSE, size=1)

ggplot(mn_ny_il_me, aes(Year,PRCP, color=factor(Location)))+
   geom_line()+
   facet_wrap(~Location, ncol=2)+
   scale_color_brewer(palette="Set1")+
   ylim(0,90)+
   stat_smooth(method="gam", formula=y~s(x,k=5), col="black", se=FALSE, size=1)

ggplot(la_fl_tn_dc, aes(Year,PRCP, color=factor(Location)))+
   geom_line()+
   facet_wrap(~Location, ncol=2)+
   scale_color_brewer(palette="Set1")+
   ylim(0,90)+
   stat_smooth(method="gam", formula=y~s(x,k=5), col="black", se=FALSE, size=1)

# Pearson correlations for monthly temperature/precip/ONI using pairwise observations:
ggcorr(elnino[,c("TMIN","TMAX","TAVG","PRCP","ONI")], 
       low = "steelblue", mid = "white", high = "darkred", 
       label=TRUE, label_size = 4, label_round = 3)

# Pearson correlations for annual average temperature, ONI, precip. using pairwise obs:
annual_agg <- elnino[,c("Location", "PRCP", "TMAX", "TMIN", "TAVG", "ONI", "Year")]
annual_avgs <- aggregate(.~Location+Year, annual_agg, mean)

ggcorr(annual_avgs[,c("TMIN","TMAX","TAVG","PRCP","ONI")], 
       low = "steelblue", mid = "white", high = "darkred", 
       label=TRUE, label_size = 4, label_round = 3)

# even comparing annual precip acc. with average ONI, correlation is about 0.04.


# Average temperature per month 1950-2016 for all 16 domestic cities
monthly_avgtemps <- elnino[,c("Location", "TAVG", "Date")]

ca_ak_hi_t <- monthly_avgtemps[monthly_avgtemps$Location %in% c("Sacramento,CA","LosAngeles,CA","Honolulu,HI","Juneau,AK"),]
or_wa_co_mo_t <- monthly_avgtemps[monthly_avgtemps$Location %in% c("Seattle,WA","Portland,OR","ColoradoSprings,CO","KansasCity,MO"),]
mn_ny_il_me_t <- monthly_avgtemps[monthly_avgtemps$Location %in% c("Minneapolis,MN","Buffalo,NY","Chicago,IL","Bangor,ME"),]
la_fl_tn_dc_t <- monthly_avgtemps[monthly_avgtemps$Location %in% c("NewOrleans,LA","Miami,FL","Nashville,TN","WashingtonD.C."),]

g1 <- ggplot(ca_ak_hi_t, aes(Date,TAVG, color=factor(Location)))+
   geom_line()+
   facet_wrap(~Location, ncol=2, scales="free")+
   scale_color_brewer(palette="Set1")+
   stat_smooth(method="lm", col="black", se=FALSE, size = 1)
   
g1_split <- split(ggplot_build(g1)$data[[2]], ggplot_build(g1)$data[[2]]$PANEL)
ca_ak_hi_fitted <- t(as.data.frame(sapply(g1_split, summary))[c(7, 12),])
colnames(ca_ak_hi_fitted) <- c("1950_avg", "2016_avg")

g2 <- ggplot(or_wa_co_mo_t, aes(Date,TAVG, color=factor(Location)))+
   geom_line()+
   facet_wrap(~Location, ncol=2, scales="free")+
   scale_color_brewer(palette="Set1")+
   stat_smooth(method="lm", col="black", se=FALSE, size = 1)

g2_split <- split(ggplot_build(g2)$data[[2]], ggplot_build(g2)$data[[2]]$PANEL)
or_wa_co_mo_fitted <- t(as.data.frame(sapply(g2_split, summary))[c(7, 12),])
colnames(or_wa_co_mo_fitted) <- c("1950_avg", "2016_avg")

g3 <- ggplot(mn_ny_il_me_t, aes(Date,TAVG, color=factor(Location)))+
   geom_line()+
   facet_wrap(~Location, ncol=2, scales="free")+
   scale_color_brewer(palette="Set1")+
   stat_smooth(method="lm", col="black", se=FALSE, size = 1)

g3_split <- split(ggplot_build(g3)$data[[2]], ggplot_build(g3)$data[[2]]$PANEL)
mn_ny_il_me_fitted <- t(as.data.frame(sapply(g3_split, summary))[c(7, 12),])
colnames(mn_ny_il_me_fitted) <- c("1950_avg", "2016_avg")

g4 <- ggplot(la_fl_tn_dc_t, aes(Date,TAVG, color=factor(Location)))+
   geom_line()+
   facet_wrap(~Location, ncol=2, scales="free")+
   scale_color_brewer(palette="Set1")+
   stat_smooth(method="lm", col="black", se=FALSE, size = 1)

g4_split <- split(ggplot_build(g4)$data[[2]], ggplot_build(g4)$data[[2]]$PANEL)
la_fl_tn_dc_fitted <- t(as.data.frame(sapply(g4_split, summary))[c(7, 12),])
colnames(la_fl_tn_dc_fitted) <- c("1950_avg", "2016_avg")

# Fitted values for 1950 and 2016 to see deviance - evidence of global warming:
d1 <- as.data.frame(ca_ak_hi_fitted)
d2 <- as.data.frame(or_wa_co_mo_fitted) 
d3 <- as.data.frame(mn_ny_il_me_fitted) 
d4 <- as.data.frame(la_fl_tn_dc_fitted)

temp_fitted_values <- rbind(d1, d2, d3, d4)
cities <- c("Honolulu,HI","Juneau,AK","LosAngeles,CA","Sacramento,CA","ColoradoSprings,CO","KansasCity,MO","Portland,OR","Seattle,WA","Bangor,ME","Buffalo,NY","Chicago,IL","Minneapolis,MN","Miami,FL","Nashville,TN","NewOrleans,LA","WashingtonD.C.")

pattern <- ":|Min.|Max.| "
num_1950 <- as.numeric(gsub(pattern, "", temp_fitted_values$`1950_avg`))
num_2016 <- as.numeric(gsub(pattern, "", temp_fitted_values$`2016_avg`))

fitted_values <- data.frame(matrix(ncol = 0, nrow = 16))
fitted_values$city <- cities
fitted_values$avgtemp1950 <- num_1950
fitted_values$avgtemp2016 <- num_2016
fitted_values$net_deviance <- fitted_values$avgtemp2016-fitted_values$avgtemp1950

fitted_values <- fitted_values[order(-fitted_values$net_deviance),]
fitted_values

ggplot(fitted_values, aes(x=reorder(city, -net_deviance), y=net_deviance)) + 
   geom_bar(stat="identity", fill="coral")+
   xlab("City")+
   ylab("Net Deviance (Degrees F)")+
   ggtitle("Net Deviance in Avg. Recorded Temp. in U.S. Cities, 1950-2016")+
   theme(axis.text.x=element_text(angle=70, vjust=1.15, hjust=1.3))
   



