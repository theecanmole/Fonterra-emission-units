Fonterra 
# link @ 11/01/2023 
https://www.epa.govt.nz/assets/Uploads/Documents/Emissions-Trading-Scheme/Reports/Industrial-Allocations/Industrial-Allocations-Final-Decisions_2022.xlsx

R.version.string
[1] "R version 4.2.0 (2022-04-22)"
# load packages 
library(readxl)
library(dplyr)
library(RColorBrewer)
library(tidyr)

# check and/or reset working folder
getwd()
[1] "/home/user/R/Fonterra"
setwd("/home/user/R/Fonterra")

# obtain 2010 to 2021 emission unit allocation to industry data from EPA

download.file("https://www.epa.govt.nz/assets/Uploads/Documents/Emissions-Trading-Scheme/Reports/Industrial-Allocations/Industrial-Allocations-Final-Decisions_2022.xlsx","Industrial-Allocations-Final-Decisions_2022.xlsx")

# check names of work sheets
excel_sheets("Industrial-Allocations-Final-Decisions_2022.xlsx")
[1] "IA Final Decisions"

# read in allocation of emissions units data

Allocations <- read_excel("Industrial-Allocations-Final-Decisions_2022.xlsx", sheet = "IA Final Decisions",skip=3)
# check dataframe variables
str(Allocations) 
tibble [1,286 × 4] (S3: tbl_df/tbl/data.frame)
 $ Activity        : chr [1:1207] "Aluminium smelting" "Burnt lime" "Burnt lime" "Burnt lime" ...
 $ Applicant’s name: chr [1:1207] "New Zealand Aluminium Smelters Limited" "Graymont (NZ) Limited" "Holcim (New Zealand) Limited" "Perry Resources (2008) Ltd" ...
 $ Year            : num [1:1207] 2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
 $ Final Allocation: num [1:1207] 210421 47144 3653 4712 948 ...

# revise and shorten column names
colnames(Allocations) <- c("Activity", "Applicant", "Year", "Allocation") 
# check range of variables 
summary(Allocations)   
  Activity          Applicant              Year        Allocation       
 Length:1286        Length:1286        Min.   :2010   Min.   :      1.0  
 Class :character   Class :character   1st Qu.:2012   1st Qu.:    180.8  
 Mode  :character   Mode  :character   Median :2014   Median :   1142.0  
                                       Mean   :2015   Mean   :  47896.6  
                                       3rd Qu.:2018   3rd Qu.:   7580.0  
                                       Max.   :2021   Max.   :2145482.0 
                                       
# separate allocations of emissions units data into years
nzu2010 <- filter(Allocations, Year =="2010")
nzu2011 <- filter(Allocations, Year =="2011")
nzu2012 <- filter(Allocations, Year =="2012")
nzu2013 <- filter(Allocations, Year =="2013")
nzu2014 <- filter(Allocations, Year =="2014")
nzu2015 <- filter(Allocations, Year =="2015")
nzu2016 <- filter(Allocations, Year =="2016")
nzu2017 <- filter(Allocations, Year =="2017")
nzu2018 <- filter(Allocations, Year =="2018")
nzu2019 <- filter(Allocations, Year =="2019") 
nzu2020 <- filter(Allocations, Year =="2020")  
nzu2021 <- filter(Allocations, Year =="2021")

# check just the 2010 data
str(nzu2010)
tibble [141 × 4] (S3: tbl_df/tbl/data.frame)
 $ Activity  : chr [1:141] "Aluminium smelting" "Burnt lime" "Burnt lime" "Burnt lime" ...
 $ Applicant : chr [1:141] "New Zealand Aluminium Smelters Limited" "Graymont (NZ) Limited" "Holcim (New Zealand) Limited" "Perry Resources (2008) Ltd" ...
 $ Year      : num [1:141] 2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
 $ Allocation: num [1:141] 210421 47144 3653 4712 948 ...
 
# Each year, usually in May, the EPA makes a 'provisional' allocation of emssion units to selected industries. see https://www.epa.govt.nz/industry-areas/emissions-trading-scheme/industrial-allocations/ I want to estimate the market value of free allocation of units. I understand that the deadline for a provisional allocation is 30 April of each year so I assume the transfer of the allocation to NZ Steel is made in May of each year. There is an online 'open data' Github repository of New Zealand Unit (NZU) prices going back to May 2010. https://github.com/theecanmole/nzu
# The NZU repository has it's own citation and DOI: Theecanmole. (2016). New Zealand emission unit (NZU) monthly prices 2010 to 2016: V1.0.01 [Data set]. Zenodo. http://doi.org/10.5281/zenodo.221328
# I will add a NZU market price value at the May average price from 2010 to 2021
nzu2010[["Value"]] <- nzu2010[["Allocation"]]*17.58
nzu2011[["Value"]] <- nzu2011[["Allocation"]]*19.84
nzu2012[["Value"]] <- nzu2012[["Allocation"]]*6.23
nzu2013[["Value"]] <- nzu2013[["Allocation"]]*1.94
nzu2014[["Value"]] <- nzu2014[["Allocation"]]*4.08
nzu2015[["Value"]] <- nzu2015[["Allocation"]]*5.34
nzu2016[["Value"]] <- nzu2016[["Allocation"]]*14.63
nzu2017[["Value"]] <- nzu2017[["Allocation"]]*16.96
nzu2018[["Value"]] <- nzu2018[["Allocation"]]*21.28
nzu2019[["Value"]] <- nzu2019[["Allocation"]]*25.29 
nzu2020[["Value"]] <- nzu2020[["Allocation"]]*24.84
nzu2021[["Value"]] <- nzu2021[["Allocation"]]*37.14
# add the average May spot prices
# Mayprice <-c(17.58, 19.84,6.23,1.94,4.08,5.34,  14.63, 16.96 , 21.28 , 25.29 , 24.84 ,37.14)

# combine all the year data together into 1 dataframe - I use rbind as all the column names are consistent
Allocations <- rbind(nzu2010,nzu2011,nzu2012,nzu2013,nzu2014,nzu2015,nzu2016,nzu2017,nzu2018,nzu2019,nzu2020,nzu2021)

# check the new dataframe
str(Allocations)
tibble [1,286 × 5] (S3: tbl_df/tbl/data.frame)
 $ Activity  : chr [1:1286] "Aluminium smelting" "Burnt lime" "Burnt lime" "Burnt lime" ...
 $ Applicant : chr [1:1286] "New Zealand Aluminium Smelters Limited" "Graymont (NZ) Limited" "Holcim (New Zealand) Limited" "Perry Resources (2008) Ltd" ...
 $ Year      : num [1:1286] 2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
 $ Allocation: num [1:1286] 210421 47144 3653 4712 948 ...
 $ Value     : num [1:1286] 3699201 828792 64220 82837 16666 ...

FonterraLtdunits <- filter(Allocations, Applicant =="Fonterra Ltd")
FonterraLimitedunits <- filter(Allocations, Applicant =="Fonterra Limited")
Fonterraunits <- rbind(FonterraLtdunits,FonterraLimitedunits) 
str(Fonterraunits) 
'data.frame':	24 obs. of  5 variables:
 $ Activity  : chr  "Lactose" "Whey powder" "Lactose" "Whey powder" ...
 $ Applicant : chr  "Fonterra Ltd" "Fonterra Ltd" "Fonterra Ltd" "Fonterra Ltd" ...
 $ Year      : int  2010 2010 2011 2011 2012 2012 2013 2013 2014 2014 ...
 $ Allocation: int  13418 888 21290 1066 25210 970 28140 622 26109 830 ...
 $ Value     : num  235888 15611 422394 21149 157058 ... 
 
# How many emission units p.a ?
Fonterraunitsannualno <- aggregate(Allocation ~ Year, Fonterraunits, sum) 
Fonterraunitsannualno 
   Year Allocation
1  2010      14306
2  2011      22356
3  2012      26180
4  2013      28762
5  2014      26939
6  2015      31317
7  2016      32124
8  2017      43651
9  2018      50664
10 2019      57190
11 2020      59209
12 2021      57715
# How many total over 12 years?
sum(Fonterraunitsannualno[["Allocation"]])
[1] 450413 
# make matrix for a barplot
Fonterramatrix <- matrix(c(Fonterraunitsannualno[["Allocation"]]), nrow = 1, ncol=12, byrow=TRUE, dimnames = list(c("Units"), c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021")))
Fonterramatrix 
        2010  2011  2012  2013  2014  2015  2016  2017  2018  2019  2020  2021
Units 14306 22356 26180 28762 26939 31317 32124 43651 50664 57190 59209 57715      

# create svg & a small .png format chart of the market value of free emission units
svg(filename ="Fonterra-2010-2021-allocations_720-540font11.svg", width = 8, height = 6, pointsize = 11, onefile = FALSE, family = "sans", bg = "white")
#png("Fonterra-2010-2021-560by420F12.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(4.4, 4.4, 4.4, 2)+0.1)
barplot(Fonterramatrix/10^3,col="#E2A100",las=1) # color is Tangerine! 
title(ylab="Emission units (thousands)")
mtext(side=3,line=2.25,cex=1.4,expression(paste("NZETS emission units allocated to Fonterra")))
mtext(side=3,line=-0.25,cex=1,expression(paste(
"From 2010 to 2021 Fonterra were given 450,413 free emission \nunits to make their coal thermal process heat costs cheaper")))
mtext(side=1,line=2.5,cex=0.9,expression(paste("Data: https://www.epa.govt.nz/industry-areas/emissions-trading-scheme/industrial-allocations/decisions/")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
dev.off() 


# What was the value of the mission units p.a ?
Fonterraunitsannualvalue <- aggregate(Value ~ Year, Fonterraunits, sum)
sum(Fonterraunitsannualvalue[["Value"]])
[1] 8540133 $ 8.5 million!!!
Fonterraunitsannualvalue
  Year      Value
1  2010  251499.48
2  2011  443543.04
3  2012  163101.40
4  2013   55798.28
5  2014  109911.12
6  2015  167232.78
7  2016  469974.12
8  2017  740320.96
9  2018 1078129.92
10 2019 1446335.10
11 2020 1470751.56
12 2021 2143535.10

# create a matrix to draw barplots with
Fonterradatamatrix <- matrix(c( Fonterraunitsannualvalue[["Value"]]/10^6), nrow = 1, ncol=12, byrow=TRUE, dimnames = list(c("Market Value"), c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021")))
Fonterradatamatrix 
> Fonterradatamatrix 
                  2010     2011      2012       2013      2014      2015
Market Value 0.2514995 0.443543 0.1631014 0.05579828 0.1099111 0.1672328
                  2016     2017    2018     2019     2020     2021
Market Value 0.4699741 0.740321 1.07813 1.446335 1.470752 2.143535 
# colour #38BEB6		Puerto Rico
svg(filename="Fonterra-units-marketvalue-720by540.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
#png("Fonterra-units-marketvalue-565by420.png", bg="white", width=570, height=428,pointsize = 12)
#png("Fonterra-units-marketvalue-600by450.png", bg="white", width=600, height=450,pointsize = 12)
#png("Fonterra-units-marketvalue-720by540.png", bg="white", width=720, height=540,pointsize = 14)
par(mar=c(4, 3, 4, 1)+0.1)
barplot(Fonterradatamatrix,las=1,space=0.5, beside = FALSE, col=c("#38BEB6")) 
mtext(side=1,line=2.5,cex=1,expression(paste("Source: EPA Industrial Allocation")))
#legend("topleft", inset=c(0.0,0.0) ,bty="n",c("Annual market value of allocated units"),fill=c("#38BEB6"))
mtext(side=2,cex=1, line=-1.8,expression(paste("$million")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
mtext(side=3,cex=1.5, line=1.2,expression(paste("Fonterra market value of emission units allocated from 2010 to 2021")) )
mtext(side=3,cex=1, line=-0.4,expression(paste("The 450,413 free emission units allocated to Fonterra have a market value of $8.5 million")) )
dev.off()

svg(filename="Fonterra-units-marketvalue-720by540a.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
#png("Fonterra-units-marketvalue-565by420a.png", bg="white", width=570, height=428,pointsize = 12)
#png("Fonterra-units-marketvalue-600by450.png", bg="white", width=600, height=450,pointsize = 12)
#png("Fonterra-units-marketvalue-720by540.png", bg="white", width=720, height=540,pointsize = 14)
par(mar=c(4, 3, 4, 1)+0.1)
barplot(Fonterradatamatrix,las=1,space=0.5, beside = FALSE, col=c("#38BEB6")) 
mtext(side=1,line=2.5,cex=1,expression(paste("Source: EPA Industrial Allocation")))
legend("topleft", inset=c(0.0,0.0) ,bty="n",c("The 450,413 free emission units allocated to Fonterra have a\nmarket value of $8.5 million"),fill=c("#38BEB6"))
mtext(side=2,cex=1, line=-1.8,expression(paste("$million")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
mtext(side=3,cex=1.5, line=1.2,expression(paste("Fonterra market value of emission units allocated from 2010 to 2021")) )
#mtext(side=3,cex=1, line=-0.4,expression(paste("The 450,413 free emission units allocated to Fonterra have a market value of $8.5 million")) )
dev.off()

# filter for only lactose
filter(Fonterraunits,Activity =="Lactose") 
# A tibble: 12 × 5
   Activity Applicant         Year Allocation    Value
   <chr>    <chr>            <dbl>      <dbl>    <dbl>
 1 Lactose  Fonterra Ltd      2010      13418  235888.
 2 Lactose  Fonterra Ltd      2011      21290  422394.
 3 Lactose  Fonterra Ltd      2012      25210  157058.
 4 Lactose  Fonterra Ltd      2013      28140   54592.
 5 Lactose  Fonterra Ltd      2014      26109  106525.
 6 Lactose  Fonterra Ltd      2015      29515  157610.
 7 Lactose  Fonterra Limited  2016      29633  433531.
 8 Lactose  Fonterra Limited  2017      39574  671175.
 9 Lactose  Fonterra Limited  2018      44739  952046.
10 Lactose  Fonterra Limited  2019      51787 1309693.
11 Lactose  Fonterra Limited  2020      53229 1322208.
12 Lactose  Fonterra Limited  2021      51403 1909107.
# filter for only whey powder
filter(Fonterraunits,Activity =="Whey powder") 
# A tibble: 12 × 5
   Activity    Applicant         Year Allocation   Value
   <chr>       <chr>            <dbl>      <dbl>   <dbl>
 1 Whey powder Fonterra Ltd      2010        888  15611.
 2 Whey powder Fonterra Ltd      2011       1066  21149.
 3 Whey powder Fonterra Ltd      2012        970   6043.
 4 Whey powder Fonterra Ltd      2013        622   1207.
 5 Whey powder Fonterra Ltd      2014        830   3386.
 6 Whey powder Fonterra Ltd      2015       1802   9623.
 7 Whey powder Fonterra Limited  2016       2491  36443.
 8 Whey powder Fonterra Limited  2017       4077  69146.
 9 Whey powder Fonterra Limited  2018       5925 126084 
10 Whey powder Fonterra Limited  2019       5403 136642.
11 Whey powder Fonterra Limited  2020       5980 148543.
12 Whey powder Fonterra Limited  2021       6312 234428 

# filter only lactose
Fonterralactose <- filter(Fonterraunits,Activity =="Lactose") 
# filter only whey powder
Fonterrawheypowder <- filter(Fonterraunits,Activity =="Whey powder") 
Fonterrawheypowder[["Allocation"]]
[1]  888 1066  970  622  830 1802 2491 4077 5925 5403 5980 6312
sum(Fonterrawheypowder[["Allocation"]])
[1] 36366
# create  unit allocations into a matrix
# make matrix for a barplot
Fonterrawheypowdermatrix <- matrix(c(Fonterrawheypowder[["Allocation"]]), nrow = 1, ncol=12, byrow=TRUE, dimnames = list(c("Units"), c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021")))
Fonterrawheypowdermatrix
      2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021
Units  888 1066  970  622  830 1802 2491 4077 5925 5403 5980 6312 

# create svg & a small .png format chart of the market value of free emission units
#svg(filename ="Fonterrawheypowder-2010-2021-allocations_720-540font11.svg", width = 8, height = 6, pointsize = 11, onefile = FALSE, family = "sans", bg = "white")
png("Fonterrawheypowder-2010-2021-560by420F12.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(4.4, 4.4, 4.4, 2)+0.1)
barplot(Fonterrawheypowdermatrix/10^3,col="#E2A100",las=1) # color is Tangerine! 
mtext(side=3,line=2,cex=1.4,expression(paste("NZETS emission units allocated to Fonterra for whey powder")))
mtext(side=3,line=-0.25,cex=1,expression(paste(
"From 2016 to 2021 Fonterra were given 30,188 free emission \nunits to make their whey powder coal thermal costs cheaper")))
mtext(side=1,line=2.5,cex=0.9,expression(paste("Data: https://www.epa.govt.nz/industry-areas/emissions-trading-scheme/industrial-allocations/decisions/")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
mtext(side=2,line=-1.5,cex=1,expression(paste("Emission units (thousands)")))
dev.off()
