#############################################################################################################
# Mortality among centenarians - Cohort
#############################################################################################################

library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(directlabels)
library(RCurl) # load functions that connects R to the internet
library(msm)
library(MortalityLaws)
library(data.table)
library(ROMIplot)
library(MortalitySmooth)
library(MortalityLaws)
library(readr)

setwd("C:/Users/jmartinez/OneDrive - Syddansk Universitet/Centenarians/R")
source("LifeTableFUN.R") 
source("DecompFUN.R") 
username<- "jesusalvarezmtz@gmail.com"
password<- "Muffles123."
# these are the country codes used in the HMD
countries.all <-
  c("AUS","AUT","BLR","BEL","BGR","CAN","CHL","CZE","DNK","EST",
    "FIN","FRATNP","FRACNP","DEUTNP","HUN","ISL","IRL","ISR","ITA","JPN",
    "LVA","LTU","LUX","NLD","NZL_NM","NOR","POL","PRT","RUS","SVK",
    "SVN","ESP","SWE","CHE","TWN","GBR_NP","GBRTENW","GBRCENW","GBR_SCO",
    "GBR_NIR","USA","UKR")

countries <- c("AUS","CAN","FRACNP","ITA","JPN","NLD","BEL","DNK","NOR","SWE","USA","GBRCENW",
               "FIN", "CHE", "POL", "AUT", "CZE", "DEUTW", "HUN", "GBR_SCO" )


# download and read the life expectancy data in from the HMD
readHMD <- function (country,username, password,what) {
  whichFile <- switch(what,LifeTableF = "fltcoh_1x1", LifeTableM = "mltcoh_1x1",
                      Deaths = "Deaths_1x10.txt", Exp = "cExposures_1x10.txt", Mx = "cMx_1x10.txt")
  print(paste("Downloading",country,what))
  path <- paste("https://www.mortality.org/hmd/", country, "/STATS/", 
                whichFile, sep = "")
  userpwd <- paste(username, ":", password, sep = "")
  txt <- getURL(path, userpwd = userpwd)
  con <- textConnection(txt)
  dat <- read.table(con, skip = 2, header = TRUE, na.strings = ".")
  close(con)
  datCnt <- cbind(dat,country)
  return(datCnt)
}



###################################################################
#Get Exposures and Death Rates from text
###################################################################


MX.AUS    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cMx_1x10/AUS.cMx_1x10.txt", skip = 3)
MX.FRACNP <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cMx_1x10/FRACNP.cMx_1x10.txt", skip = 3)
MX.ITA    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cMx_1x10/ITA.cMx_1x10.txt", skip = 3)
MX.JPN    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cMx_1x10/JPN.cMx_1x10.txt", skip = 3)
MX.NLD    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cMx_1x10/NLD.cMx_1x10.txt", skip = 3)
MX.BEL    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cMx_1x10/BEL.cMx_1x10.txt", skip = 3)
MX.DNK    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cMx_1x10/DNK.cMx_1x10.txt", skip = 3)
MX.NOR    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cMx_1x10/NOR.cMx_1x10.txt", skip = 3)
MX.SWE    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cMx_1x10/SWE.cMx_1x10.txt", skip = 3)
MX.GBRCENW<- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cMx_1x10/GBRCENW.cMx_1x10.txt", skip = 3)
MX.CHE    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cMx_1x10/CHE.cMx_1x10.txt", skip = 3)
MX.AUT    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cMx_1x10/AUT.cMx_1x10.txt", skip = 3)
MX.CZE    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cMx_1x10/CZE.cMx_1x10.txt", skip = 3)
MX.DEUTW  <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cMx_1x10/DEUTW.cMx_1x10.txt", skip = 3)
MX.GBR_SCO<- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cMx_1x10/GBR_SCO.cMx_1x10.txt", skip = 3)



MX.AUS$country     <- rep("AUS",     nrow(MX.AUS    )) 
MX.FRACNP$country  <- rep("FRACNP" , nrow(MX.FRACNP )) 
MX.ITA$country     <- rep("ITA", nrow(MX.ITA    ))
MX.JPN$country     <- rep("JPN", nrow(MX.JPN    ))
MX.NLD$country     <- rep("NLD", nrow(MX.NLD    ))
MX.BEL$country     <- rep("BEL", nrow(MX.BEL    ))
MX.DNK$country     <- rep("DNK", nrow(MX.DNK    ))
MX.NOR$country     <- rep("NOR", nrow(MX.NOR    ))
MX.SWE$country     <- rep("SWE", nrow(MX.SWE    ))
MX.GBRCENW$country <- rep("GBRCENW", nrow(MX.GBRCENW)) 
MX.CHE$country     <- rep("CHE", nrow(MX.CHE    ))
MX.AUT$country     <- rep("AUT", nrow(MX.AUT    ))
MX.CZE$country     <- rep("CZE", nrow(MX.CZE    ))
MX.DEUTW$country   <- rep("DEUTW", nrow(MX.DEUTW  )) 
MX.GBR_SCO$country <- rep("GBR_SCO", nrow(MX.GBR_SCO)) 

Mx <- as.data.frame(rbind(MX.AUS,    
                          MX.FRACNP ,
                          MX.ITA,    
                          MX.JPN ,   
                          MX.NLD ,   
                          MX.BEL ,   
                          MX.DNK ,   
                          MX.NOR ,   
                          MX.SWE ,   
                          MX.GBRCENW,
                          MX.CHE ,   
                          MX.AUT ,   
                          MX.CZE  ,  
                          MX.DEUTW , 
                          MX.GBR_SCO))

Mx$Year <- substr(Mx$Year,1,4)
Mx <- data.frame(lapply(Mx, as.character), stringsAsFactors=FALSE)
Mx$Age[Mx$Age == "110+"] <-110

Mx[, c(1:5)] <- sapply(Mx[, c(1:5)], as.numeric)
Mx           <- arrange(Mx, country, Year, Age)
Mx[is.na(Mx)] <- 0

names(Mx) <- c("Year", "Age", "Mx.f", "Mx.m", "Mx.t", "country")

########################################################################




NX.AUS    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cExposures_1x10/AUS.cExposures_1x10.txt", skip = 3)
NX.FRACNP <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cExposures_1x10/FRACNP.cExposures_1x10.txt", skip = 3)
NX.ITA    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cExposures_1x10/ITA.cExposures_1x10.txt", skip = 3)
NX.JPN    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cExposures_1x10/JPN.cExposures_1x10.txt", skip = 3)
NX.NLD    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cExposures_1x10/NLD.cExposures_1x10.txt", skip = 3)
NX.BEL    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cExposures_1x10/BEL.cExposures_1x10.txt", skip = 3)
NX.DNK    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cExposures_1x10/DNK.cExposures_1x10.txt", skip = 3)
NX.NOR    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cExposures_1x10/NOR.cExposures_1x10.txt", skip = 3)
NX.SWE    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cExposures_1x10/SWE.cExposures_1x10.txt", skip = 3)
NX.GBRCENW<- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cExposures_1x10/GBRCENW.cExposures_1x10.txt", skip = 3)
NX.CHE    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cExposures_1x10/CHE.cExposures_1x10.txt", skip = 3)
NX.AUT    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cExposures_1x10/AUT.cExposures_1x10.txt", skip = 3)
NX.CZE    <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cExposures_1x10/CZE.cExposures_1x10.txt", skip = 3)
NX.DEUTW  <- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cExposures_1x10/DEUTW.cExposures_1x10.txt", skip = 3)
NX.GBR_SCO<- read_table2("C:/Users/jmartinez/OneDrive - Syddansk Universitet/HMD/cExposures_1x10/GBR_SCO.cExposures_1x10.txt", skip = 3)



NX.AUS$country     <- rep("AUS",     nrow(NX.AUS    )) 
NX.FRACNP$country  <- rep("FRACNP" , nrow(NX.FRACNP )) 
NX.ITA$country     <- rep("ITA", nrow(NX.ITA    ))
NX.JPN$country     <- rep("JPN", nrow(NX.JPN    ))
NX.NLD$country     <- rep("NLD", nrow(NX.NLD    ))
NX.BEL$country     <- rep("BEL", nrow(NX.BEL    ))
NX.DNK$country     <- rep("DNK", nrow(NX.DNK    ))
NX.NOR$country     <- rep("NOR", nrow(NX.NOR    ))
NX.SWE$country     <- rep("SWE", nrow(NX.SWE    ))
NX.GBRCENW$country <- rep("GBRCENW", nrow(NX.GBRCENW)) 
NX.CHE$country     <- rep("CHE", nrow(NX.CHE    ))
NX.AUT$country     <- rep("AUT", nrow(NX.AUT    ))
NX.CZE$country     <- rep("CZE", nrow(NX.CZE    ))
NX.DEUTW$country   <- rep("DEUTW", nrow(NX.DEUTW  )) 
NX.GBR_SCO$country <- rep("GBR_SCO", nrow(NX.GBR_SCO)) 

Nx <- as.data.frame(rbind(NX.AUS,    
                          NX.FRACNP ,
                          NX.ITA,    
                          NX.JPN ,   
                          NX.NLD ,   
                          NX.BEL ,   
                          NX.DNK ,   
                          NX.NOR ,   
                          NX.SWE ,   
                          NX.GBRCENW,
                          NX.CHE ,   
                          NX.AUT ,   
                          NX.CZE  ,  
                          NX.DEUTW , 
                          NX.GBR_SCO))

Nx$Year <- substr(Nx$Year,1,4)
Nx <- data.frame(lapply(Nx, as.character), stringsAsFactors=FALSE)
Nx$Age[Nx$Age == "110+"] <-110

Nx[, c(1:5)] <- sapply(Nx[, c(1:5)], as.numeric)
Nx           <- arrange(Nx, country, Year, Age)
Nx[is.na(Nx)] <- 0

names(Nx) <- c("Year", "Age", "Nx.f", "Nx.m", "Nx.t", "country")


###################################################################
#Get Exposures and Death Rates from HMD
###################################################################

#We need to obtain Exposures and death rates to get the death counts
#Mx.l <- lapply(countries, readHMD, username=username, password = password, what = "Mx")
#Nx.l <- lapply(countries, readHMD, username=username, password = password, what = "Exp")
#Mx   <- do.call(rbind.data.frame, Mx.l)
#Nx   <- do.call(rbind.data.frame, Nx.l)

#names(Mx) <- c("Year", "Age", "Mx.f", "Mx.m", "Mx.t", "country")
#names(Nx) <- c("Year", "Age", "Nx.f", "Nx.m", "Nx.t", "country")


#Merge both datasets to obtain Dx
Dx <- merge(Mx,Nx, by =c("country", "Year", "Age"))


Dx <- data.frame(lapply(Dx, as.character), stringsAsFactors=FALSE)
Dx$Age[Dx$Age == "110+"] <-110
Dx$Year <- as.numeric(substr(Dx$Year,1,4))
Dx[, c(2:8)] <- sapply(Dx[, c(2:8)], as.numeric)
Dx           <- arrange(Dx, country, Year, Age)
Dx[is.na(Dx)]<-0


Rates <-  subset(Dx, (country =="AUS"    & Year >=1845    & Year <=1903)|
                           (country =="FRACNP" & Year >1805    & Year <=1904)|
                           (country =="ITA"    & Year >=1812   & Year <=1903)|
                           (country =="JPN"    & Year >=1868    & Year <=1903)|
                           (country =="NLD"    & Year >=1849   & Year <=1903)|
                           (country =="BEL"    & Year >=1837    & Year <=1904)|
                           (country =="DNK"    & Year >=1858   & Year <=1903 )|
                           (country =="NOR"   & Year >=1805   & Year <=1901)|
                           (country =="SWE"    & Year >=1805    & Year <=1905)|
                           (country =="GBRCENW"& Year >1805    & Year <=1903)|
                           (country =="CHE"      & Year <=1871)|
                           (country =="AUT"    & Year >1871  & Year <=1904)|
                           (country =="CZE"      & Year <=1904)|
                           (country =="DEUTW"    & Year <=1904)|
                           (country =="GBR_SCO"  & Year <=1904))

Rates$Dx.f <- Rates$Mx.f * Rates$Nx.f
Rates$Dx.m <- Rates$Mx.m * Rates$Nx.m


############################################################################
#Fit a Gamma Gompertz with three parameters using Marius package
############################################################################

Data          <- data.table(subset(Rates, Age > 79 & Year > 1804))

#Fit the model
fit.f         <- Data[,MortalityLaw(x = Age, mx = Mx.f,
                                    law = "ggompertz", 
                                    opt.method = "poissonL", 
                                    parS = NULL, 
                                    fit.this.x = Age,
                                    scale.x = T,
                                    custom.law = NULL, 
                                    show = F), by = list(Year, country)]

GG.f <- fit.f[,c("Year", "country", "fitted.values")]
GG.f$Age <- c(80:110)

GG.par.f1 <- fit.f[,c("Year", "country", "coefficients", "deviance")]
GG.par.f1<-GG.par.f1[!duplicated(GG.par.f1), ]
GG.par.f1$c <- c("a","b","gamma")
GG.par.f1 <- GG.par.f1 %>% spread(c, coefficients )


GG.par.f <- fit.f[,c("Year", "country", "goodness.of.fit")]
GG.par.f<-GG.par.f[!duplicated(GG.par.f), ]
GG.par.f$c <- c("loglik","AIC","BIC")
GG.par.f <- GG.par.f %>% spread(c,goodness.of.fit)

GG.par.f <- as.data.frame(merge(GG.par.f1, GG.par.f, by = c("Year", "country")))

GG.f <- as.data.frame(merge(GG.f, GG.par.f, by =c("Year", "country")))
GG.f <- GG.f[,c(1,2,4,3,5,6,7,8)]
names(GG.f)<- c("Year","country","Age","mx","deviance","a","b","gamma")
GG.f <- data.table(subset(GG.f,b<1 & gamma<1 & Age>=100)) #CHECAR EDAD 100


#FALTA HACER LO MISMO PARA HOMBRES
###################################################################
# Get Life tables
###################################################################
LT.f  <- as.data.frame(GG.f[,lifetable.mx(x = Age, mx = mx, sex = "F"), by = list(country, Year)])
All.f <- merge(LT.f, GG.f, by = c("country", "Year", "mx"))

LT100.f <- subset(LT.f,   x==100)

ggplot(LT100.f)+
  geom_point(aes(as.integer(Year), ex))+
  geom_line(aes(as.integer(Year), ex))+
  #scale_y_continuous(limits = c(1,3),expand = c(0,0))+
  scale_x_continuous(expand = c(0,0), breaks = seq(1800,2010, by = 10))+
  facet_wrap(~country, ncol = 3)+
  theme_bw()+
  theme(legend.title=element_blank(), 
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        axis.text.x  = element_text(angle = 45, hjust = 1))+
  ylab("e100")+
  xlab("Year")


##############################################################################
# Apply the functions
##############################################################################

countries <- c("AUS","FRACNP","ITA","JPN","NLD","BEL","DNK","NOR","SWE","GBRCENW",
               "CHE", "AUT", "CZE", "DEUTW", "GBR_SCO")

# Select the country to analyze
LT.AUS <- subset(All.f, country == "AUS")   
LT.FRA <- subset(All.f, country == "FRACNP")
LT.ITA <- subset(All.f, country == "ITA") 
LT.JPN <- subset(All.f, country == "JPN") 
LT.NLD <- subset(All.f, country == "NLD") 
LT.BEL <- subset(All.f, country == "BEL") 
LT.DNK <- subset(All.f, country == "DNK") 
LT.NOR <- subset(All.f, country == "NOR") 
LT.SWE <- subset(All.f, country == "SWE") 
LT.GBR <- subset(All.f, country == "GBRCENW")
LT.CHE <- subset(All.f, country == "CHE") 
LT.AUT <- subset(All.f, country == "AUT") 
LT.CZE <- subset(All.f, country == "CZE") 
LT.DEU <- subset(All.f, country == "DEUTW") 
LT.SCO <- subset(All.f, country == "GBR_SCO")

# Calculate rho
input.AUS <- get.rho(LT.AUS)
input.FRA <- get.rho(LT.FRA)
input.ITA <- get.rho(LT.ITA)
input.JPN <- get.rho(LT.JPN)
input.NLD <- get.rho(LT.NLD)
input.BEL <- get.rho(LT.BEL)
input.DNK <- get.rho(LT.DNK)
input.NOR <- get.rho(LT.NOR)
input.SWE <- get.rho(LT.SWE)
input.GBR <- get.rho(LT.GBR)
input.CHE <- get.rho(LT.CHE)
input.AUT <- get.rho(LT.AUT)
input.CZE <- get.rho(LT.CZE)
input.DEU <- get.rho(LT.DEU)
input.SCO <- get.rho(LT.SCO)

input <- data.table(rbind(input.AUS,
                          input.FRA,
                          input.ITA,
                          input.JPN,
                          input.NLD,
                          input.BEL,
                          input.DNK,
                          input.NOR,
                          input.SWE,
                          input.GBR,
                          input.CHE,
                          input.AUT,
                          input.CZE,
                          input.DEU,
                          input.SCO))

# Calculate rho bars
rhos <- as.data.frame(input[,rho.bars(dx= dx, rho = rho, rhobar = rhobar), by = list(Year, country)])

# Decompose edot for all years
decomp <-   as.data.frame(input[,ex.dot.decomp(ax=ax, dx=dx, ex=ex, rho=rho, rhobar=rhobar), by = list(Year, country)])
#decomp$ex.dot2 <- c(0, decomp$e100[-1] - decomp$e100[-length(decomp$e100)])


#################################################################################################
#Graphs of the decomposition
################################################################################################# 

rho100 <- subset(input, Age ==110)

ggplot(rho100)+
  geom_line(aes(as.integer(Year), rho), colour = "red")+
  geom_line(aes(as.integer(Year), rhobar))+
  facet_wrap(~country, ncol = 5)



# Rho individual vs rho population

ggplot(rhos)+
  geom_line(aes(as.integer(Year), rho.bari), colour = "red")+
  geom_line(aes(as.integer(Year), rho.barp), colour = "black")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  facet_wrap(~country, ncol = 5)+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0), breaks = seq(1800,1900, by = 10))+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))+
  theme(legend.title=element_blank(), 
        panel.background = element_blank(),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(2, "lines"))+
  xlab("Year") +
  ylab("average rho for individuals and population")







#594F4F,#547980,#45ADA8,#9DE0AD,#E5FCC2,#594F4F,#547980,#45ADA8

low_col <- RColorBrewer::brewer.pal(11, "RdGy")[11:7]
high_col <- RColorBrewer::brewer.pal(11, "RdGy")[5:1]

glimpse_colors <- function(colors_string){
  n <- length(colors_string)
  hist(1:n, breaks = 0:n, col = colors_string)
}

glimpse_colors(low_col)
glimpse_colors(high_col)

#Rho - Heatmap
ggplot(input)+
  geom_tile(aes(x = as.integer(Year), y = Age, fill = rho))+
  # scale_fill_manual(values= kk)+
  scale_fill_gradient2(low= low_col, midpoint = 0, high = high_col,
                       limits = c(-3, 3))+
  scale_y_continuous(expand=c(0,0), breaks=seq(100,110,1))+
  scale_x_continuous(expand=c(0,0), breaks = seq(1800, 1915, by = 10))+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))+
  theme(legend.title=element_blank(), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position = "right")+
  xlab("Year") +
  ylab("Age")+
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 15))


#Rhobar - Heatmap
ggplot(input)+
  geom_tile(aes(x = as.integer(Year), y = Age, fill = rhobar))+
  # scale_fill_manual(values= kk)+
  scale_fill_gradient2(low= low_col, midpoint = 0, high = high_col,
                       limits = c(-3, 3))+
  scale_y_continuous(expand=c(0,0), breaks=seq(100,110,1))+
  scale_x_continuous(expand=c(0,0), breaks = seq(1800, 1915, by = 10))+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))+
  theme(legend.title=element_blank(), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position = "right")+
  xlab("Year") +
  ylab("Age")+
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 15))



#e100 vs e-dagger 
ggplot(decomp)+
  geom_line(aes(as.integer(Year), e100), size = 1.5)+
  geom_line(aes(as.integer(Year), e.dagger), linetype = "dashed", size = 1)+
  scale_y_continuous(limits = c(1,3),expand = c(0,0), breaks = seq(1,3, by = 0.25))+
  scale_x_continuous(expand = c(0,0), breaks = seq(1800,1915, by = 10))+
  theme_bw()+
  theme(legend.title=element_blank(), 
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))+
  ylab("Years")+
  xlab("Period") 


#Entropy
ggplot(decomp)+
  geom_hline(yintercept = 1, linetype = "dotted")+
  geom_line(aes(as.integer(Year),H), colour ="#655643", size= 1.3)+
  scale_y_continuous(expand = c(0,0),limits = c(0.9,1.1), breaks = seq(0.5,1.1, by =0.05 ))+
  scale_x_continuous(expand = c(0,0), breaks = seq(1800,1915, by = 10))+
  theme_bw()+
  theme(legend.title=element_blank(), 
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))+
  ylab("Keyfitz's entropy (H)")+
  xlab("Year") 


#Derivative all together
ggplot(decomp)+
  geom_hline(yintercept = 0, linetype = "dotted")+
  #geom_segment(aes(x=Year, y=ex.dot, xend=c(Year[2:nrow(decomp)], NA), yend=ex.dot), colour ="#BF4D28", size = 1.2)+
  #geom_segment(aes(x=Year, y=rho.tilde.bar, xend=c(Year[2:nrow(decomp)], NA), yend=rho.tilde.bar), colour ="#80BCA3", size = 1.2)+
  geom_line(aes(as.integer(Year), ex.dot), colour ="#BF4D28", size = 1.2)+
  geom_line(aes(as.integer(Year), rho.tilde.bar), colour ="#80BCA3", size = 1.2)+
  scale_y_continuous(expand = c(0,0), limits = c(-0.5,0.5), breaks = seq(-0.5,0.5, by =0.1 ))+
  scale_x_continuous(expand = c(0,0), breaks = seq(1800,1915, by = 10))+
  theme_bw()+
  theme(legend.title=element_blank(), 
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))+
  ylab("Change in mortality rates and e100")+
  xlab("Year") 

#Average rho 
ggplot(decomp)+
  geom_hline(yintercept = 0, linetype = "dotted")+
  geom_line(aes(as.integer(Year), rho.tilde.bar))+
  #scale_y_continuous(expand = c(0,0),limits = c(-0.03,0.05), breaks = seq(-0.03,0.05, by =0.01 ))+
  #scale_x_continuous(expand = c(0,0), breaks = seq(1900,2010, by = 10))+
  theme_bw()+
  theme(legend.title=element_blank(), 
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))+
  ylab("Average improvement in mortality rates")+
  xlab("Year") 


#655643,#80BCA3,#F6F7BD,#E6AC27,#BF4D28,#655643,#80BCA3,#F6F7BD




