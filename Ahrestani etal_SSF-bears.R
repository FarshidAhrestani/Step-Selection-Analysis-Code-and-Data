# Title: "Step Selection of Bears in PA, USA"
# Author: Farshid S. Ahrestani
# Date: 15 December 2018

# The following code allows you to analyze resource selection of bears living in suburbia in Pennsylvania, USA
# The location data of the bears were got from colaring bears in the study area
# The covariate data were accessed from raster layers sourced mainly from the publically available U.S. National Land Cover Dataset

# Set your working directory
workdir = "YOUR WORKING DIRECTORY!!!"
setwd(workdir)

# Read the .csv dataset of interested, for eg.PeerJ-DennedBears
all.seasons <- read.csv("Ahrestani.etal_SSF-PA.bears.csv", stringsAsFactors=TRUE)

# Ensure categorical variables that have numerical values remain factors the analysis
all.seasons$HousingDensity <- as.factor(all.seasons$HousingDensity)
all.seasons$Huntable <- as.factor(all.seasons$Huntable)
all.seasons$Landcover <- as.factor(all.seasons$Landcover)

# You will require the mclogit library, which is best sourced from the link provided
require(devtools)
install_version("mclogit", version = "0.3-1", repos = "http://cran.us.r-project.org")
require(mclogit)

# Seperating the data into the three seasons of interest
season1 <- all.seasons[all.seasons$season == 1,] # pre-hunt season
season2 <- all.seasons[all.seasons$season == 2,] # hunting season
season3 <- all.seasons[all.seasons$season == 3,] # post-hunt season
season1$bearID <- droplevels(season1$bearID)
season2$bearID <- droplevels(season2$bearID)
season3$bearID <- droplevels(season3$bearID)

# Create empty vectors to store resuults of the eight models
results1 <- vector("list", 3) 
results2 <- vector("list", 3) 
results3 <- vector("list", 3) 
results4 <- vector("list", 3) 
results5 <- vector("list", 3) 
results6 <- vector("list", 3) 
results7 <- vector("list", 3) 
results8 <- vector("list", 3) 

# Create a list of the data of the three different seasons as they will be analyzed using lapply
seasons <- list(season1, season2, season3)
#seasons <- list(season1, season2)

# Execute all eight models for all three seasons
results1 <- lapply(seasons, function(x) mclogit(cbind(case,id) ~ std_elev + Asp + std_slope, random=~1|bearID, start.theta=0.5, data=x))
results2 <- lapply(seasons, function(x) mclogit(cbind(case,id) ~ Huntable + std_elev + Asp + std_slope, random=~1|bearID, start.theta=0.5, data=x))
results3 <- lapply(seasons, function(x) mclogit(cbind(case,id) ~ HousingDensity + std_elev + Asp + std_slope, random=~1|bearID, start.theta=0.5, data=x))
results4 <- lapply(seasons, function(x) mclogit(cbind(case,id) ~ Landcover + std_elev + Asp + std_slope, random=~1|bearID, start.theta=0.5, data=x))
results5 <- lapply(seasons, function(x) mclogit(cbind(case,id) ~ Landcover + Huntable + HousingDensity, random=~1|bearID, start.theta=0.5, data=x))
results6 <- lapply(seasons, function(x) mclogit(cbind(case,id) ~ Huntable, random=~1|bearID, start.theta=0.5, data=x))
results7 <- lapply(seasons, function(x) mclogit(cbind(case,id) ~ HousingDensity, random=~1|bearID, start.theta=0.5, data=x))
results8 <- lapply(seasons, function(x) mclogit(cbind(case,id) ~ Landcover, random=~1|bearID, start.theta=0.5, data=x))
results.all <- list(results1, results2, results3, results4, results5, results6, results7, results8)

# Use the model.sel function in the MuMIN library to evalute the strengths of all eight models for all three seasons
library(MuMIn)
season1.model <- model.sel(list(results1[[1]], results2[[1]], results3[[1]], results4[[1]], results5[[1]], results6[[1]], results7[[1]], results8[[1]]), rank = AIC)
season2.model <- model.sel(list(results1[[2]], results2[[2]], results3[[2]], results4[[2]], results5[[2]], results6[[2]], results7[[2]], results8[[2]]), rank = AIC)
season3.model <- model.sel(list(results1[[3]], results2[[3]], results3[[3]], results4[[3]], results5[[3]], results6[[3]], results7[[3]], results8[[3]]), rank = AIC)
season1.model
season2.model
season3.model
####################################################################################################################
# The following code allows you to viewthe results of specific models and estimates and CI of the different variables
library(memisc)
sesaon1.mod1 <- mclogit(cbind(case,id) ~ Landcover + std_elev + Asp + std_slope, random=~1|bearID, start.theta=0.5, data=season1)
sesaon2.mod1 <- mclogit(cbind(case,id) ~ Landcover + std_elev + Asp + std_slope, random=~1|bearID, start.theta=0.5, data=season2)
sesaon3.mod1 <- mclogit(cbind(case,id) ~ Landcover + std_elev + Asp + std_slope, random=~1|bearID, start.theta=0.5, data=season3)
sesaon3.mod2 <- mclogit(cbind(case,id) ~ Landcover + Huntable + HousingDensity, random=~1|bearID, start.theta=0.5, data=season3)

getSummary(sesaon1.mod1)
summary(sesaon1.mod1)
getSummary(sesaon2.mod1)
summary(sesaon2.mod1)
getSummary(sesaon3.mod1)
summary(sesaon3.mod1)
getSummary(sesaon3.mod2)
summary(sesaon3.mod2)