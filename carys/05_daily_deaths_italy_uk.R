# load libraries
library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)

# import the data
input <- read.csv("https://covid.ourworldindata.org/data/ecdc/total_deaths.csv")

# firstly strip out countries that are NA or less than 10 for the most recent report
# ===================================================================================
dates <- input[,1]
today <- dim(input)[1]
not_date <- input[,-1]
tnot_date <- t(not_date)
tnot_date <- subset(tnot_date, tnot_date[,today] > 10, na.rm = T)
ttnot_date <- t(tnot_date)
df <- as.data.frame(cbind(as.character(dates), ttnot_date))
colnames(df)[1] <- "date"
countries <- colnames(df[2:dim(df)[2]])
num.countries <- dim(df)[2] - 1
days <- dim(df)[1]
df$date <- as.Date(as.character(df$date), "%Y-%m-%d")

for(i in 2:num.countries){
	df[,i] <- as.numeric(as.character(df[,i], na.rm = T))
}

# Get daily totals for each country
# ==================================
for(i in 2:(num.countries+1)){
	offset <- 2*i - 2
	daily <- 2*i - 1
	df[,num.countries+offset] <- c(0, as.numeric(as.character(df[1:(days-1),i])))
	colnames(df)[num.countries+offset] <- paste(colnames(df)[i],"offset", sep=".")
	df[,num.countries+daily] <- as.numeric(as.character(df[,i])) - as.numeric(as.character(df[,num.countries+offset]))
	colnames(df)[num.countries+daily] <- paste(colnames(df)[i],"daily", sep=".")
}

# remove the offset columns
# ==========================
matches <- grep("offset$", colnames(df), ignore.case = T)
df <- df[,-c(matches)]
wide <- dim(df)[2]

# create twp data frames, in the correct format for ggplot, for cumulative and daily counts
# ==========================================================================================
cum <- df[,1:(num.countries+1)]
daily <- df[,c(1,(num.countries+2):wide)]

cum.long <- gather(cum, country, cumulative.count, World:United.States, factor_key=TRUE)
cum.long <- subset(cum.long, cumulative.count !="NA")
cum.long$cumulative.count <- as.numeric(as.character(cum.long$cumulative.count))
daily.long <- gather(daily, country, daily.count, World.daily:United.States.daily, factor_key=TRUE)
daily.long <- subset(daily.long, daily.count !="NA")
daily.long$daily.count <- as.numeric(as.character(daily.long$daily.count))
daily.long$country <- gsub(".daily", "", daily.long$country)

# Subsetting the data to include the countries of interest
# ==========================================================
include <- unique(daily.long$country)
interest <- c("Italy", "United.Kingdom")

# Creating parameters to set the width of the confidence intervals
# ==================================================================
today <- max(daily.long$date)
window <- 10 # looking at a window of 10 days
gap <- 16 # assuming UK is 16 days behind Italy

# Subset to only include days with a count greater than zero
# ===========================================================
over.zero.daily <- subset(daily.long, daily.count > 0)

# create plot window (for mac)
# quartz(width = 20, height = 10)
# quartz(width = 10, height = 5)


# Plot
# =====
g <- ggplot(subset(over.zero.daily, country %in% interest), 
	aes(x = date, y = daily.count, colour = country)) 
g <- g + scale_colour_manual(values=c("chartreuse1", "red"))
g <- g + geom_point(size = 3)
g <- g + scale_y_continuous(trans=log2_trans())
g <- g + theme_bw()
g <- g + geom_smooth(data = subset(over.zero.daily, country == "Italy" & 
	date > today - (window + gap) &
	date < today - gap ), method = "glm", se = T, method.args = list(family = "poisson"))
g <- g + geom_smooth(data = subset(over.zero.daily, country == "United.Kingdom" & 
	date > today - window), method = "glm", se = T, method.args = list(family = "poisson"))
g

