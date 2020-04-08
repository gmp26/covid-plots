###
# Deaths: Data tidying (Carys)
###

# Strip out countries that are NA or less than 10 for the most recent report
data <- read.csv("https://covid.ourworldindata.org/data/ecdc/total_deaths.csv")
dates <- data[, 1]
today <- dim(data)[1]
not_date <- data[, -1]
tnot_date <- t(not_date)
tnot_date <- subset(tnot_date, tnot_date[, today] > 10, na.rm = T)
ttnot_date <- t(tnot_date)
df <- as.data.frame(cbind(as.character(dates), ttnot_date))
colnames(df)[1] <- "date"
countries <- colnames(df[2:dim(df)[2]])
num.countries <- dim(df)[2] - 1
days <- dim(df)[1]
df$date <- as.Date(as.character(df$date), "%Y-%m-%d")

for (i in 2:num.countries) {
  df[, i] <- as.numeric(as.character(df[, i], na.rm = T))
}

# Get daily totals for each country
# ==================================
for (i in 2:(num.countries + 1)) {
  offset <- 2 * i - 2
  daily <- 2 * i - 1
  df[, num.countries + offset] <-
    c(0, as.numeric(as.character(df[1:(days - 1), i])))
  colnames(df)[num.countries + offset] <-
    paste(colnames(df)[i], "offset", sep = ".")
  df[, num.countries + daily] <-
    as.numeric(as.character(df[, i])) - as.numeric(as.character(df[, num.countries +
                                                                     offset]))
  colnames(df)[num.countries + daily] <-
    paste(colnames(df)[i], "daily", sep = ".")
}

# remove the offset columns
# ==========================
matches <- grep("offset$", colnames(df), ignore.case = T)
df <- df[, -c(matches)]
wide <- dim(df)[2]

