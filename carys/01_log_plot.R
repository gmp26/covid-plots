library(tidyr)
library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
	
	headerPanel("Reported COVID-19 deaths"),
	
	checkboxInput(inputId = "log.scale",
	label = "Log scale",
	value = T),
	
	checkboxInput(inputId = "Italy",
	label = "Show Italy",
	value = T),

	checkboxInput(inputId = "France",
	label = "Show France",
	value = F),

	checkboxInput(inputId = "Germany",
	label = "Show Germany",
	value = F),

	checkboxInput(inputId = "Spain",
	label = "Show Spain",
	value = F),

	checkboxInput(inputId = "United.Kingdom",
	label = "Show the United Kingdom",
	value = T),

	checkboxInput(inputId = "United.States",
	label = "Show the United States",
	value = F),
	
	# checkboxInput(inputId = "day.date",
	# label = "x axis by date rather than day",
	# value = T),
	
	checkboxInput(inputId = "double.3",
	label = "Add slope indicative of deaths doubling every three days (blue)",
	value = F),
	
	checkboxInput(inputId = "double.4",
	label = "Add slope indicative of deaths doubling every three days (black)",
	value = F),

	# conditionalPanel(
	#   condition = "input$day.date == T",
	#   dateRangeInput(
	# 	  inputId = "date.range",
	# 	  label = "Date range",
	# 	  start = "2020-01-01",
	# 	  end = Sys.Date()
	# ),
	
	
# 	ifelse(day.date = T,
# 		(dateRangeInput(
# 			inputId = "date.start",
# 			label = "Choose a range of dates to consider",
# 			min = "2020-01-01", max = Sys.Date()
# 			)),
# #
# # 	# Input() functions
# # 		sliderInput(
# # 			inputId = "day.start",
# # 			label = "Choose a day to start from (minumum is the day with at least 10 cumulative deaths)",
# # 			value = 25, min = 10, max = 60)
# # 			)
#
#
		sliderInput(inputId = "window",
		label = "Choose a number of days to consider",
		value = 10, min = 2, max = 30
		),
# 	)

	

	
	# Output() functions
	plotOutput("outplot")
)

server <- function(input, output){
	output$outplot <- renderPlot({
		
		data <- read.csv("https://covid.ourworldindata.org/data/ecdc/total_deaths.csv")

		# firstly strip out countries that are NA or less than 10 for the most recent report
		# ===================================================================================
		dates <- data[,1]
		today <- dim(data)[1]
		not_date <- data[,-1]
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

		df <- merge(daily.long, cum.long, by = c("date", "country"), all = T)
		df[df==0] <- NA

		# Get days since 01/01/20
		# ========================
		dateseq <- seq(as.Date("2020-01-01"), Sys.Date(), by="days")
		daynumber <- c(1:length(dateseq))
		daydate <- data.frame(date = dateseq, dayofyear = daynumber)

		df <- merge(df, daydate, by = "date")

		# Subsetting the data to include the countries of interest
		# ==========================================================
		countries <- unique(df$country)
		num.countries <- length(countries) # 68 on 04/04/20

		# get offsets
		# ============
		today <- max(df$date)

		# what is the highest numbers of daily deaths for each country?
		max.daily.counts <- as.data.frame(df %>% group_by(country) %>%
		    summarise(max.daily = max(daily.count, na.rm = T)))

		# when were those highest daily counts reported?	
		max.daily.count.dates <- merge(df[,1:3], max.daily.counts,
			 by.x = c("country", "daily.count"), by.y = c("country", "max.daily"))
	 
		# for some, there is more than one date, which is the earliest?	 
		earliest.max.counts <- as.data.frame(max.daily.count.dates %>% group_by(country) %>%
			summarise(first.report.of.max = min(date)))

		# when were those highest daily counts <<first>> reported?		
		first.max.daily.count.date <- merge(max.daily.counts, earliest.max.counts, by = "country")
		colnames(first.max.daily.count.date)[3] <- "first.max.date"

		ref <- "United.Kingdom"
		ref.number.today <- subset(first.max.daily.count.date, country %in% ref)[c("max.daily")]

		below <- subset(df, daily.count <= ref.number.today[[1]])
		belows <- merge(below, first.max.daily.count.date, by = "country")
		under <- subset(belows, date < first.max.date)
		window.end.dates <- as.data.frame(under %>% group_by(country) %>%
		    summarise(window.end.date = max(date)))
		window.end.dates$gap <- today - window.end.dates$window.end.date



		###############################################################
		###############################################################

		# Creating parameters to set the width of the confidence intervals
		# ==================================================================
		today <- max(df$date)
		uk.daily <- subset(df, country == "United.Kingdom")
		uk.today <- max(uk.daily$daily.count, na.rm = T)

		italy.daily <- subset(df, country == "Italy")
		italy.max.daily <- max(italy.daily$daily.count, na.rm = T)
		italy.max.date <- subset(italy.daily, daily.count == italy.max.daily)[1]
		italy.under <- subset(italy.daily, daily.count < uk.today & date < italy.max.date[[1]])
		italy.end.date <- max(italy.under$date)
		italy.gap <- Sys.Date() - italy.end.date # assuming UK is 16 days behind Italy (now 13 days 04/04/20)

		# # For static plot - don't comment this out, it breaks the dynamic plot!
		# # ================
		interest <- c("Italy", "United.Kingdom")
		use <- subset(df, country %in% interest & !is.na(daily.count))
		window <- 10
		log.scale <- T
		date.range <- c(as.Date(min(use$date), origin = "1970-01-01"), Sys.Date())
		double.3 <- F
		double.4 <- F
		
		# # For static plot (countries)
		# # ==============================
		# ctry = c("Italy", "France", "Germany", "Spain", "United.Kingdom", "United.States")
		# input.logic = c(T, F, F, F, T, F)
		# interest.df <- data.frame(ctry, input.logic)
		# interest.df <- subset(interest.df, input.logic == T)
		# interest <- interest.df$ctry
		
		# For dynamic plot (countries)
		# ==============================
		interest.df <- data.frame(ctry = c("Italy", "France", "Germany", "Spain", "United.Kingdom", "United.States"),
			input.logic =c(input$Italy, input$France, input$Germany, input$Spain,
				input$United.Kingdom, input$United.States))
		interest.df <- subset(interest.df, input.logic == T)
		interest <- interest.df$ctry

		# create at dataframe to get exponential curves
		# ==============================================
		begin <- Sys.Date() - window
		exp.dates <- as.Date(seq(from = as.Date(begin), to = as.Date(Sys.Date()), by="days"))
		middle.value <- subset(df, country == ref & date == exp.dates[5])[[3]]
		top.seq2 <- c(middle.value, middle.value*(2^((1:5)/2)))
		bottom.seq2 <- c(middle.value*((2)^(-(1:5)/2)))
		yvalues2 <- unique(c(sort(bottom.seq2), top.seq2))

		top.seq3 <- c(middle.value, middle.value*(2^((1:5)/3)))
		bottom.seq3 <- c(middle.value*(2^(-(1:5)/3)))
		yvalues3 <- c(sort(bottom.seq3), top.seq3)

		top.seq4 <- c(middle.value, middle.value*(2^((1:5)/4)))
		bottom.seq4 <- c(middle.value*((2)^(-(1:5)/4)))
		yvalues4 <- unique(c(sort(bottom.seq4), top.seq4))

		exp.lines <- data.frame(date = exp.dates, double.in.2.days = yvalues2, 
			double.in.3.days = yvalues3, double.in.4.days = yvalues4)

		# Plot
		# =====
		g <- ggplot(subset(df, country %in% interest & !is.na(daily.count)),
			aes(x = date, y = daily.count, colour = country))
		# g <- g + scale_colour_manual(values=c("chartreuse1", "red"))
		g <- g + geom_point(size = 3)
		g <- g + theme_bw()
		
			# elements from input (static)
			# =============================
			
			# window
			# g <- g + geom_smooth(data = subset(df, country == "Italy" &
			# 	date > italy.end.date - window &
			# 	date < italy.end.date ), method = "glm", se = T, method.args = list(family = "poisson"))
			# g <- g + geom_smooth(data = subset(df, country == "United.Kingdom" &
			# 	date > today - window), method = "glm", se = T, method.args = list(family = "poisson"))
	
			# scaling
			# if(log.scale == T){
			# 	g <- g + coord_trans(y = "log10")
			# 	}
			
			# x axis range
			# g <- g + scale_x_date(limits = date.range)
			
			## indicative slopes for doubling rates
			# if(double.3 == T){
			# 	g <- g + geom_line(data = exp.lines, aes(x = date - 3, y = double.in.3.days), inherit.aes = FALSE, colour = "blue")
			# }
			#
			# if(double.3 == T){
			# 	g <- g + geom_line(data = exp.lines, aes(x = date - 3, y = double.in.4.days), inherit.aes = FALSE, colour = "black")
			# }
			
			# elements from input (dynamic)
			# ==============================

			# window
			g <- g + geom_smooth(data = subset(df, country == "Italy" &
				date > italy.end.date - input$window &
				date < italy.end.date ), method = "glm", se = T, method.args = list(family = "poisson"))
			g <- g + geom_smooth(data = subset(df, country == "United.Kingdom" &
				date > today - input$window), method = "glm", se = T, method.args = list(family = "poisson"))

			# scaling
			if(input$log.scale == T){
			g <- g + coord_trans(y = "log10")
			}

			## x axis range
			# g <- g + scale_x_continuous(limits = input$date.range)

			# indicative slope lines
			if(input$double.3 == T){
				g <- g + geom_line(data = exp.lines, aes(x = date - 3, y = double.in.3.days), inherit.aes = FALSE, colour = "blue")
			}

			if(input$double.4 == T){
				g <- g + geom_line(data = exp.lines, aes(x = date - 3, y = double.in.4.days), inherit.aes = FALSE, colour = "black")
			}
			
		print(g)
		
	}) # end of renderPlot funx	
	# ,
	# renderText	
} # end of server funx
		

shinyApp(ui = ui, server = server)
