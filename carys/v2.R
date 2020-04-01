# With the same data frame as before:
  
  df <- subset(df.no.zero, day >=26 | (day >= 10 & country == "UK.daily"))
  df$dminus <- ifelse(df$day > 25, df$day - 35, df$day - 19) 
  
  g <- ggplot(df, aes(x = dminus, y = daily.count, colour = country)) 
  g <- g + geom_point()
  g <- g + geom_smooth(method = "glm" , se = T, method.args = list(family = "poisson"))
  g <- g + scale_x_continuous(breaks=seq(-10, 0, 1))
  g <- g + theme_bw()
  g <- g + ylab("Daily reported deaths")
  g <- g + xlab("Days before 31st March")
  g <- g + theme(legend.title=element_blank())
  g <- g + scale_colour_discrete(labels=c("Italy", "UK"))
  g <- g + theme(legend.position=c(0.9,0.1))
  g <- g + ggtitle("Daily reported deaths in Italy and UK\nReporting period midday 22nd - 31st March\nPoisson smoothed fit and standard error in grey")
  g
  
  ggplotly(g)