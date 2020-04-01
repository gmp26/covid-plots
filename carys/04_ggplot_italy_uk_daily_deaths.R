library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)
library(plotly)

df.no.zero <- read.csv("data/italy_uk_no_zeroes_daily_count.csv")

g <- ggplot(df.no.zero, aes(x = day, y = daily.count, colour = country)) 
g <- g + geom_point()
g <- g + geom_smooth(method = "loess") 
g <- g + scale_y_continuous(trans=log2_trans())
g <- g + theme_bw()
g <- g + ylab("Daily reported deaths")
g <- g + xlab("Number of days since a total of ten deaths were reported")
g <- g + theme(legend.title=element_blank())
g <- g + scale_colour_discrete(labels=c("Italy from 26th Feb", "UK from 13th March"))
g <- g + theme(legend.position=c(0.8,0.1))
g <- g + ggtitle("Daily reported deaths in Italy and UK\nloess smoothed fit and standard error in grey")
g
#ggsave("plots/02_italy_uk_no_zeroes_daily_death_count.pdf")

ggplotly(g)