# script to make things... like pictures

library(ggplot2)
library(viridis)
library(tidyverse)
library(plyr)
library(cowplot)
library(lubridate)
library(ggraph)

setwd("../data")

atr <- read_csv("Allocation_Trades.csv")
atr <- as.data.frame(atr)

#####
# Trends in trading over time: intra-week, inter-week, and yearly
#####

atr$date_of_approval <- dmy(atr$date_of_approval)

atr <- atr %>% mutate( day=day(date_of_approval),
					   month=month(date_of_approval),
					   year=year(date_of_approval)) %>%
				mutate( dow=wday(date_of_approval, label=TRUE),
						woy=week(date_of_approval))

atr_weekly <- ddply(atr, c("woy","month","year"), summarise, mean_q=mean(quantity_traded))

fig1a <- ggplot(data=atr_weekly, aes(x=woy, y=mean_q) ) + 
	#geom_smooth(method="loess", alpha=0.3) +
	geom_point(aes(color=as.factor(year)), size=1) + 
	scale_color_viridis(discrete=TRUE) +
	scale_x_continuous(breaks = round( seq(1,52,by=4) ) ) +
	guides(color=FALSE) +
	xlab("Week of year") + ylab("Mean ML traded") + ggtitle("Weekly water trading") +
	theme_bw()
# Inter-week trends across years. Seems like trading picks up around July -- probably coinciding with the dry season. There's some more variance after July, probably fewer rainfall "supply shocks". The largest trade seems to have occurred around July 2014.

fig1b <- ggplot(data=atr, aes(x=year, y=log10(quantity_traded)) ) + 
	geom_jitter(aes(color=as.factor(year)), size=0.5, height=0) + 
	scale_color_viridis(discrete=TRUE) +
	scale_y_continuous(trans="log10") +
	scale_x_continuous(breaks=NULL) +
	facet_grid(cols=vars(dow),switch='x') +
	guides(color=FALSE) +
	xlab("") + ylab("Log10 ML traded") + ggtitle("Daily water trading") +
	theme_bw()
# Intra-week trends across years. This graph seems more informative than the inter-week trend plot.
# (a) Trading is lower on the weekends than during the week, but weekend trades tend to be larger than 10 megaliters (log10(1) ML). 
# (b) Seems like 2010-2011 saw a big difference in rainfall patterns, either more or less. If they have fewer observations than the other years, then perhaps less rainfall -- small trades weren't worthwhile (evaporative loss? another "iceberg" trade cost?). If they have roughly the same number of observations, perhaps more rainfall -- more water flowing, so larger trades?

# Overall, both graphs support the story that trading was thin in the early years.

fig1_left <- plot_grid(fig1a, fig1b, align=c("v"), axis=1, ncol=1, rel_heights=1, labels=c("a","b"))

#dummy fig1a with legend to extract
fig1a_hack <- ggplot(data=atr_weekly, aes(x=woy, y=mean_q) ) + 
	geom_point(aes(color=as.factor(year)), size=1.5) + 
	scale_color_viridis(discrete=TRUE) +
	labs(color="Year") +
fig1_legend <- get_legend(fig1a_hack)

( fig1 <- plot_grid(fig1_left, fig1_legend, align=c("h"), axis=1, ncol=2, rel_widths=c(0.85,0.15), labels=c("","")) )

png("../images/trading_over_time.png",width=700,height=525)
fig1
dev.off()

#####
# price-quantity analysis
#####

atr <- atr %>% mutate( rainy_season=(month==12|month<=3) ) %>% 
			mutate( govt_trade=(net_price==0) ) %>%
			mutate( moy=month(date_of_approval, label=TRUE) )

atr_monthly <- ddply(atr, c("moy","year","rainy_season","govt_trade"), summarise, mean_q=mean(quantity_traded))

govt_trade_labels <- c("Govt trade", "Non-govt trade")
names(govt_trade_labels) <- c("TRUE","FALSE")

 fig2a <- ggplot(data=atr_monthly, aes(x=moy, y=mean_q) ) + 
	geom_point(aes(color=as.factor(year)), size=1.25) + 
	scale_color_viridis(discrete=TRUE) +
	#scale_x_continuous(breaks = round( seq(1,52,by=4) ) ) +
	guides(color=FALSE) +
	facet_grid(rows=vars(govt_trade), labeller=labeller(govt_trade=govt_trade_labels)) +
	#geom_vline(xintercept=11.5) +
	xlab("Month") + ylab("Mean ML traded") + ggtitle("Monthly water trading") +
	theme_bw() 
 fig2b <- ggplot(data=atr[atr$govt_trade==FALSE,], aes(x=log10(quantity_traded),y=log10(price_per_ML))) +
		geom_jitter(aes(group=as.factor(year),color=as.factor(year)), size=0.45, alpha=0.8) +
		scale_color_viridis(discrete=TRUE) +
		scale_y_continuous(trans="log10") +
		scale_x_continuous(trans="log10") +
		guides(color=FALSE) +
		xlab("log10(ML traded)") + ylab("log10(Price per ML)") + ggtitle("Non-govt trades") +
	theme_bw() 

( fig2 <- plot_grid(fig2a, fig2b, fig1_legend, align=c("h"), axis=1, ncol=3, rel_widths=c(0.45,0.45,0.1), labels=c("a","b","")) )

png("../images/govt_vs_nongovt_trades.png",width=700,height=525)
fig2
dev.off()

#####
# Trading map/pairs analysis: see which regions send the most out or take the most in, see which states/regions trade the most with each other, and what directions the flows go
#####
