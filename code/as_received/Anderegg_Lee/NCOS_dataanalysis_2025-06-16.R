
require(lubridate)
require(tidyverse)
require(calecopal)

mypaldark <- rep(cal_palette("kelp1",n=6, type = "discrete"), each=2)
opacities <- rep(c("","66"), times=3)

mypal <- paste0(mypaldark, opacities)

palette(mypal)
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

### sets default ggplot so that there's no brown grid in the background
ggplot <- function(...) { ggplot2::ggplot(...) + 
    theme(panel.background = element_rect(fill='white', colour='black'), # Make background white and border black
          panel.grid.major = element_blank(),  # Hide major gridlines
          panel.grid.minor = element_blank())  # Hide minor gridlines
}





# load in upper VWC sensor data
upper <- read.csv("/Users/leeanderegg/Desktop/ongoing collabs/NCOS/z6B01375 Upper 14May25-1120.csv", header=T)
upper$Timestamp <-  mdy_hm(upper$Timestamp)
# make it long form for plotting
upperlong <- upper %>% select(-Bat_percent,-mV.Battery.Voltage) %>% pivot_longer(names_to="Port", cols = c(2:7), values_to = "VWC")
# add in depth
upperlong$depth <- "shallow"
upperlong$depth[which(upperlong$Port %in% c("Port2_VWC","Port4_VWC","Port6_VWC"))] <- "deep"


# load in lower VWC sensor data
lower <- read.csv("/Users/leeanderegg/Desktop/ongoing collabs/NCOS/z6B01383 Lower 14May25-1130.csv", header=T, na.strings = "#N/A")
lower$Timestamp <-  mdy_hm(lower$Timestamp)
# make it long form
lowerlong <- lower %>% select(-Bat_percent,-mV.Battery.Voltage) %>% pivot_longer(names_to="Port", cols = c(2:7), values_to = "VWC")
# add in depth
lowerlong$depth <- "shallow"
lowerlong$depth[which(lowerlong$Port %in% c("Port2_VWC","Port4_VWC","Port6_VWC"))] <- "deep"


# load in Water Potential Data
wp <- read.csv("/Users/leeanderegg/Desktop/ongoing collabs/NCOS/NCOS Water Potentials - 20250220_clean.csv", header=T)
xtabs(~Site+Individual+Species+PD.or.MD, wp)

wp.mean <- wp %>% group_by(Species, PlantID, Site, PD.or.MD) %>% summarise(MPa = mean(Water.Potential, na.rm=T), sd.MPa=sd(Water.Potential, na.rm=T),n = n())

####### Visualizing VWC ##############

ggplot(upperlong, aes(x=Timestamp, y=VWC, col=depth, group=Port)) + geom_line()

ggplot(lowerlong, aes(x=Timestamp, y=VWC, group=Port, col=depth)) + geom_line()


pupper1 <- ggplot(upperlong, aes(x=Timestamp, y=VWC, col=depth, group=Port)) + geom_line()+ ggtitle("Upper")
pupper2 <- ggplot(upperlong, aes(x=Timestamp, y=VWC, col=Port)) + geom_line()

plower1 <- ggplot(lowerlong, aes(x=Timestamp, y=VWC, group=Port, col=depth)) + geom_line() + ggtitle("Lower")
plower2 <- ggplot(lowerlong, aes(x=Timestamp, y=VWC, col=Port)) + geom_line()

multiplot(pupper1, plower1, pupper2, plower2, layout = matrix(c(1,3,2,4), nrow=2))


##### Visualizing Water Potentials ###########
brewer
boxplot(MPa~Species+Site, wp.mean[which(wp.mean$PD.or.MD=="PD"),], col= mypal, las=2, ylim=c(0.2,2.5)
        , whiskcol=mypal, whisklty=1, whisklwd=2
        , staplelty=0
        , boxwex=0.5, at=c(1,1.5,2.5,3,4,4.5)
        , ylab="Water Potential (-MPa)", xlab="", boxlty=0)
boxplot(MPa~Species+Site, wp.mean[which(wp.mean$PD.or.MD=="MD"),]
        , border=mypal, las=2, add=T
        , boxwex=0.5, at=c(1,1.5,2.5,3,4,4.5)
        , whiskcol=mypal, whisklty=1, whisklwd=2,staplelty=0, boxlwd=2, boxfill=NA)

PD <- lm(MPa~Species+Site, wp.mean[which(wp.mean$PD.or.MD=="PD"),])
anova(PD) # nothing really going on here...

MD <- lm(MPa~Species+Site, wp.mean[which(wp.mean$PD.or.MD=="MD"),])
anova(MD) # nothing really going on here...

text(x=c(1,2.5,4)+.25,y = c(2.4,2.4,2.4), labels = c("Control","Upper","Lower"))
