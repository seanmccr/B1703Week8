# ----- B1703 Week 8 | Advanced Plots in R | 06.03.2024 -----

# ----- 1. Loading Data -----
library(tidyverse)
ATP <- read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1703/Week 8 /Practical/ATP.csv")

# ----- 2. Ensuring Date Formatting -----
ATP$ranking_date<- as.character(ATP$ranking_date)
ATP$ranking_date<- as.Date(ATP$ranking_date,format="%Y%m%d")

# ----- 3. Create No.1 Subset -----
ATPLeaders <- subset(ATP, rank==1)
paste("Number of world leaders: ",n_distinct(ATPLeaders$player))

# ----- 4. Creating Start_End Variable -----
ATPLeaders <- ATPLeaders[order(ATPLeaders$ranking_date),] #This step is important, we need the dates in order.

ATPLeaders <- ATPLeaders %>%
  mutate(Start_End=if_else(player !=lag(player),"Start", # #If you haven't come across lead(), this function takes the value in the next cell of the relevant column. Lag() does the opposite. 
                           if_else(player !=lead(player), "End",""))) #this line examines whether the next row's player is the same to the current row, if not it will assign "End" to the Start_End column.

ATPLeaders[1,19]<-"Start" #I ensure that the first line also receives a Start or it will be an incomplete period

# ----- 5. Remove Rows that aren't start or end period -----
ATPLeaders <- subset(ATPLeaders, Start_End== "Start" | Start_End=="End")

# ----- 6. Adjust ATPLeaders2 so the start and end dates are in separate columns and all players have only 1 row per period. -----
ATPLeaders <- ATPLeaders %>%
  mutate(Start=ranking_date,
         End=lead(ranking_date)) %>% 
  subset(Start_End=="Start") 
# I then remove the Ends as we have all relevant information in the first line (and in fact the information in the End line is incorrect)

ATPLeaders[42,21]<-today() #ensure that the last period has an end date

ATPLeaders<-ATPLeaders[order(ATPLeaders$player, ATPLeaders$Start),] 

ATPLeaders <- ATPLeaders %>%
  group_by(player) %>%
  mutate(period=row_number(),
         Duration = difftime(End, Start, units="weeks")) #I give each individual period a number per player. This allows us to see how many times they have been at the top.

# ----- 7. Create a gantt chart using geom_segment? -----
Gantt<-ATPLeaders %>%
  mutate(name_last=as.factor(name_last)%>%
           fct_reorder(Start, min)) %>%
  ggplot() + 
  geom_segment(aes(x=Start, xend=End, y=name_last, yend=name_last), colour="orange", linewidth=5) +
  xlab("Year")+
  theme_minimal()

Gantt


# ----- 8. Add Rectangle Highlights for Big 4 Period -----

startFed<-filter(ATPLeaders, name_last == "Federer" & period == 1) #Federer started the big 4 era so we want to know when he led the world ranks first
endDok <- filter(ATPLeaders, name_last == "Djokovic" & period == 5) #Djokovic ended the big 4 era so we want to the last period he led before another player outside of the big 4 took over (which is period 5)

Gantt2<- Gantt + 
  geom_rect(aes(xmin=startFed$Start, xmax=endDok$End, ymin=0, ymax=Inf), fill="lightblue", alpha=0.01)+
  ylab(label="")

Gantt2

# ----- 9. Lines denoting start and end of big 4 era; player career length; sections they were world no.1 -----

Leaders<-c(ATPLeaders$player) # Creating a vector with player id's for all player's who have led the ranks at one point in time.

ATPL<-filter(ATP, ATP$player==Leaders) #filter original data for just the world leading players using the vector created in the line above

ATPL<-ATPL[order(ATPL$player, ATPL$ranking_date),] #important to order on player and ranking_date, not doing this would provide us with the wrong timeperiods.

ATPL <- ATPL %>%
  mutate(Start_End=if_else(player !=lag(player),"Start",
                           if_else(player !=lead(player), "End",""))) #This bit of code looks for the start of a players ATP career and the end.

ATPL[1,19]<-"Start" #Ensure that the first line in the data frame also receives a Start or it will be an incomplete period

ATPL <- subset(ATPL, Start_End== "Start" | Start_End=="End") #Remove all entries between the start and end of a players career

ATPL <- ATPL %>%
  mutate(Start=ranking_date,
         End=lead(ranking_date)) %>% 
  # remember when using geom_segment we want the two time points in two seperate columns, this is what happens here.
  subset(Start_End=="Start") 
#Remove the rows which have "End" as we have all relevant information in the rows with "Start" (and in fact the information in the End line is incorrect)

ATPL[13,21]<-today() #ensure that the last line in the data frame has an end date

ATPL<-ATPL[order(ATPL$player, ATPL$Start),] 

Gantt3<-ATPLeaders %>%
  mutate(name_last=as.factor(name_last)%>%
           fct_reorder(Start, min)) %>%
  ggplot() + 
  geom_segment(aes(x=Start, xend=End, y=name_last, yend=name_last, colour="orange"), size=5) + 
  xlab("Year")+
  theme_minimal() + 
  # above I recreate the original gantt plot but in this bit of code you will have seen colour has been added to the aes() part. This is to ensure we can use it as a label later (in the original gantt it was outside of aes() which means no label was assigned to it)
  
  #below the years played is added to the gantt
  geom_segment(data=ATPL, aes(x=Start, xend=End, y=name_last, yend=name_last, colour="lightblue"), size=5,alpha=0.4)+
  ylab(label=NULL)+ #removing the y-axis label
  xlab(label=NULL)+ #removing the x-axis label
  scale_color_manual(values = c(lightblue = "lightblue", orange = "orange"),
                     labels = c(lightblue = "Years Played", orange = "World lead"))+ # this is were the legend gets created
  labs(colour="", title="ATP leaders")+ #We don't want a legend title hence colour="". 
  theme(plot.title = element_text(hjust=0.5))+
  geom_vline(ATPL, xintercept = startFed$Start, linetype=3)+ #adding the reference lines using the start of federers career and the end of djokovic career as xintercepts.
  geom_vline(ATPL, xintercept=endDok$End, linetype=3)+
  annotate("text", label="Start big
           4 era", x=startFed$Start+700, y=1.5, size=4, colour="darkgrey")+ #Adding annotations. The exact x and y placement of the annotations is always a bit of trial and error.
  annotate("text", label="End big 
           4 era", x=endDok$Start-365, y=13, size=4, colour="darkgrey")
Gantt3


# ----- Radar Charts -----

# ----- 10. Sorting Big4 ----- 
BigFour <- ATPL %>%
  filter(!is.na(service_games_won))

# ----- 11. Sorting for Agassi and Federer -----
library(fmsb)
BigFour2 <- BigFour %>%
  filter(name_last=="Agassi" | name_last=="Federer") %>%
  select(name_last, "X1st_serve_won", "X2nd_serve_won", service_games_won, return_games_won, break_points_converted, break_points_saved)
colnames(BigFour2) <- c("name", "X1st_serve_won", "X2nd_serve_won", "service_games_won", "return_games_won", "break_points_converted", "break_points_saved")

#add the max and min values
BigFour2 <- rbind(rep(100,7), rep(0,7), BigFour2)

#print simple radar chart
radarchart(BigFour2[,c(2:7)])

# ----- 12. Creating Nicer Chart -----
#we assign row names to a vector which we will use later to create a legend.
rowname<-BigFour2[c(-1,-2),1]

labels<-c(colnames(BigFour2[,c(2:7)])) # we will use these for our variable labels.

# We will use this function, which includes a set of basic settings for a nice radar chart. You can adjust the settings but having a function ready saves a lot of formatting if you regularly create radar charts.

Radar_chart<- function(data, color = "lightblue", vlabels = colnames(data), vlcex = 0.7, caxislabels = NULL, title = NULL, ...){ #this line contains all the input we can give later on. Below this line is the function code.
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.4), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

op <- par(mar = c(2,0.5,2,0.5)) #this sets the margins and determines how your labels are displayed. Bit of trial and error to get it right.

Radar_chart(BigFour2[,c(2:7)], caxislabels = c(0, 25, 50, 75, 100), title= "Agassi vs Federer", color= c("orange", "lightblue"), vlabels=labels) #we use the function created and add relevant input.
legend(x=1, y=0.4, legend=rowname, horiz=FALSE, bty="n", pch=20, col=c("orange", "lightblue"), text.col="black", cex=1, pt.cex=1.5) # add the legend, make sure your colours are matching the ones in the radar_chart function.


# ----- 13. Charts for All 4 + Agassi -----

# Define colors and titles
BigFourAll <- BigFour %>%
  select(name_last, "X1st_serve_won", "X2nd_serve_won", service_games_won, return_games_won, break_points_converted, break_points_saved)
colnames(BigFourAll) <- c("name", "X1st_serve_won", "X2nd_serve_won", "service_games_won", "return_games_won", "break_points_converted", "break_points_saved")

#calculating minimums of group.
col_min <- BigFourAll %>%
  summarise(name="",
            "X1st_serve_won"=min(BigFourAll$"X1st_serve_won", na.rm=TRUE),
            "X2nd_serve_won"=min(BigFourAll$"X2nd_serve_won", na.rm=TRUE),
            service_games_won=min(BigFourAll$service_games_won, na.rm=TRUE),
            return_games_won=min(BigFourAll$return_games_won, na.rm=TRUE),
            break_points_converted=min(BigFourAll$break_points_converted, na.rm=TRUE),
            break_points_saved=min(BigFourAll$break_points_saved, na.rm=TRUE))

BigFourAll <- rbind(rep(100,7), rep(0,7), col_min, BigFourAll)

titles <- c(BigFourAll[c(4:8),1])


# Split images in two rows of 3.
par(mar = rep(0.8,4)) #adjust margins
par(mfrow = c(2,3)) #split images

# Create the radar chart. Note we use the normal radarchart function not the function we created earlier as we will need to adjust some of the settings
for(i in 1:5){
  radarchart(
    BigFourAll[c(1:3, i+3),c(2:7)], #the radarchart functions always takes row 1 and 2 to plot the radar min and max values. Anything after that will be plotted as data onto the radar. In this case we tell the chart to take row 3 (the minimum) and plot those in grey on avery chart and also plot the value for each player in red.
    pfcol = scales:: alpha("lightblue",0.4),
    pcol= c(NA,"orange"), plty = 1, plwd = 2,
    title = titles[i]
  )
}

# ----- Slope Graphs -----
# ----- 14. Downloading Libraries and Data ----
PL <- readxl::read_xlsx("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1703/Week 8 /Practical/PLHistorical.xlsx")
library(CGPfunctions)

# ----- 15. Creating Slope Graph -----
PL<-PL[order(PL$Club),]

PL$Year <- as.factor(PL$Year)
SlopeChart<-PL %>%
  group_by(Club) %>%
  filter(n()>1) %>% # Filter to ensure those religated or promoted in 2012 are not included (they will only have 1 datapoint)
  newggslopegraph(Year, Points, Club, Title=NULL, SubTitle = NULL, Caption=NULL, ThemeChoice = "bw", RemoveMissing=TRUE) 

SlopeChart

# ----- 16. Add Percent Change Column -----

PL<-PL[order(PL$Club),]

PL<- PL %>%
  group_by(Club) %>%
  mutate(PerChange=if_else(Year=="2012", ((lag(Points)- Points)/(Points))*100, NA))

#Identify most improved and most declined club.
Top <- as.vector(max(PL$PerChange, na.rm=TRUE))
Top<-PL%>%
  filter(Year=="2012" & PerChange == Top[[1]])%>%
  select(Club)
Top<-as.vector(Top[['Club']])

Bottom <- as.vector(min(PL$PerChange, na.rm=TRUE))
Bottom<-PL%>%
  filter(Year=="2012" & PerChange == Bottom[[1]])%>%
  select(Club)
Bottom<-as.vector(Bottom[['Club']])

Others<-PL%>%
  filter(Year=="2012" & PerChange != Top[[1]] & PerChange != Bottom[[1]])%>%
  select(Club)
Others<-as.vector(Others[['Club']])


# I will assign the colours to the names of the teams creating a color_pallet vector
color_palette <- c(setNames(rep("green", length(Top)), Top),
                   setNames(rep("red", length(Bottom)), Bottom),
                   setNames(rep("grey", length(Others)), Others))

#I will now enter my color_palette to the newggslopegraph code using LineColor. I will also add a title to the figure and change my x-axis text to grey.
SlopeChart<-PL %>%
  group_by(Club) %>%
  filter(n()>1) %>%
  newggslopegraph(Year, Points, Club, Title=NULL, SubTitle = NULL, Caption=NULL, ThemeChoice = "bw", RemoveMissing=TRUE, LineColor = color_palette) +
  theme(axis.text.x= element_text(colour = "darkgrey"))+
  theme(plot.title= element_text(size=14, face ="bold.italic", hjust = 0.5)) +
  labs(
    title = "Total points in 15th week of competition",
  )

SlopeChart

# ----- 17. Doing this on ggplot2 package -----

library(ggrepel)
#Make the base plot
SlopeChart2 <- ggplot(data=PL,aes(x=Year, y=Points, group=Club))+
  geom_line(aes(color=Club, alpha=1) , size=1)+
  
  # Add the annotations (we use geom_text_repel part of the ggrepel package as this allows us to avoid overlapping labels)
  #Club labels for 2012 (filtering for year 2012 as well as n>1 to avoid including teams who were relegated and only have 1 data point)
  geom_text_repel(data = PL %>% filter(n()>1 & Year == "2012"), 
                  aes(label = Club) , 
                  hjust = "left", 
                  size = 3,
                  nudge_x = -0.45,
                  direction = "y") +
  #Club labels for 2012 (filtering for year 2012 as well as n>1 to avoid including teams who were relegated and only have 1 data point)
  geom_text_repel(data = PL %>% filter(n()>1 & Year == "2013"), 
                  aes(label = Club) , 
                  hjust = "right", 
                  size = 3,
                  nudge_x = 0.5,
                  direction = "y")+
  #Points labels
  geom_label(data = PL %>% filter(n()>1),
             aes(label = Points), 
             size = 3, 
             label.padding = unit(0.05, "lines"), 
             label.size = 0.0)+
  #add x-scale to top
  scale_x_discrete(position = "top") +
  theme(axis.text.x=element_text(face="bold"))+
  #add the color_pallete previously created so the top and bottom performing are red and green
  scale_color_manual(values = color_palette) 

SlopeChart2 

#The figure above still has a lot of non-data ink and legends which aren't required. 

# Remove almost all non-data ink
SlopeChart2<- SlopeChart2 +  theme(legend.position = "none")+ 
  theme(panel.border     = element_blank()) +
  theme(axis.title.y     = element_blank()) +
  theme(axis.text.y      = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.title.x     = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x.top      = element_text(size=12)) +
  theme(axis.ticks       = element_blank()) +
  theme(plot.title       = element_text(size=14, face ="bold", hjust = 0.5)) +
  theme(plot.subtitle    = element_text(hjust = 0.5)) +
  
  #  Labelling as desired
  labs(
    title = "Total points in 15th week of competition",
  )

SlopeChart2











