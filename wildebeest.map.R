library(maptools)
library(dplyr)
library("readxl")


data(wrld_simpl)
afr=wrld_simpl[wrld_simpl$REGION==2,]

# We can visualize the region's boundaries with the plot function
plot(afr,border="gray",lwd=0.5)


data <- read_excel("/home/xiaodong/Downloads/wildebeest.xlsx", sheet = "Sheet1")
head(data)

data2 <-data %>% 
  group_by(Longitude, Latitude) %>% 
  summarise(n=n(),population=Subspecies) %>%
  ungroup %>%
  as.data.frame()

data3 <- data2[!duplicated(data2), ]

points(data3$Longitude,data3$Latitude,pch=16,col=as.numeric(as.factor(data3$population)) , cex=sqrt(data3$n)/2 )

legend('topright',levels(as.factor(data3$population)),col=unique(as.numeric(as.factor(data$Subspecies))), pch=16 )
