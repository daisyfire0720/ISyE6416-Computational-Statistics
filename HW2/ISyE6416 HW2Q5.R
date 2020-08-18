#ISyE 6416 Homework 2
#House Price Dataset
realestate.df = read.csv("RealEstate.csv")
View(realestate.df)
realestate_shortsale.df = realestate.df[which(realestate.df$Status == "Short Sale"),]
View(realestate_shortsale.df)
realestate_foreclosure.df = realestate.df[which(realestate.df$Status == "Foreclosure"),]
View(realestate_foreclosure.df)
realestate_regular.df = realestate.df[which(realestate.df$Status == "Regular"),]
View(realestate_regular.df)

install.packages("tidyverse")
library(tidyverse)

lm(Price ~ Bedrooms+Bathrooms+Size+Price.SQ.Ft+factor(Location),data = realestate_shortsale.df) %>% summary
lm(Price ~ Bedrooms+Bathrooms+Size+Price.SQ.Ft+factor(Location),data = realestate_foreclosure.df) %>% summary
lm(Price ~ Bedrooms+Bathrooms+Size+Price.SQ.Ft+factor(Location),data = realestate_regular.df) %>% summary
