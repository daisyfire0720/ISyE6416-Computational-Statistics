#IsyE6416 Homework 4 Question 4
library(ggplot2)
copper_data = read.table("copper-new.txt")
names(copper_data) = c("coeff","temp")
View(copper_data)
#fit into a linear regression model
fit_copper = lm(coeff~temp,data = copper_data)
summary(fit_copper)
#calculate the fitting error
error = copper_data$coeff - fitted(fit_copper)
copper_error = data.frame(copper_data$temp,error)
names(copper_error) = c("temp","error")
avg_error = sum(error^2)/nrow(copper_data)
avg_error
#plot the coefficient-temperature relationship
pic1 = ggplot(aes(x = temp, y = coeff),data = copper_data) + geom_point()
pic1 = pic1 + stat_smooth(method = "lm", formula = y ~ x, color = "red")
pic1 = pic1 + labs(x = "Temperature", y = "Coefficient of Thermal Expansion", title = "Coefficient-Temperature Relationship", size = 12, face = "bold", family = "serif")
pic1 = pic1 + theme(plot.title = element_text(hjust = 0.5))
pic1
#plot the error-temperature relationship
pic2 = ggplot(aes(x = temp, y = error),data = copper_error) + geom_point()
pic2 = pic2 + labs(x = "Temperature", y = "Fitting Error", title = "Error-Temperature Relationship", size = 12, face = "bold", family = "serif")
pic2 = pic2 + theme(plot.title = element_text(hjust = 0.5))
pic2
#prepare the data
coeff_data = copper_data$coeff
temp_data = copper_data$temp
#fit into a spline non-linear regression
fit_copper_spline = smooth.spline(temp_data,coeff_data, cv = TRUE)
fit_copper_spline
#plot the fitted spline
optimal_fit_spline = smooth.spline(temp_data,coeff_data,spar = fit_copper_spline$spar)
plot(copper_data$temp,copper_data$coeff)
lines(optimal_fit_spline,col = "red")
#calculate the fitting error
predict_coeff = predict(optimal_fit_spline)$y
spline_error = (sum((copper_data$coeff - predict_coeff)^2))/(fit_copper_spline$df)
spline_error
#predict a specific coefficient using optimal spline
predict(optimal_fit_spline,400)$y
