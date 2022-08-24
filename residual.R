library(boot)
library(openintro)
library(ggplot2)
par(mfrow=c(1,1))

#A data frame with 12 observations representing neighborhoods on the following 2 variables.
#lunch -Percent of students receiving reduced-fee school lunches.
#helmet -Percent of bike riders wearing helmets.
View(helmet)
plot(helmet)
abline(h=h_mean)
#mean of helmet
h_m = mean(helmet$helmet)
#Could you predict the the percent of children wearing bicycle helmets?
#yes we can use the mean of the helmet as baseline, clearly this will have error
#lets say its 30, How much error this will introduce (y-y')
resids = (helmet$helmet - h_m)
m_residual = sum(resids^2)  # error in this model

#Coefficient of correlation
print(corr(helmet)^2)
#Let me ask you this: How much "error" in the prediction of the percent
#of children wearing bicycle helmets, as measured by the sums of squared
#residual did you decrease or SAVE by using the model over the sample mean
#value?

#try to find the best model
model = lm(helmet$helmet~helmet$lunch)
#squared residuals in the model
model_residual = sum(model$residuals^2)
save_err = m_residual- model_residual
#error saved in context of base error 
save_err/m_residual

ggplot(helmet, aes(x = lunch, y = helmet)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = lunch, yend = model$fitted.values), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(model$residuals), size = abs(model$residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  #  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = model$fitted.values), shape = 1) +
  theme_bw()



