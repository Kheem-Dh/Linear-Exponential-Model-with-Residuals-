library(boot)
library(openintro)
library(ggplot2)
par(mfrow=c(1,1))

View(helmet)
plot(helmet$lunch,helmet$helmet)

h_mean = mean(helmet$helmet)
l_mean = mean(helmet$lunch)
abline(h=h_mean)
abline(v=l_mean)


par(mfrow=c(1,2))
error_y = data.frame(intercept="", error="")
for (i in seq(from=1, to = 100, by=3)) {
  plot(helmet$lunch,helmet$helmet)
  grid()
  y_intercept = i
  slope = (y_intercept-h_mean)/(0-l_mean)
  abline(y_intercept, slope, col='red')
  y_bar = y_intercept + slope*helmet$lunch
  error = sum((helmet$helmet-y_bar)^2)
  print(paste(y_intercept,round(error,2), sep=" ---- "))
  error_y = rbind(error_y,c(y_intercept,error))
  plot(error_y, col='blue')
  grid()
  Sys.sleep(0.5)
}
plot(helmet$lunch,helmet$helmet)
abline(h=h_mean)
