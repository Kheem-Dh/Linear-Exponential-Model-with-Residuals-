x=1:20
y=c(1, 3, 5, 7, 9, 12, 15, 19, 23, 28, 33, 38, 44, 50, 56, 64, 73, 84, 97, 113)
plot(x,y)
lines(x, y, col="black", lty=3)
#fit the model
model <- lm(log(y)~ x)

#view the output of the model
summary(model)

#ln(y) = 0.9817 + 0.2041(x)
#Applying e to both sides, we can rewrite the equation as:
yval = 2.6689 * 1.2264^x
points(x, yval, col='red', pch='+')
lines(x, yval, col="dark red", lty=3)
