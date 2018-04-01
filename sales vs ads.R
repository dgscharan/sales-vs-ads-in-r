#advertising for 30 companies with mean of 400 and standard deviation of 200
ads <- rnorm(30,400,200)
#sales of 30 companied with mean of 4000 and sd of 1000
sales <- rnorm(30,4000,1000)

plot(sales~ ads , main = "sales vs ads" ,
     xlab = "ads cost" , ylab = "sales in 1000's" ,
     sub = "data for 30 companies" , 
     col = "red")
fit <- lm(sales~ads)
abline(fit) #treand_line_denotes_the_growth_of_the_chart
 
#classifiying the companies into manufacturing ,services , retail
sector <- sample(1:3 , 30 , replace = TRUE)
sector2 <- factor(sector , levels = c(1,2,3),
                  labels = c("manufactuing" , "services" ,"retail") )
sector2



#plotting graph for each sector
plot(sales~ads , col = sector2 , pch = 12)

legend(x= 800 , y = 2000 , legend = levels(sector2) , col = 1:3 ,lty = "solid" , cex = 1 )

#analysing the profit for 10 years(2007-2016)
years <- 2007:2016
profita <- c(54,99,66,84,133,167,155,149,171,188)
profitb <- c(99,78,65,97,148,166,172,175,177,178)
profitc <- c(66,49,88,77,96,112,119,125,140,142)
plot(profita~years , typr = "l" , lwd = 5)
lines(profitb~years, type = "l" , col = "red" , lwd =4)
lines(profitc~years , type = "l" , col = "blue" , lwd =4)

#treand line for the profit chart
fit <- lm(profit~years)
abline(fit)

plot(profit~years , type = "s")
