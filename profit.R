

product <- read.csv("products.csv")
transaction <- read.csv("edited_transaction.csv")
sale_daily <- read.csv("daily sale.csv")

day_list = unique(transaction$day)

# Date when new week starts
index_vector <- vector(length=52)
i = 1
j = 1
while (i<365){
  index_vector[j] = i
  i = i+7
  j = j+1
}


# Time series : daily and weekly profit
# Daily

purchase_daily <- vector()

for (i in day_list){
  product_id = transaction[transaction$day == i,][c("product_id")] # Get product Id from daily transaction
  purchase_day = vector() # Purchase price that supermarket buy from suppliers.
  
  for (j in product_id$product_id){
    purchase_day = c(purchase_day, product[product$product_id == j,]$purchase_price)
  }
  
  purchase_daily = c(purchase_daily, sum(purchase_day, na.rm=TRUE))
}
purchase_daily

profit_daily = sale_daily$sale - purchase_daily 
profit_daily

plot(profit_daily,type = "line", col = "red", xlab = "day", ylab = "Profit(euro)", main = "Daily profit over time")
abline(h=mean(profit_daily), col="blue")
legend(x=280,y=2060,legend="Mean profit",cex=.8,col=c("blue"),lty=c(1))
  
#Weekly
profit_week = vector(length=52)
for (i in 1:52){
  week_start = index_vector[i]
  week_end = week_start+6
  profit_week[i] = sum(profit_daily[week_start:week_end])
}

plot(profit_week,type = "o", col = "red", xlab = "week", ylab = "Profit(euro)", main = "Weekly profit over time")
abline(h=mean(profit_week), col="blue")
legend(x=35,y=10000,legend="Mean profit",cex=.8,col=c("blue"),lty=c(1))

# No seasonality or trend in daily and weekly profit
# Daily profits can be fitted to AR model

# Distribution : histogram of weekly sales & profit
hist(profit_daily, main="Daily profit", xlab="Profit(euro)", col="blue")
hist(profit_week, main="Weekly profit", xlab="Profit(euro)", col='blue')


