
data <- read.csv("edited_transaction.csv")
inventory <- read.csv("inventory.csv")
promotion <- read.csv("promotions.csv")
products <- read.csv("products.csv")


# Time series model to predict sales
# Date when new week starts
index_vector <- vector(length=52)
i = 1
j = 1
while (i<365){
  index_vector[j] = i
  i = i+7
  j = j+1
}


# List to contain dates of a year
day_list = unique(data$day)


##### Time-series #####
# 1. Daily/weekly total sales 
sale_daily = vector(length=364)
sale_week = vector(length=52)
s = 1

for (i in day_list){
  day_sale = data[data$day == i,]$purchase_price
  sale_daily[s] = sum(day_sale, na.rm=TRUE) # Pass NA 
  s = s+1
}

for (i in 1:52){
  week_start = index_vector[i]
  week_end = week_start+6
  sale_week[i] = sum(sale_daily[week_start:week_start+6])
}

plot(sale_daily,type = "o", col = "red", xlab = "date", ylab = "Total sales",
     main = "Total sales over time")

plot(sale_week,type = "o", col = "red", xlab = "week", ylab = "Total sales",
     main = "Total sales over time")

hist(sale_daily, main='Daily total sales', xlab='total sales')
hist(sale_week, main='Weekly total sales', xlab='total sales')



# Customer number 
nr_of_daily_customer = vector(length=364)

i=1
for (date in day_list){
  daily_customer = unique(na.omit(data[data$day == date,]$customer)) # Exclude NA
  nr_of_daily_customer[i] = length(daily_customer)
  i = i+1
}

nr_of_weekly_customer = vector(length=52)

for (i in 1:52){
  week_start = index_vector[i]
  week_end = week_start+6
  nr_of_weekly_customer[i] = sum(nr_of_daily_customer[week_start:week_end])
}

plot(nr_of_daily_customer,type = "o", col = "red", xlab = "date", ylab = "The number of customers",
     main = "The number of customers over time")

plot(nr_of_weekly_customer,type = "o", col = "red", xlab = "week", ylab = "The number of customers",
     main = "The number of customers over time")

hist(nr_of_daily_customer, main="The number of customers(day)", xlab="The number of customers(day)")
hist(nr_of_weekly_customer, main="The number of customers(week)", xlab="The number of customers(week)")



# 2. # Daily sales per category

category_list = c("vegetable", "snack", "fruit", "meat", "bread")
sale_daily = vector(length=364)
s = 1

for (cate in category_list){
  for (i in day_list){
    day_sale = data[data$day == i |data$category == cate,]$purchase_price
    sale_daily[s] = sum(day_sale, na.rm=TRUE) # Pass NA 
    s = s+1
  }
}


# Weekly sales per category
sale_week_category <- function(cate){
  # cate : name of category
  sale_daily = vector(length=364)
  s = 1
  
  for (i in day_list){
    day_sale = data[data$day == i |data$category == cate,]$purchase_price
    sale_daily[s] = sum(day_sale, na.rm=TRUE) # Pass NA 
    s = s+1
  }
  
  sale_week = vector(length=52)
  
  for (i in 1:52){
    week_start = index_vector[i]
    week_end = week_start+6
    sale_week[i] = sum(sale_daily[week_start:week_start+6])
  }
  
  plot(sale_week, type = "o", col = 'red', xlab = "week", ylab = "Total sales(Euro)", main = sprintf("Total sales(%s) over time", cate)) 
}
sale_week_category("snack")
sale_week_category("meat")
sale_week_category("bread")
sale_week_category("fruit")






# 3. specific item

# Weekely sales of individual item
sales_week <- function(item){
  # item : The name of item(str)
  
  day_list = unique(data$day)
  sale_daily = vector(length=364)
  s = 1
  
  for (i in day_list){
    day_sale = data[data$day == i |data$description == item,]$purchase_price
    sale_daily[s] = sum(day_sale, na.rm=TRUE) # Pass NA 
    s = s+1
  }
  
  sale_week = vector(length=52)
  
  for (i in 1:52){
    week_start = index_vector[i]
    week_end = week_start+6
    sale_week[i] = sum(sale_daily[week_start:week_start+6])
  }
  
  # Found weeks with discount
  discount_week = promotion[promotion$description == item,]$week
  

  plot(sale_week, type = "o", col = 'red', xlab = "week", ylab = "Total sales(Euro)", main = sprintf("Total sales(%s) over time", item)) 
  points(discount_week, sale_week[discount_week], col='blue', pch=15)
}
sales_week("Eierkoeken")
sales_week("Varkensbraadworst")
sales_week("Komkommer")
sales_week("Sappige gelders rookworst")


# Conclusion : no trend / no seasonality / no significant effect of discount on sales


# calculating profit




