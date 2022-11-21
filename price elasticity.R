

### Price elasticity ###

# model : ln(quantity) = ln(price) + ln(Nr_of_customer) + ln(price_of_others(price of substitutes)) 
# quantity : the number of products being sold in a day

# explanation in detail:
# 1. Supply > Demand(wastes always remain), thus quantity = demand
# 2. Price is only affected by discount, but not supply or demand. No need to use IV(instrumental variable)
# 3. Number of customers visiting the supermarket affects the number of quantity 
# 4. By theory, price elasticity is affected by substitues. So consider prices of other products in corresponding category.
# 5. no need to consider seasonality(time sereies plot of weekly sales of each item indicates no seasonality) 

# Further research 
# 1. personal promotion strategy based on personal transaction data
# 2. Optimize duration of discount 
#    How quantity has been changed from the day before promotion and the days after promotion.
#    Investigate whether the promotion effect only in the first few days of promotion week
# 3. Find products where cross elasticity is huge. -> useful for discount planning


# Data preparation : 
customer <- read.csv("weekly number of customers.csv")
sales <- read.csv("log sales.csv", check.names=FALSE) # Dont replace space with dot
products <- read.csv("products.csv", check.names=FALSE)

customer_number <- log(customer$customer.number)

# Basic model (without consideration of cross-elasticity)
# Model : ln(Q) = ln(P) + ln(nr_of_customer)
item_list = products$description
price_elasticity <- function(item){
  
  price <- sales[,sprintf("%s price", item)]
  quantity <- sales[,sprintf("%s sold", item)]
  price_elasticity <- lm(quantity ~ price + customer_number)
  
  if (summary(price_elasticity)$coefficients[2,4] > 0.05){
    result = 0
  }
  else{
    result = summary(price_elasticity)$coefficients[2,1] # coefficient of Price 
  }
  
  return(result)
}

elasticity_list = vector()
for (item in item_list){
  elasticity_list = c(elasticity_list, price_elasticity(item))
}
all_elasticity = do.call(rbind, Map(data.frame, A=item_list, B=elasticity_list))
all_elasticity = all_elasticity[order(all_elasticity$B, decreasing = FALSE),]


vegetable_elasticity = do.call(rbind, Map(data.frame, A=item_list[1:35], B=elasticity_list[1:35]))
vegetable_elasticity = vegetable_elasticity[order(vegetable_elasticity$B, decreasing = FALSE),]
vegetable_elasticity

# bar plot for vegetables
par(las=2)
par(mar=c(5,12,4,1)+.1)
barplot(height = abs(vegetable_elasticity$B[1:10]),  # A vector of heights
        names.arg = vegetable_elasticity$A[1:10], # A vector of names
        main = "Price elasticity of demand(vegetables)", 
        xlab = "Elasticity(magnitude)", 
        horiz=TRUE,
        las=1,
        xlim=c(0,5),
        col=rgb(0.2,0.4,0.6,0.6))

# elasticity = 0, so no discount effect on them
all_elasticity[all_elasticity$B == 0,]$A 


# Optimized discount for maximizing profit
names = vegetable_elasticity$A[1:10]

i=1
optimized_discount_vector = vector(length=10)
for (name in names){
  p_from_supplier = products[products$description == name,]$purchase_price
  p_sale = products[products$description == name,]$std_sales_price
  e = vegetable_elasticity$B[i]

  optimized_discount_vector[i] = round((e+1-(e*p_from_supplier/p_sale))*(1/(0.02*e)), digits = 2)
  i = i+1
}
optimized_discount_vector

# Barplot of optimized discount of vegetables
par(las=2)
par(mar=c(5,12,4,1)+.1)
barplot(height = optimized_discount_vector,  # A vector of heights
        names.arg = names[1:10], # A vector of names
        main = "Optimized discount for maximizing profit", 
        xlab = "Discount(%)", 
        horiz=TRUE,
        las=1,
        xlim=c(0,30),
        col=rgb(0.2,0.4,0.6,0.6))


