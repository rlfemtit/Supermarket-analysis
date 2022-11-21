

library(MASS)
library(plotly)

# Input (5 variables)
# name, duration, amount, stock, days until expirational date

item = "Courgette" # item name
duration = c(1,2,3,4,5,6,7) # available discount duration(day)
discount = c(10,20,30,40,50) # available discount amount(%)
stock = 100 # The number of stock left
m = 10 # the days left until the expirational date

# Extra information to add
p_purchase = 0.17 # purchase price of the item
p_sale = 0.75 # sale price of the item

# Function to output the optimized duration/amount of the discount

optimization <- function(item,stock,m,duration,discount){
  data <- read.csv("SellsPerDayVegetablesAndPromotions.csv", check.names=FALSE)
  nr_customer <- read.csv("daily number of customers.csv", check.names=FALSE)
  
  data <- cbind(data,nr_customer)
  data <- na.omit(data)
  
  expectation <- function(item,stock,m,duration,discount){
    
    price = data[,paste(item, "price", sep=" ")]
    quantity = data[,paste(item, "sold", sep=" ")]
    customer = data[,"customer number"]
    
    
    regression = lm(quantity ~ price + customer)
    
    c_effect = summary(regression)$coefficients[3,1]
    p_effect = summary(regression)$coefficients[2,1]
    intercept = summary(regression)$coefficient[1,1]
    
    # Function to calculate the profit
    profit <- function(stock,m,duration,discount){
      # m : left days until expiration date
      
      price = round(p_sale * (1 - 0.01 * discount),2)
      
      # Profit when promotion
      quantity1 = 0
      
      for (i in 1:duration){
        C = round(rnorm(1,56.82967,8.62654),0)
        
        quantity1 = quantity1 + round(c_effect*C + p_effect*price + intercept,0)
      }
      
      quantity2 = 0
      # Profit when no promotion
      no_promotion = m-duration+1 # duration of no promotion
      for (j in 1:no_promotion){
        C = round(rnorm(1,56.82967,8.62654),0)
        
        quantity2 = quantity2 + round(c_effect*C + p_effect*p_sale + intercept,0)
      }
      
      if (quantity1 >= stock){
        quantity1 = stock
        quantity2 = 0
        cost = 0
      } else if (stock - quantity1 <= quantity2){
        quantity2 = stock - quantity1 
        cost = 0
      } else{
        cost = (stock-quantity1-quantity2)*(price - p_purchase)
      } 
      
      profit1 = quantity1*(price - p_purchase)
      profit2 = quantity2*(p_sale - p_purchase)
      
      profit = profit1 + profit2 - cost
      
      return(profit)
    } 
    
    expected_profit = vector(length=10000) # simulate 100000 times
    
    for (s in 1:10000){
      expected_profit[s] = profit(stock,m,duration,discount)
    }
    
    E_profit = round(mean(expected_profit), 2)
    
    return(E_profit)
      }
  
  df = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("duration", "discount", "profit"))
  for (i in duration){
    for (j in discount){
      z = expectation(item,stock,m,i,j)
      df=rbind(df,data.frame(i,j,z))
    }
  }
  
  # Find the optimized duration/amount of the discount
  index = which.max(df$z)
  maxima = df[index,]
  
  # Visualization
  fig <- plot_ly(x=df$i,y=df$j,z = df$z, type = "contour")
  fig <- fig %>% layout(
    title = "Profit(x: duration, y: discount(%))",
    scene = list(
      xaxis = list(title = "Durationy)"),
      yaxis = list(title = "Discount(unit:10%)")
    ))
  
  # Find the optimized duration/amount of the discount
  index = which.max(df$z)
  maxima = df[index,]
  names(maxima)[1] = 'duration'
  names(maxima)[2] = 'discount'
  names(maxima)[3] = 'profit'
  
  return(list(maxima,fig))
  
}

optimization(item,stock,m,duration,discount) # output

