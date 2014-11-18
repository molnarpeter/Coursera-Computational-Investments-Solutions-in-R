marketSimulator <- function(orders,capital=100000)
{

# The function assumes that it is in the same directory with the data including the orders file, so make sure that the working directoy is set properly  
  
library("date")

# Read in the orders
#orders <- 
orders <- read.csv(paste(orders, ".csv", sep=""), header=FALSE)
# Get the list of the symbols which will be traded
symbols <- as.character(unique(orders[,4]))
# Little bit of reorganizing of the orders
order_dates <- as.Date(ISOdate(orders[,1],orders[,2],orders[,3]))
orders[,3] <- order_dates
orders <- orders[,3:6]

# Get the full trading horizon
horizont <- seq(min(order_dates)-1, max(order_dates), by="days")


# Now let's load in the data of the symbols
for(i in 1:length(symbols))
{
  stock0 <- subset(read.csv(paste(symbols[i], ".csv", sep=""), header=TRUE), as.Date(Date) >= min(horizont)+1 & as.Date(Date) <= max(horizont))
  stock0[,1] <- as.Date(stock0[,1]) 
  stock0 <- merge(stock0, data.frame(list(Date=horizont)), by.x='Date', by.y='Date', all.x=TRUE , all.y=TRUE)
  #for(m in nrow(stock0)) {if (is.na(stock0[m,7])) {stock0[m,7]=stock[m-1,7]}}
  assign(symbols[i], stock0)
}

# Create the portfolio
portfolio <- data.frame(horizont, matrix(data=0,nrow=length(horizont),ncol=length(symbols)+2))
names(portfolio) <- c("Date","Capital", symbols, "Sum")
portfolio[1,2] = capital

volume <- data.frame(horizont, matrix(data=0,nrow=length(horizont),ncol=length(symbols)))
names(volume) <- c("Date", symbols)

# Loop over the trading period and see if there is any action on the given day
for(j in seq_along(portfolio[,1])[-1]) 
{
  date = portfolio[j,1]
  row_nums <- grep(date, orders[,1])
  
    if (length(row_nums) == 0)
    {portfolio[j,2] = portfolio[j-1,2]
     # If there is no action then update the value of the holdings
     for (k in symbols)
     {
       stock <- eval(as.name(k))
       volume[names(volume)==k][j,1] = volume[names(volume)==k][j-1,1]
       if (is.na(stock[grep(date, stock[,1]),7])) {portfolio[names(portfolio)==k][j,1] = portfolio[names(portfolio)==k][j-1,1]}
       else portfolio[names(portfolio)==k][j,1] = volume[names(volume)==k][j,1]*stock[grep(date, stock[,1]),7]
             }
     portfolio[j,ncol(portfolio)] = sum(portfolio[j,2:(ncol(portfolio)-1)])}
  else 
    for (k in symbols)
    { portfolio[j,2] = portfolio[j-1,2]
      stock <- eval(as.name(k))
      volume[names(volume)==k][j,1] = volume[names(volume)==k][j-1,1]                                  
      if (is.na(stock[grep(date, stock[,1]),7])) {portfolio[names(portfolio)==k][j,1] = portfolio[names(portfolio)==k][j-1,1]}
      else portfolio[names(portfolio)==k][j,1] = volume[names(volume)==k][j,1]*stock[grep(date, stock[,1]),7]
      
    }
    # Open a long/close a short position
      {for (l in row_nums)  {if (orders[l,3] == "Buy") {name=as.character(orders[l,2])
                                                      stock <- eval(as.name(name))
                                                      volume[names(volume) == eval(name)][j,1]= volume[names(volume) == eval(name)][j,1] + orders[l,4]
                                                      portfolio[names(portfolio)==eval(name)][j,1] = volume[names(volume)==eval(name)][j,1]*stock[grep(date, stock[,1]),7]
                                                      portfolio[j,2] = portfolio[j,2] - orders[l,4]*stock[grep(date, stock[,1]),7]
                                                      portfolio[j,ncol(portfolio)] = sum(portfolio[j,2:(ncol(portfolio)-1)])                    
      }
      # Open a short/close a long position 
      else {name=as.character(orders[l,2])
                                      stock <- eval(as.name(name))
                                      volume[names(volume) == eval(name)][j,1]= volume[names(volume) == eval(name)][j-1,1] - orders[l,4]
                                      portfolio[names(portfolio)==eval(name)][j,1] = volume[names(volume)==eval(name)][j,1]*stock[grep(date, stock[,1]),7]
                                      portfolio[j,2] = portfolio[j,2] + orders[l,4]*stock[grep(date, stock[,1]),7]
                                      portfolio[j,ncol(portfolio)] = sum(portfolio[j,2:(ncol(portfolio)-1)])  }
      
      }
    }
}

# Finally get the statistics
volume <<- volume
portfolio <<- portfolio
#portfolio <<- na.omit(portfolio)
Average_daily_return <- mean(diff(log(portfolio[2:nrow(portfolio),ncol(portfolio)])))
Std_deviation <- sd(diff(log(portfolio[2:nrow(portfolio),ncol(portfolio)])))
Cummulative_return <- portfolio[nrow(portfolio),ncol(portfolio)]/capital-1
Sharpe_ratio <- sqrt(252)*Average_daily_return/Std_deviation
Performance <- data.frame(Average_daily_return,Std_deviation,Cummulative_return,Sharpe_ratio)

print(Performance)
orders <<- orders
}