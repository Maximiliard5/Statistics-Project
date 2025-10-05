#####################################################################################################################################
# EX I
#1 a) Simulate Cauchy(x0,g)
n <- 100 #nr of samples
x0 <- 5
g <- 2
simulate_cauchy <- function(n,x0,g){
  U <- runif(n,0,1)
  X <- x0+g*tan(pi*(U-1/2))
  return(X)
}

samples <- simulate_cauchy(n,x0,g)
hist(samples,breaks=100,main="Histogram of simulated Cauchy samples")

# b) 
n <- 100
simulate_discrete <- function(n){
  x_vals <- c(1,2,3)
  p_vals <- c(1/2,1/3,1/6)
  X <- sample(x_vals,size=n,replace=TRUE,prob=p_vals)
}

samples <- simulate_discrete(n)
hist(samples,main="Histogram of simulated discrete samples")

# c)
n <- 100
x1 <- 1
x2 <- 2
p <- 0.3

simulate_discrete <- function(n,x1,x2,p){
  U <- runif(n,0,1)
  X <- ifelse(U<=1-p,x1,x2)
  return(X)
}

samples <- simulate_discrete(n,x1,x2,p)
hist(samples,main="Histogram of simulated discrete samples with given parameters(Bernoulli)")

# d) 
simulate_perturbed_random <- function(a,b){
  values <- seq(a,b)
  n <- length(values)
  
  #We will use the current date that we convert to a numeric value which calculates the number of seconds since the UNIX epoch (1 Jan 1970 )
  noise <- as.numeric(Sys.time())%%1000 #We take the last 3 digits of the number of seconds
  
  #Calculate the position using the noise
  position <- (noise%%n)+1
  
  #print(paste("Chosen position:", position)) #uncomment if you would like to see what position was picked.
  
  #Create weights with a higher bias towards the selected position
  weights <- rep(1,n)
  weights[position] <- weights[position]+5
  
  probs <- weights/sum(weights)
  
  result <- sample(values,size=1,prob=probs)
  return(result)
}
simulate_perturbed_random(1,1000)


# 501 STB line
#We will write the functions again just to have everything together
#a)
simulate_cauchy <- function(n,x0,g){
  U <- runif(n,0,1)
  X <- x0+g*tan(pi*(U-1/2))
  return(X)
}
#b)
simulate_discrete_b <- function(n){
  x_vals <- c(1,2,3)
  p_vals <- c(1/2,1/3,1/6)
  X <- sample(x_vals,size=n,replace=TRUE,prob=p_vals)
}
#c)
simulate_discrete_c <- function(n,x1,x2,p){
  U <- runif(n,0,1)
  X <- ifelse(U<=1-p,x1,x2)
  return(X)
}
#d)
simulate_perturbed_random <- function(a,b){
  values <- seq(a,b)
  n <- length(values)
  
  #We will use the current date that we convert to a numeric value which calculates the number of seconds since the UNIX epoch (1 Jan 1970 )
  noise <- as.numeric(Sys.time())%%1000 #We take the last 3 digits of the number of seconds
  
  #Calculate the position using the noise
  position <- (noise%%n)+1
  
  #Create weights with a higher bias towards the selected position
  weights <- rep(1,n)
  weights[position] <- weights[position]+5
  
  probs <- weights/sum(weights)
  
  result <- sample(values,size=1,prob=probs)
  return(result)
}

#Line 501 STB

#Function that simulates one day (nr of passengers and day type)
#I am a little confused the exercise states that the type fo day depends on the number of passengers but then it states that we need
#to simulate it using the function at point b) but the function b) doesn't depend on the number of passengers.
#The only way i can make sense of this is if we first call b) for 1 sample and this tells us the type of day and then we call d) on the interval
# (1,350) for example.
simulate_one_day <- function(){
  dayTypeNr <- simulate_discrete_b(1)
  
  if(dayTypeNr==1){
    dayType <- "easy"
    dailyCount <- simulate_perturbed_random(1,350)
  } else if (dayTypeNr==2){
    dayType <- "normal"
    dailyCount <- simulate_perturbed_random(350,670)
  } else {
    dailyCount <- simulate_perturbed_random(671,1000)#we will consider the upper bound for the number of passengers to be 1000
    dayType <- "busy"
  }
  return(list(dayType=dayType,passengers=dailyCount))
}

#Function that calculates the passenger split (pass,ticket,nothing)
daySplitPassengers <- function(totalPassengers){
  #Here perhaps I misunderstood the formula but the interval for d) is not specified and sometimes the cauchy distribution gets huge values
  #so i will be forcing the interval to be between (1,99)
  x <- min(99,max(1,simulate_perturbed_random(1,99)+sample(c(-1,1),1)*abs(floor(simulate_cauchy(1,5,2)))))#
  probPass <- x/100
  nrPass <- round(probPass*totalPassengers)
  
  #The number of passengers without a pass
  noPass <- totalPassengers-nrPass
  
  #Again here the formula does not explicitly state the parameters of the function a) so i will take x0=0,g=1/2
  v <- simulate_cauchy(1,0,1/2)
  v <- abs(v)-floor(abs(v))
  
  x <- min(99,max(1,simulate_discrete_c(noPass,sample(1:99,1),sample(1:99,1),v))) #Again we forge the value between (1,99)
  
  nrTicket <- min(noPass,x)
  nrNothing <- noPass-nrTicket
  
  return(c(nrPass=nrPass,nrTicket=nrTicket,nrNothing=nrNothing))
}

#Function that simulates one month
simulate_one_month <- function(days=30){
  dayList <- replicate(days, simulate_one_day(), simplify = FALSE)
  
  dayTypeVect <- sapply(dayList,function(x) x$dayType)
  passengerVect <- sapply(dayList,function(x) x$passengers)
  
  splits <- t(sapply(passengerVect,daySplitPassengers))
  colnames(splits) <- c("pass","ticket","nothing")
  
  y <- mean(passengerVect)
  xmin <- min(passengerVect)
  xmax <- max(passengerVect)
  
  df <- data.frame(
    day=seq_len(days),
    dayType=dayTypeVect,
    passengers=passengerVect,
    nrPass=splits[, "pass"],
    nrTicket=splits[, "ticket"],
    nrNothing=splits[, "nothing"]
  )
  return(df)
}
df <- simulate_one_month()
View(df)

#Function that simulates a year (2024)
simulate_year <- function(){
  days_per_month <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  months <- month.abb
  
  results <- data.frame(
    Month = character(),
    Mean_Passengers = numeric(),
    Min_Passengers = numeric(),
    Max_Passengers = numeric(),
    Percent_Easy = numeric(),
    Percent_Normal = numeric(),
    Percent_Busy = numeric(),
    Total_Pass_Buyers = numeric(),
    Total_Ticket_Buyers = numeric(),
    Total_Non_Paying = numeric(),
    Revenue_Passes = numeric(),
    Revenue_Tickets = numeric(),
    Missed_Revenue = numeric()
  )
  
  for (i in 1:12){
    days <- days_per_month[i]
    month_name <- months[i]
    
    daily_data <- replicate(days,simulate_one_day(),simplify=FALSE)
    
    passenger_count <- sapply(daily_data,function(x) x$passengers)
    day_types <- sapply(daily_data, function(x) x$dayType)
  
    mean_passengers <- mean(passenger_count)
    min_passengers <- min(passenger_count)
    max_passengers <- max(passenger_count)
    
    percent_easy <- sum(day_types=="easy")/days*100
    percent_normal <- sum(day_types=="normal")/days*100
    percent_busy <- sum(day_types=="busy")/days*100
    
    passenger_splits <- t(sapply(passenger_counts,daySplitPassengers))
    total_passes <- sum(passenger_splits[,"nrPass"])
    total_tickets <- sum(passenger_splits[,"nrTicket"])
    total_nothing <- sum(passenger_splits[,"nrNothing"])
    
    revenue_passes <- total_passes*70
    revenue_tickets <- total_tickets*3
    missed_revenue <- total_nothing*3
    
    results <- rbind(results,data.frame(
      Month = month_name,
      Mean_Passengers=mean_passengers,
      Min_Passengers=min_passengers,
      Max_Passengers=max_passengers,
      Percent_Easy=percent_easy,
      Percent_Normal=percent_normal,
      Percent_Busy=percent_busy,
      Total_Pass_Buyers = total_passes,
      Total_Ticket_Buyers = total_tickets,
      Total_Non_Paying = total_nothing,
      Revenue_Passes = revenue_passes,
      Revenue_Tickets = revenue_tickets,
      Missed_Revenue = missed_revenue
    ))
  }
  return(results)
}

#Function to calculate the fine depending on the type of day
#as parameters I take the day type, all passengers, pass passengers, ticket passengers, nothing passengers
#In the exercise we assume that he stays in the tram and checks everyone that gets in
calculate_fine <- function(dayType,allP,passP,ticketP,noP){
  fines <- 0
  checks <- 0
  if(noP==0){
    return(0)
  }
  if(dayType=="easy"){
    nrCheck <- sample(2:11,1)
    for (i in 1:nrCheck){
      if(fines>=3 || allP<=0){
        break
      }
      chosenP <- sample(c("pass","ticket","nothing"),1,prob=c(passP,ticketP,noP)/allP)
      
      if(chosenP=="nothing"){
        fines <- fines+1
        noP <- noP-1
      }
      allP <- allP-1
    }
  } else if (dayType=="normal"){
    while(fines<5 && checks<allP && allP>0){
      chosenP <- sample(c("pass","ticket","nothing"),1,prob=c(passP,ticketP,noP)/allP)
      if(chosenP=="nothing"){
        fines <- fines+1
        noP <- noP-1
      }
      allP <- allP-1
      checks <- checks+1
    } 
  } else if (dayType=="busy"){
    nrCheck <- sample(2:5,1)
    for (i in 1:nrCheck){
      if(fines>=1 || allP<=0){
        break
      }
      chosenP <- sample(c("pass","ticket","nothing"),1,prob=c(passP,ticketP,noP)/allP)
      if(chosenP=="nothing"){
        fines <- fines+1
        noP <- noP-1
      }
      allP <- allP-1
    }
  }
  return(fines)
}

#Function that simulates the fines in year
#It is stated that the the tram makes 14 routes a day and we only check 2 full routes
simulate_year_with_fines <- function(){
  days_per_month <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  months <- month.abb
  
  results <- data.frame(
    Month=character(),
    Day=integer(),
    Fine_Revenue=numeric(),
    Missed_Revenue = numeric(),
    Controller_Cost = numeric(),
    Profit = numeric()
  )
  #Loop through every month
  for(i in 1:12){
    days <- days_per_month[i]
    month_name <- months[i]
    #Loop through every day of the month
    for(day in 1:days){
      daily_data <- simulate_one_day()
      total_passengers <- daily_data$passengers
      day_type <- daily_data$dayType
      
      split <- daySplitPassengers(total_passengers)
      passP <- as.numeric(split["nrPass"])
      ticketP <- as.numeric(split["nrTicket"])
      noP <- as.numeric(split["nrNothing"])
      
      #Split the passengers into 14 groups
      route_passengers <- rep(floor(total_passengers/14),14)
      remainder <- total_passengers%%14
      if(remainder>0){
        route_passengers[1:remainder] <- route_passengers[1:remainder]+1
      }
      
      routePassP <- rep(floor(passP/14),14)
      routeTicketP <- rep(floor(ticketP/14),14)
      routeNoP <- rep(floor(noP/14),14)
      
      passP_remainder <- passP%%14
      ticketP_remainder <- ticketP%%14
      noP_remainder <- noP%%14
      
      if(passP_remainder>0){
        routePassP[1:passP_remainder] <- routePassP[1:passP_remainder]+1
      }
      if(ticketP_remainder>0){
        routeTicketP[1:ticketP_remainder] <- routeTicketP[1:ticketP_remainder]+1
      }
      if(noP_remainder>0){
        routeNoP[1:noP_remainder] <- routeNoP[1:noP_remainder]+1
      }
      
      #Perform checks
      total_fines <- 0
      total_missed_revenue <- 0
      checked_routes <- sample(1:14,2)
      
      for (route in checked_routes){
        route_total <- route_passengers[route]
        
        fines <- calculate_fine(day_type,route_total,routePassP[route],routeTicketP[route],routeNoP[route])
        total_fines <- total_fines+fines*50
        
        total_missed_revenue <- total_missed_revenue+(routeNoP[route]*3)
      }
      controller_cost <- 428
      profit <- total_fines-(total_missed_revenue+controller_cost)
      
      results <- rbind(results,data.frame(
        Month = month_name,
        Day = day,
        Fine_Revenue = total_fines,
        Missed_Revenue = total_missed_revenue,
        Controller_Cost = controller_cost,
        Controller_Profit = profit
      ))
    }
  }
  return(results)
}

simulate_year_with_2controller <- function(){
  days_per_month <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  months <- month.abb
  
  results <- data.frame(
    Month=character(),
    Day=integer(),
    Fine_Revenue=numeric(),
    Missed_Revenue = numeric(),
    Controller_Cost = numeric(),
    Controller_Steal = numeric(),
    Profit = numeric()
  )
  profit <- 0
  #Loop through every month
  for(i in 1:12){
    days <- days_per_month[i]
    month_name <- months[i]
    #Loop through every day of the month
    for(day in 1:days){
      daily_data <- simulate_one_day()
      total_passengers <- daily_data$passengers
      day_type <- daily_data$dayType
      
      split <- daySplitPassengers(total_passengers)
      passP <- as.numeric(split["nrPass"])
      ticketP <- as.numeric(split["nrTicket"])
      noP <- as.numeric(split["nrNothing"])
      
      route_passengers <- rep(floor(total_passengers/14),14)
      remainder <- total_passengers%%14
      if(remainder>0){
        route_passengers[1:remainder] <- route_passengers[1:remainder]+1
      }
      
      routePassP <- rep(floor(passP/14),14)
      routeTicketP <- rep(floor(ticketP/14),14)
      routeNoP <- rep(floor(noP/14),14)
      
      passP_remainder <- passP%%14
      ticketP_remainder <- ticketP%%14
      noP_remainder <- noP%%14
      
      if(passP_remainder>0){
        routePassP[1:passP_remainder] <- routePassP[1:passP_remainder]+1
      }
      if(ticketP_remainder>0){
        routeTicketP[1:ticketP_remainder] <- routeTicketP[1:ticketP_remainder]+1
      }
      if(noP_remainder>0){
        routeNoP[1:noP_remainder] <- routeNoP[1:noP_remainder]+1
      }
      
      #Perform checks
      total_fines <- 0
      total_missed_revenue <- 0
      checked_routes <- sample(1:14,3)
      
      for (route in checked_routes){
        route_total <- route_passengers[route]

        fines <- calculate_fine(day_type,route_total,routePassP[route],routeTicketP[route],routeNoP[route])
        total_fines <- total_fines+fines*50
        
        total_missed_revenue <- total_missed_revenue+(noP*3)
      }
      
      #Here the controller steals the money
      controller_steal <- total_fines*0.3
      total_fines <- total_fines-controller_steal
      
      controller_cost <- 642
      profit <- total_fines-(total_missed_revenue+controller_cost)
      
      
      results <- rbind(results,data.frame(
        Month = month_name,
        Day = day,
        Fine_Revenue = total_fines,
        Missed_Revenue = total_missed_revenue,
        Controller_Cost = controller_cost,
        Controller_Steal = controller_steal,
        Controller_Profit = profit
      ))
    }
  }
  return(results)
}


#Exercise a)
days_in_december <- 31
passenger_counts <- sapply(1:days_in_december,function(x) simulate_one_day()$passengers)

hist(passenger_counts,
     main = "Histogram of Daily Passenger Counts (December 2024)",
     xlab = "Number of Passengers",
     ylab = "Frequency",
     col = "magenta",
     )

#Exercise b) +c)

yearly_data <- simulate_year()
View(yearly_data)

#Exercise d) 
daily_fine_data <- simulate_year_with_fines()
View(daily_fine_data) #Profit is the value that shows how much money employing the controller loses each day

#It appears the controller is not doing a very good job of catching people without tickets. This is most likely due to the fact that
#He only checks 2 routes of 14 every day so there are a lot of people that get away
#On the route that he does start asking for tickets he stops after searching at most the whole tram or he fines 5 people (normal day)
#So the most profit he can make in a day is 2*250=500. In a normal day there are between 350 and 670 passengers, we split these into 14 groups
#Because there are 14 full routes a day. This results in between 25 and ~47 people that are checked. 
positive_profit_days <- sum(daily_fine_data$Profit>daily_fine_data$Missed_Revenue)
cat("NUmber of days with positive profit: ", positive_profit_days)

#Exercise e)
daily_fine_data_2controllers <- simulate_year_with_2controller()
View(daily_fine_data_2controllers)

mean1 <- mean(daily_fine_data$Controller_Profit)
mean2 <- mean(daily_fine_data_2controllers$Controller_Profit)
cat("2 controls a day has the average profit of: ",mean1)
cat("3 controls a day has the average profit of: ",mean2)
cat("We can observe that even though we introduced another control because the controllers started stealing, the profit went down!!!")

######################################################################################################################################
# Ex II
# Most parameters are chosen similar to the ones in cookbook 
#2 a)+b) Binomial distribution
#Parameters
n_vals <- c(3,10,20,30) 
p_vals <- c(0.9,0.6,0.3,0.1)

# Plot
plot(NA,xlim=c(0,max(n_vals)),ylim=c(0,1),main='Binomial PMF',xlab='x',ylab='Probability')

for (i in 1:length(n_vals)){
  n <- n_vals[i]
  p <- p_vals[i]
  x <- 0:n
  points(x,dbinom(x,n,p),col=i,pch=19)
  lines(x,dbinom(x,n,p),col=i,lty=3)
}
# Add legend
legend('topright',legend=c('n=3,p=0.9','n=10,p=0.6','n=20,p=0.3','n=30,p=0.1'),col=1:4,pch=19,lty=3)

# c) Geometric distribution
#Parameters
p_vals <- c(0.2,0.5,0.7,0.9)

# Plot
plot(NA,xlim=c(0,10),ylim=c(0,1),main='Geometric PMF',xlab='x',ylab='Probability')

for (i in 1:length(p_vals)){
  p <- p_vals[i]
  x <- 0:10 # Possible x values
  points(x,dgeom(x,p),col=i,pch=19)
  lines(x,dgeom(x,p),col=i,lty=3)
}
# Add legend
legend('topright',legend=c('p=0.2','p=0.5','p=0.7','p=0.9'),col=1:4,pch=19,lty=3)

# d) Poisson distribution
#Parameters
l_vals <- c(1,4,10,12)

# Plot
plot(NA,xlim=c(0,20),ylim=c(0,1),main='Poisson PMF',xlab='x',ylab='Probability')

for (i in 1:length(l_vals)){
  l <- l_vals[i]
  x <- 0:20 # Possible x values
  points(x,dpois(x,l),col=i,pch=19)
  lines(x,dpois(x,l),col=i,lty=3)
}
# Add legend
legend('topright',legend=c('l=1','l=4','l=10','l=12'),col=1:4,pch=19,lty=3)

# e) Gamma distribution
#Parameters
a_vals <- c(1,2,3,5)
b_vals <- c(2,2,2,1)

x <- seq(0,20,length.out=500) # Continuous range for x

# Plot
plot(NA,xlim=c(0,20),ylim=c(0,2),main='Gamma PDF',xlab='x',ylab='Density')

for (i in 1:length(a_vals)){
  a <- a_vals[i]
  b <- b_vals[i]
  lines(x,dgamma(x,a,b),col=i)
}
# Add legend
legend('topright',legend=c('a=1,b=2','a=2,b=2','a=3,b=2','a=5,b=1'),col=1:4,pch=19,lty=3)

# f) Beta distribution
#Parameters
a_vals <- c(0.5,5,1,2)
b_vals <- c(0.5,1,3,2)

x <- seq(0,1,length.out=500) # Continuous range for x

# Plot
plot(NA,xlim=c(0,1),ylim=c(0,2),main='Beta PDF',xlab='x',ylab='Density')

for (i in 1:length(a_vals)){
  a <- a_vals[i]
  b <- b_vals[i]
  lines(x,dbeta(x,a,b),col=i)
}
# Add legend
legend('topright',legend=c('a=0.5,b=0.5','a=5,b=1','a=1,b=3','a=2,b=2'),col=1:4,pch=19,lty=3)

# g) Chi-Squared distribution
#Parameters
k_vals <- c(1,2,3,4)

x <- seq(0,8,length.out=500) # Continuous range for x

# Plot
plot(NA,xlim=c(0,8),ylim=c(0,0.5),main='Chi-squared PDF',xlab='x',ylab='Density')

for (i in 1:length(a_vals)){
  k <- k_vals[i]
  lines(x,dchisq(x,k),col=i)
}
# Add legend
legend('topright',legend=c('k=1','k=2','k=3','k=4'),col=1:4,pch=19,lty=3)

# 4)
# Geometric Log-Likelihood

geom_log_likelihood <- function(p,x){
  S <- sum(x)
  n <- length(x)
  return((S-n)*log(1-p)+n*log(p))
}

p <- 0.3
x_geom <- rgeom(1000,prob=p)+1 # +1 beacuse we consider the geometric with exponent x-1

p_vals <- seq(0.01,0.99,length.out=100)
log_like_geo <- sapply(p_vals,geom_log_likelihood,x=x_geom)

plot(p_vals,log_like_geo,type='l',col='blue',main='Geometric Log-Likelihood',xlab='p',ylab='Log-Likelihood')

geo_result <- optimise(geom_log_likelihood,interval=c(0.01,0.99),x=x_geom,maximum = TRUE)

cat('True p = :' ,p,'\n')
cat('Empirical Geometric MLE for p:' ,geo_result$maximum,'\n')


# Poisson Log-Likelihood

pois_log_likelihood <- function(l,x){
  S <- sum(x)
  n <- length(x)
  return (S*log(l)-n*l+sum(log(1/factorial(x))))
}

lambda <- 3
x_pois <- rpois(1000,l=lambda)

l_vals <- seq(1,10,length.out=100)
log_like_pois <- sapply(l_vals,pois_log_likelihood,x=x_pois)

plot(l_vals,log_like_pois,type='l',col='green',main='Poisson Lok-Likelihood',xlab='Lambda',ylab='Log-Likelihood')
     
pois_result <- optimise(pois_log_likelihood,interval=c(1,10),x=x_pois,maximum=TRUE)

theoretic_pois <- sum(x_pois)/length(x_pois)

cat('True lambda =:',lambda,'\n')
cat('Empiric Poisson MLE for lambda:',pois_result$maximum,'\n')


# Chi-Squared Log-Likelihood

chi_log_likelihood <- function(k,x){
  n <- length(x)
  S1 <- sum(log(x))
  S2 <- sum(x)
  term1 <- (k/2-1)*S1
  term2 <- -S2/2
  term3 <- -n*k/2*log(2)
  term4 <- -n*lgamma(k/2)
  return(term1+term2+term3+term4)
}

k <- 5
x_chi <- rchisq(1000,df=k)#k=5

niu_vals <- seq(1,10,length.out=100)
log_like_chi <- sapply(niu_vals, chi_log_likelihood,x=x_chi)

plot(niu_vals,log_like_chi,type='l',col='red',main='Chi-Square Log-Likelihood',xlab='Niu degrees of freedom',ylab='Log-Likelihood')

S1 <- sum(log(x_chi))
n <- length(x_chi)

chi_empirical <- optimise(chi_log_likelihood,interval=c(1,10),x=x_chi,maximum=TRUE)$maximum

cat('True lambda = :',k,'\n')
cat('Empiric Chi-Square MLE for lambda:',chi_empirical,'\n')


# 5)

#Binomial Log-Likelihood for n fixed
binom_log_likelihood <- function(p,x,n){
  m <- length(x)
  return (sum(lchoose(n,x))+sum(x)*(log(p)-log(1-p))+n*m*log(1-p))
}

n_binom <- 3
true_prob <- 0.4
x_binom <- rbinom(1000,size=n_binom,prob=true_prob)

p_vals=seq(0.01,0.99,length.out=100)
log_like_binom_p <- sapply(p_vals,binom_log_likelihood,x=x_binom,n=n_binom)

plot(p_vals,log_like_binom_p,type='l',col='blue',main='Binomial Log-Likelihod (n fixed)',xlab='p',ylab='Log-Likelihood')

binom_result_p <- optimise(binom_log_likelihood,interval=c(0.01,0.99),x=x_binom,n=n_binom,maximum=TRUE)$maximum

cat('True p = ',true_prob,'\n')
cat('Empiric Binomial MLE for p (fix n)',binom_result_p,'\n')


#Gamma Log-Likelihood 
#Case 1: fix a, vary b

gamma_log_likelihood_b <- function(b,x,a){
  n <- length(x)
  sum_log_x <- sum(log(x))
  sum_x <- sum(x)
  return (n*(-lgamma(a)-a*log(b))+(a-1)*sum_log_x-(1/b)*sum_x)
}
a <- 2
b <- 3
x_gamma <- rgamma(100,shape=a,scale=b)

b_vals <- seq(0.1,10,length.out=100)
log_like_beta <- sapply(b_vals,gamma_log_likelihood_b,x=x_gamma,a=a)

plot(b_vals,log_like_beta,type='l',col='red',main='Gamma Log-Likelihod (a fixed)',xlab='Beta',ylab='Log-Likelihood')

beta_mle <- optimise(gamma_log_likelihood_b,interval=c(0.1,10),x=x_gamma,a=a,maximum = TRUE)$maximum

cat('True beta = ',b,'\n')
cat('Empiric Gamma MLE for Beta (fix Alpha)',beta_mle,'\n')


#Case 2: vary a, fix b

gamma_log_likelihood_a <- function(a,x,b){
  n <- length(x)
  sum_log_x <- sum(log(x))
  sum_x <- sum(x)
  return (n*(-lgamma(a)-a*log(b))+(a-1)*sum_log_x-(1/b)*sum_x)
}
a <- 2
b <- 3
x_gamma <- rgamma(1000,shape=a,scale=b) #The empirical MLE converges to the theoretical one for a large n

a_vals <- seq(0.1,10,length.out=100)
log_like_beta <- sapply(a_vals,gamma_log_likelihood_a,x=x_gamma,b=b)

plot(a_vals,log_like_beta,type='l',col='blue',main='Gamma Log-Likelihod (b fixed)',xlab='Alpha',ylab='Log-Likelihood')

alpha_mle <- optimise(gamma_log_likelihood_a,interval=c(0.1,10),x=x_gamma,b=b,maximum = TRUE)$maximum

cat('True alpha = ',a,'\n')
cat('Empiric Gamma MLE for Alpha (fix Beta)',alpha_mle,'\n')


# Beta Log-Likelihood
# Case 1: fix a, vary b

beta_log_likelihood <- function(b,x,a){
  n <- length(x)
  S1 <- sum(log(x))
  S2 <- sum(log(1-x))
  return(n*lgamma(a+b)-n*lgamma(a)-n*lgamma(b)+(a-1)*S1+(b-1)*S2)
}

a <- 2
b <- 3
x_beta <- rbeta(1000, shape1 = a, shape2 = b)

beta_vals <- seq(0.1,10,length.out=100)
long_like_beta <- sapply(beta_vals,beta_log_likelihood,x=x_beta,a=a)

plot(beta_vals,long_like_beta,type='l',col='red',main='Beta Log-Likelihod (a fixed)',xlab='Beta',ylab='Log-Likelihood')

beta_mle <- optimise(beta_log_likelihood,interval=c(0.1,10),x=x_beta,a=a,maximum=TRUE)$maximum
cat("True beta = ",b,"\n")
cat('Empiric Beta MLE for Beta (fix Alpha)',beta_mle,'\n')

#Case 2: vary a, fix b
beta_log_likelihood <- function(a,x,b){
  n <- length(x)
  S1 <- sum(log(x))
  S2 <- sum(log(1-x))
  return(n*lgamma(a+b)-n*lgamma(a)-n*lgamma(b)+(a-1)*S1+(b-1)*S2)
}

a <- 2
b <- 2
x_beta <- rbeta(1000, shape1 = a, shape2 = b)

alpha_vals <- seq(0.1,10,length.out=100)
long_like_beta <- sapply(alpha_vals,beta_log_likelihood,x=x_beta,b=b)

plot(alpha_vals,long_like_beta,type='l',col='red',main='Beta Log-Likelihod (b fixed)',xlab='Alpha',ylab='Log-Likelihood')

alpha_mle <- optimise(beta_log_likelihood,interval=c(0.1,10),x=x_beta,b=b,maximum=TRUE)$maximum
cat("True alpha = ",b,"\n")
cat('Empiric Beta MLE for Beta (fix Alpha)',alpha_mle,'\n')

#7)
#Function that return the Fisher Information for a specific distribution
calculate_mirc <- function(distribution,n,params=list()){
  fisher_info <- function(distribution,params){
    if(distribution=="geometric"){
      p <- params$p
      return(1/(p^2*(1-p)))
    }
    else if(distribution=="poisson"){
      lambda <- params$lambda
      return (1/lambda)
    }
    else if(distribution=="chi_square"){
      k <- params$k
      return(1/4*trigamma(k/2))
    }
    else if(distribution=="normal"){
      sigma <- params$sigma^2
      return(1/sigma)
    }
    else if(distribution=="exponential"){
      lambda <- params$lambda
      return(1/lambda^2)
    }
    else if(distribution=="uniform"){
      a <- params$a
      b <- params$b
      return(n/(b-a)^2)
    }
    else if(distribution=="bernoulli"){
      p <- params$p
      return(1/(p*(1-p)))
    }
    else if(distribution=="gamma"){
      alpha <- params$alpha
      beta <- params$beta
      return(alpha/(beta^2))
    }
    else{
      stop("Unknown distribution!!!")
    }
  }
  I_n <- fisher_info(distribution,params)*n
  MIRC <- 1/I_n
  
  cat("For ", distribution," distribution\n")
  cat("FIsher Information: ", I_n, "\n")
  cat("Rao Cramer Lowe Bound (MIRC): ",MIRC,"\n")
}
#Example usage
#To add more examples write the name of the distribution, select a n>0, and then give the acording parameters in a list
#More information about how the fisher information formulas were calculated can be found in the paper at exercise II) 7)
calculate_mirc("poisson",n=20,params=list(lambda=3))

calculate_mirc("normal",n=50,params=list(sigma=2))

calculate_mirc("gamma",n=100,params=list(alpha=3,beta=2))

calculate_mirc("uniform",n=50,params=list(a=1,b=4))


#9)
#Function to calculate the log-likelihood for the Gamma distribution
n <- 1000

#i) Binomial(n=3,p)
true_p=0.4
x_binom <- rbinom(n,size = 3,prob = true_p)

p_mle_binom <- mean(x_binom)/3
p_mme_binom <- mean(x_binom)/3

cat("\nBinom(n=3) example")
cat("\n True p = ",true_p)
cat("\n MLE p = ",p_mle_binom)
cat("\n MME p= ",p_mme_binom,"\n")

#ii) Geometric(p)
true_p_geo <- 0.3
x_geo <- rgeom(n,prob = true_p_geo)+1 #+1 pentru ca folosim versiounea cu ^(x-1)

p_mle_geo <- 1/mean(x_geo)
p_mme_geo <- 1/mean(x_geo)

cat("\nGeometric example")
cat("\n True p = ",true_p_geo)
cat("\n MLE p = ",p_mle_geo)
cat("\n MME p= ",p_mme_geo,"\n")

#iii)Poisson(lambda)
true_lambda <- 5
x_pois <- rpois(n,lambda = true_lambda)

lambda_mle <- mean(x_pois)
lambda_mme <- mean(x_pois)

cat("\nPoisson example")
cat("\n True p = ",true_lambda)
cat("\n MLE p = ",lambda_mle)
cat("\n MME p= ",lambda_mme,"\n")

#iv) Gamma(alpha,beta)
true_alpha <- 2
true_beta <- 5
x_gamma <- rgamma(n,shape = true_alpha,scale=true_beta)

#MME
x_bar <- mean(x_gamma)
s2 <- var(x_gamma)

alpha_mme <- (x_bar^2/s2)
beta_mme <- s2/x_bar

#MLE
beta_mle <- x_bar/true_alpha

sum_log_x <- sum(log(x_gamma))
n <- length(x_gamma)

f_alpha <- function(a,b){
  sum_log_x-n*digamma(a)-n*log(b)
}
alpha_mle <- uniroot(
  f=function(a) f_alpha(a,b=true_beta),
  interval=c(0.01,100))$root

cat("\nGamma(alpha,beta) example")
cat("\n True alpha = ",true_alpha, ", True beta =", true_beta)
cat("\n MME: alpha = ",alpha_mme, ", beta =", beta_mme)
cat("\n MLE: alpha = ",alpha_mle, ", beta = ", beta_mle,"\n")

#v) Beta(alpha,beta)
true_alpha <- 3
true_beta <- 6
x_beta <- rbeta(n,shape1 = true_alpha,shape2 = true_beta)

#MME
x_bar <- mean(x_beta)
s2 <- var(x_beta)

g <- x_bar*(1-x_bar)/s2-1
alpha_mme <- x_bar*g
beta_mme <- (1-x_bar)*g

#MLE
f_alpha <- function(a,b){
  n*(digamma(a+b)-digamma(a))+sum(log(x_beta))
}

f_beta <- function(b,a){
  n*(digamma(a+b)-digamma(b))+sum(log(1-x_beta))
}

alpha_mle <- uniroot(f=function(a) f_alpha(a,b=true_beta),interval=c(0.01,100))$root
beta_mle <- uniroot(f=function(b) f_beta(b,a=true_alpha),interval=c(0.01,100))$root

cat("\nBeta(alpha,beta) example")
cat("\n True alpha = ",true_alpha, ", True beta =", true_beta)
cat("\n MME: alpha = ",alpha_mme, ", beta =", beta_mme)
cat("\n MLE: alpha = ",alpha_mle, ", beta = ", beta_mle,"\n")


#vi) Chi-square(k)
true_k <- 5
x_chisq <- rchisq(n,df = true_k)

#MME
k_mme <- mean(x_chisq)

#MLE
f_k <- function(k){
  digamma(k/2)+log(2)-mean(log(x_chisq))
}

k_mle <- uniroot(f=f_k,interval=c(0.01,100))$root

cat("\nChi-square(k) example")
cat("\n True p = ",true_k)
cat("\n MLE p = ",k_mle)
cat("\n MME p= ",k_mme,"\n")

#######################################################################################################################################
# EX III

#a) Bin(r,p)
n <- 50
r <- 5
p <- 0.3

x_vals <- seq(-3,3,by=0.01)

CDF_bin <- sapply(x_vals,function(x){
  maxim <- floor(n*r*p+x*sqrt(n*r*p*(1-p)))
  
  pbinom(maxim,size=r*n,prob=p) #We can use the fact that sum of n Bernoulli(r,p) is distributed Bernoulli(rn,p) 
})

plot(x_vals,CDF_bin,type="l",main="CDF of Zn for Binomial(n=50,p=0.3)",xlab="x",ylab="P(Zn<=x)")

#b) Geometric(p)
n <- 50
p <- 0.4

x_vals <- seq(-3,3,by=0.01)

mean <- 1/p
sigma <- sqrt((1-p)/p^2)

#We take Nrsim simulations and then we sum the rows to get a vector whose value is the sum of n random numbers
#in the interval [a,b]. WE then divide by the mean and multiply by sqrt(n)/sigma. For these values we create the CDF.
#Then we apply it on values from an interval. We can observe that as n grows the CDF starts to look like a CDF from a Normal distribution with mean=0 and var=1
Nrsim <- 1000 #Nr of simulations

X_matrix <- matrix(rgeom(Nrsim*n,prob=p),nrow=Nrsim,ncol=n)+1

S_vals <- rowSums(X_matrix)

Z_vals <- (S_vals-n*mean)/(sigma*sqrt(n))

Func <- ecdf(Z_vals)

CDF_geom <- sapply(x_vals,Func)

plot(x_vals,CDF_geom,type="l",main="CDF of Zn for Geometric(n=50,p=0.4)",xlab="x",ylab="P(Zn<=x)")


#c) Poisson(l)
n <- 50
l <- 3

x_vals <- seq(-3,3,by=0.01)

CDF_pois <- sapply(x_vals,function(x){
  maxim <- floor(n*l+x*sqrt(n*l))
  ppois(maxim,lambda = n*l) #We use the fact the sum of n Poisson(l) is distributed Poisson(nl)
})

plot(x_vals,CDF_pois,type="l",main="CDF of Zn for Poisson(lambda=3)",xlab="x",ylab="P(Zn<=x)")


#d) Uniform {a,..,b}
n <- 50
a <- 1
b <- 5

mean <- (a+b)/2
sigma <- sqrt(((b-a+1)^2-1)/12)

x_vals <- seq(-3,3,by=0.01)

#We take Nrsim simulations and then we sum the rows to get a vector whose value is the sum of n random numbers
#in the interval [a,b]. WE then divide by the mean and multiply by sqrt(n)/sigma. For these values we create the CDF.
#Then we apply it on values from an interval. We can observe that as n grows the CDF starts to look like a CDF from a Normal distribution with mean=0 and var=1
Nrsim <- 1000 #The number of simulations
X_matrix <- matrix(
  sample(a:b,size = n*Nrsim,replace = TRUE), 
  nrow = Nrsim, ncol = n
)

S_vals <- rowSums(X_matrix)

Z_vals <- (S_vals-n*mean)/(sigma*sqrt(n))

Func <- ecdf(Z_vals)

CDF_unif <- sapply(x_vals,Func)

plot(x_vals,CDF_unif,type="l",main="CDF of Zn for Uniform{1,...,5}",xlab="x",ylab="P(Zn<=x)")

#e) Exp(lambda)

n <- 50
l <- 3

x_vals <- seq(-3,3,by=0.01)

CDF_exp <- sapply(x_vals,function(x){
  maxim <- n/l+x*sqrt(n*1/(l)^2)
  pgamma(maxim,shape=n,rate=l)
})

plot(x_vals,CDF_exp,type="l",main="CDF of Zn for Exp(3)",xlab="x",ylab="P(Zn<=x)")


#f)Gamma(alpha,beta)

n <- 50
a <- 2
b <- 3

x_vals <- seq(-3,3,by=0.01)
#For simplicity reasons we use the gamma function with rate instead of scale
#Because we have already calculated the moment generating function for the gamma distribution to prove that sum of exponentials is gamma
CDF_gamma <- sapply(x_vals,function(x){
  maxim <- n*a/b+x*sqrt(n*a/(b^2))
  pgamma(maxim,shape=n*a,rate=b)
})

plot(x_vals,CDF_gamma,type="l",main="CDF of Zn for Gamma(2,3)",xlab="x",ylab="P(Zn<=x)")


#g) Beta(alpha,beta)

n <- 50
a <- 2
b <- 4

x_vals <- seq(-3,3,by=0.01)

mean <- a/(a+b)
sigma <- sqrt((a*b)/((a+b)^2*(a+b+1)))

Nrsim <- 1000
X_matrix <- matrix(rbeta(n*Nrsim,shape1=a,shape2=b),nrow=Nrsim,ncol=n)

S_vals <- rowSums(X_matrix)

Z_vals <- (S_vals-n*mean)/(sigma*sqrt(n))

Func <- ecdf(Z_vals)

CDF_beta <- sapply(x_vals,Func)

plot(x_vals,CDF_beta,type="l",main="CDF of Zn for Beta(2,4)",xlab="x",ylab="P(Zn<=x)")

#3) 

#a) Binomial(r,p)

n <- 5000
r <- 5
p <- 0.4

# Function that calculates the abs(P(Zn<=x)-fi(x)) 
err_binom <- function(x){
  maxim <- floor(n*r*p+x*sqrt(n*r*p*(1-p)))
  
  P_Zn <- pbinom(maxim,size=r*n,prob=p)
  fi <- pnorm(x)
  
  abs(P_Zn-fi)
}

res <- optimize(err_binom,interval = c(-3,3),maximum=TRUE)
cat("The best x value: ",res$maximum)
cat("The value of function at that point: ",res$objective)

#b) Geometric(p)

n <- 5000
p <- 0.4

mean <- 1/p
sigma <- sqrt((1-p)/p^2)

Nrsim <- 1000 #Nr of simulations

X_matrix <- matrix(rgeom(Nrsim*n,prob=p),nrow=Nrsim,ncol=n)+1

S_vals <- rowSums(X_matrix)

Z_vals <- (S_vals-n*mean)/(sigma*sqrt(n))

Func <- ecdf(Z_vals)

err_geometric <- function(x){
  abs(Func(x)-pnorm(x))
}
res_geom <- optimize(err_geometric,interval=c(-3,3),maximum=TRUE)
cat("The best x value: ",res_geom$maximum)
cat("The value of function at that point: ",res_geom$objective)

#c) Poisson(lambda)

n <- 5000
l <- 2

err_pois <- function(x){
  maxim <- floor(n*l+x*sqrt(n*l))
  F_Zn <- ppois(maxim,lambda=n*l)
  abs(F_Zn-pnorm(x))
}

res_pois <- optimize(err_pois,interval=c(-3,3),maximum=TRUE)
cat("The best x value: ",res_pois$maximum)
cat("The value of function at that point: ",res_pois$objective)

#d) Unif{a,...,b}

n <- 5000
a <- 1
b <- 5

mean <- (a+b)/2
sigma <- sqrt(((b-a+1)^2-1)/12)

Nrsim <- 1000
X_matrix <- matrix(sample(a:b,size=n*Nrsim,replace=TRUE),nrow=Nrsim,ncol=n)

S_vals <- rowSums(X_matrix)
Z_vals <- (S_vals-n*mean)/(sigma*sqrt(n))

Func <- ecdf(Z_vals)

err_unif <- function(x){
  abs(Func(x)-pnorm(x))
}
res_unif <- optimize(err_unif,interval=c(-3,3),maximum=TRUE)
cat("The best x value: ",res_unif$maximum)
cat("The value of function at that point: ",res_unif$objective)

#e) Exp(lambda)

n <- 5000
l <- 3

err_exp <- function(x){
  maxim <- n/l+x*sqrt(n/(l^2))
  F_Zn <- pgamma(maxim,shape=n,rate=l)
  abs(F_Zn-pnorm(x))
}

res_exp <- optimize(err_exp,interval=c(-3,3),maximum=TRUE)
cat("The best x value: ",res_exp$maximum)
cat("The value of function at that point: ",res_exp$objective)

#f) Gamma(alpha,beta)

n <- 5000
a <- 2
b <- 3

err_gamma <- function(x){
  maxim <- n*a*b+x*sqrt(n*a*(b^2))
  F_Zn <- pgamma(maxim,shape=n*a,scale=b)
  abs(F_Zn-pnorm(x))
}

res_gamma <- optimize(err_gamma,interval=c(-3,3),maximum=TRUE)
cat("The best x value: ",res_gamma$maximum)
cat("The value of function at that point: ",res_gamma$objective)

#g) Beta(alpha,beta)

n <- 5000
a <- 2
b <- 3

mean <- a/(a+b)
sigma=sqrt((a*b)/((a+b)^2*(a+b+1)))

Nrsim <- 1000
X_matrix <- matrix(rbeta(n*Nrsim,shape1=a,shape2=b),nrow=Nrsim,ncol=n)

S_vals <- rowSums(X_matrix)

Z_vals <- (S_vals-n*mean)/(sigma*sqrt(n))

Func <- ecdf(Z_vals)

err_beta <- function(x){
  abs(Func(x)-pnorm(x))
}

res_beta <- optimize(err_beta,interval=c(-3,3),maximum=TRUE)
cat("The best x value: ",res_beta$maximum)
cat("The value of function at that point: ",res_beta$objective)

#4)

calculate_mean_var <- function(prob_func,x_values=NULL,type="Unknown",lower=-inf,upper=Inf){
  if(type == "Unknown"){
    stop("Type must be discrete or continuous!!!")
  }
  
  if(type == "discrete"){
    E_X <- sum(x_values*prob_func)
    Var_X <- sum((x_values-E_X)^2*prob_func)
    return(list(Mean=E_X,Var=Var_X))
  }
  
  if(type=="continuous"){
    E_X <- integrate(function(x) x*prob_func(x),lower,upper)$value
    Var_X <- integrate(function(x) (x-E_X)^2*prob_func(x),lower,upper)$value
    return(list(Mean=E_X,Var=Var_X))
  }
}
#Example discrete
x_vals <- c(1,2,3)
PMF <- c(0.2,0.5,0.3)

calculate_mean_var(PMF,x_vals,type="discrete")

#Example discrete(dice roll)
x_values <- 1:6
pmf <- rep(1/6,6)

calculate_mean_var(pmf,x_values,type="discrete")

#Example continuous(Exponential(2))
l <- 2
pdf <- function(x){
  ifelse(x>=0,l*exp(-l*x),0)
}
calculate_mean_var(pdf,type="continuous",lower=0,upper=Inf)

#5)

calculate_abs_moment <- function(prob_func,x_values=NULL,type="Unknown",lower=-inf,upper=Inf){
  if(type == "Unknown"){
    stop("Type must be discrete or continuous!!!")
  }
  
  if(type == "discrete"){
    mean <- sum(x_values*prob_func)
    
    abs_moment <- sum(abs(x_values-mean)^3*prob_func)
    return(abs_moment)
  }
  
  if(type=="continuous"){
    mean <- integrate(function(x) x*prob_func(x),lower,upper)$value
    
    abs_moment <- integrate(function(x) abs(x-mean)^3*prob_func(x),lower,upper)$value
    return(abs_moment)
  }
}

#Example discrete
x_vals <- c(1,2,3)
pmf <- c(0.2,0.5,0.3)

calculate_abs_moment(pmf,x_vals,type="discrete")

#Example continuous(Exponential(2))
l <- 2
pdf <- function(x){
  ifelse(x>=0,l*exp(-l*x),0)
}
calculate_abs_moment(pdf,type="continuous",lower=0,upper=Inf)

#6)
#Function to calculate the Berry Esseen bound
berry_esseen_bound <- function(E_abs_cube,sigma,n){
  (33/4)*(E_abs_cube/(sqrt(n)*sigma^3))
}

n_vals <- c(30,100,1000)

#Define the data frame
results <- data.frame(
  distribution=character(),
  n=numeric(),
  Berry_Esseen_Bound=numeric(),
  stringsAsFactors = FALSE
)

#Now we will add to the date frame

#a) Binomial(r=50,p=0.4)
for (n in n_vals){
  r <- 50
  p <- 0.4
  x_vals <- 1:r
  
  pmf <- dbinom(x_vals,size=r,prob=p)
  
  moments <- calculate_mean_var(pmf,x_vals,type="discrete")
  
  abs_moment_3 <- calculate_abs_moment(pmf,x_vals,type="discrete")

  sigma <- sqrt(moments$Var)

  bound <- berry_esseen_bound(abs_moment_3,sigma,n)
  
  results <- rbind(results,data.frame(distribution="Binomial(r=50,p=0.4)",n=n,Berry_Esseen_Bound=bound))
}

#b) Geometric(p=0.4)
for (n in n_vals){
  p <- 0.4
  x_vals <- 1:50
  
  pmf <- dgeom(x_vals-1,prob=p)
  
  moments <- calculate_mean_var(pmf,x_vals,type="discrete")
  
  abs_moment_3 <- calculate_abs_moment(pmf,x_vals,type="discrete")
  
  sigma <- sqrt(moments$Var)
  
  bound <- berry_esseen_bound(abs_moment_3,sigma,n)
  
  results <- rbind(results,data.frame(distribution="Geometric(p=0.4)",n=n,Berry_Esseen_Bound=bound))
}

#c) Poisson(lambda=4)
for (n in n_vals){
  l <- 4
  x_vals <- 0:20
  
  pmf <- dpois(x_vals,lambda=l)
  
  moments <- calculate_mean_var(pmf,x_vals,type="discrete")
  
  abs_moment_3 <- calculate_abs_moment(pmf,x_vals,type="discrete")
  
  sigma <- sqrt(moments$Var)
  
  bound <- berry_esseen_bound(abs_moment_3,sigma,n)
  
  results <- rbind(results,data.frame(distribution="Poisson(lambda=4)",n=n,Berry_Esseen_Bound=bound))
}

#d) Unif{1,...,5}
for (n in n_vals){
  a <- 1
  b <- 5
  
  x_vals=a:b
  
  pmf <- rep(1/length(x_vals),length(x_vals))
  
  moments <- calculate_mean_var(pmf,x_vals,type="discrete")
  
  abs_moment_3 <- calculate_abs_moment(pmf,x_vals,type="discrete")
  
  sigma <- sqrt(moments$Var)
  
  bound <- berry_esseen_bound(abs_moment_3,sigma,n)
  
  results <- rbind(results,data.frame(distribution="Uniform{1,...,5}",n=n,Berry_Esseen_Bound=bound))
}

#e) Exponential(lambda=3)
for (n in n_vals){
  l <- 3
  
  
  pdf <- function(x){
    ifelse(x>=0,l*exp(-l*x),0)
  }
  
  moments <- calculate_mean_var(pdf,type="continuous",lower=0,upper=Inf)
  
  abs_moment_3 <- calculate_abs_moment(pdf,type="continuous",lower=0,upper=Inf)
  
  sigma <- sqrt(moments$Var)
  
  bound <- berry_esseen_bound(abs_moment_3,sigma,n)
  
  results <- rbind(results,data.frame(distribution="Exponential(lambda=3)",n=n,Berry_Esseen_Bound=bound))
}

#f) Gamma(alpha=2,beta=3)
for (n in n_vals){
  a <- 2
  b <- 3
  
  pdf <- function(x){
    dgamma(x,shape=a,scale=b)
  }
  
  moments <- calculate_mean_var(pdf,type="continuous",lower=0,upper=Inf)
  
  abs_moment_3 <- calculate_abs_moment(pdf,type="continuous",lower=0,upper=Inf)
  
  sigma <- sqrt(moments$Var)
  
  bound <- berry_esseen_bound(abs_moment_3,sigma,n)
  
  results <- rbind(results,data.frame(distribution="Gamma(alpha=2,beta=3)",n=n,Berry_Esseen_Bound=bound))
}

#g) Beta(alpha=2,beta=3)
for (n in n_vals){
  a <- 2
  b <- 3
  
  pdf <- function(x){
    dbeta(x,shape1=a,shape2=b)
  }
  
  moments <- calculate_mean_var(pdf,type="continuous",lower=0,upper=1)
  
  abs_moment_3 <- calculate_abs_moment(pdf,type="continuous",lower=0,upper=1)
  
  sigma <- sqrt(moments$Var)
  
  bound <- berry_esseen_bound(abs_moment_3,sigma,n)
  
  results <- rbind(results,data.frame(distribution="Beta(alpha=2,beta=3)",n=n,Berry_Esseen_Bound=bound))
}
#Show the results
View(results)


#7)

plot_diff <- function(distribution,n_vals,x_range,CDF_func=NULL,simulate_func=NULL){
  x_vals <- seq(x_range[1],x_range[2],length.out=100)
  colors <- c("red","green","blue")
  
  plot(NULL,xli=x_range,ylim=c(-0.1,0.1),type="n",xlab="x",ylab="P(Zn<=x)-Phi(x)",main=paste("Difference for", distribution))
  
  for (i in seq_along(n_vals)){
    n <- n_vals[i]
    if(!is.null(CDF_func)){
      CDF_Zn <- sapply(x_vals,function(x) CDF_func(x,n))
    }
    else if(!is.null(simulate_func)){
      Z_vals <- simulate_func(n)
      Func <- ecdf(Z_vals)
      CDF_Zn <- sapply(x_vals,Func)
    }
    else{
      stop("Either CDF_func or simulate_func must be provided")
    }
    
    CDF_Phi <- pnorm(x_vals)
    
    lines(x_vals,CDF_Zn-CDF_Phi,col=colors[i],lwd=1,lty=1)
  }
  legend("topright",legend=paste("n=",n_vals),col=colors,lwd=1,lty=1)
}

#We know the CDF for these distributions
CDF_binomial <- function(x,n){
  r <- 50
  p <- 0.4
  maxim <- floor(n*r*p+x*sqrt(n*r*p*(1-p)))
  pbinom(maxim,size=n*r,prob=p)
}

CDF_poisson <- function(x,n){
  l <- 3
  maxim <- floor(n*l+x*sqrt(n*l))
  ppois(maxim,lambda=n*l)
}

CDF_exponential <- function(x,n){
  l <- 3
  maxim <- n/l+x*sqrt(n/l^2)
  pgamma(maxim,shape=n,rate=l)
}

CDF_gamma <- function(x,n){
  a <- 2
  b <- 3
  maxim <- n*a*b+x*sqrt(n*a*b^2)
  pgamma(maxim,shape=n*a,scale=b)
}

#For the following we need to simulate
simulate_geometric <- function(n){
  p <- 0.4
  mean <- 1/p
  sigma <- sqrt((1-p)/p^2)
  S_n <- replicate(10000,sum(rgeom(n,prob=p)+1))
  (S_n-n*mean)/(sqrt(n)*sigma)
}

simulate_uniform <- function(n){
  a <- 1
  b <- 5
  mean <- (a+b)/2
  sigma <- sqrt(((b-a+1)^2-1)/12)
  S_n <- replicate(10000,sum(sample(a:b,n,replace=TRUE)))
  (S_n-n*mean)/(sqrt(n)*sigma)
}

simulate_beta <- function(n){
  a <- 2
  b <- 3
  mean <- a/(a+b)
  sigma <- sqrt((a*b)/((a+b)^2*(a+b+1)))
  S_n <- replicate(10000,sum(rbeta(n,shape1=a,shape2=b)))
  (S_n-n*mean)/(sqrt(n)*sigma)
}

n_vals <- c(30, 100, 1000)
x_range <- c(-3, 3)

#Binomial
plot_diff("Binomial(r=50,p=0.4)",n_vals,x_range,CDF_func=CDF_binomial)

#Geometric
plot_diff("Geometric(p=0.4)",n_vals,x_range,simulate_func=simulate_geometric)

#Poisson
plot_diff("Poisson(lambda=3)",n_vals,x_range,CDF_func=CDF_poisson)

#Uniform
plot_diff("Uniform{1,...,5}",n_vals,x_range,simulate_func=simulate_uniform)

#Exponential
plot_diff("Exponential(lambda=3)",n_vals,x_range,CDF_func=CDF_exponential)

#Gamma
plot_diff("Gamma(alpha=2,beta=3)",n_vals,x_range,CDF_func=CDF_gamma)

#BEta
plot_diff("Beta(alpha=2,beta=3))",n_vals,x_range,simulate_func=simulate_beta)

#8)
#Function that calculates the berry esseen bound for:
#A given PMF or PDF (prob_func)
#x_values is the support of the random variable (for discrete distribution)
#n is the sample size

calculate_berry_esseen <- function(prob_func,x_values=NULL,type="Unknown",n,lower=-Inf,upper=Inf){
  if(type == "Unknown"){
    stop("Type must be discrete or continuous")
  }
  
  moments <- calculate_mean_var(prob_func,x_values,type=type,lower=lower,upper=upper)
  sigma <- sqrt(moments$Var)
  
  abs_moment_3 <- calculate_abs_moment(prob_func,x_values,type=type,lower=lower,upper=upper)
  
  C <- 33/4
  bound <- (C*abs_moment_3)/(sigma^3*sqrt(n))
  return(bound)
}
#A couple of examples

#Binomial(r=50,p=0.4)
r <- 50
p <- 0.4
n <- 100
x_vals <- 0:r
pmf <- dbinom(x_vals,size=r,prob=p)

berry_esseen_binom <- calculate_berry_esseen(pmf,x_vals,type="discrete",n=n)
print(berry_esseen_binom)

#Geometric(p=0.4)
p <- 0.4
n <- 100
x_vals <- 1:50
pmf <- dgeom(x_vals,prob=p)

berry_esseen_geom <- calculate_berry_esseen(pmf,x_vals,type="discrete",n=n)
print(berry_esseen_geom)

#Exponential(lambda=3)
l <- 3
n <- 100
pdf <- function(x) ifelse(x>=0,l*exp(-l*x),0)

berry_esseen_exp <- calculate_berry_esseen(pdf,type="continuous",n=n,lowe=0,upper=Inf)
print(berry_esseen_exp)

#Weibull Distribution (from coockbook)

l <- 2
k <- 3
n <- 1000

weibull_pdf <- function(x){
  ifelse(x>=0,(k/l)*(k/l)^(k-1)*exp(-(x/l)^k),0)
}

berry_esseen_weibull <- calculate_berry_esseen(pdf,type="continuous",n=n,lowe=0,upper=Inf)
print(berry_esseen_weibull)







