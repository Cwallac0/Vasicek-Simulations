# Functions to simulate Vasicek interest rates & plot results

# Simulation function
vasicek_sim <- function(a = a, b = b, r0 = r0, sd =sd, seed = NULL,
                         trials = 10, time = 100, subintervals = 200){
  
  # Model:  drt = a(b-rt)dt + sd*dWt
  # a  = speed of reversion
  # b  = long-term mean level
  # sd = instantaneous volatility
  # r0 = starting interest rate
  # seed = for reproducibility
  # trials = simulations trials
  # time  = total time
  # subintervals = number of "resets"
  
  set.seed(seed)
  dt <- time / subintervals  # difference in time each subinterval
  r <- matrix(0, subintervals + 1, trials)  # matrix to hold short rate paths
  r[1,] <- r0
  
  for(j in 1 : trials){
    for(i in 2 : (subintervals + 1)){
      dr <- a * (b-r[i-1,j]) * dt + sd * sqrt(dt) * rnorm(1,0,1)
      r[i,j] <- r[i-1,j] + dr
    }
  } 
  r <- cbind.data.frame(time = 1:dim(r)[1], r)
  return(r)
}

# Example 
vasicek_sim(a = 0.05, b = 0.10, r0 = 0.03, sd = 0.10, seed = 1234, trials = 15)


# Plotting functions
vasicek_sim_plot <- function(data = data){
  require(ggplot2)
  data %>% 
    gather(sims, rates, -time) %>% 
    ggplot(., aes(time, rates, col = sims)) + geom_line() +
    labs(title = "Vasicek Simulated Short Rates") +
    theme(legend.position = "none") 
}

# Example
data <- vasicek_sim(a = 0.05, b = 0.10, r0 = 0.03, sd = 0.10, seed = 1234, 
                    trials = 15)

vasicek_sim_plot(data)
