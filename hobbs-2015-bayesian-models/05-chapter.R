# Load
library(tictoc)

# Function
f <- function (a, b) {
	p <- rbeta(1e7, a, b)
	hist(p, breaks = 200)
	quantile(p, probs = c(0.025, 0.975))
}
# Rough: want q025 = 0.1 and q975 = 0.5
f(1, 1)
f(2, 4)
f(4, 8)
# ...
f(4.87, 12.82)
#      2.5%     97.5% 
# 0.1002785 0.4983400 

# Define objective
g <- function (x) {
	abs(0.025 - pbeta(0.1, x[1], x[2])) + abs(0.975 - pbeta(0.5, x[1], x[2]))
}
# optim
{
  tic("optim()")
  opt <- optim(c(4,12), g)
  toc()	
}
# optim(): 0.001 sec elapsed
opt
# $par
# [1]  4.829394 12.677369
# 
# $value
# [1] 3.742098e-10
# 
# $counts
# function gradient 
#      117       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL

# nlminb
{
  tic("nlminb()")
  nlb <- nlminb(c(4, 12), g)
  toc()
}
# nlminb(): 0.001 sec elapsed
nlb
# $par
# [1]  4.829394 12.677369
# 
# $objective
# [1] 6.563729e-10
# 
# $convergence
# [1] 0
# 
# $iterations
# [1] 25
# 
# $evaluations
# function gradient 
# 54       50 
# 
# $message
# [1] "X-convergence (3)"

# Check
f(nlb$par[1], nlb$par[2])
#       2.5%      97.5% 
# 0.09995771 0.50001494 
