library(rethinking)

p_grid <-  seq(from=0, to=1, length.out=1000)

#data <- rep(1, 3)

unstd_prior <-  rep(1, 1000)
#bino_dist <- dbinom(data, length(data), p_grid)
likelihood <- dbinom(3, 3, p_grid)
unstd_posterior <-  unstd_prior * likelihood
std_posterior <-  unstd_posterior/sum(unstd_posterior)

plot(p_grid,std_posterior, type='b')


#HW1
#4water and 11 Land   --> W-1, L-->0
likelihood <- dbinom(4, 15, p_grid)
unstd_posterior <-  unstd_prior * likelihood
std_posterior <-  unstd_posterior/sum(unstd_posterior)

plot(p_grid,std_posterior, type='b')

#Alter
curve( dbeta(x,4+1,11+1) , from=0 , to=1 , xlab="p" )


#HW2 Posterior predictive
#First computing sample probabilities from the posterior
post_sample <- sample(p_grid, prob = std_posterior, size = 10000, replace = TRUE)
#example
w <- rbinom(10, size=5, prob=0.6) # 10 simulated predictions of 5 coin tosses
simplehist(w)
#Now posterior predictive dist for next 5 tosses
post_pred <-  rbinom(10000, size=5, prob=post_sample)
simplehist(post_pred)
plot(table(post_pred))

#Alter:
p_samples <- rbeta(1e4,4+1,11+1)
W_sim <- rbinom(1e4,size=5,p=p_samples)
plot(table(W_sim))


#HW-3 Three or more water samples in the next 5 tosses
prob_3 <-  sum(post_pred >=3)/1e4


#HW-4 p is known here. so we create a distribution around N
w <- 5
p <- 0.7

N_grid <- seq(from=5, to=100, by=1)
N_prior <-  rep(1, 96)

db_bino <- dbinom(5, N_grid, prob=0.7)
N_unstd_post <- db_bino * N_prior
N_std_post <-  N_unstd_post/sum(N_unstd_post)

N_sample_post <-  sample(N_grid, size=1000, prob=N_std_post, replace=TRUE)
simplehist(N_sample_post)
