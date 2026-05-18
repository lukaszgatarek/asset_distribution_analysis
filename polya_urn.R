polya_urn <- function(alpha0, beta0, n_steps, reinforcement = 1) {
  
  # initial states
  alpha <- alpha0  # number of A balls
  beta  <- beta0   # number of B balls
  
  # history storage
  history <- data.frame(
    step = 0,
    alpha = alpha,
    beta = beta,
    prob_A = alpha / (alpha + beta)
  )
  
  for (t in 1:n_steps) {
    
    # probability of drawing A
    pA <- alpha / (alpha + beta)
    
    # draw
    draw_A <- rbinom(1, 1, pA)  # 1 = A, 0 = B
    
    # urn update (reinforcement)
    if (draw_A == 1) {
      alpha <- alpha + reinforcement
    } else {
      beta <- beta + reinforcement
    }
    
    # save step
    history <- rbind(history, data.frame(
      step = t,
      alpha = alpha,
      beta = beta,
      prob_A = alpha / (alpha + beta)
    ))
  }
  
  return(history)
}




result <- polya_urn(
  alpha0 = 0.1,
  beta0 = 0.1,
  n_steps = 100,
  reinforcement = 1
)

head(result)
plot(result$step, result$prob_A, type = "l", xlab = "step", ylab = "P(A)", main = "Pólya urn: evolution of porbability")

# number of simulations
n_sim <- 500

# vector to store final probabilities
final_probs <- numeric(n_sim)

set.seed(123)

for (i in 1:n_sim) {
  
  result <- polya_urn(
    alpha0 = 0.5,
    beta0 = 0.5,
    n_steps = 100,
    reinforcement = 1
  )
  
  # store final value of prob_A
  final_probs[i] <- tail(result$prob_A, 1)
}

# plot distribution of final probabilities
hist(final_probs,
     breaks = 30,
     main = "Distribution of final P(A) in Polya Urn",
     xlab = "Final P(A)",
     col = "lightblue")

# optional summary
summary(final_probs)