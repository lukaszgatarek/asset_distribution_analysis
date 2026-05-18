polya_urn_rl <- function(alpha0, beta0, n_steps,
                         pA_true = 0.7,
                         pB_true = 0.4) {
  
  # ============================================================
  # ???? INITIALIZATION (AGENT'S INTERNAL STATE / BELIEF)
  # ============================================================
  
  # alpha and beta represent the agent's internal belief
  # about how good actions A and B are.
  #
  # RL interpretation:
  # ???? These are NOT true environment values
  # ???? They are learned estimates of action value
  #
  # You can think of them as:
  # - alpha = "evidence supporting action A"
  # - beta  = "evidence supporting action B"
  alpha <- alpha0
  beta  <- beta0
  
  # We store the learning trajectory for analysis
  history <- data.frame(
    step = 0,
    alpha = alpha,
    beta = beta,
    prob_A = alpha / (alpha + beta),
    
    # RL diagnostics:
    action = NA,   # which action was taken
    reward = NA    # reward received from environment
  )
  
  # ============================================================
  # ???? MAIN REINFORCEMENT LEARNING LOOP
  # (agent interacts with environment repeatedly)
  # ============================================================
  for (t in 1:n_steps) {
    
    # ------------------------------------------------------------
    # 1. POLICY (DECISION MAKING RULE)
    # ------------------------------------------------------------
    
    # The agent computes probability of choosing action A
    # based on its current belief (alpha, beta)
    pA <- alpha / (alpha + beta)
    
    # RL interpretation:
    # ???? This is the policy ??(A)
    # ???? It is stochastic (probabilistic decision making)
    # ???? More evidence for A › higher probability of choosing A
    
    # This naturally balances:
    # - exploitation (choose what looks best)
    # - exploration (still some randomness)
    
    # ------------------------------------------------------------
    # 2. ACTION SELECTION (SAMPLING FROM POLICY)
    # ------------------------------------------------------------
    
    action_A <- rbinom(1, 1, pA)
    
    # Interpretation:
    # 1 = choose action A
    # 0 = choose action B
    
    # RL view:
    # ???? agent samples action from ??(a)
    # ???? this is a stochastic policy execution step
    
    # ------------------------------------------------------------
    # 3. ENVIRONMENT RESPONSE (TRUE REWARD GENERATION)
    # ------------------------------------------------------------
    
    if (action_A == 1) {
      
      action <- "A"
      
      # The environment returns a reward for action A
      # IMPORTANT:
      # ???? pA_true is unknown to the agent
      # ???? this is the "hidden reality" the agent is trying to learn
      
      reward <- rbinom(1, 1, pA_true)
      
      # --------------------------------------------------------
      # 4. LEARNING / UPDATE RULE (CORE RL MECHANISM)
      # --------------------------------------------------------
      
      # If reward = 1:
      # ???? strengthen belief in action A
      # If reward = 0:
      # ???? no reinforcement happens (in this simple model)
      
      alpha <- alpha + reward
      
      # Note:
      # beta is unchanged because action B was not chosen
      
      # RL interpretation:
      # ???? this is a simple reinforcement rule
      # ???? similar to Hebbian learning: "what is rewarded is strengthened"
      
    } else {
      
      action <- "B"
      
      # Environment generates reward for action B
      reward <- rbinom(1, 1, pB_true)
      
      # Update belief for B
      beta <- beta + reward
    }
    
    # ------------------------------------------------------------
    # 5. LOGGING (FOR ANALYSIS OF LEARNING DYNAMICS)
    # ------------------------------------------------------------
    
    history <- rbind(history, data.frame(
      step = t,
      alpha = alpha,
      beta = beta,
      
      # current estimated policy after update
      prob_A = alpha / (alpha + beta),
      
      # action taken at this step
      action = action,
      
      # reward received from environment
      reward = reward
    ))
  }
  
  # ============================================================
  # OUTPUT: FULL LEARNING TRAJECTORY
  # ============================================================
  
  # In RL terms, this contains:
  # - policy evolution over time
  # - learning dynamics
  # - convergence behavior (does it stabilize on A or B?)
  
  return(history)
}


result <- polya_urn_rl(
  alpha0 = 1,
  beta0 = 1,
  n_steps = 200,
  pA_true = 0.05,
  pB_true = 0.4
)


plot(result$step, result$prob_A,
     type = "l",
     xlab = "Step",
     ylab = "P(A)",
     main = "Learning dynamics (RL bandit)")

