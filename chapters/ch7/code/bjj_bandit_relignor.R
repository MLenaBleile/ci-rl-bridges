set.seed(1998)


base_success <- c(AnkleLock = 0.7, Triangle = 0.9, Kimura = 0.5)
skilled_success <- c(AnkleLock = 0.5, Triangle = 0.7, Kimura = 0.3)


observations = c(rep("AnkleLock", 100), rep("Triangle",100),rep("Kimura",100))


fatigued_observations = c(rep("AnkleLock", 100), rep("Triangle",100),rep("Kimura",100))

fatigue = c(rep(0, length(observations)), rep(1, length(fatigued_observations)))
submission <- factor(c(observations, fatigued_observations), levels=names(base_success))

success_prob = base_success[c(observations, fatigued_observations)]
opponent_skill = sample(c(0,1),length(success_prob),replace=T)

skilled_success_prob = skilled_success[c(observations, fatigued_observations)]
success_prob[opponent_skill==1]  = skilled_success_prob[opponent_skill==1]

n=length(submission)

# Simulate binary outcome: 1 = successful submission
outcome <- sapply(1:n,function(x){sample(c(1,0),size=1, prob = c(success_prob[x],1-success_prob[x]), replace=T)})

# Estimate propensity scores (multinomial logistic)
library(nnet)
ps_model <- multinom(submission~fatigue, trace = FALSE)
ps <- predict(ps_model, type = "probs")


# Compute propensity score success rates
library(dplyr)
data <- data.frame(sub=submission, oc=outcome, ps=ps) %>% 
  mutate(ps_weighted= oc*ps)


ps_est <- data %>%
  group_by(sub) %>%
  summarize(success_rate = mean(ps_weighted))




# Get the propensity for the actually chosen action
submission_int <- as.integer(submission)
pi_x <- ps[cbind(1:n, submission_int)]

# Compute marginal probability of each submission
pi_marginal <- table(submission) / n
pi_a <- pi_marginal[submission]

# Compute stabilized weights
sw <- pi_a / pi_x
names(sw)<-NULL

data2 <- data.frame(sub=submission, oc=outcome, ps=sw) %>% 
  mutate( sw_weighted=oc*sw)

weighted_est <- data2 %>%
  group_by(sub) %>%
  summarize(success_rate = mean(sw_weighted))

weighted_est

result= list(
  True = base_success,
  Propensity = ps_est,
  Stabilized = weighted_est
)

print(result)