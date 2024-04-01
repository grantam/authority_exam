library(tidyverse)
library(rstanarm)
library(ggplot2)
library(ggthemes)
library(ggfortify)

# Read in the data.

jesus <- read.csv("Poli 202 quiz_March 27, 2024_11.44.csv", header = T)

# Clean Data

jesus <- as.matrix(jesus)
jesus <- jesus[-1,]
jesus <- jesus[-1,]
jesus <- as.data.frame(jesus)

jesus_t1 <- jesus %>%
  filter(treatment_1. != "") %>%
  select(netted, Duration..in.seconds., , Q1_1:Q10_1) %>%
  rename(time = Duration..in.seconds., q1 = Q1_1, q2 = Q2_1, q2 = Q2_1, q3 = Q3_1, q4 = Q4_1, q5 = Q5_1, q6 = Q6_1, q7 = Q7_1, q8 = Q8_1, q9 = Q9_1, q10 = Q10_1) %>%
  mutate(treat = "Christ")

jesus_t2 <- jesus %>%
  filter(treatment_2 != "") %>%
  select(netted, Duration..in.seconds., Q1_2:Q10_2) %>%
  rename(time = Duration..in.seconds., q1 = Q1_2, q2 = Q2_2, q2 = Q2_2, q3 = Q3_2, q4 = Q4_2, q5 = Q5_2, q6 = Q6_2, q7 = Q7_2, q8 = Q8_2, q9 = Q9_2, q10 = Q10_2) %>%
  mutate(treat = "Professor")

jesus_t3 <- jesus %>%
  filter(treatment_3. != "") %>%
  select(netted, Duration..in.seconds., Q1_3:Q10_3) %>%
  rename(time = Duration..in.seconds., q1 = Q1_3, q2 = Q2_3, q2 = Q2_3, q3 = Q3_3, q4 = Q4_3, q5 = Q5_3, q6 = Q6_3, q7 = Q7_3, q8 = Q8_3, q9 = Q9_3, q10 = Q10_3) %>%
  mutate(treat = "Stock")

jesus_t4 <- jesus %>%
  filter(treatment_4 != "") %>%
  select(netted, Duration..in.seconds., Q1_4:Q10_4) %>%
  rename(time = Duration..in.seconds., q1 = Q1_4, q2 = Q2_4, q2 = Q2_4, q3 = Q3_4, q4 = Q4_4, q5 = Q5_4, q6 = Q6_4, q7 = Q7_4, q8 = Q8_4, q9 = Q9_4, q10 = Q10_4) %>%
  mutate(treat = "Control")


jesus_final <- rbind(jesus_t1, jesus_t2, jesus_t3, jesus_t4) %>%
  mutate(q1 = ifelse(q1 == "Justice in holdings", 1, 0), q2 = ifelse(q2 == "Hypothetical consent, because Rawls thinks that securing actual consent of all to a basic structure is impossible.", 1, 0), q3 = ifelse(q3 == "The power to form a plan for one’s own life (rationality) and the power to form agreements on fair terms with others (reasonability)", 1, 0), q4 = ifelse(q4 == "You can only achieve a patterned principle of justice through interfering with the free choice of others.", 1, 0), q5 = ifelse(q5 == "The rights of persons should not be violated, even if doing so would produce greater utility than any alternative.", 1, 0), q6 = ifelse(q6 == "Imperfect procedural justice", 1, 0), q7 = ifelse(q7 == "A right to own private property.", 1, 0), q8 = ifelse(q8 == "Rawls would likely say that political parties should then be publicly rather than privately funded, and Mill would say that the funding of policies should be according to what best supported utility, understood as the aggregate permanent higher interests.", 1, 0), q9 = ifelse(q9 == "He would say that he favors historical principles, while Rawls and Mill favor versions of patterned principles.", 1, 0), q10 = ifelse(q10 == "Rawls would favor free expression as protected by his first principle of justice, and Mill would favor free expression as promoting utility.", 1, 0), sum = (q1 + q2 + q3+ q4+ q5+ q6 + q7+ q8+ q9+ q10), time = as.double(time), treat = as.factor(treat), logtime = log(time - 115)) %>%
  filter(sum != 0)

jesus_final$treat <- relevel(jesus_final$treat, ref = "Control")

jesus_final$netted <- tolower(jesus_final$netted)

pre <- read.csv("C:/Users/ochoc/Downloads/Pre-Survey Responses_March 26, 2024_17.58.csv", header = T) %>%
  rename(netted = Q2)
pre$netted <- tolower(pre$netted)

pre <- as.matrix(pre)
pre <- pre[-1,]
pre <- pre[-1,]
pre <- as.data.frame(pre)

jesus_final <- left_join(jesus_final, pre, by = "netted") %>%
  mutate(picture = as.factor(ifelse(treat != "Control", "Picture", "None")), author = as.factor(ifelse(treat == "Control" | treat == "Stock", "None", "Author")))


jesus_cov <- jesus_final %>%
  select(Q1, Q5, Q6, Q3, Q4, Q7, Q9, treat, sum) %>%
  na.omit() %>%
  filter(Q9 != "")

# Exploritory Analysis

ggplot(jesus_final) +
  geom_density(aes(x = sum, group = treat, fill = treat)) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) + 
  theme(legend.position = "bottom") +
  facet_wrap(~treat) 

ggplot(jesus_final) +
  geom_boxplot(aes(x = sum, group = treat, fill = treat)) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) + 
  theme(legend.position = "none")


ggplot(jesus_final) +
  geom_density(aes(x = logtime, group = treat, fill = treat)) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) + 
  theme(legend.position = "bottom") +
  facet_wrap(~treat) 

ggplot(jesus_final) +
  geom_boxplot(aes(x = logtime, group = treat, fill = treat)) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) + 
  theme(legend.position = "bottom")

# Models

m1 <- stan_glm(sum ~ treat + log(time), data = jesus_final, refresh = 0)
summary(m1)

m1a <- lm(sum ~ picture, data = jesus_final) 

m2 <- lm(logtime ~ treat, data = jesus_final)
autoplot(m2)
autoplot(m2, which = 4, nrow = 1, ncol = 1) ### not above .5. Leave as is.
autoplot(m2, which = 1)
summary(m2)
shapiro.test(m2$residuals)

jesus_final %>%
  group_by(treat) %>%
  summarise(variance = var(logtime))

power.anova.test(groups = 4, power = .8, between.var = .75, within.var = 4, sig.level = .05)

power.anova.test(groups = 4, n = 25, between.var = .9, within.var = 3.75, sig.level = .05)

# Poster Viz

m3 <- stan_glm(logtime ~ treat, data = jesus_final, refresh = 0)

new <- data.frame(treat = c("Control", "Christ", "Professor", "Stock"))

pred_mat <- posterior_linpred(m3, newdata = new)

predictions <- matrix(NA, nrow = 3, ncol = 3)
holder <- matrix(NA, nrow = 4000, ncol = 3)
for (i in 1:3) {
  holder[,i] <- pred_mat[,i+1] - pred_mat[,1]
  predictions[i,] <- quantile(holder[,i], c(.025, .5, .975))
}

names <- c("Christ", "Professor", "Stock")
colnames(predictions) <- c("lower", "median", "upper")
predictions <- cbind(predictions, names) %>%
  as.data.frame() %>%
  mutate(lower = as.double(lower), median = as.double(median), upper = as.double(upper), g = "Time")
predictions

m4 <- stan_glm(sum ~ treat, data = jesus_final, refresh = 0)

new <- data.frame(treat = c("Control", "Christ", "Professor", "Stock"))

pred_mat1 <- posterior_linpred(m4, newdata = new)

predictions1 <- matrix(NA, nrow = 3, ncol = 3)
holder1 <- matrix(NA, nrow = 4000, ncol = 3)
for (i in 1:3) {
  holder1[,i] <- pred_mat1[,i+1] - pred_mat1[,1]
  predictions1[i,] <- quantile(holder1[,i], c(.025, .5, .975))
}

names <- c("Christ", "Professor", "Stock")
colnames(predictions1) <- c("lower", "median", "upper")
predictions1 <- cbind(predictions1, names) %>%
  as.data.frame() %>%
  mutate(lower = as.double(lower), median = as.double(median), upper = as.double(upper), g = "Score")
predictions1

preds <- rbind(predictions, predictions1)


ggplot(data = preds, aes(color = names, shape = g, group = g)) +
  geom_point(aes(x = median, y = names), size = 6) +
  geom_linerange(aes(xmin = lower, xmax = upper, y = names), linewidth = 2) +
  labs(title = "Effect of Treatment on Test Score", x = "Effect Size", y = "") +
  geom_vline(xintercept = 0, color = "grey", linewidth = 1.5) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = 25), axis.text = element_text(size = 25), plot.title = element_text(size = 35), strip.text = element_text(size = 25), strip.background = element_blank()) +
  scale_color_manual(values = c("gold","#7851A9", "#6699CC")) +
  facet_wrap(~g)

ggplot(data = jesus_final, aes(x = sum)) +
  geom_histogram(binwidth = 1, color = "white", fill = "#7851A9") +
  theme_bw() +
  labs(title = "Histogram of Scores", y = "Frequency", x = "Scores") +
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 25), plot.title = element_text(size = 35), strip.text = element_text(size = 25), strip.background = element_blank())

