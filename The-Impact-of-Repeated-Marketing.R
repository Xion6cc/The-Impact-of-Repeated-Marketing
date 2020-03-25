# Load libraries
library(dplyr)
library(MatchIt)
library(ggplot2)

# read data
bank_mrktng <- read.csv("<path> /bank-additional-full.csv", sep=";", header=TRUE)

# Data source: https://archive.ics.uci.edu/ml/datasets/Bank+Marketing

# Visualization

## Distribution of Age
ggplot(bank_mrktng, aes(age)) + geom_histogram() +
  labs(title = 'Distribution of Age', y="Number of People",x='Age')

## Distribution of Having Credit Card
ggplot(bank_mrktng, aes(default)) + geom_bar(stat = 'count',position=position_dodge()) +
  labs(title = 'Distribution of Having Credit Card', x='Credit Card', y="Number of People")

## Distribution of Having Personal Loan
ggplot(bank_mrktng, aes(loan)) + geom_bar(stat = 'count',position=position_dodge())+
  labs(title = 'Distribution of Having Personal Loan', y='Number of People', x='Loan')

## Distribution of Ways of Contact
ggplot(bank_mrktng, aes(contact)) + geom_bar(stat = 'count',position=position_dodge()) +
  labs(title = 'Distribution of Ways of Contact', x='Way of Contact', y='Number of People')

## Distribution of Last Contact Month
bank_mrktng %>%
  mutate(month = factor(month, levels = c("mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))) %>% 
  ggplot(aes(month)) + geom_bar(stat = 'count',position=position_dodge()) +
  labs(title = 'Distribution of Last Contact Month', x='Month', y='Number of People')

## Distribution of Last Contact Day of Week
ggplot(bank_mrktng, aes(day_of_week)) +
  geom_bar(stat = 'count',position=position_dodge()) +
  labs(title = 'Distribution of Last Contact Day of Week', x='Day of Week', y='Number of People')

## Distribution of Last Contact Duration
ggplot(bank_mrktng, aes(duration,fill=y)) +
  geom_histogram() +
  xlim(0,1500) +
  labs(title = 'Distribution of Last Contact Duration', x='Duration', y='Number of People') +
  scale_fill_discrete(name = "Subscription")

# Distribution of campaign calls
ggplot(bank_mrktng %>% group_by(campaign) %>% count() , aes(x=campaign, y=n)) +
  geom_bar(stat = "identity") +
  xlab("Number of calls") +
  ylab("Count")

# Add treatment flag
bank_mrktng <- bank_mrktng %>% mutate(treatment = ifelse(campaign <=2, 0, 1))
bank_mrktng %>% group_by(treatment) %>% count()

# Perform randomization checks on treatment vs control
t.test(age ~ treatment, data = bank_mrktng)
chisq.test(bank_mrktng$treatment, bank_mrktng$job)
chisq.test(bank_mrktng$treatment, bank_mrktng$marital)
chisq.test(bank_mrktng$treatment, bank_mrktng$education)
chisq.test(bank_mrktng$treatment, bank_mrktng$loan)
chisq.test(bank_mrktng$treatment, bank_mrktng$contact)
chisq.test(bank_mrktng$treatment, bank_mrktng$month)
chisq.test(bank_mrktng$treatment, bank_mrktng$day_of_week)

# Divide based on contact frequency
mktg_lessThan3 <- bank_mrktng %>% filter(campaign <= 2)
mktg_moreThan2 <- bank_mrktng %>% filter(campaign > 2)
# Check distribution of propensity scores
PScore = glm(treatment ~ age + job + marital + education + default + loan + contact + month +
               day_of_week + duration , data = bank_mrktng, family = "binomial")$fitted.values
bank_mrktng$PScore = PScore
ggplot(bank_mrktng, aes(x = PScore, color = factor(treatment))) +
  geom_density() +
  xlab("propensity score") +
  theme_minimal()
# Control matching to get similar treatment and control groups
matched_IDs <- matchit(treatment ~ age + job + marital + education + default + loan + contact + month +
                         day_of_week + duration, data = bank_mrktng, method = 'nearest', distance = "logit", caliper = 0.001,
                       replace = FALSE, ratio = 1)
summary(matched_IDs)
matched_data = match.data(matched_IDs)
# Evaluatepropensity score distribution, after matching
ggplot(matched_data, aes(x = PScore, color = factor(treatment))) +
  geom_density() + xlab("propensity score") + theme_minimal()
# Check if greater than 2 calls have an effect on client subscribing to term deposit
matched_data$y_num <- ifelse(matched_data$y == "yes", 1, 0)
summary(glm(y ~ treatment, data = matched_data, family="binomial"))
# Sensitivity analysis by changing caliper
matched_IDs_2 <- matchit(treatment ~ age + job + marital + education + default + loan + contact +
                           month + day_of_week + duration, data = bank_mrktng, method = 'nearest', distance = "logit", caliper =
                           0.005, replace = FALSE, ratio = 1)
summary(matched_IDs_2)
matched_data_2 = match.data(matched_IDs_2)

# Plot propensity score distribution
ggplot(matched_data_2, aes(x = PScore, color = factor(treatment))) +
  geom_density() + xlab("propensity score") + theme_minimal()

# Estimate effect
summary(glm(y ~ treatment, data = matched_data_2, family="binomial"))

# Stress test by changing parameters

# Let's change parameters that showed little variation wrt treatment
matched_IDs_3 <- matchit(treatment ~ job + default + loan + contact + month + day_of_week + duration,
                         data = bank_mrktng, method = 'nearest', distance = "logit", caliper = 0.001, replace = FALSE, ratio = 1)
summary(matched_IDs_3)
matched_data_3 = match.data(matched_IDs_3)

# Plot propensity score distribution
ggplot(matched_data_3, aes(x = PScore, color = factor(treatment))) +
  geom_density() + xlab("propensity score") + theme_minimal()

# Estimate effect
summary(glm(y ~ treatment, data = matched_data_3, family="binomial"))

# Checks for Heterogeneous treatment effect
print(summary(glm(y ~ treatment*job , data = matched_data, family="binomial")))
print(summary(glm(y ~ treatment*marital , data = matched_data, family="binomial")))
print(summary(glm(y ~ treatment*education , data = matched_data, family="binomial")))
print(summary(glm(y ~ treatment*housing , data = matched_data, family="binomial")))
print(summary(glm(y ~ treatment*loan , data = matched_data, family="binomial")))
print(summary(glm(y ~ treatment*contact , data = matched_data, family="binomial")))
print(summary(glm(y ~ treatment*month , data = matched_data, family="binomial")))
print(summary(glm(y ~ treatment*day_of_week , data = matched_data, family="binomial")))