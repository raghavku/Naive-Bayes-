install.packages("naivebayes")
library(naivebayes)
?naive_bayes

# ---------------------------------------------------------------------------------------------------------------------
# Example 1 Naive Bayes: Iris Data Set
# ---------------------------------------------------------------------------------------------------------------------
attach(iris)
data(iris)
nb <- naive_bayes(Species ~ ., data = iris)  #Naive Bayes Plot
plot(nb, ask = TRUE) # Density plots for Species (response variable) and each explanatory variable


#nb_kernel <- naive_bayes(x = iris[-5], y = iris[ ,5], usekernel = T) #Naive Bayes Plot using kernel
#plot(nb_kernel, ask=TRUE)

library(rpart)
library(party)
fit <- ctree(Species ~ ., data = iris)
plot(fit) #Decision Tree Plot

rm(fit)
# Exploratory analysis. These are "mosaic plots".
attach(iris)
plot(Species ~ Petal.Length, data = iris)
plot(Species ~ Petal.Width, data = iris)
plot(Species ~ Sepal.Length, data = iris)
plot(Species ~ Sepal.Width, data = iris)


# ---------------------------------------------------------------------------------------------------------------------
# Example 2 Naive Bayes: Coin Toss (one fair, one unfair)
# ---------------------------------------------------------------------------------------------------------------------

bayes_theor = function(x, y, cond_xy = (x*y)/y){
  if (x < 0 || x > 1) stop("Invalid probability for x!", call. = FALSE)
  if (y < 0 || y > 1) stop("Invalid probabilty for y!", call. = FALSE)
  if (cond_xy < 0 || cond_xy > 1) stop("Invalid probability for cond_xy!", call. = FALSE)
  return((cond_xy*x)/y)
}

coin_F = c("H", "T")
coin_U = c("H", "H")
dataframe_coin = cbind(coin_F,coin_U)

#Given that the flip is a Heads, what is the prob that the coin is double-sided?

prob_heads = 3/4                 #P(H)
prob_fair = 1/2                  #P(FAIR)
prob_unfair = 1/2                #P(UNFAIR)
prob_heads_given_fair = 1/2      #P(H | FAIR)

bayes_theor(prob_fair, prob_heads, prob_heads_given_fair)
# ---------------------------------------------------------------------------------------------------------------------
#Example 3 Naive Bayes: Red and Blue Dots
# ---------------------------------------------------------------------------------------------------------------------

#load red-blue-points.RData file



summary(known)
#sum(known$colour == "blue")   194
#sum(known$colour == "red")    106

#Area, dividing the plot into 4 quartiles
library(dplyr)
library(pipeR)
known = mutate(known, area = ifelse(x<0 & y<0, "q3",ifelse(x<0 & y>0, "q1",ifelse(x>=0 & y<0, "q4",ifelse(x>=0 & y>=0, "q2","")))))
known

blues <- filter(known, known$colour == 'blue')
reds <- filter(known, known$colour == 'red')

bayes_theor = function(x, y, cond_xy = (x*y)/y){
  if (x < 0 || x > 1) stop("Invalid probability for x!", call. = FALSE)
  if (y < 0 || y > 1) stop("Invalid probabilty for y!", call. = FALSE)
  if (cond_xy < 0 || cond_xy > 1) stop("Invalid probability for cond_xy!", call. = FALSE)
  return((cond_xy*x)/y)
}

# Probability of choosing a blue
prob_blue = 194/300

#sum(blues$area == "q1") 46
#sum(known$area == "q1") 76

# Q1
# Given that the point you choose is blue, this is the probability of it landing in q1
prob_q1 = 1/4
prob_blue_in_q1 = 46 / 76
bayes_theor(prob_q1,prob_blue,prob_blue_in_q1)

#sum(blues$area == "q2") 48
#sum(known$area == "q2") 70

# Q2
prob_q2 = 1/4
prob_blue_in_q2 = 48 / 70
bayes_theor(prob_q2,prob_blue,prob_blue_in_q2)

#sum(blues$area == "q3")  53
#sum(known$area == "q3")  83

# Q3
prob_q3 = 1/4
prob_blue_in_q3 = 53 / 83
bayes_theor(prob_q3,prob_blue,prob_blue_in_q3)

#sum(blues$area == "q4")
#sum(known$area == "q4")

# Q4
prob_q4 = 1/4
prob_blue_in_q4 = 47 / 71
bayes_theor(prob_q4,prob_blue,prob_blue_in_q4)

library(ggplot2)
ggplot(known, aes(x=x,y=y,color=colour)) + geom_point() + geom_hline(mapping = NULL, data = known,yintercept=0) + geom_vline(mapping = NULL, data = known,xintercept = 0)


# ---------------------------------------------------------------------------------------------------------------------
# Helpful Resources
# ---------------------------------------------------------------------------------------------------------------------

# 1. Conditional probability with Bayes' Theorem: https://www.khanacademy.org/math/ap-statistics/probability-ap/stats-conditional-probability/v/bayes-theorem-visualized
# 2. Naive Bayes Classification: https://www.youtube.com/watch?v=8yvBqhm92xA
# 3. Stackoverflow's Explanation of NB: https://stackoverflow.com/questions/10059594/a-simple-explanation-of-naive-bayes-classification
# 4. Naive Bayes Package: https://cran.r-project.org/web/packages/naivebayes/naivebayes.pdf
