# file preview shows a header row
diabetes <- read.csv("/home/juanmange/Documentos/data/analisisdeDatos/diabetes.csv", header = TRUE)

# first look at the data set using summary() and str() to understand what type of data are you working
# with
summary(diabetes)
str(diabetes)

diabetes$Outcome <- factor(diabetes$Outcome)

# removing those observation rows with 0 in any of the variables
for (i in 2:6) {
  diabetes <- diabetes[-which(diabetes[, i] == 0), ]
}

# modify the data column names slightly for easier typing
names(diabetes)[7] <- "dpf"
names(diabetes) <- tolower(names(diabetes))

str(diabetes)
print(paste0("number of observations = ", dim(diabetes)[1]))
print(paste0("number of predictors = ", dim(diabetes)[2]))

par(mfrow = c(2, 2))

# the $ notation can be used to subset the variable you're interested in.
hist(diabetes$pregnancies)
hist(diabetes$age)
hist(diabetes$glucose)
hist(diabetes$bmi)


par(mfrow = c(1, 2))

# boxplot
with(diabetes, boxplot(dpf ~ outcome, 
                       ylab = "Diabetes Pedigree Function", 
                       xlab = "Presence of Diabetes",
                       main = "Figure A",
                       outline = FALSE))

# subsetting based on response
with <- diabetes[diabetes$outcome == 1, ]
without <- diabetes[diabetes$outcome == 0, ]

# density plot
plot(density(with$glucose), 
     xlim = c(0, 250),
     ylim = c(0.00, 0.02),
     xlab = "Glucose Level",
     main = "Figure B",
     lwd = 2)
lines(density(without$glucose), 
      col = "red",
      lwd = 2)
legend("topleft", 
       col = c("black", "red"), 
       legend = c("With Diabetes", "Without Diabetes"), 
       lwd = 2,
       bty = "n")

# simple two sample t-test with unequal variance
t.test(with$dpf, without$dpf)
# correlation matrix
library(reshape2)
cor_melt <- melt(cor(diabetes[, 1:8]))
cor_melt <- cor_melt[which(cor_melt$value > 0.5 & cor_melt$value != 1), ]
cor_melt <- cor_melt[1:3, ]
cor_melt


# correlation matrix
library(reshape2)
cor_melt <- melt(cor(diabetes[, 1:8]))
cor_melt <- cor_melt[which(cor_melt$value > 0.5 & cor_melt$value != 1), ]
cor_melt <- cor_melt[1:3, ]
cor_melt


library(glmnet)

# creating a random set of observations to be in the training set
set.seed(100)
inTrain <- sample(x = seq(1, 392), size = 294, replace = FALSE)

# preparing the inputs for the function cv.glmnet()
# you can use ?glmnet to understand more
x <- model.matrix(outcome ~ . - 1, data = diabetes)
y <- diabetes$outcome

# model fitting with lasso (alpha = 1)
# since response is binary, we'll set the [family = "binomial"] in the argument
# lasso regression also perform variable selection to determine which are the important variables
fit.lasso.cv <- cv.glmnet(x[inTrain, ], y[inTrain], alpha = 1, family = "binomial")
plot(fit.lasso.cv)

print(paste0("minimum binomial deviance = ", round(min(fit.lasso.cv$cvm), 3)))
print(paste0("log(lambda) with minimum binomial deviance = ", round(log(fit.lasso.cv$lambda.min), 3)))
coef(fit.lasso.cv)

# prediction with the validation data set
pred <- predict(fit.lasso.cv, newx = x[-inTrain, ])
pred <- exp(pred) / (1 + exp(pred))
pred <- ifelse(pred >= 0.5, 1, 0)
table(pred, y[-inTrain])

# calculate the accuracy
correct_pred <- sum(table(pred, y[-inTrain])[c(1, 4)])
total <- length(y[-inTrain])
acc <- correct_pred / total
print(paste0("accuracy = ", round(acc, 3)))


library(caret)
fit.glm <- glm(outcome ~ ., data = diabetes[inTrain, ], family = binomial)
pred.glm.logistic <- predict(fit.glm, diabetes[-inTrain, ])
pred.glm <- exp(pred.glm.logistic) / (1 + exp(pred.glm.logistic))
pred.glm <- as.integer(pred.glm >= 0.5)
confusionMatrix(pred.glm, y[-inTrain])[2:3]
par(mfrow = c(2, 2))
plot(fit.glm)


library(randomForest)
set.seed(123)
fit.rf <- randomForest(outcome ~ .,
                       diabetes[inTrain, ],
                       mtry = 3, # number of predictors to use for generation of tree 
                       ntree = 500, # number of trees to create
                       importance = TRUE)
pred.rf <- predict(fit.rf, diabetes[-inTrain, ])
confusionMatrix(pred.rf, y[-inTrain])[2:3]
importance(fit.rf)
varImpPlot(fit.rf)



library(tree)
set.seed(123)
fit.tree <- tree(outcome ~ ., 
                 data = diabetes[inTrain, ])
pred.tree <- predict(fit.tree, diabetes[-inTrain, ], type = "class")
confusionMatrix(pred.tree, y[-inTrain])[2:3]
plot(fit.tree)
text(fit.tree, pretty = 0)




