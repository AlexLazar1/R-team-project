###read csv dataset
raw.data <- read.csv("creditcard.csv")



###functie care calculeaza ROC si AUC (Reciver open characteristic curve si Area under Curve)
calculate_roc <- function(verset, cost_of_fp, cost_of_fn, n=100) {
  
  tp <- function(verset, threshold) {
    sum(verset$predicted >= threshold & verset$Class == 1)
  }
  
  fp <- function(verset, threshold) {
    sum(verset$predicted >= threshold & verset$Class == 0)
  }
  
  tn <- function(verset, threshold) {
    sum(verset$predicted < threshold & verset$Class == 0)
  }
  
  fn <- function(verset, threshold) {
    sum(verset$predicted < threshold & verset$Class == 1)
  }
  
  tpr <- function(verset, threshold) {
    sum(verset$predicted >= threshold & verset$Class == 1) / sum(verset$Class == 1)
  }
  
  fpr <- function(verset, threshold) {
    sum(verset$predicted >= threshold & verset$Class == 0) / sum(verset$Class == 0)
  }
  
  cost <- function(verset, threshold, cost_of_fp, cost_of_fn) {
    sum(verset$predicted >= threshold & verset$Class == 0) * cost_of_fp + 
      sum(verset$predicted < threshold & verset$Class == 1) * cost_of_fn
  }
  fpr <- function(verset, threshold) {
    sum(verset$predicted >= threshold & verset$Class == 0) / sum(verset$Class == 0)
  }
  
  threshold_round <- function(value, threshold)
  {
    return (as.integer(!(value < threshold)))
  }
  auc_ <- function(verset, threshold) {
    auc(verset$Class, threshold_round(verset$predicted,threshold))
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tp <- sapply(roc$threshold, function(th) tp(verset, th))
  roc$fp <- sapply(roc$threshold, function(th) fp(verset, th))
  roc$tn <- sapply(roc$threshold, function(th) tn(verset, th))
  roc$fn <- sapply(roc$threshold, function(th) fn(verset, th))
  roc$tpr <- sapply(roc$threshold, function(th) tpr(verset, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(verset, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(verset, th, cost_of_fp, cost_of_fn))
  roc$auc <-  sapply(roc$threshold, function(th) auc_(verset, th))
  
  return(roc)
}





###Reprezentare grafica ROC si AUC

plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  library(gridExtra)
  
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=2, alpha=0.5) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
  
  p_auc <- ggplot(roc, aes(threshold, auc)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=2, alpha=0.5) +
    labs(title = sprintf("AUC")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=2, alpha=0.5) +
    labs(title = sprintf("cost function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  sub_title <- sprintf("threshold at %.2f - cost of FP = %d, cost of FN = %d", threshold, cost_of_fp, cost_of_fn)
  # 
  grid.arrange(p_roc, p_auc, p_cost, ncol=2,sub=textGrob(sub_title, gp=gpar(cex=1), just="bottom"))
}




###Reprezentare matrice de confuzie

plot_confusion_matrix <- function(verset, sSubtitle) {
  tst <- data.frame(round(verset$predicted,0), verset$Class)
  opts <-  c("Predicted", "True")
  names(tst) <- opts
  cf <- plyr::count(tst)
  cf[opts][cf[opts]==0] <- "Not Fraud"
  cf[opts][cf[opts]==1] <- "Fraud"
  
  ggplot(data =  cf, mapping = aes(x = True, y = Predicted)) +
    labs(title = "Confusion matrix", subtitle = sSubtitle) +
    geom_tile(aes(fill = freq), colour = "grey") +
    geom_text(aes(label = sprintf("%1.0f", freq)), vjust = 1) +
    scale_fill_gradient(low = "lightblue", high = "blue") +
    theme_bw() + theme(legend.position = "none")
  
}



###Explorare baza de date
sprintf("Rows: %d Columns: %d",nrow(raw.data), length(names(raw.data)))

###Primele linii din tabel
library(knitr)
library(kableExtra)
head(raw.data,10) %>%
  kable( "html", escape=F, align="c") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center")


###Sunt 31 de coloane (features) in baza de date, class este targetul, are o valoare binara 0 daca nu este frauda,
###1 daca este frauda, mai exista feature-ul amount -suma tranzactiei si Time, ora tranzactiei, celelalte feature-uri
###sunt anonime de la V1 la V28. Datele sunt unbalanced cu exceptie facand class unde sunt numai 0.17274% valori de 1. Se va folosi AUC


####Corelarea
library(corrplot)
correlations <- cor(raw.data,method="pearson")
corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black")

##Se poate observa ca majoritaratea feature-ilor nu sunt corelate deoarece inainte de a fi publicate, majoritatea
##feature-ilor au fost prezentate unui algoritm Principal Component Analysis (PCA). Feature-urile V1 pana la V28 sunt
##cel mai probabil componentele principale rezultate dupa propagarea feature-urilor reale prin PCA. Nu se stie daca
##numerele cu care au fost notate feature-urile reflecta importanta lor in ceea ce priveste predictia. Aceasta informatie
##poate fi testata cu ajutorul Random Forest.

library(randomForest)
nrows <- nrow(raw.data)
set.seed(314)
indexT <- sample(1:nrow(raw.data), 0.7 * nrows)

#separate train and validation set
trainset = raw.data[indexT,]
verset =   raw.data[-indexT,]

n <- names(trainset)
rf.form <- as.formula(paste("Class ~", paste(n[!n %in% "Class"], collapse = " + ")))

trainset.rf <- randomForest(rf.form,trainset,ntree=100,importance=T)











##Pentru modelul antrenat. Se poate observa importanta fiecarui feature.
varimp <- data.frame(trainset.rf$importance)
library(ggplot2)
vi1 <- ggplot(varimp, aes(x=reorder(rownames(varimp),IncNodePurity), y=IncNodePurity)) +
  geom_bar(stat="identity", fill="tomato", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (IncNodePurity)", x="Variable", y="Variable importance (IncNodePurity)")


vi2 <- ggplot(varimp, aes(x=reorder(rownames(varimp),X.IncMSE), y=X.IncMSE)) +
  geom_bar(stat="identity", fill="lightblue", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (%IncMSE)", x="Variable", y="Variable importance (%IncMSE)")

library(gridExtra)
grid.arrange(vi1, vi2, ncol=2)







##Se foloseste modelul antrenat pentru predictie frauda / nu e frauda si se ploteaza matricea de confuzie
##pentru un treshold de 0.5
library(caret)
library(lattice)
verset$predicted <- predict(trainset.rf ,verset)
plot_confusion_matrix(verset, "Random Forest with 100 trees")


##Pentru o problema de genul acesta, unde numarul TP este foarte mic in comparatie cu TN, matricea de confuzie nu
##este foarte utila. Este foarte important sa se micsoreze cat de mult se poate numarul de FN (s-a prezis ca nu este frauda si de fapt a fost frauda)
##deoarece paguba ar putea fi foarte mare 

#######

library(pROC)
library(dplyr)
library(kableExtra)
roc <- calculate_roc(verset, 1, 10, n = 100)

mincost <- min(roc$cost)
roc %>%
  mutate(
    auc = ifelse(cost == mincost,
                 cell_spec(sprintf("%.5f", auc), "html", color = "green", background = "lightblue", bold = T),
                 cell_spec(sprintf("%.5f", auc), "html", color = "black", bold = F))
  ) %>%
  kable( "html", escape=F, align="c") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center") %>%
  scroll_box(height = "600px")





#####
library(ggplot2)
library(ggpubr)
library(gridGraphics)
threshold = 0.3
plot_roc(roc, threshold, 1, 10)


##Concluzie: acuratetea calculata nu este foarte relevanta in conditiile in care este o diferenta foarte mare in
##ceea ce priveste frauda si nu e frauda din dataset. Valoarea obtinuta cu ajutorul RF este relativ buna 0.93 cu Random Forest, tinand cont
## ca nu s-a facut tunning pe acest algoritm







###Decisional tree

library(tree)

alpha = 0.7 ### percentage of training set
Train = sample(1:nrow(raw.data), alpha * nrow(raw.data)) ### training set
Data_test = raw.data[-Train,]                        ### test data
target_test  = raw.data$Class[-Train] ### test target
print(target_test)

ClassificationTree_Test = tree(rf.form, data = raw.data, subset = Train)



print(ClassificationTree_Test)
summary(ClassificationTree_Test)


plot(ClassificationTree_Test)
text(ClassificationTree_Test, all=TRUE, cex=.9)





library(caret)
library(lattice)
Data_test$predicted <- predict(ClassificationTree_Test ,Data_test)
plot_confusion_matrix(Data_test, "Decisional tree")



library(pROC)
library(dplyr)
library(kableExtra)
roc_tree <- calculate_roc(Data_test, 1, 10, n = 100)

mincost_tree <- min(roc_tree$cost)
roc_tree %>%
  mutate(
    auc = ifelse(cost == mincost_tree,
                 cell_spec(sprintf("%.5f", auc), "html", color = "green", background = "lightblue", bold = T),
                 cell_spec(sprintf("%.5f", auc), "html", color = "black", bold = F))
  ) %>%
  kable( "html", escape=F, align="c") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center") %>%
  scroll_box(height = "600px")


##0.91 cu Decisional tree

library(ggplot2)
library(ggpubr)
library(gridGraphics)
threshold<-0.3
plot_roc(roc_tree, threshold, 1, 10)



tree_predict <- predict(ClassificationTree_Test ,Data_test)
auc.tree = roc(Data_test$Class, tree_predict, plot = TRUE, col = "red")
print(auc.model)




###Linear Model


# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(raw.data), 0.7*nrow(raw.data))  # row indices for training data
trainingData <- raw.data[-trainingRowIndex, ]  # model training data


scatter.smooth(x=trainingData$Class, y=trainingData$V1,main="Class ~ v1")
scatter.smooth(x=trainingData$Class, y=trainingData$V2,main="Class ~ v2")
scatter.smooth(x=trainingData$Class, y=trainingData$V3,main="Class ~ v3")
scatter.smooth(x=trainingData$Class, y=trainingData$V4,main="Class ~ v4")
scatter.smooth(x=trainingData$Class, y=trainingData$V5,main="Class ~ v5")
scatter.smooth(x=trainingData$Class, y=trainingData$V6,main="Class ~ v6")


cor(trainingData$V1,trainingData$Class)
cor(trainingData$V2,trainingData$Class)
cor(trainingData$V3,trainingData$Class)
cor(trainingData$V4,trainingData$Class)
cor(trainingData$V5,trainingData$Class)
cor(trainingData$V6,trainingData$Class)
cor(trainingData$V7,trainingData$Class)
cor(trainingData$V8,trainingData$Class)
cor(trainingData$V9,trainingData$Class)
cor(trainingData$V10,trainingData$Class)
cor(trainingData$V11,trainingData$Class)
cor(trainingData$V12,trainingData$Class)
cor(trainingData$V13,trainingData$Class)
cor(trainingData$V14,trainingData$Class)
cor(trainingData$V15,trainingData$Class)
cor(trainingData$V16,trainingData$Class)
cor(trainingData$V17,trainingData$Class)
cor(trainingData$V18,trainingData$Class)
cor(trainingData$V19,trainingData$Class)
cor(trainingData$V20,trainingData$Class)
cor(trainingData$V21,trainingData$Class)
cor(trainingData$V22,trainingData$Class)
cor(trainingData$V23,trainingData$Class)
cor(trainingData$V24,trainingData$Class)
cor(trainingData$V25,trainingData$Class)
cor(trainingData$V26,trainingData$Class)
cor(trainingData$V27,trainingData$Class)
cor(trainingData$V28,trainingData$Class)



lmMod <- lm(Class ~ ., data=trainingData)
distPred <- predict(lmMod, testData)
summary (lmMod)
AIC (lmMod)
BIC (lmMod)

actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)






library(caret)
library(lattice)
trainingData$predicted <- predict(lmMod ,trainingData)
plot_confusion_matrix(trainingData, "Linear Model")


library(pROC)
library(dplyr)
library(kableExtra)
roc_model <- calculate_roc(trainingData, 1, 10, n = 100)

mincost_model <- min(roc_model$cost)
roc_model %>%
  mutate(
    auc = ifelse(cost == mincost_model,
                 cell_spec(sprintf("%.5f", auc), "html", color = "green", background = "lightblue", bold = T),
                 cell_spec(sprintf("%.5f", auc), "html", color = "black", bold = F))
  ) %>%
  kable( "html", escape=F, align="c") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center") %>%
  scroll_box(height = "600px")


##0.90 cu Linear Model

library(ggplot2)
library(ggpubr)
library(gridGraphics)
threshold<-0.3
plot_roc(roc_model, threshold, 1, 10)


auc.model = roc(trainingData$Class, distPred, plot = TRUE, col = "red")
print(auc.model)


###Gradient boosting


library(gbm)
library(stringr)
set.seed(314)




train.test.split <- sample(1:nrow(raw.data), 0.7 * nrows)

#separate train and validation set
train = raw.data[train.test.split,]
test =   raw.data[-train.test.split,]







#separate train and validation set
gbm.model <- gbm(Class ~ .
                 , distribution = "bernoulli"
                 , data = rbind(train, test)
                 , n.trees = 1000
                 , interaction.depth = 6
                 , n.minobsinnode = 100
                 , shrinkage = 0.02
                 , bag.fraction = 0.5
                 , train.fraction = nrow(train) / (nrow(train) + nrow(test))
)
# Determine best iteration based on test data
best.iter = gbm.perf(gbm.model, method = "test")

# Get feature importance
gbm.feature.imp = summary(gbm.model, n.trees = best.iter)

gbm.model
summary(gbm.model)

###Conform Boosting feature-urile V25, V23, V19, V15, V13, V5, V2 sunt irelevante


# Plot and calculate AUC on test data
gbm.test = predict(gbm.model, newdata = test, n.trees = best.iter)
auc.gbm = roc(test$Class, gbm.test, plot = TRUE, col = "red")
print(auc.gbm)


train$predicted <- predict(gbm.model ,train)
plot_confusion_matrix(train, "Boosting")

roc_gbm <- calculate_roc(train, 1, 10, n = 100)

mincost_gbm <- min(roc_gbm$cost)
roc_gbm %>%
  mutate(
    auc = ifelse(cost == mincost_gbm,
                 cell_spec(sprintf("%.5f", auc), "html", color = "green", background = "lightblue", bold = T),
                 cell_spec(sprintf("%.5f", auc), "html", color = "black", bold = F))
  ) %>%
  kable( "html", escape=F, align="c") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center") %>%
  scroll_box(height = "600px")



library(ggplot2)
library(ggpubr)
library(gridGraphics)
threshold<-0.3
plot_roc(roc_gbm, threshold, 1, 10)

###Gradient boosting are un scor de 0.94











raw.data$Class<-0

library(ggplot2)
library(ggpubr)
library(gridGraphics)
threshold<-0.3

plot_roc(raw.data, threshold, 1, 10)


nrows <- nrow(raw.data)
set.seed(314)
indexT <- sample(1:nrow(raw.data), 0.7 * nrows)

#separate train and validation set
trainset = raw.data[indexT,]
verset =   raw.data[-indexT,]

library(caret)
library(lattice)
verset$predicted <- predict(trainset.rf ,verset)
plot_confusion_matrix(verset, "Random Forest with 100 trees")
