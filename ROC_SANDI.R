library(caret)
library(ROCR)

data_de <- data.frame(
  De = c(AD_SANDI_De, B6_SANDI_De), 
  Group = c(rep(1, length(AD_SANDI_De)), rep(0, length(B6_SANDI_De)))
)

data_din <- data.frame(
  Din = c(AD_SANDI_Din, B6_SANDI_Din), 
  Group = c(rep(1, length(AD_SANDI_Din)), rep(0, length(B6_SANDI_Din)))
)

data_fextra <- data.frame(
  fextra = c(AD_SANDI_fextra, B6_SANDI_fextra), 
  Group = c(rep(1, length(AD_SANDI_fextra)), rep(0, length(B6_SANDI_fextra)))
)

data_fneurite <- data.frame(
  fneurite = c(AD_SANDI_fneurite, B6_SANDI_fneurite), 
  Group = c(rep(1, length(AD_SANDI_fneurite)), rep(0, length(B6_SANDI_fneurite)))
)

data_fsoma <- data.frame(
  fsoma = c(AD_SANDI_fsoma, B6_SANDI_fsoma), 
  Group = c(rep(1, length(AD_SANDI_fsoma)), rep(0, length(B6_SANDI_fsoma)))
)

data_rsoma <- data.frame(
  Rsoma = c(AD_SANDI_Rsoma, B6_SANDI_Rsoma), 
  Group = c(rep(1, length(AD_SANDI_Rsoma)), rep(0, length(B6_SANDI_Rsoma)))
)


#set.seed(12) 
train_indices1 <- createDataPartition(data_de$Group, p = 0.8, list = FALSE)
train_data_de <- data_de[train_indices1, ]
test_data_de <- data_de[-train_indices1, ]

train_indices2 <- createDataPartition(data_din$Group, p = 0.8, list = FALSE)
train_data_din <- data_din[train_indices2, ]
test_data_din <- data_din[-train_indices2, ]

train_indices3 <- createDataPartition(data_fextra$Group, p = 0.8, list = FALSE)
train_data_fextra <- data_fextra[train_indices3, ]
test_data_fextra <- data_fextra[-train_indices3, ]

train_indices4 <- createDataPartition(data_fneurite$Group, p = 0.8, list = FALSE)
train_data_fneurite <- data_fneurite[train_indices4, ]
test_data_fneurite <- data_fneurite[-train_indices4, ]

train_indices5 <- createDataPartition(data_fsoma$Group, p = 0.8, list = FALSE)
train_data_fsoma <- data_fsoma[train_indices5, ]
test_data_fsoma <- data_fsoma[-train_indices5, ]

train_indices6 <- createDataPartition(data_rsoma$Group, p = 0.8, list = FALSE)
train_data_rsoma <- data_rsoma[train_indices6, ]
test_data_rsoma <- data_rsoma[-train_indices6, ]

model_de <- glm(Group ~ De, data = train_data_de, family = binomial())
model_din <- glm(Group ~ Din, data = train_data_din, family = binomial())
model_fextra <- glm(Group ~ fextra, data = train_data_fextra, family = binomial())
model_fneurite <- glm(Group ~ fneurite, data = train_data_fneurite, family = binomial())
model_fsoma <- glm(Group ~ fsoma, data = train_data_fsoma, family = binomial())
model_rsoma <- glm(Group ~ Rsoma, data = train_data_rsoma, family = binomial())

predictions_de <- predict(model_de, test_data_de, type = "response")
predicted_class_de <- ifelse(predictions_de > 0.5, 1, 0)
confusionMatrix(as.factor(predicted_class_de), as.factor(test_data_de$Group))

predictions_din <- predict(model_din, test_data_din, type = "response")
predicted_class_din <- ifelse(predictions_din > 0.5, 1, 0)
confusionMatrix(as.factor(predicted_class_din), as.factor(test_data_din$Group))

predictions_fextra <- predict(model_fextra, test_data_fextra, type = "response")
predicted_class_fextra <- ifelse(predictions_fextra > 0.5, 1, 0)
confusionMatrix(as.factor(predicted_class_fextra), as.factor(test_data_fextra$Group))

predictions_fneurite <- predict(model_fneurite, test_data_fneurite, type = "response")
predicted_class_fneurite <- ifelse(predictions_fneurite > 0.5, 1, 0)
confusionMatrix(as.factor(predicted_class_fneurite), as.factor(test_data_fneurite$Group))

predictions_fsoma <- predict(model_fsoma, test_data_fsoma, type = "response")
predicted_class_fsoma <- ifelse(predictions_fsoma > 0.5, 1, 0)
confusionMatrix(as.factor(predicted_class_fsoma), as.factor(test_data_fsoma$Group))

predictions_rsoma <- predict(model_rsoma, test_data_rsoma, type = "response")
predicted_class_rsoma <- ifelse(predictions_rsoma > 0.5, 1, 0)
confusionMatrix(as.factor(predicted_class_rsoma), as.factor(test_data_rsoma$Group))

perf_de <- performance(prediction(predictions_de, as.factor(test_data_de$Group)), "tpr", "fpr")
perf_din <- performance(prediction(predictions_din, as.factor(test_data_din$Group)), "tpr", "fpr")
perf_fextra <- performance(prediction(predictions_fextra, as.factor(test_data_fextra$Group)), "tpr", "fpr")
perf_fneurite <- performance(prediction(predictions_fneurite, as.factor(test_data_fneurite$Group)), "tpr", "fpr")
perf_fsoma <- performance(prediction(predictions_fsoma, as.factor(test_data_fsoma$Group)), "tpr", "fpr")
perf_rsoma <- performance(prediction(predictions_rsoma, as.factor(test_data_rsoma$Group)), "tpr", "fpr")

plot(perf_de, main="ROC Curve for SANDI", col="blue", lwd=2)
plot(perf_din, add=TRUE, col="green", lwd=2)
plot(perf_fextra, add=TRUE, col="red", lwd=2)
plot(perf_fneurite, add=TRUE, col="orange", lwd=2)
plot(perf_fsoma, add=TRUE, col="purple", lwd=2)
plot(perf_rsoma, add=TRUE, col="brown", lwd=2)

auc_de <- performance(prediction(predictions_de, as.factor(test_data_de$Group)), "auc")
text(0.5, 0.8, paste("SANDI_De AUC =", round(auc_de@y.values[[1]], 2)), col="blue", cex=1.2)

auc_din <- performance(prediction(predictions_din, as.factor(test_data_din$Group)), "auc")
text(0.5, 0.7, paste("SANDI_Din AUC =", round(auc_din@y.values[[1]], 2)), col="green", cex=1.2)

auc_fextra <- performance(prediction(predictions_fextra, as.factor(test_data_fextra$Group)), "auc")
text(0.5, 0.6, paste("SANDI_fextra AUC =", round(auc_fextra@y.values[[1]], 2)), col="red", cex=1.2)

auc_fneurite <- performance(prediction(predictions_fneurite, as.factor(test_data_fneurite$Group)), "auc")
text(0.5, 0.5, paste("SANDI_fneurite AUC =", round(auc_fneurite@y.values[[1]], 2)), col="orange", cex=1.2)

auc_fsoma <- performance(prediction(predictions_fsoma, as.factor(test_data_fsoma$Group)), "auc")
text(0.5, 0.4, paste("SANDI_fsoma AUC =", round(auc_fsoma@y.values[[1]], 2)), col="purple", cex=1.2)

auc_rsoma <- performance(prediction(predictions_rsoma, as.factor(test_data_rsoma$Group)), "auc")
text(0.5, 0.3, paste("SANDI_Rsoma AUC =", round(auc_rsoma@y.values[[1]], 2)), col="brown", cex=1.2)

library(pROC)

roc_de <- roc(test_data_de$Group, predictions_de)
roc_din <- roc(test_data_din$Group, predictions_din)
roc_fextra <- roc(test_data_fextra$Group, predictions_fextra)
roc_fneurite <- roc(test_data_fneurite$Group, predictions_fneurite)
roc_fsoma <- roc(test_data_fsoma$Group, predictions_fsoma)
roc_rsoma <- roc(test_data_rsoma$Group, predictions_rsoma)

# DeLong's test
# De vs. Din
delong_test_de_din <- roc.test(roc_de, roc_din, method = "delong")
print("De vs. Din:")
print(delong_test_de_din)

# De vs. fextra
delong_test_de_fextra <- roc.test(roc_de, roc_fextra, method = "delong")
print("De vs. fextra:")
print(delong_test_de_fextra)

# De vs. fneurite
delong_test_de_fneurite <- roc.test(roc_de, roc_fneurite, method = "delong")
print("De vs. fneurite:")
print(delong_test_de_fneurite)

# De vs. fsoma
delong_test_de_fsoma <- roc.test(roc_de, roc_fsoma, method = "delong")
print("De vs. fsoma:")
print(delong_test_de_fsoma)

# De vs. rsoma
delong_test_de_rsoma <- roc.test(roc_de, roc_rsoma, method = "delong")
print("De vs. rsoma:")
print(delong_test_de_rsoma)

# Din vs. fextra
delong_test_din_fextra <- roc.test(roc_din, roc_fextra, method = "delong")
print("Din vs. fextra:")
print(delong_test_din_fextra)

# Din vs. fneurite
delong_test_din_fneurite <- roc.test(roc_din, roc_fneurite, method = "delong")
print("Din vs. fneurite:")
print(delong_test_din_fneurite)

# Din vs. fsoma
delong_test_din_fsoma <- roc.test(roc_din, roc_fsoma, method = "delong")
print("Din vs. fsoma:")
print(delong_test_din_fsoma)

# Din vs. rsoma
delong_test_din_rsoma <- roc.test(roc_din, roc_rsoma, method = "delong")
print("Din vs. rsoma:")
print(delong_test_din_rsoma)

# fextra vs. fneurite
delong_test_fextra_fneurite <- roc.test(roc_fextra, roc_fneurite, method = "delong")
print("fextra vs. fneurite:")
print(delong_test_fextra_fneurite)

# fextra vs. fsoma
delong_test_fextra_fsoma <- roc.test(roc_fextra, roc_fsoma, method = "delong")
print("fextra vs. fsoma:")
print(delong_test_fextra_fsoma)

# fextra vs. rsoma
delong_test_fextra_rsoma <- roc.test(roc_fextra, roc_rsoma, method = "delong")
print("fextra vs. rsoma:")
print(delong_test_fextra_rsoma)

# fneurite vs. fsoma
delong_test_fneurite_fsoma <- roc.test(roc_fneurite, roc_fsoma, method = "delong")
print("fneurite vs. fsoma:")
print(delong_test_fneurite_fsoma)

# fneurite vs. rsoma
delong_test_fneurite_rsoma <- roc.test(roc_fneurite, roc_rsoma, method = "delong")
print("fneurite vs. rsoma:")
print(delong_test_fneurite_rsoma)

# fsoma vs. rsoma
delong_test_fsoma_rsoma <- roc.test(roc_fsoma, roc_rsoma, method = "delong")
print("fsoma vs. rsoma:")
print(delong_test_fsoma_rsoma)

