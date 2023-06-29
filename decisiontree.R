library(dplyr)
library(mice)
library(visdat)
library(metan)
library(readr)
library(rpart)
library(caret)
library(party)

waterQuality <- read_csv("C:/Users/Nitro/Downloads/waterQuality1.csv")
head(waterQuality)

# mengecek tipe data
str(waterQuality)

# Mengecek mising value
md.pattern(waterQuality, rotate.names = TRUE)

# Menghilangkan baris yang mengandung missing value
data <- na.omit(waterQuality)

# Mengecek kembali missing value
md.pattern(data, rotate.names = TRUE)

# cek duplikasi data (tidak ada duplikasi data)
vis_dat(data)

# melihat sebaran data
boxplot(data)

# Normalisasi data
normalize <- function(x) {
  return((x - min(x))/(max(x)-min(x)))
}
normal <- as.data.frame(lapply(data, normalize))
head(normal)

# feature selection dengan heatmap
# membuat heatmap dengan metan
ALL <- corr_coef(normal)
ALL
plot(ALL)

# variabel yang dipilih setelah heatmap
df <- data.frame(waterQuality)
data <- df[, c(1,2,3,4,5,6,7,8,10,11,12,14,16,17,19,21)]
head(data)

# RESAMPLING DATA
# Menampilkan jumlah baris data pada setiap probabilitas
table(data$is_safe)

# oversampling 7000 data
df_oversampled <- data %>%
  group_by(is_safe) %>%
  do(sample_n(., size = 7000, replace = TRUE))

# Menampilkan jumlah baris data pada setiap probabilitas setelah dilakukan oversampling
table(df_oversampled$is_safe)
View(df_oversampled)

# Mengecek mising value
md.pattern(df_oversampled, rotate.names = TRUE)
# Menghilangkan baris yang mengandung missing value
data_clean <- na.omit(df_oversampled)
# Mengecek lagi mising value
md.pattern(data_clean, rotate.names = TRUE)

#cek data balence
table(data_clean$is_safe)

# Ubah tipe data
data_clean$is_safe <- as.factor(data_clean$is_safe)

# mencari pembagian data trainig dan data testing terbaik
data<-data_clean
n <- nrow(data)
train_acc <- vector()
test_acc <- vector()
train_perc <- seq(from = 0.6, to = 0.9, by = 0.05)

# Loop untuk membagi data dan melakukan decision tree
for (p in train_perc) {
  train_size <- floor(p * n)
  set.seed(123)
  train_index <- sample(seq_len(n), size = train_size)
  
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  # Membangun model decision tree
  model <- ctree(is_safe ~ ., data = train_data)
  
  # Memprediksi data training dan testing
  train_pred <- predict(model, train_data)
  test_pred <- predict(model, test_data)
  
  # Menghitung akurasi
  train_accuracy <- sum(train_pred == train_data$is_safe) / length(train_pred)
  test_accuracy <- sum(test_pred == test_data$is_safe) / length(test_pred)
  
  # Menyimpan hasil akurasi
  train_acc <- c(train_acc, train_accuracy)
  test_acc <- c(test_acc, test_accuracy)
}

# Plot hasil akurasi
df <- data.frame(Train_Percentage = train_perc, Train_Accuracy = train_acc, Test_Accuracy = test_acc)

ggplot(df, aes(x = Train_Percentage)) +
  geom_line(aes(y = Train_Accuracy, color = "Training Accuracy")) +
  geom_line(aes(y = Test_Accuracy, color = "Testing Accuracy")) +
  labs(x = "Train Percentage", y = "Accuracy") +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal()

trainIndex <- sample(2, nrow(data_clean), replace=TRUE, prob=c(0.85, 0.15))
trainData <- data.frame(data_clean)[trainIndex==1,]
testData <- data.frame(data_clean)[trainIndex==2,]

# Membuat model decision tree
tree <- ctree(is_safe ~ ., data = trainData)
tree

# Menampilkan tree
plot(tree)
plot(tree, type="simple")
plot(tree, type="simple",           
     inner_panel=node_inner(tree,
                            abbreviate = TRUE,           
                            pval = FALSE,                 
                            id = FALSE),                  
     terminal_panel=node_terminal(tree, 
                                  abbreviate = TRUE,
                                  digits = 1,                   
                                  fill = c("white"),            
                                  id = FALSE)
)

# Evaluasi model menggunakan data training
pred_train <- predict(tree, newdata = trainData)
confusionMatrix(pred_train, trainData$is_safe)

# Evaluasi model menggunakan data testing
pred_test <- predict(tree, newdata = testData)
confusionMatrix(pred_test, testData$is_safe)

# melakukan prediksi menggunakan data testing (data ke-35)

prediksi1 <- data.frame(
  aluminium = 0.02,
  ammonia = 23.98,
  arsenic= 0.010,
  barium = 1.03,
  cadmium = 0.090,
  chloramine = 0.37,
  chromium = 0.01,
  copper = 0.40,
  bacteria = 0.20,
  viruses = 0.008,
  lead = 0.022,
  nitrites = 1.96,
  perchlorate = 1.92,
  radium = 0.67,
  silver = 0.06)
prediksi1 <- predict(tree, newdata=prediksi1)
prediksi1

##########################################
# tes tuning

library(mlr)
d.tree.params <- makeClassifTask( data=trainData, target="is_safe")
param_grid <- makeParamSet(makeDiscreteParam("maxdepth", values=1:30))

# Define Grid
control_grid = makeTuneControlGrid()
# Define Cross Validation
resample = makeResampleDesc("CV", iters = 3L)
# Define Measure
measure = acc

param_grid_multi <- makeParamSet( 
  makeDiscreteParam("maxdepth", values=1:30),
  makeNumericParam("cp", lower = 0.001, upper = 0.01),
  makeDiscreteParam("minsplit", values=1:30)
)

dt_tuneparam_multi <- tuneParams(learner='classif.rpart', 
                                 task=d.tree.params, 
                                 resampling = resample,
                                 measures = measure,
                                 par.set=param_grid_multi, 
                                 control=control_grid, 
                                 show.info = TRUE)

# Extracting best Parameters from Multi Search
best_parameters_multi = setHyperPars(
  makeLearner("classif.rpart", predict.type = "prob"), 
  par.vals = dt_tuneparam_multi$x
)

best_model_multi = train(best_parameters_multi, d.tree.params)

# Predicting the best Model
results <- predict(best_model_multi, task = d.tree.params)$data
cm <- confusionMatrix(data = results$response, reference = results$truth)
accuracy <- cm$overall["Accuracy"]
accuracy

#menampilkan decision tree
tree_tuning <- ctree(is_safe ~ ., data = trainData,
              controls = ctree_control(maxdepth = 28, minsplit = 4, mincriterion = 0.001))
tree_tuning

plot(tree_tuning)
plot(tree_tuning, type="simple")
plot(tree_tuning, type="simple",           
     inner_panel=node_inner(tree,
                            abbreviate = TRUE,           
                            pval = FALSE,                 
                            id = FALSE),                  
     terminal_panel=node_terminal(tree, 
                                  abbreviate = TRUE,
                                  digits = 1,                   
                                  fill = c("white"),            
                                  id = FALSE)
)

# Evaluasi model menggunakan data training
pred_train_tuning <- predict(tree_tuning, newdata = trainData)
confusionMatrix(pred_train_tuning, trainData$is_safe)

# Evaluasi model menggunakan data training
pred_test_tuning <- predict(tree_tuning, newdata = testData)
confusionMatrix(pred_test_tuning, testData$is_safe)

# melakukan prediksi menggunakan data testing (data ke-35)
prediksi2 <- data.frame(
  aluminium = 0.02,
  ammonia = 23.98,
  arsenic= 0.010,
  barium = 1.03,
  cadmium = 0.090,
  chloramine = 0.37,
  chromium = 0.01,
  copper = 0.40,
  bacteria = 0.20,
  viruses = 0.008,
  lead = 0.022,
  nitrites = 1.96,
  perchlorate = 1.92,
  radium = 0.67,
  silver = 0.06)
prediksi2 <- predict(tree, newdata=prediksi2)
prediksi2