library(randomForest)
library(caret)
library(readr)

waterQuality <- read_csv("C:/Users/Nitro/Downloads/waterQuality1.csv")
head(waterQuality)

## Pra-proses

# cek missing value
library(mice)
md.pattern(waterQuality, rotate.names = TRUE)

# Menghilangkan baris yang mengandung missing value 
waterQuality<- na.omit(waterQuality)

# Cek missing value
md.pattern(waterQuality, rotate.names = TRUE)

#cek duplikasi data (tidak ada duplikasi data)
library(visdat)
vis_dat(waterQuality)

#cek tipe data
str(waterQuality)

#mengubah tipe data pada atribut "is_safe"
waterQuality$is_safe<- as.factor(waterQuality$is_safe)

# mencari pembagian data trainig dan data testing terbaik
data<-waterQuality
n <- nrow(data)
train_acc <- vector()
test_acc <- vector()
train_perc <- seq(from = 0.6, to = 0.9, by = 0.05)

# Loop untuk membagi data dan melakukan Random Forest
for (p in train_perc) {
  train_size <- floor(p * n)
  set.seed(123)
  train_index <- sample(seq_len(n), size = train_size)
  
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  # Membangun model Random Forest
  model <- randomForest(is_safe ~ ., data = train_data)
  
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

# Pembagian data training dan data testing
set.seed(1234)
trainIndex <- createDataPartition(waterQuality$is_safe, p = 0.9, list = FALSE)
trainData <- waterQuality[trainIndex, ]
testData <- waterQuality[-trainIndex, ]

# Membuat model random forest
model <- randomForest(trainData$is_safe~., data=trainData)
model

# Evaluasi model menggunakan data test
predictions <- predict(model, trainData)
cmtrain = confusionMatrix(table(predictions, trainData$is_safe))
cmtrain

# Evaluasi model menggunakan data test
predictions <- predict(model, testData)
cmtest = confusionMatrix(table(predictions, testData$is_safe))
cmtest

# melakukan prediksi menggunakan data testing (data ke-7)

prediksi1 <- data.frame(
  aluminium = 2.45,
  ammonia = 27.89,
  arsenic= 0.020,
  barium = 3.25,
  cadmium = 0.004,
  chloramine = 3.35,
  chromium = 0.77,
  copper = 1.99,
  flouride = 0.22,
  bacteria = 0.25,
  viruses = 0.008,
  lead = 0.085,
  nitrates = 3.29,
  nitrites = 1.88,
  mercury = 0.008,
  perchlorate = 15.08,
  radium = 2.35,
  selenium = 0.09,
  silver = 0.09,
  uranium = 0.03)
prediksi1 <- predict(model, newdata=prediksi1)
prediksi1


# Tuning
hyper_grid <- expand.grid(
  ntree = c(100,200,300,400,500, 600, 700, 800, 900, 1000),
  nodesize = c(2, 5, 10),
  mtry = c(3, 4, 5),
  OOB = 0
)
nrow(hyper_grid)
minmodOOB <- model
minOOB <- model$err.rate[model$ntree,1]
maxeaccuracy <- 1-(minOOB)
for(i in 1:nrow(hyper_grid)) {
  # train model
  model <- randomForest(
    trainData$is_safe~., 
    data            = trainData, 
    ntree           = hyper_grid$ntree[i],
    nodesize        = hyper_grid$nodesize[i],
    mtry            = hyper_grid$mtry[i],
  )
  if(model$err.rate[model$ntree,1] < minOOB) {
    minmodOOB <- model
    minOOB <- model$err.rate[model$ntree,1]
  }
  # add OOB error to grid
  hyper_grid$OOB[i] <- model$err.rate[model$ntree,1]
}

hyper_grid[which.min(hyper_grid$OOB),]

print(minmodOOB)

# Evaluasi model menggunakan data training (setelah tuning)
predictions_train_tun <- predict(minmodOOB, trainData)
cmtrain_tuning <- confusionMatrix(table(predictions_train_tun, trainData$is_safe))
cmtrain_tuning

# Evaluasi model menggunakan data testing (setelah tuning)
predictions_test_tun <- predict(minmodOOB, testData)
cmtest_tuning <- confusionMatrix(table(predictions_test_tun, testData$is_safe))
cmtest_tuning

# melakukan prediksi menggunakan data testing (setelah tuning)
prediksi2 <- data.frame(
  aluminium = 2.45,
  ammonia = 27.89,
  arsenic= 0.020,
  barium = 3.25,
  cadmium = 0.004,
  chloramine = 3.35,
  chromium = 0.77,
  copper = 1.99,
  flouride = 0.22,
  bacteria = 0.25,
  viruses = 0.008,
  lead = 0.085,
  nitrates = 3.29,
  nitrites = 1.88,
  mercury = 0.008,
  perchlorate = 15.08,
  radium = 2.35,
  selenium = 0.09,
  silver = 0.09,
  uranium = 0.03)
prediksi2 <- predict(minmodOOB, newdata=prediksi2)
prediksi2