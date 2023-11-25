# Veri Setinin Ice Aktarilmasi
library(readr)
desktop_path <- file.path("C:", "Users", "mehmet", "Desktop")
file_name <- "Admission_Predict.csv"
file_path <- file.path(desktop_path, file_name)
data <- read.csv(file_path, stringsAsFactors = FALSE)

# Ilk Sutunun Cikarilmasi
data <- data[,-1]

# Gerekli Kutuphanelerin Yuklenmesi
library(dplyr)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(MASS)
library(caret)
library(car)
library(lmtest)

# Veri Seti Ozetleme ve Gorsellestirme
# Veri Setinin Kisa Ozeti
summary(data)
glimpse(data)

# Korelasyon Matrisi
cor_matrix <- round(cor(data[,1:7]), 2)
corrplot(cor_matrix, method = "circle", type = "lower", order = "hclust")

# Dagilim Grafikleri
plot1 <- ggplot(data, aes(x = GRE.Score, y = Chance.of.Admit)) + geom_point()
plot2 <- ggplot(data, aes(x = TOEFL.Score, y = Chance.of.Admit)) + geom_point()
plot3 <- ggplot(data, aes(x = CGPA, y = Chance.of.Admit)) + geom_point()
plot4 <- ggplot(data, aes(x = Research, y = Chance.of.Admit)) + geom_point()
plot5 <- ggplot(data, aes(x = LOR, y = Chance.of.Admit)) + geom_point()
plot6 <- ggplot(data, aes(x = University.Rating, y = Chance.of.Admit)) + geom_point()
plot7 <- ggplot(data, aes(x = SOP, y = Chance.of.Admit)) + geom_point()
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, ncol = 3)


# Egitim ve Test Veri Setlerinin Ayrilmasi:
set.seed(53)
train_index <- sample(1:nrow(data), size = floor(0.8 * nrow(data)))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Model Olusturma ve Degerlendirme
# Model Olusturma
model <- lm(Chance.of.Admit ~ ., data = train_data)
summary(model)

# Egitim Veri Setindeki Performans Degerlendirmesi
train_predictions <- predict(model, train_data)
train_actuals <- train_data$Chance.of.Admit
train_mse <- mean((train_predictions - train_actuals) ^ 2)
train_rmse <- sqrt(train_mse)
train_r_squared <- summary(model)$r.squared

# Test Veri Setindeki Performans Degerlendirmesi
test_predictions <- predict(model, test_data)
test_actuals <- test_data$Chance.of.Admit
test_mse <- mean((test_predictions - test_actuals) ^ 2)
test_rmse <- sqrt(test_mse)

#Optimum Modelin Olusturulmasi
# Veri Setinin Yeniden Ayrilmasi
set.seed(53)
train_index_1 <- sample(1:nrow(data), size = floor(0.8 * nrow(data)))
train_data_1 <- data[train_index_1, ]
test_data_1 <- data[-train_index_1, ]

# Mahalanobis Aykiri Deger Kontrolu
cov_matrix <- cov(train_data_1[, sapply(train_data_1, is.numeric)])
center <- colMeans(train_data_1[, sapply(train_data_1, is.numeric)])
train_data_1$mahalanobis <- mahalanobis(train_data_1[, sapply(train_data_1, is.numeric)], center, cov_matrix)
threshold <- qchisq(0.95, ncol(train_data_1) - 1)
outliers <- which(train_data_1$mahalanobis > threshold)
train_data_clean_1 <- train_data_1[-outliers, ]
train_data_final_1 <- train_data_clean_1[!colnames(train_data_clean_1) %in% c("University.Rating", "SOP")]
test_data_final_1 <- test_data_1[!colnames(test_data_1) %in% c("University.Rating", "SOP")]

# Log Donusumu
train_data_final_1$log_GRE <- log(train_data_final_1$GRE.Score)
train_data_final_1$log_TOEFL <- log(train_data_final_1$TOEFL.Score)
test_data_final_1$log_GRE <- log(test_data_final_1$GRE.Score)
test_data_final_1$log_TOEFL <- log(test_data_final_1$TOEFL.Score)

# Ikinci Modelin Olusturulmasi ve Degerlendirilmesi
model2 <- lm(Chance.of.Admit ~ log_GRE + log_TOEFL + CGPA + Research + LOR, data = train_data_final_1)
summary(model2)