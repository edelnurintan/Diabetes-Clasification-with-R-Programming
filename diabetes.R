install.packages("ggcorrplot")
install.packages("car") 
install.packages("pheatmap")
install.packages("pROC")
library(pROC)
library(pheatmap)
library(car)
library(ggplot2)
library(ggcorrplot)
library(stats)
library(dplyr)

#Load Dataset
data_diabetes <- read.csv("diabetes.csv", header = TRUE, sep = ",")

# ----------------- Data Preprocessing ------------------------------
# Cek missing value
colSums(is.na(data_diabetes))
# Pisahkan data berdasrkan outcome
data_outcome_0 <- subset(data_diabetes, Outcome == 0)
data_outcome_1 <- subset(data_diabetes, Outcome == 1)

# Mendeteksi Outlier pada outcome 0
# Mengubah data frame ke bentuk panjang untuk ggplot
library(tidyr)
diabetes_long <- gather(data_outcome_0, key="variable", value="value", -Outcome)
diabetes_long1 <- gather(data_outcome_1, key = "variable", value = "value", -Outcome)

# Membuat boxplot dengan ggplot
ggplot(diabetes_long, aes(x=variable, y=value)) +
  geom_boxplot(fill="lightblue", color="darkblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Memutar label sumbu X
  labs(title="Boxplot Variabel Diabetes (Outcome = 0)", y="Nilai")  # Menambahkan judul dan label

ggplot(diabetes_long1, aes(x=variable, y=value)) +
  geom_boxplot(fill="lightblue", color="darkblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Memutar label sumbu X
  labs(title="Boxplot Variabel Diabetes (Outcome = 1)", y="Nilai")  # Menambahkan judul dan label


# Menghitung Mean, Median, dan Mode untuk setiap variabel dan Outcome(0)
stats <- diabetes_long %>%
  group_by(variable, Outcome) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    mode_value = as.numeric(names(sort(table(value), decreasing = TRUE)[1])) # Modus
  )

# Membuat density plot untuk semua variabel berdasarkan Outcome (0), dengan facet untuk setiap variabel
ggplot(diabetes_long, aes(x=value, fill=factor(Outcome))) +
  geom_density(alpha=0.5) +  # Transparansi
  facet_wrap(~ variable, scales="free", ncol=4) +  # Membuat plot terpisah untuk setiap variabel dalam 4 kolom
  labs(title="Density Plot Berdasarkan Outcome (0) untuk Setiap Variabel", 
       x="Nilai", y="Kepadatan", fill="Outcome", color="Keterangan") +
  theme_minimal()+
  # Menambahkan garis rata-rata untuk setiap variabel
  geom_vline(data = stats, aes(xintercept=mean_value, color="Mean"), linetype="dashed", size=.6)+
  geom_vline(data = stats, aes(xintercept=median_value, color="Median"), linetype="dashed", size=.6)+
  geom_vline(data = stats, aes(xintercept=mode_value, color="Mode"), linetype="dashed", size=.6)+
  scale_color_manual(
    values = c("Mean" = "green", "Median" = "blue", "Mode" = "red"),  # Warna untuk legend
    breaks = c("Mean", "Median", "Mode"),  # Urutan elemen dalam legend
    labels = c("Mean", "Median", "Mode")  # Label di legend
  )

# Menghitung Mean, Median, dan Mode untuk setiap variabel dan Outcome(1)
stats <- diabetes_long1 %>%
  group_by(variable, Outcome) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    mode_value = as.numeric(names(sort(table(value), decreasing = TRUE)[1])) # Modus
  )

# Membuat density plot untuk semua variabel berdasarkan Outcome (1), dengan facet untuk setiap variabel
ggplot(diabetes_long1, aes(x=value, fill=factor(Outcome))) +
  geom_density(alpha=0.5) +  # Transparansi
  facet_wrap(~ variable, scales="free", ncol=4) +  # Membuat plot terpisah untuk setiap variabel dalam 4 kolom
  labs(title="Density Plot Berdasarkan Outcome (1) untuk Setiap Variabel", 
       x="Nilai", y="Kepadatan", fill="Outcome", color="Keterangan") +
  theme_minimal()+
  # Menambahkan garis rata-rata untuk setiap variabel
  geom_vline(data = stats, aes(xintercept=mean_value, color="Mean"), linetype="dashed", size=.6)+
  geom_vline(data = stats, aes(xintercept=median_value, color="Median"), linetype="dashed", size=.6)+
  geom_vline(data = stats, aes(xintercept=mode_value, color="Mode"), linetype="dashed", size=.6)+
  scale_color_manual(
    values = c("Mean" = "green", "Median" = "blue", "Mode" = "red"),  # Warna untuk legend
    breaks = c("Mean", "Median", "Mode"),  # Urutan elemen dalam legend
    labels = c("Mean", "Median", "Mode")  # Label di legend
  )+
  # Menentukan warna isian untuk Outcome
  scale_fill_manual(
    values = c("1" = "purple")  # Warna isian untuk Outcome = 1
  )

#Menghapus kolom dependent
pca_diabetes <- data_diabetes[, -c(9)]
print(head(pca_diabetes))

# ------------------ Dimension Reduction (PCA) ------------------
pca_result <- prcomp(pca_diabetes, scale. = TRUE)
pca_result

summary(pca_result)

# Menampilkan komponen utama pertama
pca_result$rotation[, 1]  # Vektor eigen pertama (koefisien komponen utama pertama)

#karena kta memilih pc1 dan pc2
# Nilai komponen utama pertama (PC1) untuk setiap observasi
pca_result$x[, 1:5]  # Hasil proyeksi data pada komponen utama pertama

# Visualisasi antara PC1 dan PC2
plot(pca_result$x[, 1], pca_result$x[, 2], xlab = "PC1", ylab = "PC2", main = "PCA: PC1 vs PC2")


#----------------- REGRESI LOGISTIC -----------------------------------
#Karena kita memilih pc1 dan pc2 maka

data_pca <- pca_result$x[, 1:5] 
data_logistic <- cbind(data_pca , Outcome = data_diabetes$Outcome)
data_logistic <- as.data.frame(data_logistic)


#Membagi Data Menjadi Data Latih (Training) dan Data Uji (Testing)
set.seed(123)  # Agar hasil pembagian acak dapat direproduksi

train_index <- sample(1:nrow(data_logistic), size = 0.8 * nrow(data_logistic))  # 80% untuk data latih
train_data <- data_logistic[train_index, ]
test_data <- data_logistic[-train_index, ]

model_logistic <- glm(Outcome ~ PC1 + PC2 + PC3 + PC4 +PC5, 
                   family = binomial, data = train_data)

# Melakukan prediksi pada data uji (test_data)
test_probabilities <- predict(model_logistic, newdata = test_data, type = "response")
# Menghitung ROC curve
roc_curve <- roc(test_data$Outcome, test_probabilities)

# Menampilkan ROC curve
par(pty = "s")
plot(roc_curve, legacy.axes=TRUE, xlab='False Positive Rate', ylab="True Positive Rate", print.auc=TRUE)
# Ekstrak data ROC: sensitivitas, spesifisitas, dan thresholds
roc_df <- data.frame(
  FPR = 1 - roc_curve$specificities,  # False Positive Rate = 1 - Specificity
  TPR = roc_curve$sensitivities,     # True Positive Rate
  Thresholds = roc_curve$thresholds  # Threshold values
)
roc_df
# Menampilkan beberapa baris data ROC
head(roc_df)

# Menentukan threshold optimal berdasarkan titik terbaik di ROC curve
optimal_threshold <- coords(roc_curve, "best", ret = "threshold")
print(paste("Threshold optimal: ", optimal_threshold))
# Menghitung AUC
auc_value <- auc(roc_curve)
print(paste("AUC: ", auc_value))

# Mengonversi probabilitas menjadi kelas biner berdasarkan threshold optimal
test_predicted_class <- ifelse(test_probabilities > 0.3203, 1, 0)

results_df <- data.frame(Actual = test_data$Outcome, 
                         Predicted = test_predicted_class)

print(head(results_df))
# Melihat ringkasan model
summary(model_logit)

#-------------------- Evaluation Model (Confusion Matrix) -----------------------
confusion_matrix <- table(Predicted = test_predicted_class, Actual = test_data$Outcome)

# Menampilkan Confusion Matrix
print(confusion_matrix)

# Menghitung akurasi

TP <- confusion_matrix[2, 2]  # True Positives
FP <- confusion_matrix[1, 2]  # False Positives
TN <- confusion_matrix[1, 1]  # True Negative
FN <- confusion_matrix[2, 1]  # False Negative

acuracy <- (TP+TN) / (TP+FP+FN+TN)
precision <- TP / (TP + FP)
recall  <- TP / (TP + FN)
F1_Score  <-  (2 * recall * precision) / (recall + precision)

Evaluasi_df <- data.frame (
              Accuracy = accuracy, 
              Precission = precision,
              Recall = recall,
              F1_Score = F1_Score
             )
Evaluasi_df


# Data baru untuk satu individu
new_data <- data.frame(
  Pregnancies = c(2),           # Jumlah kehamilan
  Glucose = c(130),             # Glukosa
  BloodPressure = c(70),        # Tekanan darah
  SkinThickness = c(30),        # Ketebalan kulit
  Insulin = c(0),               # Insulin
  BMI = c(32),                  # BMI
  DiabetesPedigreeFunction = c(0.5),  # Fungsi silsilah diabetes
  Age = c(45)                   # Usia
)

# Langkah 1: Standarkan data baru menggunakan rata-rata dan deviasi standar dari data pelatihan
new_data_scaled <- scale(new_data, center = pca_result$center, scale = pca_result$scale)

# Langkah 2: Proyeksikan data baru ke dalam ruang komponen utama yang sudah dihitung
# Menggunakan hasil PCA yang sudah dilatih (menyesuaikan dengan komponen utama pertama hingga kelima)
new_data_pca <- predict(pca_result, newdata = new_data_scaled)[, 1:5]
new_data_pca_df <- data.frame(t(new_data_pca))
new_data_pca_df
pca_list <- as.list(new_data_pca)

# Langkah 4: Prediksi dengan model regresi logistik
test_probabilities_new <- predict(model_logistic, newdata = new_data_pca_df, type = "response")

# Menampilkan probabilitas prediksi
print(dim(new_data_pca_df))
print(test_probabilities_new[1])

# Langkah 5: Mengonversi probabilitas menjadi kelas biner berdasarkan threshold yang telah dihitung sebelumnya
predicted_class_new <- ifelse(test_probabilities_new > 0.3203, 1, 0)  # Sesuaikan threshold yang telah dihitung
print(predicted_class_new)
predict <- as.numeric(predicted_class_new)
if(predict==1){
  print("Terkena diabetes")
} else {
  print("Tidak Terkena Diabetes")
}