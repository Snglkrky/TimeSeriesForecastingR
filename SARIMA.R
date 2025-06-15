Sys.setlocale("LC_ALL", "en_US.UTF-8")
library(readxl)
library(tseries)      # adf.test, kpss.test, pp.test için
library(forecast)     # diff ve auto.arima için
library(ggplot2)

# Veri Yükleme ve Zaman Serisine Dönüştürme
# Lütfen bu dosya yolunu kendi sisteminize göre güncelleyin
gasprice <- read_xlsx("C:/Users/msi/Desktop/importsgasprice.xlsx")
gasprice$date <- as.Date(gasprice$date)
ts_data <- ts(gasprice$price, start = c(1989,1), frequency = 12)

# Orijinal serinin grafiği
ts.plot(ts_data, main = "Doğalgaz Fiyatları Zaman Serisi", ylab = "Fiyat", xlab = "Zaman")

# Zaman serisi dekompozisyonu
dekompozisyon <- decompose(ts_data, type = "additive")
plot(dekompozisyon)

# --- Durağanlık ve Fark Alma ---

# Orijinal serinin durağanlık testleri
cat("Orijinal Serinin Durağanlık Testleri:\n")
print(adf.test(ts_data))                      # Augmented Dickey-Fuller Testi
print(pp.test(ts_data))                       # Phillips-Perron Testi
print(kpss.test(ts_data, null = "Level"))     # KPSS Testi (Null hipotez: seri durağandır)

# Orijinal serinin ACF ve PACF grafikleri
par(mfrow=c(1,2)) # Grafikleri yan yana göstermek için
acf(ts_data, lag.max = 50, main = "Orijinal Seri ACF")
pacf(ts_data, lag.max = 50, main = "Orijinal Seri PACF")
par(mfrow=c(1,1)) # Grafik düzenini sıfırlama

# --- d=1, D=0 (Birinci Derece Mevsimsel Olmayan Fark) ---
diff1_ts_data <- diff(ts_data, differences = 1)
ts.plot(diff1_ts_data, main = "d=1 Fark Alınmış Seri", ylab = "Farklı Fiyat", xlab = "Zaman")
cat("\nBirinci Derece Fark Alınmış Serinin Durağanlık Testleri (d=1):\n")
print(adf.test(diff1_ts_data))
print(pp.test(diff1_ts_data))
print(kpss.test(diff1_ts_data, null = "Level"))

# d=1 fark alınmış serinin ACF ve PACF grafikleri
par(mfrow=c(1,2))
acf(diff1_ts_data, lag.max = 60, main = "d=1 ACF")
pacf(diff1_ts_data, lag.max = 60, main = "d=1 PACF")
par(mfrow=c(1,1))


# --- d=1, D=1 (Birinci Derece Mevsimsel Olmayan ve Birinci Derece Mevsimsel Fark) ---
diff1_seasonal1_ts_data <- diff(diff1_ts_data, lag = 12, differences = 1)
ts.plot(diff1_seasonal1_ts_data, main = "d=1, D=1 Seri", ylab = "Fiyat", xlab = "Zaman")
cat("\nBirinci Derece ve Mevsimsel Fark Alınmış Serinin Durağanlık Testleri (d=1, D=1):\n")
print(adf.test(diff1_seasonal1_ts_data))
print(pp.test(diff1_seasonal1_ts_data))
print(kpss.test(diff1_seasonal1_ts_data, null = "Level"))

# d=1, D=1 fark alınmış serinin ACF ve PACF grafikleri
par(mfrow=c(1,2))
acf(diff1_seasonal1_ts_data, lag.max = 50, main = "ACF (d=1, D=1)")
pacf(diff1_seasonal1_ts_data, lag.max = 50, main = "PACF (d=1, D=1)")
par(mfrow=c(1,1))

# --- LOG DÖNÜŞÜMÜ UYGULAMA ---
cat("\n--- Log Dönüşümü Uygulanmış Seri Analizi ---\n")
log_ts_data <- log(ts_data)

# Log dönüşümü uygulanmış serinin grafiği
ts.plot(log_ts_data, main = "Log Dönüşümü Uygulanmış Doğalgaz Fiyatları", ylab = "Log(Fiyat)", xlab = "Zaman")

# Log dönüşümü uygulanmış serinin ACF ve PACF grafikleri
par(mfrow=c(1,2))
acf(log_ts_data, lag.max = 50, main = "Log Dönüşümü Uygulanmış Serinin ACF")
pacf(log_ts_data, lag.max = 50, main = "Log Dönüşümü Uygulanmış Serinin PACF")
par(mfrow=c(1,1))

# --- Log Dönüşümü ve d=1 Fark ---
diff1_log_ts_data <- diff(log_ts_data, differences = 1)

# Log dönüşümü ve d=1 fark alınmış serinin durağanlık testleri
cat("\nLog Dönüşümü ve d=1 Fark Alınmış Serinin Durağanlık Testleri:\n")
print(adf.test(diff1_log_ts_data))
print(pp.test(diff1_log_ts_data))
print(kpss.test(diff1_log_ts_data, null = "Level"))

# Log dönüşümü ve d=1 fark alınmış serinin grafiği
ts.plot(diff1_log_ts_data, main = "Log Dönüşümü ve d=1 Fark Alınmış Seri", ylab = "Farklı Log(Fiyat)", xlab = "Zaman")
par(mfrow=c(1,2))
acf(diff1_log_ts_data, lag.max = 50, main = "Log ACF (d=1)")
pacf(diff1_log_ts_data, lag.max = 50, main = "Log PACF (d=1)")
par(mfrow=c(1,1))

# --- Log Dönüşümü ve D=1 Fark (d=0, D=1 log) ---
diff_seasonal1_log_ts_data <- diff(log_ts_data, lag = 12, differences = 1)

# Log dönüşümü ve D=1 fark alınmış serinin durağanlık testleri
cat("\nLog Dönüşümü ve D=1 Fark Alınmış Serinin Durağanlık Testleri:\n")
print(adf.test(diff_seasonal1_log_ts_data))
print(pp.test(diff_seasonal1_log_ts_data))
print(kpss.test(diff_seasonal1_log_ts_data, null = "Level"))

# Log dönüşümü ve D=1 fark alınmış serinin grafiği
ts.plot(diff_seasonal1_log_ts_data, main = "Log Dönüşümü ve D=1 Fark Alınmış Seri", ylab = "Farklı Log(Fiyat)", xlab = "Zaman")
par(mfrow=c(1,2))
acf(diff_seasonal1_log_ts_data, lag.max = 50, main = "Log Dönüşümlü Serinin ACF (D=1)")
pacf(diff_seasonal1_log_ts_data, lag.max = 50, main = "Log Dönüşümlü Serinin PACF (D=1)")
par(mfrow=c(1,1))

# --- Log Dönüşümü ve D=2 Fark (d=0, D=2 log) ---
diff_seasonal2_log_ts_data <- diff(diff_seasonal1_log_ts_data, lag = 12, differences = 1)

# Log dönüşümü ve D=2 fark alınmış serinin durağanlık testleri
cat("\nLog Dönüşümü ve D=2 Fark Alınmış Serinin Durağanlık Testleri:\n")
print(adf.test(diff_seasonal2_log_ts_data))
print(pp.test(diff_seasonal2_log_ts_data))
print(kpss.test(diff_seasonal2_log_ts_data, null = "Level"))

# Log dönüşümü ve D=2 fark alınmış serinin grafiği
ts.plot(diff_seasonal2_log_ts_data, main = "Log Dönüşümü ve D=2 Fark Alınmış Seri", ylab = "Farklı Log(Fiyat)", xlab = "Zaman")
par(mfrow=c(1,2))
acf(diff_seasonal2_log_ts_data, lag.max = 50, main = "Log Dönüşümlü Serinin ACF (D=2)")
pacf(diff_seasonal2_log_ts_data, lag.max = 50, main = "Log Dönüşümlü Serinin PACF (D=2)")
par(mfrow=c(1,1))


# --- Log Dönüşümü, d=1 ve D=1 Fark ---
# Önce d=1 fark, sonra D=1 fark
diff1_seasonal1_log_ts_data <- diff(diff1_log_ts_data, lag = 12, differences = 1)

# Log dönüşümü, d=1 ve D=1 fark alınmış serinin grafiği
ts.plot(diff1_seasonal1_log_ts_data, main = "Log Dönüşümü, d=1 ve D=1 Fark Alınmış Seri", ylab = "Farklı Log(Fiyat)", xlab = "Zaman")

# Log dönüşümü, d=1 ve D=1 fark alınmış serinin durağanlık testleri
cat("\nLog Dönüşümü, d=1 ve D=1 Fark Alınmış Serinin Durağanlık Testleri:\n")
print(adf.test(diff1_seasonal1_log_ts_data))
print(pp.test(diff1_seasonal1_log_ts_data))
print(kpss.test(diff1_seasonal1_log_ts_data, null = "Level"))

# Log dönüşümü, d=1 ve D=1 fark alınmış serinin ACF ve PACF grafikleri
par(mfrow=c(1,2))
acf(diff1_seasonal1_log_ts_data, lag.max = 50, main = "Log d=1, D=1 Serinin ACF")
pacf(diff1_seasonal1_log_ts_data, lag.max = 50, main = "Log d=1, D=1 Serinin PACF")
par(mfrow=c(1,1))

# --- Log Dönüşümü, d=1 ve D=2 Fark ---
# Önce d=1 fark, sonra D=1 fark, sonra bir daha D=1 fark
diff1_seasonal2_log_ts_data <- diff(diff1_seasonal1_log_ts_data, lag = 12, differences = 1)

# Log dönüşümü, d=1 ve D=2 fark alınmış serinin grafiği
ts.plot(diff1_seasonal2_log_ts_data, main = "Log Dönüşümü, d=1 ve D=2 Fark Alınmış Seri", ylab = "Farklı Log(Fiyat)", xlab = "Zaman")

# Log dönüşümü, d=1 ve D=2 fark alınmış serinin durağanlık testleri
cat("\nLog Dönüşümü, d=1 ve D=2 Fark Alınmış Serinin Durağanlık Testleri:\n")
print(adf.test(diff1_seasonal2_log_ts_data))
print(pp.test(diff1_seasonal2_log_ts_data))
print(kpss.test(diff1_seasonal2_log_ts_data, null = "Level"))

# Log dönüşümü, d=1 ve D=2 fark alınmış serinin ACF ve PACF grafikleri
par(mfrow=c(1,2))
acf(diff1_seasonal2_log_ts_data, lag.max = 50, main = "Log d=1, D=2 Serinin ACF")
pacf(diff1_seasonal2_log_ts_data, lag.max = 50, main = "Log d=1, D=2 Serinin PACF")
par(mfrow=c(1,1))

# --- Log Dönüşümü ve d=2 Fark ---
# Önce d=1, sonra bir daha d=1 (yani d=2)
diff2_log_ts_data <- diff(diff1_log_ts_data, differences = 1)

# Log dönüşümü ve d=2 fark alınmış serinin durağanlık testleri
cat("\nLog Dönüşümü ve d=2 Fark Alınmış Serinin Durağanlık Testleri:\n")
print(adf.test(diff2_log_ts_data))
print(pp.test(diff2_log_ts_data))
print(kpss.test(diff2_log_ts_data, null = "Level"))

# Log dönüşümü ve d=2 fark alınmış serinin grafiği
ts.plot(diff2_log_ts_data, main = "Log Dönüşümü ve d=2 Fark Alınmış Seri", ylab = "Farklı Log(Fiyat)", xlab = "Zaman")
par(mfrow=c(1,2))
acf(diff2_log_ts_data, lag.max = 60, main = "Log Dönüşümlü Serinin ACF (d=2)")
pacf(diff2_log_ts_data, lag.max = 60, main = "Log Dönüşümlü Serinin PACF (d=2)")
par(mfrow=c(1,1))

# --- Log Dönüşümü, d=2 ve D=1 Fark ---
# Önce d=2 fark, sonra D=1 fark
diff2_seasonal1_log_ts_data <- diff(diff2_log_ts_data, lag = 12, differences = 1)

# Log dönüşümü, d=2 ve D=1 fark alınmış serinin grafiği
ts.plot(diff2_seasonal1_log_ts_data, main = "Log Dönüşümü, d=2 ve D=1 Fark Alınmış Seri", ylab = "Farklı Log(Fiyat)", xlab = "Zaman")

# Log dönüşümü, d=2 ve D=1 fark alınmış serinin durağanlık testleri
cat("\nLog Dönüşümü, d=2 ve D=1 Fark Alınmış Serinin Durağanlık Testleri:\n")
print(adf.test(diff2_seasonal1_log_ts_data))
print(pp.test(diff2_seasonal1_log_ts_data))
print(kpss.test(diff2_seasonal1_log_ts_data, null = "Level"))

# Log dönüşümü, d=2 ve D=1 fark alınmış serinin ACF ve PACF grafikleri
par(mfrow=c(1,2))
acf(diff2_seasonal1_log_ts_data, lag.max = 50, main = "Log d=2, D=1 Serinin ACF")
pacf(diff2_seasonal1_log_ts_data, lag.max = 50, main = "Log d=2, D=1 Serinin PACF")
par(mfrow=c(1,1))

# --- Log Dönüşümü, d=2 ve D=2 Fark ---
# Önce d=2 fark, sonra D=1 fark, sonra bir daha D=1 fark
diff2_seasonal2_log_ts_data <- diff(diff2_seasonal1_log_ts_data, lag = 12, differences = 1)

# Log dönüşümü, d=2 ve D=2 fark alınmış serinin grafiği
ts.plot(diff2_seasonal2_log_ts_data, main = "Log Dönüşümü, d=2 ve D=2 Fark Alınmış Seri", ylab = "Farklı Log(Fiyat)", xlab = "Zaman")

# Log dönüşümü, d=2 ve D=2 fark alınmış serinin durağanlık testleri
cat("\nLog Dönüşümü, d=2 ve D=2 Fark Alınmış Serinin Durağanlık Testleri:\n")
print(adf.test(diff2_seasonal2_log_ts_data))
print(pp.test(diff2_seasonal2_log_ts_data))
print(kpss.test(diff2_seasonal2_log_ts_data, null = "Level"))

# Log dönüşümü, d=2 ve D=2 fark alınmış serinin ACF ve PACF grafikleri
par(mfrow=c(1,2))
acf(diff2_seasonal2_log_ts_data, lag.max = 50, main = "Log d=2, D=2 Serinin ACF")
pacf(diff2_seasonal2_log_ts_data, lag.max = 50, main = "Log d=2, D=2 Serinin PACF")
par(mfrow=c(1,1))


# Seçilen Fark Alma Yöntemi için p, q, P ve Q Değerlerine Karar Verme: 

# Log dönüşümü, d=1 ve D=1 fark alınmış serinin ACF ve PACF grafikleri
par(mfrow=c(1,2))
acf(diff1_seasonal1_log_ts_data, lag.max = 90, main = "Log d=1, D=1 Serinin ACF")
pacf(diff1_seasonal1_log_ts_data, lag.max = 90, main = "Log d=1, D=1 Serinin PACF")
par(mfrow=c(1,1))

acf_result <- acf(diff1_seasonal1_log_ts_data, lag.max = 90, plot = FALSE, na.action = na.pass, type = "correlation")

# acf_result$lag: Her bir lag değeri
# acf_result$acf: Her bir lag için ACF değeri
# İlk eleman (lag 0) her zaman 1'dir ve genellikle atlanır.
cat("--- ACF Değerleri ---\n")
for (i in 1:length(acf_result$lag)) {
	if (acf_result$lag[i] > 0) { # Lag 0'ı atla
		cat(sprintf("Lag %f: ACF = %f\n", acf_result$lag[i], acf_result$acf[i]))
	}
}


# --- PACF Değerlerini Hesaplama ve Konsola Yazdırma ---
pacf_result <- pacf(diff1_seasonal1_log_ts_data, lag.max = 91, plot = FALSE, na.action = na.pass)


cat("--- PACF Değerleri ---\n")
for (i in 1:length(pacf_result$lag)) {
	if (pacf_result$lag[i] > 0) { # Lag 0'ı atla
		cat(sprintf("Lag %f: PACF = %f\n", pacf_result$lag[i], pacf_result$acf[i]))
	}
}

# İPUCU: Daha düzenli bir tablo için
# Yukarıdaki ACF ve PACF değerlerini tablolaştıran kodu da kullanabilirsiniz.
df_results <- data.frame(
  Lag = acf_result$lag[-1],
  ACF = acf_result$acf[-1],
  PACF = pacf_result$acf[-1] # PACF değerleri için de aynı lagleri varsayarak
)
print(df_results)



# ---  auto.arima() ile Otomatik Model Seçimi ---
cat("--- auto.arima() ile en iyi model aranıyor... ---\n")
auto_model <- auto.arima(log_ts_data,
												 d = 1, D = 1,          # Analizlerinize göre fark dereceleri
												 stepwise = FALSE,      # Daha kapsamlı arama için
												 approximation = FALSE, # Daha hassas arama için
												 trace = TRUE)          # Hangi modellerin denendiğini göster

cat("\n--- auto.arima() tarafından bulunan en iyi model: ---\n")
print(summary(auto_model))


# --- En İyi Modelin Artık Analizi ---
# En düşük AICc değerine sahip modeli seçiyoruz.
# Genellikle auto.arima'nın bulduğu model en iyisi olur.

cat("\n--- En İyi Modelin Artık Analizi (Residual Diagnostics) ---\n")
# checkresiduals fonksiyonu hem grafikleri çizer hem de Ljung-Box testini yapar.
# Ljung-Box testinde p-değerinin 0.05'ten büyük olması, artıkların bağımsız olduğunu
# ve modelin başarılı olduğunu gösterir.
checkresiduals(auto_model)


# --- En İyi Model ile Gelecek Tahmini ---
cat("\n--- En İyi Model ile Gelecek 24 Ay için Tahmin --- \n")
forecast_horizon <- 24
future_forecast <- forecast(auto_model, h = forecast_horizon)

# Tahmin sonuçlarını çizdirelim
autoplot(future_forecast) +
	ggtitle("Doğalgaz Fiyatları için 24 Aylık Tahmin") +
	xlab("Yıl") +
	ylab("Log(Fiyat)") +
	theme_minimal()

print(future_forecast)



# SARIMA Model Denemeleri:


# Model 1: ARIMA(0,1,2)(0,1,2)[12]
model_A <- Arima(log_ts_data, order = c(0, 1, 2), seasonal = list(order = c(0, 1, 2), period = 12))
cat("Model A: ARIMA(0,1,2)(0,1,2)[12]\n")
print(summary(model_A))
cat("\n-------------------------------------------\n")


# Model 2: ARIMA(2,1,0)(1,1,0)[12]
model_B <- Arima(log_ts_data, order = c(2, 1, 0), seasonal = list(order = c(1, 1, 0), period = 12))
cat("Model B: ARIMA(2,1,0)(1,1,0)[12]\n")
print(summary(model_B))
cat("\n-------------------------------------------\n")


# Model 3: SARIMA(1,1,1)(1,1,1)[12]
model_C <- Arima(log_ts_data, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12))
cat("Model C: SARIMA(1,1,1)(1,1,1)[12]\n")
print(summary(model_C))
cat("\n-------------------------------------------\n")


# Model 4: SARIMA(4,1,0)(1,1,0)[12]
model_D <- Arima(log_ts_data, order = c(4, 1, 0), seasonal = list(order = c(1, 1, 0), period = 12))
cat("Model D: SARIMA(4,1,0)(1,1,0)[12]\n")
print(summary(model_D))
cat("\n-------------------------------------------\n")


# Model 5: SARIMA(4,1,4)(1,1,0)[12]
model_E <- Arima(log_ts_data, order = c(4, 1, 4), seasonal = list(order = c(1, 1, 0), period = 12))
cat("Model E: SARIMA(4,1,4)(1,1,0)[12]\n")
print(summary(model_E))
cat("\n-------------------------------------------\n")



# --- Modellerin Bilgi Kriterleri ile Karşılaştırılması ---
cat("\n--- MODELLERİN KARŞILAŞTIRILMASI (AIC ve BIC) ---\n\n")
comparison_df <- data.frame(
	Model = c("ARIMA(0,1,2)(0,1,2)", "ARIMA(2,1,0)(1,1,0)", "ARIMA(1,1,1)(1,1,1)", "ARIMA(4,1,0)(1,1,0)", "ARIMA(4,1,4)(1,1,0)"),
	AIC = c(AIC(model_A), AIC(model_B), AIC(model_C), AIC(model_D), AIC(model_E)),
	BIC = c(BIC(model_A), BIC(model_B), BIC(model_C), BIC(model_D), BIC(model_E))
)

# AIC değerine göre sıralayalım (en düşük en iyi)
comparison_df <- comparison_df[order(comparison_df$AIC), ]
print(comparison_df)

# --- En İyi Modelin Belirlenmesi ve Detaylı Analizi ---
# En iyi model, en düşük AIC/BIC değerine sahip olan ve
# Ljung-Box testinde p > 0.05 sonucu veren modeldir.
en_iyi_model_adi <- comparison_df$Model[1]
en_iyi_model <- switch(en_iyi_model_adi,
											 "ARIMA(0,1,2)(0,1,2)" = model_A,
											 "ARIMA(2,1,0)(1,1,0)" = model_B,
											 "ARIMA(1,1,1)(1,1,1)" = model_C,
											 "ARIMA(4,1,0)(1,1,0)" = model_D,
											 "ARIMA(4,1,4)(1,1,0)" = model_E)

cat("\n--- EN İYİ MODELİN ARTIK TESTLERİ ('", en_iyi_model_adi, "') ---\n\n")
checkresiduals(en_iyi_model)

# --- En İyi Model ile Gelecek Tahmini (Forecast) ---
cat("\n--- EN İYİ MODEL İLE 24 AYLIK TAHMİN ---\n")
tahmin <- forecast(en_iyi_model, h = 24)

# Tahmin grafiğini çizdirelim
autoplot(tahmin) +
	ggtitle(paste("En İyi Model ile Tahmin:", format(en_iyi_model))) +
	xlab("Yıl") +
	ylab("Log(Fiyat)") +
	theme_bw()

tahmin











