# TimeSeriesForecastingR
R ile Doğalgaz Fiyatları SARIMA Zaman Serisi Analizi
Proje Amacı: Mevsimsel zaman serisi analizi
Kullanılan Veri: 1989'dan itibaren aylık doğalgaz ithalat fiyatları. https://www.eia.gov/dnav/ng/hist/n9100us3m.htm
Analiz Adımları: Veri yükleme, görselleştirme, dekompozisyon, durağanlık testleri, fark alma, logaritmik dönüşüm, model seçimi - auto.arima ve manuel, model tanılama, tahmin.
Kullanılan Teknolojiler: R, readxl, tseries, forecast, ggplot2
Nasıl Çalıştırılır: R dosyasını çalıştırmak için öncelikle gerekli kütüphaneleri kurmalısınız, kendi dosya uzantınızı girmelisiniz: read_xlsx("dosya yolu")

Sonuçlar ve Bulgular: 

1. Verinin durağan olmadığı tespit edildi.

2. Durağanlık sağlamak için:
   - Varyans stabilizasyonu: Logaritmik dönüşüm
   - Trend ve mevsimsellik giderimi: Mevsimsel olmayan (d=1) ve mevsimsel (D=1) fark alma

3. SARIMA modellerinden AICc kriterine göre SARIMA(0,1,1)(0,1,1)[12] seçildi.

4. Modelin artıkları "White noise" olarak doğrulandı, dolayısıyla istatistiksel olarak geçerli.

5. Seçilen model ile 24 aylık güven aralıklı tahmin yapıldı.
