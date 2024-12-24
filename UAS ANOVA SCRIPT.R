#Putu Padma Widya Saraswati/2415091078/SI IKI

#Uji ANOVA kali ini saya ingin mengecek apakah ada perbedaan yang signifikan terhadap pengetahuan masyarakat umum di pedesaan dengan di perkotaan

library(readxl)
Pengetahuan_2012 <- read_excel("C:/Users/LENOVO/Downloads/Pengetahuan, 2012.xlsx")
View(Pengetahuan_2012)

install.packages("car")
install.packages("ggplot2")

#Uji Normalitas
shapiro.test(Pengetahuan_2012$Pedesaan)
shapiro.test(Pengetahuan_2012$Perkotaan)

#Uji Varians
var.test(Pengetahuan_2012$Pedesaan,Pengetahuan_2012$Perkotaan)

# Uji Independent
t.test(Pengetahuan_2012$Pedesaan,Pengetahuan_2012$Perkotaan,var.equal = TRUE,conf.level = 0.95)

#Uji ANOVA
anova <- aov(Pengetahuan_2012$Pedesaan~Pengetahuan_2012$Perkotaan,data = NULL)
summary(anova)

#UJi Lanjutan
#Tidak melakukan uji lanjutan karena tidak ada perbedaan yang signifikan

#Visualisasi
desa = Pengetahuan_2012$Pedesaan
desa
kota = Pengetahuan_2012$Perkotaan
kota
provinsi = Pengetahuan_2012$Provinsi
provinsi
hist(desa, breaks = 4,freq = TRUE, main = 'Histogram Desa',xlab = 'desa',col = 'blue')
hist(kota,breaks = 4,freq = TRUE,main = 'Histogram Kota',xlab = 'provinsi',col = 'yellow')

#Interprestasi
#Terdapat tabel yang berguna untuk mengecek apakah ada perbedaan yang signifikan terhadap
#nilai persentase pengetahuan di perkotaan dengan pedesaan. Disini saya menggunakan uji anova 
#yang terdiri dari uji normalitas, uji varians, dan uji independent. 
#Pada Uji normalitas menghasilkan bahwa data tersebut merupakan distibusi normal(terpenuhi)
#Pada uji varians menghasilkan bahwa data tersebut homogen (terpenuhi)
#Pada uji independent menghasilkan bahwa data tersebut ada perbedaan yang signifikan(terpenuhi)
#Pada uji anova menghasilkan bahwa data tersebut tidak ada perbedaan yang signifikan
#Kesimpulan nya adalah ada perbedaan nyata jika kota dan desa di bandingkan, namun rata-rata perbedaan 
#pada daerah desa dan kota tidak ada perbedaan signifikan.
#sehingga pengetahuan di desa dan kota tidak berbeda jauh.