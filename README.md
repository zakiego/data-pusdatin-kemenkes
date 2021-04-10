# Data Covid-19 Pusdatin Kementrian Kesehatan

Spreadsheet https://docs.google.com/spreadsheets/d/1nai80Cfxw2OINV4JIkomlP80JbtkXYkAQsAfn5mu0o4/

Sumber data : API ```https://data.covid19.go.id/public/api/prov.json```

## Data Yang Tersedia

- Kasus positif **berdasarkan usia** per provinsi
- Kasus positif **berdasarkan jenis kelamin** per provinsi
- Kasus kumulatif, dirawat, sembuh, dan meninggal per provinsi
- Daftar rumah sakit
- Daftar laboratorium

## Bermain dengan API

### Install Packages

```R
install.packages('httr')
install.packages('dplyr')
install.packages('tidyverse')
```

### Library

```R
library(httr)
library(dplyr)
library(tidyverse)
```

### Memancing API

```R
raw <- GET("https://data.covid19.go.id/public/api/prov.json") # memancing API
raw <- content(raw, as = "parsed", simplifyVector = TRUE) # membaca respons
print(raw)
```

### Tanggal Data Terakhir
```R
tanggal <- raw$last_date 
print(tanggal)
```


### List Provinsi
```R
provinsi <- data.frame(raw$list_data$key) 
names(provinsi) <- c("provinsi") # mengganti nama kolom
print(provinsi)
```

### Data Kumulatif, Dirawat, Sembuh, dan Meninggal
```R
# overall
kumulatif <- data.frame(tanggal, provinsi, raw$list_data$jumlah_kasus)      # mengubah list ke df dan menambah kolom provinsi dan tanggal
dirawat <- data.frame(tanggal, provinsi, raw$list_data$jumlah_dirawat)      # mengubah list ke df dan menambah kolom provinsi dan tanggal
sembuh <- data.frame(tanggal, provinsi,raw$list_data$jumlah_sembuh)         # mengubah list ke df dan menambah kolom provinsi dan tanggal
meninggal <- data.frame(tanggal, provinsi, raw$list_data$jumlah_meninggal)  # mengubah list ke df dan menambah kolom provinsi dan tanggal


# memperbaiki nama kolom
names(kumulatif) <- c("tanggal", "provinsi", "kumulatif")
names(dirawat) <- c("tanggal", "provinsi", "dirawat")
names(sembuh) <- c("tanggal", "provinsi", "sembuh")
names(meninggal) <- c("tanggal", "provinsi", "meninggal")

# menggabungkan
master <-
  kumulatif %>%
  right_join(dirawat,by = c("tanggal", "provinsi")) %>%
  right_join(sembuh, by = c("tanggal", "provinsi")) %>% 
  right_join(meninggal, by = c("tanggal", "provinsi"))

print(master)
```

### Data Kelompok Umur
```R
kelompok_umur_raw <- raw$list_data$kelompok_umur # mengekstrak ke list khusus
kelompok_umur <- data.frame(matrix(unlist(kelompok_umur_raw), nrow = length(kelompok_umur_raw), byrow = TRUE), stringsAsFactors = FALSE) # mengubah list menjadi df

remove(kelompok_umur_raw) # menghapus list khusus karena sudah tidak terpakai

kelompok_umur <- kelompok_umur[7:12] # mengambil kolom yang perlu
names(kelompok_umur) <- c("x0_5", "x6_18", "x19_30", "x31_45", "x46_59", "x60_up") # mengubah nama kolom

kelompok_umur$x0_5 <- as.numeric(kelompok_umur$x0_5)      # mengubah chr menjadi num
kelompok_umur$x6_18 <- as.numeric(kelompok_umur$x6_18)    # mengubah chr menjadi num
kelompok_umur$x19_30 <- as.numeric(kelompok_umur$x19_30)  # mengubah chr menjadi num
kelompok_umur$x31_45 <- as.numeric(kelompok_umur$x31_45)  # mengubah chr menjadi num
kelompok_umur$x46_59 <- as.numeric(kelompok_umur$x46_59)  # mengubah chr menjadi num
kelompok_umur$x60_up <- as.numeric(kelompok_umur$x60_up)  # mengubah chr menjadi num

kelompok_umur <- data.frame(kumulatif, kelompok_umur) # menggabung dengan data kumulatif untuk mecari data "tidak diketahui"
kelompok_umur <- mutate(kelompok_umur, "tidak_diketahui" = kumulatif - (x0_5 + x6_18 + x19_30 + x31_45 + x46_59 + x60_up)) # menambah kolom data "tidak diketahui"
kelompok_umur <- select(kelompok_umur, -c(tanggal, kumulatif))  # menghapus kolom tanggal dan kumulatif, karena mengganggu pivot longer
kelompok_umur <- pivot_longer(kelompok_umur, !provinsi, names_to = "kelompok_umur", values_to = "jumlah") # mengubah enam kolom menjadi satu kolom
kelompok_umur <- data.frame(tanggal, kelompok_umur) # menggabungkan dengan kolom tanggal

kelompok_umur <- kelompok_umur %>% 
  pivot_wider(c(!kelompok_umur, !jumlah), names_from = kelompok_umur, values_from =  jumlah) %>% 
  arrange(provinsi, tanggal)

names(kelompok_umur) <- c("tanggal", "provinsi", "0-5", "6-18",
                          "19-30", "31-45", "45-59", ">=60",
                          "tidak diketahui")
print(kelompok_umur)
```


### Data Jenis Kelamin
```R
jenis_kelamin_raw <- raw$list_data$jenis_kelamin # mengekstrak ke list khusus
jenis_kelamin <- data.frame(matrix(unlist(jenis_kelamin_raw), nrow = length(jenis_kelamin_raw), byrow = TRUE), stringsAsFactors = FALSE) # mengubah list menjadi df
remove(jenis_kelamin_raw) # menghapus list khusus karena sudah tidak terpakai
jenis_kelamin <- jenis_kelamin[3:4] # mengambil kolom yang perlu
names(jenis_kelamin) <- c('laki_laki', 'perempuan') # mengubah nama kolom
jenis_kelamin$laki_laki <- as.numeric(jenis_kelamin$laki_laki) # mengubah chr menjadi num
jenis_kelamin$perempuan <- as.numeric(jenis_kelamin$perempuan) # mengubah chr menjadi num
jenis_kelamin <- data.frame(kumulatif, jenis_kelamin) # menggabung dengan data kumulatif untuk mecari data "tidak diketahui"
jenis_kelamin <- mutate(jenis_kelamin, "tidak_diketahui" = kumulatif-(laki_laki+perempuan)) # menambah kolom data "tidak diketahui"
jenis_kelamin <- select(jenis_kelamin, -c(tanggal, kumulatif)) # menghapus kolom tanggal dan kumulatif, karena mengganggu pivot longer
jenis_kelamin <- pivot_longer(jenis_kelamin, !provinsi, names_to = "jenis_kelamin", values_to = "jumlah") # mengubah dua kolom menjadi satu kolom
jenis_kelamin <- data.frame(tanggal, jenis_kelamin) # menggabungkan dengan kolom tanggal

jenis_kelamin <- jenis_kelamin %>% 
  pivot_wider(c(!jenis_kelamin, !jumlah), names_from = jenis_kelamin, values_from =  jumlah) %>% 
  arrange(provinsi, tanggal)

names(jenis_kelamin) <- c("tanggal", "provinsi",
                          "laki-laki", "perempuan",
                          "tidak diketahui")
print(jenis_kelamin)

```

### Save File
```R
write.csv(master, "provinsi.csv", row.names = FALSE)        

write.csv(kelompok_umur, "kumulatif_provinsi_kelompok_umur_pivot.csv", row.names = FALSE)
write.csv(jenis_kelamin, "kumulatif_provinsi_jenis_kelamin_pivot.csv", row.names = FALSE)

```
