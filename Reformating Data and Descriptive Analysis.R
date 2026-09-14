# Import Data
library(readxl)
Data <- read_excel("customers.xlsx")
names(Data)

# Filtering Data
Others <- Data[,c(1,2,6,10,14,15,20,22,23,25,27,28,31,33,36,39,40,45,46,47)]
writexl::write_xlsx(Others,"Others.xlsx")
Data <- Data[,-c(1,2,6,10,14,15,20,22,23,25,27,28,31,33,36,39,40,45,46,47)]
names(Data)
Sosiometri <- Data[,c(1,2,3,4)]
writexl::write_xlsx(Sosiometri,"Sosiometri.xlsx")
Penjual_Keliling <- Data[,c(5:19)]
names(Penjual_Keliling)
Others_Penjual_Keliling <- Penjual_Keliling[,c(4,10:15)]
writexl::write_xlsx(Others_Penjual_Keliling,"Others Penjual Keliling.xlsx")
Penjual_Keliling <- Penjual_Keliling[,c(1,2,3,5,6,7,8,9)]
names(Penjual_Keliling)
Pedagang <- Data[,c(20:27)]
names(Pedagang)

n <- nrow(Data)
Dataset <- matrix(NA,nrow = 2*n,ncol =(ncol(Pedagang)+3))

for (i in 1:n) {
  Dataset[(2*i),1] <- i 
  Dataset[(2*i-1),1] <- i 
}

for (i in 1:n) {
  Dataset[(2*i),2] <- "Pedagang"
  Dataset[(2*i-1),2] <- "Penjual Sayur Keliling"
}

for (i in 1:n) {
  Dataset[(2*i),4:11] <- as.matrix(Pedagang)[i,1:8]
  Dataset[(2*i-1),4:11] <- as.matrix(Penjual_Keliling)[i,1:8]
}
colnames(Dataset) <- c("individual","sales","choice","shopping frequency",
                       "products bought","spending","shopping variations attention",
                       "amount purchased on weekdays","amount purchased on weekend",
                       "amount purchased on holidays","shopping amount attention")
Dataset <- data.frame(Dataset)
str(Dataset)
names(Dataset)
head(Dataset)

Dataset$individual <- as.factor(Dataset$individual)
Dataset$sales <- as.factor(Dataset$sales)
Dataset$shopping.frequency<- factor(Dataset$shopping.frequency,levels = c("Jarang","1 kali per minggu","2 kali per minggu","3-5 kali per minggu","Setiap hari"))
Dataset$products.bought <- as.factor(Dataset$products.bought)
Dataset$spending <- factor(Dataset$spending,levels = c("Kurang dari Rp 50.000","Rp 50.001-Rp 100.000","Rp 100.001-Rp 200.000","Lebih dari Rp 200.000"))
Dataset$shopping.variations.attention <- factor(Dataset$shopping.variations.attention,levels = c("Ya","Tidak"))
Dataset$amount.purchased.on.weekdays <- factor(Dataset$amount.purchased.on.weekdays,levels = c("Kurang dari 1 kg","1-2 kg","3-5 kg","Lebih dari 5 kg"))
Dataset$amount.purchased.on.weekend <- factor(Dataset$amount.purchased.on.weekend,levels = c("Kurang dari 1 kg","1-2 kg","3-5 kg","Lebih dari 5 kg"))
Dataset$amount.purchased.on.holidays <- factor(Dataset$amount.purchased.on.holidays,levels = c("Kurang dari 2 kg","3-5 kg","5-10 kg","Lebih dari 10 kg"))
Dataset$shopping.amount.attention <- factor(Dataset$shopping.amount.attention,levels = c("Ya","Tidak"))
str(Dataset)

# Membuat pie chart
pie(table(Dataset$shopping.frequency), labels =  paste(names(table(Dataset$shopping.frequency)), " (", round(table(Dataset$shopping.frequency) / sum(table(Dataset$shopping.frequency)) * 100, 1), "%)", sep = ""), main = "Shopping Frequency")
pie(table(Dataset$products.bought), labels =  paste(names(table(Dataset$products.bought)), " (", round(table(Dataset$products.bought) / sum(table(Dataset$products.bought)) * 100, 1), "%)", sep = ""), main = "Products Bought")
pie(table(Dataset$spending), labels =  paste(names(table(Dataset$spending)), " (", round(table(Dataset$spending) / sum(table(Dataset$spending)) * 100, 1), "%)", sep = ""), main = "Spending")
pie(table(Dataset$shopping.variations.attention), labels =  paste(names(table(Dataset$shopping.variations.attention)), " (", round(table(Dataset$shopping.variations.attention) / sum(table(Dataset$shopping.variations.attention)) * 100, 1), "%)", sep = ""), main = "Shopping Variations Attention")
pie(table(Dataset$amount.purchased.on.weekdays), labels =  paste(names(table(Dataset$amount.purchased.on.weekdays)), " (", round(table(Dataset$amount.purchased.on.weekdays) / sum(table(Dataset$amount.purchased.on.weekdays)) * 100, 1), "%)", sep = ""), main = "Amount Purchased on Weekdays")
pie(table(Dataset$amount.purchased.on.weekend), labels =  paste(names(table(Dataset$amount.purchased.on.weekend)), " (", round(table(Dataset$amount.purchased.on.weekend) / sum(table(Dataset$amount.purchased.on.weekend)) * 100, 1), "%)", sep = ""), main = "Amount Purchased on Weekend")
pie(table(Dataset$amount.purchased.on.holidays), labels =  paste(names(table(Dataset$amount.purchased.on.holidays)), " (", round(table(Dataset$amount.purchased.on.holidays) / sum(table(Dataset$amount.purchased.on.holidays)) * 100, 1), "%)", sep = ""), main = "Amount Purchased on Holidays")
pie(table(Dataset$shopping.amount.attention), labels =  paste(names(table(Dataset$shopping.amount.attention)), " (", round(table(Dataset$shopping.amount.attention) / sum(table(Dataset$shopping.amount.attention)) * 100, 1), "%)", sep = ""), main = "Shopping Amount Attention")

for (i in 1:n) {
  if(as.numeric(Dataset$shopping.frequency[(2*i)]) > as.numeric(Dataset$shopping.frequency[(2*i-1)])){
    Dataset$choice[(2*i)] <- "yes"
    Dataset$choice[(2*i-1)] <- "no"
  }
  else{
    Dataset$choice[(2*i)] <- "no"
    Dataset$choice[(2*i-1)] <- "yes"
  }
}
Dataset$choice <- factor(Dataset$choice,levels = c("no","yes"))
head(Dataset)
names(Dataset)
str(Dataset)

Dataset$shopping.frequency<- as.numeric(Dataset$shopping.frequency)
Dataset$products.bought <- as.numeric(Dataset$products.bought)
Dataset$spending <- as.numeric(Dataset$spending)
Dataset$shopping.variations.attention <- as.numeric(Dataset$shopping.variations.attention)
Dataset$shopping.variations.attention <- ifelse(Dataset$shopping.variations.attention == 2, 0, Dataset$shopping.variations.attention)
Dataset$amount.purchased.on.weekdays <- as.numeric(Dataset$amount.purchased.on.weekdays)
Dataset$amount.purchased.on.weekend <- as.numeric(Dataset$amount.purchased.on.weekend)
Dataset$amount.purchased.on.holidays <- as.numeric(Dataset$amount.purchased.on.holidays)
Dataset$shopping.amount.attention <- as.numeric(Dataset$shopping.amount.attention)
Dataset$shopping.amount.attention <- ifelse(Dataset$shopping.amount.attention == 2, 0, Dataset$shopping.amount.attention)

head(Dataset)
colSums(is.na(Dataset))

writexl::write_xlsx(Dataset,"Customer Dataset.xlsx")

# Frequency Analysis
# Analisis deskriptif sosiometri
Sosiometri$`2. Usia` <- as.factor(Sosiometri$`2. Usia`)
Sosiometri$`3. Jenis kelamin` <- as.factor(Sosiometri$`3. Jenis kelamin`)
Sosiometri$`4. Pekerjaan` <- as.factor(Sosiometri$`4. Pekerjaan`)
Sosiometri$`6. Silakan pilih lokasi rumah anda (Kelurahan)` <- as.factor(Sosiometri$`6. Silakan pilih lokasi rumah anda (Kelurahan)`)
summary(Sosiometri[,1:4])

pie(table(Sosiometri$`2. Usia`), labels =  paste(names(table(Sosiometri$`2. Usia`)), " (", round(table(Sosiometri$`2. Usia`) / sum(table(Sosiometri$`2. Usia`)) * 100, 1), "%)", sep = ""), main = "Age")
pie(table(Sosiometri$`3. Jenis kelamin`), labels =  paste(names(table(Sosiometri$`3. Jenis kelamin`)), " (", round(table(Sosiometri$`3. Jenis kelamin`) / sum(table(Sosiometri$`3. Jenis kelamin`)) * 100, 1), "%)", sep = ""), main = "Gender")
pie(table(Sosiometri$`4. Pekerjaan`), labels =  paste(names(table(Sosiometri$`4. Pekerjaan`)), " (", round(table(Sosiometri$`4. Pekerjaan`) / sum(table(Sosiometri$`4. Pekerjaan`)) * 100, 1), "%)", sep = ""), main = "Job")
pie(table(Sosiometri$`6. Silakan pilih lokasi rumah anda (Kelurahan)`), labels =  paste(names(table(Sosiometri$`6. Silakan pilih lokasi rumah anda (Kelurahan)`)), " (", round(table(Sosiometri$`6. Silakan pilih lokasi rumah anda (Kelurahan)`) / sum(table(Sosiometri$`6. Silakan pilih lokasi rumah anda (Kelurahan)`)) * 100, 1), "%)", sep = ""), main = "House Location")

# Mobile Vendors Data
library(readxl)
Vendors <- read_excel("mobile vendors.xlsx")
names(Vendors)
View(Vendors)

# Filtering Data
Demographic_Characteristics <- Vendors[,c(3,4,5)]
colnames(Demographic_Characteristics) <- c("Age distribution","Gender breakdown","Educational level")
writexl::write_xlsx(Demographic_Characteristics,"Demographic Characteristics.xlsx")
Operational_Data <- Vendors[,c(6:8,13,15:23,25,27)]
colnames(Operational_Data) <- c("Years of experiences", "Location of house", "Operational area", "Type of Goods Sold",
                      "Quantity of Goods Sold per Day", "Number of Trips per Day", "Average Loading Time per Trip",
                      "Daily operational hours", "Average Time to Serve a Customer", "Average Number of Customers Served per Day",
                      "Daily Travel Distance", "Number of Operating Days per Week", "Type of Vehicle Used", "Vehicle Capacity",
                      "Average daily earnings")
writexl::write_xlsx(Operational_Data,"Operational Data.xlsx")
Others_Vendors <- Vendors[,c(9:12,14,24,26,28:34)]
writexl::write_xlsx(Others_Vendors,"Others Vendors.xlsx")

Operational_Data$`Years of experiences` <- factor(Operational_Data$`Years of experiences`,levels = c("1-3 tahun","4-6 tahun","7-10 tahun","Lebih dari 10 tahun"))
Operational_Data$`Location of house` <- as.factor(Operational_Data$`Location of house`)
Operational_Data$`Operational area`<- as.factor(Operational_Data$`Operational area`)
Operational_Data$`Type of Goods Sold` <- as.factor(Operational_Data$`Type of Goods Sold`)
# Convert all entries to numeric values
Operational_Data$`Quantity of Goods Sold per Day` <- as.numeric(gsub("[^0-9\\.-]", "", Operational_Data$`Quantity of Goods Sold per Day`))
Operational_Data$`Quantity of Goods Sold per Day` <- ifelse(Operational_Data$`Quantity of Goods Sold per Day` == 255, 17,Operational_Data$`Quantity of Goods Sold per Day`)
Operational_Data$`Quantity of Goods Sold per Day`[is.na(Operational_Data$`Quantity of Goods Sold per Day`)] <- 10

# Categorize based on quantity
Operational_Data$`Quantity of Goods Sold per Day` <- ifelse(Operational_Data$`Quantity of Goods Sold per Day` < 40, "lebih kecil dari 40 kg",
                                                            ifelse(Operational_Data$`Quantity of Goods Sold per Day` >= 40 & Operational_Data$`Quantity of Goods Sold per Day` < 60, "40-60 kg",
                                                                   ifelse(Operational_Data$`Quantity of Goods Sold per Day` >= 60 & Operational_Data$`Quantity of Goods Sold per Day` < 80, "60-80 kg",
                                                                          ifelse(Operational_Data$`Quantity of Goods Sold per Day` >= 80 & Operational_Data$`Quantity of Goods Sold per Day` < 100, "80-100 kg",
                                                                                 ifelse(Operational_Data$`Quantity of Goods Sold per Day` >= 100, "lebih besar dari 100 kg", Operational_Data$`Quantity of Goods Sold per Day`)))))

Operational_Data$`Quantity of Goods Sold per Day` <- factor(Operational_Data$`Quantity of Goods Sold per Day`,levels = c("lebih kecil dari 40 kg","40-60 kg","60-80 kg","80-100 kg","lebih besar dari 100 kg"))
Operational_Data$`Number of Trips per Day` <- factor(Operational_Data$`Number of Trips per Day`,levels = c("1 perjalanan","2-3 perjalanan","4-5 perjalanan","lebih dari 5 perjalanan"))
Operational_Data$`Average Loading Time per Trip` <- factor(Operational_Data$`Average Loading Time per Trip`,levels = c("Kurang dari 15 menit","15-30 menit","30-45 menit","Lebih dari 45 menit"))
Operational_Data$`Daily operational hours` <- factor(Operational_Data$`Daily operational hours`,levels = c("Kurang dari 4 jam","4-6 jam","6-8 jam","Lebih dari 8 jam"))
Operational_Data$`Average Time to Serve a Customer` <- factor(Operational_Data$`Average Time to Serve a Customer`,levels = c("Kurang dari 5 menit","5-10 menit","10-15 menit","Lebih dari 15 menit"))
Operational_Data$`Average Number of Customers Served per Day` <- ifelse(Operational_Data$`Average Number of Customers Served per Day` == 44105, "10-20",Operational_Data$`Average Number of Customers Served per Day`)
Operational_Data$`Average Number of Customers Served per Day` <- factor(Operational_Data$`Average Number of Customers Served per Day`,levels = c("Kurang dari 10","10-20","21-30","Lebih dari 30"))
Operational_Data$`Daily Travel Distance` <- factor(Operational_Data$`Daily Travel Distance`,levels = c("Kurang dari 5 km","5-10 km","11-15 km","Lebih dari 15 km"))
Operational_Data$`Number of Operating Days per Week` <- factor(Operational_Data$`Number of Operating Days per Week`,levels = c("3-4 hari","5-6 hari","7 hari"))
Operational_Data$`Type of Vehicle Used` <- factor(Operational_Data$`Type of Vehicle Used`,levels = c("Sepeda yang dimodifikasi","Sepeda motor yang dimodifikasi","Lainnya"))
Operational_Data$`Vehicle Capacity` <- factor(Operational_Data$`Vehicle Capacity`,levels = c("Kurang dari 50 kg","50-100 kg","101-200 kg","Lebih dari 200 kg"))
Operational_Data$`Average daily earnings` <- factor(Operational_Data$`Average daily earnings`,levels = c("Kurang dari Rp 100.000","Rp 100.001-Rp 200.000","Rp 200.001-Rp 300.000","Lebih dari Rp 300.000"))

str(Operational_Data)

# Membuat pie chart
pie(table(Operational_Data$`Years of experiences`), labels =  paste(names(table(Operational_Data$`Years of experiences`)), " (", round(table(Operational_Data$`Years of experiences`) / sum(table(Operational_Data$`Years of experiences`)) * 100, 1), "%)", sep = ""), main = "Years of experiences")
pie(table(Operational_Data$`Location of house`), labels =  paste(names(table(Operational_Data$`Location of house`)), " (", round(table(Operational_Data$`Location of house`) / sum(table(Operational_Data$`Location of house`)) * 100, 1), "%)", sep = ""), main = "Location of house")
pie(table(Operational_Data$`Operational area`), labels =  paste(names(table(Operational_Data$`Operational area`)), " (", round(table(Operational_Data$`Operational area`) / sum(table(Operational_Data$`Operational area`)) * 100, 1), "%)", sep = ""), main = "Operational area")
pie(table(Operational_Data$`Type of Goods Sold`), labels =  paste(names(table(Operational_Data$`Type of Goods Sold`)), " (", round(table(Operational_Data$`Type of Goods Sold`) / sum(table(Operational_Data$`Type of Goods Sold`)) * 100, 1), "%)", sep = ""), main = "Type of Goods Sold")
pie(table(Operational_Data$`Quantity of Goods Sold per Day`), labels =  paste(names(table(Operational_Data$`Quantity of Goods Sold per Day`)), " (", round(table(Operational_Data$`Quantity of Goods Sold per Day`) / sum(table(Operational_Data$`Quantity of Goods Sold per Day`)) * 100, 1), "%)", sep = ""), main = "Quantity of Goods Sold per Day")
pie(table(Operational_Data$`Number of Trips per Day`), labels =  paste(names(table(Operational_Data$`Number of Trips per Day`)), " (", round(table(Operational_Data$`Number of Trips per Day`) / sum(table(Operational_Data$`Number of Trips per Day`)) * 100, 1), "%)", sep = ""), main = "Number of Trips per Day")
pie(table(Operational_Data$`Average Loading Time per Trip`), labels =  paste(names(table(Operational_Data$`Average Loading Time per Trip`)), " (", round(table(Operational_Data$`Average Loading Time per Trip`) / sum(table(Operational_Data$`Average Loading Time per Trip`)) * 100, 1), "%)", sep = ""), main = "Average Loading Time per Trip")
pie(table(Operational_Data$`Daily operational hours`), labels =  paste(names(table(Operational_Data$`Daily operational hours`)), " (", round(table(Operational_Data$`Daily operational hours`) / sum(table(Operational_Data$`Daily operational hours`)) * 100, 1), "%)", sep = ""), main = "Daily operational hours")
pie(table(Operational_Data$`Average Time to Serve a Customer`), labels =  paste(names(table(Operational_Data$`Average Time to Serve a Customer`)), " (", round(table(Operational_Data$`Average Time to Serve a Customer`) / sum(table(Operational_Data$`Average Time to Serve a Customer`)) * 100, 1), "%)", sep = ""), main = "Average Time to Serve a Customer")
pie(table(Operational_Data$`Average Number of Customers Served per Day`), labels =  paste(names(table(Operational_Data$`Average Number of Customers Served per Day`)), " (", round(table(Operational_Data$`Average Number of Customers Served per Day`) / sum(table(Operational_Data$`Average Number of Customers Served per Day`)) * 100, 1), "%)", sep = ""), main = "Average Number of Customers Served per Day")
pie(table(Operational_Data$`Daily Travel Distance`), labels =  paste(names(table(Operational_Data$`Daily Travel Distance`)), " (", round(table(Operational_Data$`Daily Travel Distance`) / sum(table(Operational_Data$`Daily Travel Distance`)) * 100, 1), "%)", sep = ""), main = "Daily Travel Distance")
pie(table(Operational_Data$`Number of Operating Days per Week`), labels =  paste(names(table(Operational_Data$`Number of Operating Days per Week`)), " (", round(table(Operational_Data$`Number of Operating Days per Week`) / sum(table(Operational_Data$`Number of Operating Days per Week`)) * 100, 1), "%)", sep = ""), main = "Number of Operating Days per Week")
pie(table(Operational_Data$`Type of Vehicle Used`), labels =  paste(names(table(Operational_Data$`Type of Vehicle Used`)), " (", round(table(Operational_Data$`Type of Vehicle Used`) / sum(table(Operational_Data$`Type of Vehicle Used`)) * 100, 1), "%)", sep = ""), main = "Type of Vehicle Used")
pie(table(Operational_Data$`Vehicle Capacity`), labels =  paste(names(table(Operational_Data$`Vehicle Capacity`)), " (", round(table(Operational_Data$`Vehicle Capacity`) / sum(table(Operational_Data$`Vehicle Capacity`)) * 100, 1), "%)", sep = ""), main = "Vehicle Capacity")
pie(table(Operational_Data$`Average daily earnings`), labels =  paste(names(table(Operational_Data$`Average daily earnings`)), " (", round(table(Operational_Data$`Average daily earnings`) / sum(table(Operational_Data$`Average daily earnings`)) * 100, 1), "%)", sep = ""), main = "Average daily earnings")


Operational_Data$`Years of experiences`<- as.numeric(Operational_Data$`Years of experiences`)
Operational_Data$`Location of house` <- as.numeric(Operational_Data$`Location of house`)
Operational_Data$`Operational area` <- as.numeric(Operational_Data$`Operational area`)
Operational_Data$`Type of Goods Sold` <- as.numeric(Operational_Data$`Type of Goods Sold`)
Operational_Data$`Quantity of Goods Sold per Day` <- as.numeric(Operational_Data$`Quantity of Goods Sold per Day`)
Operational_Data$`Number of Trips per Day` <- as.numeric(Operational_Data$`Number of Trips per Day`)
Operational_Data$`Average Loading Time per Trip` <- as.numeric(Operational_Data$`Average Loading Time per Trip`)
Operational_Data$`Daily operational hours` <- as.numeric(Operational_Data$`Daily operational hours`)
Operational_Data$`Average Time to Serve a Customer` <- as.numeric(Operational_Data$`Average Time to Serve a Customer`)
Operational_Data$`Average Number of Customers Served per Day` <- as.numeric(Operational_Data$`Average Number of Customers Served per Day`)
Operational_Data$`Daily Travel Distance` <- as.numeric(Operational_Data$`Daily Travel Distance`)
Operational_Data$`Number of Operating Days per Week` <- as.numeric(Operational_Data$`Number of Operating Days per Week`)
Operational_Data$`Type of Vehicle Used` <- as.numeric(Operational_Data$`Type of Vehicle Used`)
Operational_Data$`Vehicle Capacity` <- as.numeric(Operational_Data$`Vehicle Capacity`)
Operational_Data$`Average daily earnings` <- as.numeric(Operational_Data$`Average daily earnings`)

head(Operational_Data)
colSums(is.na(Operational_Data))

writexl::write_xlsx(Operational_Data,"Customer Operational_Data.xlsx")

# Analisis deskriptif sosiometri
Demographic_Characteristics$`Age distribution` <- as.factor(Demographic_Characteristics$`Age distribution`)
Demographic_Characteristics$`Gender breakdown` <- as.factor(Demographic_Characteristics$`Gender breakdown`)
Demographic_Characteristics$`Educational level` <- as.factor(Demographic_Characteristics$`Educational level`)

pie(table(Demographic_Characteristics$`Age distribution`), labels =  paste(names(table(Demographic_Characteristics$`Age distribution`)), " (", round(table(Demographic_Characteristics$`Age distribution`) / sum(table(Demographic_Characteristics$`Age distribution`)) * 100, 1), "%)", sep = ""), main = "Age distribution")
pie(table(Demographic_Characteristics$`Gender breakdown`), labels =  paste(names(table(Demographic_Characteristics$`Gender breakdown`)), " (", round(table(Demographic_Characteristics$`Gender breakdown`) / sum(table(Demographic_Characteristics$`Gender breakdown`)) * 100, 1), "%)", sep = ""), main = "Gender breakdown")
pie(table(Demographic_Characteristics$`Educational level`), labels =  paste(names(table(Demographic_Characteristics$`Educational level`)), " (", round(table(Demographic_Characteristics$`Educational level`) / sum(table(Demographic_Characteristics$`Educational level`)) * 100, 1), "%)", sep = ""), main = "Educational level")
