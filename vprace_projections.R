setwd("C:/Users/mgarcia/Documents/STATA")
vprace <- read.csv("vprace_region2.csv")
trends <- read.csv("vprace_trends.csv")

vprace$Time2 = as.POSIXct(vprace$Time, format = "%m/%d/%Y %H:%M")
vprace$Time3 = as.POSIXlt(vprace$Time2)
vprace$value = rep(1:116,20)

trends$Time2 = as.POSIXct(trends$Time, format =  "%m/%d/%Y %H:%M")
trends$Time3 = as.POSIXlt(trends$Time2)

library(plyr)
vprace <- ddply(vprace, "Region", transform, growth = c(NA,exp(diff(log(Trans)))-1))

### Create Subset of the Data Set per Region
ncr      <- subset(vprace, Region == 0)
region1  <- subset(vprace, Region == 1)
car      <- subset(vprace, Region == 2)
region2  <- subset(vprace, Region == 3)
region3  <- subset(vprace, Region == 4)
region4a <- subset(vprace, Region == 5)
region4b <- subset(vprace, Region == 6)
region5  <- subset(vprace, Region == 7)
region6  <- subset(vprace, Region == 8)
nir      <- subset(vprace, Region == 9)
region7  <- subset(vprace, Region == 10)
region8  <- subset(vprace, Region == 11)
region9  <- subset(vprace, Region == 12)
region10 <- subset(vprace, Region == 13)
region11 <- subset(vprace, Region == 14)
region12 <- subset(vprace, Region == 15)
caraga   <- subset(vprace, Region == 16)
armm     <- subset(vprace, Region == 17)
oav      <- subset(vprace, Region == 19)
phil     <- subset(vprace, Region == 18)

### Create a Subset of the Data Set per Region on May 9
ncr_may9      <- subset(vprace, Region == 0 & value <= 39)
region1_may9  <- subset(vprace, Region == 1 & value <= 39)
car_may9      <- subset(vprace, Region == 2 & value <= 39)
region2_may9  <- subset(vprace, Region == 3 & value <= 39)
region3_may9  <- subset(vprace, Region == 4 & value <= 39)
region4a_may9 <- subset(vprace, Region == 5 & value <= 39)
region4b_may9 <- subset(vprace, Region == 6 & value <= 39)
region5_may9  <- subset(vprace, Region == 7 & value <= 39)
region6_may9  <- subset(vprace, Region == 8 & value <= 39)
nir_may9      <- subset(vprace, Region == 9 & value <= 39)
region7_may9  <- subset(vprace, Region == 10 & value <= 39)
region8_may9  <- subset(vprace, Region == 11 & value <= 39)
region9_may9  <- subset(vprace, Region == 12 & value <= 39)
region10_may9 <- subset(vprace, Region == 13 & value <= 39)
region11_may9 <- subset(vprace, Region == 14 & value <= 39)
region12_may9 <- subset(vprace, Region == 15 & value <= 39)
caraga_may9   <- subset(vprace, Region == 16 & value <= 39)
armm_may9     <- subset(vprace, Region == 17 & value <= 39)
oav_may9      <- subset(vprace, Region == 19 & value <= 39)
phil_may9     <- subset(vprace, Region == 18 & value <= 39)
all_may9      <- subset(vprace, Region < 18 & value > 1 & value <= 14, na.rm = TRUE)

### For Figure 1: Create Data Frame
dfncr      <- data.frame(y = ncr$Trans, x = ncr$Time3)
dfregion1  <- data.frame(y = region1$Trans, x = region1$Time3)
dfregion2  <- data.frame(y = region2$Trans, x = region2$Time3)
dfregion3  <- data.frame(y = region3$Trans, x = region3$Time3)
dfregion4a <- data.frame(y = region4a$Trans, x = region4a$Time3)
dfregion4b <- data.frame(y = region4b$Trans, x = region4b$Time3)
dfregion5  <- data.frame(y = region5$Trans, x = region5$Time3)
dfregion6  <- data.frame(y = region6$Trans, x = region6$Time3)
dfregion7  <- data.frame(y = region7$Trans, x = region7$Time3)
dfregion8  <- data.frame(y = region8$Trans, x = region8$Time3)
dfregion9  <- data.frame(y = region9$Trans, x = region9$Time3)
dfregion10 <- data.frame(y = region10$Trans, x = region10$Time3)
dfregion11 <- data.frame(y = region11$Trans, x = region11$Time3)
dfregion12 <- data.frame(y = region12$Trans, x = region12$Time3)
dfcar      <- data.frame(y = car$Trans, x = region1$Time3)
dfnir      <- data.frame(y = nir$Trans, x = region1$Time3)
dfcaraga   <- data.frame(y = caraga$Trans, x = region1$Time3)
dfarmm     <- data.frame(y = armm$Trans, x = armm$Time3)
dfoav      <- data.frame(y = oav$Trans, x = oav$Time3)
dfphil     <- data.frame(y = phil$Trans, x = phil$Time3)
dfall      <- data.frame(y = all_may9$Trans, x = all_may9$Time3)


### For Supplementary Figures
dfncr_PM     <- data.frame(y = ncr_may9$PM, x = ncr_may9$Time3)
dfncr_PR     <- data.frame(y = ncr_may9$PR, x = ncr_may9$Time3)
dfncr_PC     <- data.frame(y = ncr_may9$PC, x = ncr_may9$Time3)
dfncr_PE     <- data.frame(y = ncr_may9$PE, x = ncr_may9$Time3)
dfncr_PT     <- data.frame(y = ncr_may9$PT, x = ncr_may9$Time3)
dfncr_PH     <- data.frame(y = ncr_may9$PH, x = ncr_may9$Time3)

dfregion1_PM <- data.frame(y = region1_may9$PM, x = region1_may9$Time3)
dfregion1_PR <- data.frame(y = region1_may9$PR, x = region1_may9$Time3)
dfregion1_PC <- data.frame(y = region1_may9$PC, x = region1_may9$Time3)
dfregion1_PE <- data.frame(y = region1_may9$PE, x = region1_may9$Time3)
dfregion1_PT <- data.frame(y = region1_may9$PT, x = region1_may9$Time3)
dfregion1_PH <- data.frame(y = region1_may9$PH, x = region1_may9$Time3)

dfregion2_PM <- data.frame(y = region2_may9$PM, x = region2_may9$Time3)
dfregion2_PR <- data.frame(y = region2_may9$PR, x = region2_may9$Time3)
dfregion2_PC <- data.frame(y = region2_may9$PC, x = region2_may9$Time3)
dfregion2_PE <- data.frame(y = region2_may9$PE, x = region2_may9$Time3)
dfregion2_PT <- data.frame(y = region2_may9$PT, x = region2_may9$Time3)
dfregion2_PH <- data.frame(y = region2_may9$PH, x = region2_may9$Time3)

dfcar_PM <- data.frame(y = car_may9$PM, x = car_may9$Time3)
dfcar_PR <- data.frame(y = car_may9$PR, x = car_may9$Time3)
dfcar_PC <- data.frame(y = car_may9$PC, x = car_may9$Time3)
dfcar_PE <- data.frame(y = car_may9$PE, x = car_may9$Time3)
dfcar_PT <- data.frame(y = car_may9$PT, x = car_may9$Time3)
dfcar_PH <- data.frame(y = car_may9$PH, x = car_may9$Time3)

dfregion3_PM <- data.frame(y = region3_may9$PM, x = region3_may9$Time3)
dfregion3_PR <- data.frame(y = region3_may9$PR, x = region3_may9$Time3)
dfregion3_PC <- data.frame(y = region3_may9$PC, x = region3_may9$Time3)
dfregion3_PE <- data.frame(y = region3_may9$PE, x = region3_may9$Time3)
dfregion3_PT <- data.frame(y = region3_may9$PT, x = region3_may9$Time3)
dfregion3_PH <- data.frame(y = region3_may9$PH, x = region3_may9$Time3)

dfregion4a_PM <- data.frame(y = region4a_may9$PM, x = region4a_may9$Time3)
dfregion4a_PR <- data.frame(y = region4a_may9$PR, x = region4a_may9$Time3)
dfregion4a_PC <- data.frame(y = region4a_may9$PC, x = region4a_may9$Time3)
dfregion4a_PE <- data.frame(y = region4a_may9$PE, x = region4a_may9$Time3)
dfregion4a_PT <- data.frame(y = region4a_may9$PT, x = region4a_may9$Time3)
dfregion4a_PH <- data.frame(y = region4a_may9$PH, x = region4a_may9$Time3)

dfregion4b_PM <- data.frame(y = region4b_may9$PM, x = region4b_may9$Time3)
dfregion4b_PR <- data.frame(y = region4b_may9$PR, x = region4b_may9$Time3)
dfregion4b_PC <- data.frame(y = region4b_may9$PC, x = region4b_may9$Time3)
dfregion4b_PE <- data.frame(y = region4b_may9$PE, x = region4b_may9$Time3)
dfregion4b_PT <- data.frame(y = region4b_may9$PT, x = region4b_may9$Time3)
dfregion4b_PH <- data.frame(y = region4b_may9$PH, x = region4b_may9$Time3)

dfregion5_PM <- data.frame(y = region5_may9$PM, x = region5_may9$Time3)
dfregion5_PR <- data.frame(y = region5_may9$PR, x = region5_may9$Time3)
dfregion5_PC <- data.frame(y = region5_may9$PC, x = region5_may9$Time3)
dfregion5_PE <- data.frame(y = region5_may9$PE, x = region5_may9$Time3)
dfregion5_PT <- data.frame(y = region5_may9$PT, x = region5_may9$Time3)
dfregion5_PH <- data.frame(y = region5_may9$PH, x = region5_may9$Time3)

dfregion6_PM <- data.frame(y = region6_may9$PM, x = region6_may9$Time3)
dfregion6_PR <- data.frame(y = region6_may9$PR, x = region6_may9$Time3)
dfregion6_PC <- data.frame(y = region6_may9$PC, x = region6_may9$Time3)
dfregion6_PE <- data.frame(y = region6_may9$PE, x = region6_may9$Time3)
dfregion6_PT <- data.frame(y = region6_may9$PT, x = region6_may9$Time3)
dfregion6_PH <- data.frame(y = region6_may9$PH, x = region6_may9$Time3)

dfnir_PM <- data.frame(y = nir_may9$PM, x = nir_may9$Time3)
dfnir_PR <- data.frame(y = nir_may9$PR, x = nir_may9$Time3)
dfnir_PC <- data.frame(y = nir_may9$PC, x = nir_may9$Time3)
dfnir_PE <- data.frame(y = nir_may9$PE, x = nir_may9$Time3)
dfnir_PT <- data.frame(y = nir_may9$PT, x = nir_may9$Time3)
dfnir_PH <- data.frame(y = nir_may9$PH, x = nir_may9$Time3)

dfregion7_PM <- data.frame(y = region7_may9$PM, x = region7_may9$Time3)
dfregion7_PR <- data.frame(y = region7_may9$PR, x = region7_may9$Time3)
dfregion7_PC <- data.frame(y = region7_may9$PC, x = region7_may9$Time3)
dfregion7_PE <- data.frame(y = region7_may9$PE, x = region7_may9$Time3)
dfregion7_PT <- data.frame(y = region7_may9$PT, x = region7_may9$Time3)
dfregion7_PH <- data.frame(y = region7_may9$PH, x = region7_may9$Time3)

dfregion8_PM <- data.frame(y = region8_may9$PM, x = region8_may9$Time3)
dfregion8_PR <- data.frame(y = region8_may9$PR, x = region8_may9$Time3)
dfregion8_PC <- data.frame(y = region8_may9$PC, x = region8_may9$Time3)
dfregion8_PE <- data.frame(y = region8_may9$PE, x = region8_may9$Time3)
dfregion8_PT <- data.frame(y = region8_may9$PT, x = region8_may9$Time3)
dfregion8_PH <- data.frame(y = region8_may9$PH, x = region8_may9$Time3)

dfregion9_PM <- data.frame(y = region9_may9$PM, x = region9_may9$Time3)
dfregion9_PR <- data.frame(y = region9_may9$PR, x = region9_may9$Time3)
dfregion9_PC <- data.frame(y = region9_may9$PC, x = region9_may9$Time3)
dfregion9_PE <- data.frame(y = region9_may9$PE, x = region9_may9$Time3)
dfregion9_PT <- data.frame(y = region9_may9$PT, x = region9_may9$Time3)
dfregion9_PH <- data.frame(y = region9_may9$PH, x = region9_may9$Time3)

dfregion10_PM <- data.frame(y = region10_may9$PM, x = region10_may9$Time3)
dfregion10_PR <- data.frame(y = region10_may9$PR, x = region10_may9$Time3)
dfregion10_PC <- data.frame(y = region10_may9$PC, x = region10_may9$Time3)
dfregion10_PE <- data.frame(y = region10_may9$PE, x = region10_may9$Time3)
dfregion10_PT <- data.frame(y = region10_may9$PT, x = region10_may9$Time3)
dfregion10_PH <- data.frame(y = region10_may9$PH, x = region10_may9$Time3)

dfregion11_PM <- data.frame(y = region11_may9$PM, x = region11_may9$Time3)
dfregion11_PR <- data.frame(y = region11_may9$PR, x = region11_may9$Time3)
dfregion11_PC <- data.frame(y = region11_may9$PC, x = region11_may9$Time3)
dfregion11_PE <- data.frame(y = region11_may9$PE, x = region11_may9$Time3)
dfregion11_PT <- data.frame(y = region11_may9$PT, x = region11_may9$Time3)
dfregion11_PH <- data.frame(y = region11_may9$PH, x = region11_may9$Time3)

dfregion12_PM <- data.frame(y = region12_may9$PM, x = region12_may9$Time3)
dfregion12_PR <- data.frame(y = region12_may9$PR, x = region12_may9$Time3)
dfregion12_PC <- data.frame(y = region12_may9$PC, x = region12_may9$Time3)
dfregion12_PE <- data.frame(y = region12_may9$PE, x = region12_may9$Time3)
dfregion12_PT <- data.frame(y = region12_may9$PT, x = region12_may9$Time3)
dfregion12_PH <- data.frame(y = region12_may9$PH, x = region12_may9$Time3)

dfcaraga_PM <- data.frame(y = caraga_may9$PM, x = caraga_may9$Time3)
dfcaraga_PR <- data.frame(y = caraga_may9$PR, x = caraga_may9$Time3)
dfcaraga_PC <- data.frame(y = caraga_may9$PC, x = caraga_may9$Time3)
dfcaraga_PE <- data.frame(y = caraga_may9$PE, x = caraga_may9$Time3)
dfcaraga_PT <- data.frame(y = caraga_may9$PT, x = caraga_may9$Time3)
dfcaraga_PH <- data.frame(y = caraga_may9$PH, x = caraga_may9$Time3)

dfarmm_PM <- data.frame(y = armm_may9$PM, x = armm_may9$Time3)
dfarmm_PR <- data.frame(y = armm_may9$PR, x = armm_may9$Time3)
dfarmm_PC <- data.frame(y = armm_may9$PC, x = armm_may9$Time3)
dfarmm_PE <- data.frame(y = armm_may9$PE, x = armm_may9$Time3)
dfarmm_PT <- data.frame(y = armm_may9$PT, x = armm_may9$Time3)
dfarmm_PH <- data.frame(y = armm_may9$PH, x = armm_may9$Time3)

dfoav_PM <- data.frame(y = oav_may9$PM, x = oav_may9$Time3)
dfoav_PR <- data.frame(y = oav_may9$PR, x = oav_may9$Time3)
dfoav_PC <- data.frame(y = oav_may9$PC, x = oav_may9$Time3)
dfoav_PE <- data.frame(y = oav_may9$PE, x = oav_may9$Time3)
dfoav_PT <- data.frame(y = oav_may9$PT, x = oav_may9$Time3)
dfoav_PH <- data.frame(y = oav_may9$PH, x = oav_may9$Time3)

### For Figure 2
dfphil_PM <- data.frame(y = phil_may9$PM, x = phil_may9$Time3)
dfphil_PR <- data.frame(y = phil_may9$PR, x = phil_may9$Time3)
dfphil_PC <- data.frame(y = phil_may9$PC, x = phil_may9$Time3)
dfphil_PE <- data.frame(y = phil_may9$PE, x = phil_may9$Time3)
dfphil_PT <- data.frame(y = phil_may9$PT, x = phil_may9$Time3)
dfphil_PH <- data.frame(y = phil_may9$PH, x = phil_may9$Time3)

## For Figure 4
dfall_PM <- data.frame(y = all_may9$PM, x = all_may9$growth)
dfall_PR <- data.frame(y = all_may9$PR, x = all_may9$growth)
dfall_PC <- data.frame(y = all_may9$VC, x = all_may9$growth)
dfall_PE <- data.frame(y = all_may9$VE, x = all_may9$growth)
dfall_PT <- data.frame(y = all_may9$VT, x = all_may9$growth)
dfall_PH <- data.frame(y = all_may9$VH, x = all_may9$growth)

### For Figure 3
Marcos    <- subset(trends, Candidate == "Marcos")
Robredo   <- subset(trends, Candidate == "Robredo")
Cayetano  <- subset(trends, Candidate == "Cayetano")
Escudero  <- subset(trends, Candidate == "Escudero")
Honasan   <- subset(trends, Candidate == "Honasan")
Trillanes <- subset(trends, Candidate == "Trillanes")

### Data used for Figure 3
dfVM     <- data.frame(y = Marcos$Mean/1000000, x = Marcos$Time3)
dfVR     <- data.frame(y = Robredo$Mean/1000000, x = Robredo$Time3)

dfVM_low <- data.frame(y = Marcos$LowBound/1000000,  x = Marcos$Time3)
dfVR_low <- data.frame(y = Robredo$LowBound/1000000, x = Robredo$Time3)

dfVM_high <- data.frame(y = Marcos$HighBound/1000000,  x = Marcos$Time3)
dfVR_high <- data.frame(y = Robredo$HighBound/1000000, x = Robredo$Time3)

### Data used for Figure 4
dfPM      <- data.frame(y = Marcos$MeanChange*100,    x = Marcos$Time3)
dfPR      <- data.frame(y = Robredo$MeanChange*100,   x = Robredo$Time3)
dfPC      <- data.frame(y = Cayetano$MeanChange*100,  x = Cayetano$Time3)
dfPE      <- data.frame(y = Escudero$MeanChange*100,  x = Escudero$Time3)
dfPT      <- data.frame(y = Trillanes$MeanChange*100, x =Trillanes$Time3)
dfPH      <- data.frame(y = Honasan$MeanChange*100,   x = Honasan$Time3)

### Lower Bound Confidence
dfPM_low  <- data.frame(y = Marcos$LowChange*100,     x = Marcos$Time3)
dfPR_low  <- data.frame(y = Robredo$LowChange*100,    x = Robredo$Time3)
dfPC_low  <- data.frame(y = Cayetano$LowChange*100,   x = Cayetano$Time3)
dfPE_low  <- data.frame(y = Escudero$LowChange*100,   x = Escudero$Time3)
dfPT_low  <- data.frame(y = Trillanes$LowChange*100,  x =Trillanes$Time3)
dfPH_low  <- data.frame(y = Honasan$LowChange*100,    x = Honasan$Time3)

### Upper Bound Confidence
dfPM_high <- data.frame(y = Marcos$HighChange*100,    x = Marcos$Time3)
dfPR_high <- data.frame(y = Robredo$HighChange*100,   x = Robredo$Time3)
dfPC_high <- data.frame(y = Cayetano$HighChange*100,  x = Cayetano$Time3)
dfPE_high <- data.frame(y = Escudero$HighChange*100,  x = Escudero$Time3)
dfPT_high <- data.frame(y = Trillanes$HighChange*100, x =Trillanes$Time3)
dfPH_high <- data.frame(y = Honasan$HighChange*100,   x = Honasan$Time3)



### Figure 1
plot(y ~ x, data = dfncr, col = rgb(1,0,0,0.9), pch = 16, main = "Incoming Votes Transmitted (Hours Reporting)",
     xlab = "Date and Hour Reporting", ylab = "Transmission Rate (In Percent)",
     ylim = c(0,100), type = "p", lwd = 6, xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = 1.9)
daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt(max(vprace$Time3)))
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
axis(2, at = seq(0,100,10), labels = seq(0,100,10), cex.axis = 1.5)

### Marcos Transmission of Votes
lines(y ~ x, data = dfregion1,  col = rgb(1,0,0,0.8), type="p", lwd = 6, pch = 16, cex = 1.9)
lines(y ~ x, data = dfcar,      col = rgb(1,0,0,0.7), type="p", lwd = 6, pch = 16, cex = 1.9)
lines(y ~ x, data = dfregion2,  col = rgb(1,0,0,0.6), type="p", lwd = 6, pch = 16, cex = 1.9)
lines(y ~ x, data = dfregion3,  col = rgb(1,0,0,0.5), type="p",  lwd = 6, pch = 16, cex = 1.9)
lines(y ~ x, data = dfregion8,  col =  rgb(1,0,0,0.4), type="p",  lwd = 6, pch = 16, cex = 1.9)
lines(y ~ x, data = dfregion12, col =  rgb(1,0,0,0.3), type="p",  lwd = 6, pch = 16, cex = 1.9)
lines(y ~ x, data = dfoav,      col =  rgb(1,0,0,0.2), type="p",  lwd = 6, pch = 16, cex = 1.9)

### Robredo Transmission of Votes
lines(y ~ x, data = dfregion4a, col = rgb(0,0,1,0.9), type="p",  lwd = 6, pch = 16, cex = 1.9)
lines(y ~ x, data = dfregion4b, col = rgb(0,0,1,0.8), type="p",  lwd = 6, pch = 16, cex = 1.9)
lines(y ~ x, data = dfregion5,  col = rgb(0,0,1,0.7), type="p",  lwd = 6, pch = 16, cex = 1.9)
lines(y ~ x, data = dfregion6,  col =  rgb(0,0,1,0.6), type="p",  lwd = 6, pch = 16, cex = 1.9)
lines(y ~ x, data = dfnir,      col =  rgb(0,0,1,0.5), type="p",  lwd = 6, pch = 16, cex = 1.9)
lines(y ~ x, data = dfregion7,  col =  rgb(0,0,1,0.4), type="p",  lwd = 6, pch = 16, cex = 1.9)
lines(y ~ x, data = dfregion9,  col =  rgb(0,0,1,0.3), type="p",  lwd = 6, pch = 16, cex = 1.9)
lines(y ~ x, data = dfregion10, col =  rgb(0,0,1,0.2), type="p",  lwd = 6, pch = 16, cex = 1.9)
lines(y ~ x, data = dfcaraga,   col =  rgb(0,0,1,0.15), type="p",  lwd = 6, pch = 16, cex = 1.9)
lines(y ~ x, data = dfarmm,     col =  rgb(0,0,1,0.1), type="p",  lwd = 6, pch = 16, cex = 1.9)

### Cayetano Transmission of Votes
lines(y ~ x, data = dfregion11, col =  rgb(0,0.5,0,0.9), type="p",  lwd = 6, pch = 16, cex = 1.9)

legend(locator(1), legend = c("Region 4A", "Region 4B", "Region 5", "Region 6", "NIR ", "Region 7",  "Region 9", "Region 10","Region 13", "ARMM"), 
       fill = c(rgb(0,0,1,0.9), rgb(0,0,1,0.8),rgb(0,0,1,0.7), rgb(0,0,1,0.6),rgb(0,0,1,0.5), rgb(0,0,1,0.4),rgb(0,0,1,0.3), rgb(0,0,1,0.2),rgb(0,0,1,0.15),rgb(0,0,1,0.1)), box.lty=0,cex=1.3)

legend(locator(1), legend = c("NCR","Region 1","CAR","Region 2", "Region 3", "Region 8","Region 12", "OAV"), 
                   fill = c(rgb(1,0,0,0.9),rgb(1,0,0,0.8),rgb(1,0,0,0.7),rgb(1,0,0,0.6),
                   rgb(1,0,0,0.5),rgb(1,0,0,0.4), rgb(1,0,0,0.3),rgb(1,0,0,0.2)), box.lty=0,cex=1.3)

legend(locator(1), legend = "Region 11", fill = rgb(0,0.5,0,0.9), box.lty=0,cex=1.3)


n = 1500000
### Figure 2. Philippines Total
plot(y ~ x, data = dfphil_PM , col = "darkorange", pch = 16, main = "Philippines Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,0.5), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = phil_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(0,0.5,0.1), labels = seq(0,0.5,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfphil_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = phil_may9$VR/n)
     lines(y ~ x, data = dfphil_PC, col = "red", type="o", lwd = 3, pch = 16, cex = phil_may9$VC/n)
     lines(y ~ x, data = dfphil_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = phil_may9$VE/n)
     lines(y ~ x, data = dfphil_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = phil_may9$VT/n)
     lines(y ~ x, data = dfphil_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = phil_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
             fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### Supplementary Figure 1
### NCR
plot(y ~ x, data = dfncr_PM , col = "darkorange", pch = 16, main = "Metro Manila Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = ncr_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfncr_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = ncr_may9$VR/n)
     lines(y ~ x, data = dfncr_PC, col = "red", type="o", lwd = 3, pch = 16, cex = ncr_may9$VC/n)
     lines(y ~ x, data = dfncr_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = ncr_may9$VE/n)
     lines(y ~ x, data = dfncr_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = ncr_may9$VT/n)
     lines(y ~ x, data = dfncr_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = ncr_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
           fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### Region 1
plot(y ~ x, data = dfregion1_PM , col = "darkorange", pch = 16, main = "Ilocos Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = region1_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfregion1_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = region1_may9$VR/n)
     lines(y ~ x, data = dfregion1_PC, col = "red", type="o", lwd = 3, pch = 16, cex = region1_may9$VC/n)
     lines(y ~ x, data = dfregion1_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = region1_may9$VE/n)
     lines(y ~ x, data = dfregion1_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = region1_may9$VT/n)
     lines(y ~ x, data = dfregion1_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = region1_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
             fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### CAR
plot(y ~ x, data = dfcar_PM , col = "darkorange", pch = 16, main = "Cordillera Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = car_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfcar_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = car_may9$VR/n)
     lines(y ~ x, data = dfcar_PC, col = "red", type="o", lwd = 3, pch = 16, cex = car_may9$VC/n)
     lines(y ~ x, data = dfcar_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = car_may9$VE/n)
     lines(y ~ x, data = dfcar_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = car_may9$VT/n)
     lines(y ~ x, data = dfcar_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = car_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
            fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### Region 2
plot(y ~ x, data = dfregion2_PM , col = "darkorange", pch = 16, main = "Cagayan Valley Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = region2_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfregion2_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = region2_may9$VR/n)
     lines(y ~ x, data = dfregion2_PC, col = "red", type="o", lwd = 3, pch = 16, cex = region2_may9$VC/n)
     lines(y ~ x, data = dfregion2_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = region2_may9$VE/n)
     lines(y ~ x, data = dfregion2_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = region2_may9$VT/n)
     lines(y ~ x, data = dfregion2_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = region2_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
            fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### Region 3
plot(y ~ x, data = dfregion3_PM , col = "darkorange", pch = 16, main = "Central Luzon Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = region3_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfregion3_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = region3_may9$VR/n)
     lines(y ~ x, data = dfregion3_PC, col = "red", type="o", lwd = 3, pch = 16, cex = region3_may9$VC/n)
     lines(y ~ x, data = dfregion3_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = region3_may9$VE/n)
     lines(y ~ x, data = dfregion3_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = region3_may9$VT/n)
     lines(y ~ x, data = dfregion3_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = region3_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
             fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### Region 4A
plot(y ~ x, data = dfregion4a_PM , col = "darkorange", pch = 16, main = "CALABARZON Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = region4a_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfregion4a_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = region4a_may9$VR/n)
     lines(y ~ x, data = dfregion4a_PC, col = "red", type="o", lwd = 3, pch = 16, cex = region4a_may9$VC/n)
     lines(y ~ x, data = dfregion4a_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = region4a_may9$VE/n)
     lines(y ~ x, data = dfregion4a_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = region4a_may9$VT/n)
     lines(y ~ x, data = dfregion4a_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = region4a_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
            fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### Region 4B
plot(y ~ x, data = dfregion4b_PM , col = "darkorange", pch = 16, main = "MIMAROPA Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = region4b_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfregion4b_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = region4b_may9$VR/n)
     lines(y ~ x, data = dfregion4b_PC, col = "red", type="o", lwd = 3, pch = 16, cex = region4b_may9$VC/n)
     lines(y ~ x, data = dfregion4b_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = region4b_may9$VE/n)
     lines(y ~ x, data = dfregion4b_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = region4b_may9$VT/n)
     lines(y ~ x, data = dfregion4b_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = region4b_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
             fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### Region 5
plot(y ~ x, data = dfregion5_PM , col = "darkorange", pch = 16, main = "Bicol Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = region5_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfregion5_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = region5_may9$VR/n)
     lines(y ~ x, data = dfregion5_PC, col = "red", type="o", lwd = 3, pch = 16, cex = region5_may9$VC/n)
     lines(y ~ x, data = dfregion5_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = region5_may9$VE/n)
     lines(y ~ x, data = dfregion5_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = region5_may9$VT/n)
     lines(y ~ x, data = dfregion5_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = region5_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
            fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### Region 6
plot(y ~ x, data = dfregion6_PM , col = "darkorange", pch = 16, main = "Western Visayas Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = region6_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfregion6_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = region6_may9$VR/n)
     lines(y ~ x, data = dfregion6_PC, col = "red", type="o", lwd = 3, pch = 16, cex = region6_may9$VC/n)
     lines(y ~ x, data = dfregion6_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = region6_may9$VE/n)
     lines(y ~ x, data = dfregion6_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = region6_may9$VT/n)
     lines(y ~ x, data = dfregion6_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = region6_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
           fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### Supplementary Figure 2
### NIR
plot(y ~ x, data = dfnir_PM , col = "darkorange", pch = 16, main = "Negros Island Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = nir_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfnir_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = nir_may9$VR/n)
     lines(y ~ x, data = dfnir_PC, col = "red", type="o", lwd = 3, pch = 16, cex = nir_may9$VC/n)
     lines(y ~ x, data = dfnir_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = nir_may9$VE/n)
     lines(y ~ x, data = dfnir_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = nir_may9$VT/n)
     lines(y ~ x, data = dfnir_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = nir_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
            fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)     
     
### Region 7
plot(y ~ x, data = dfregion7_PM , col = "darkorange", pch = 16, main = "Central Visayas Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = region7_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
      axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfregion7_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = region7_may9$VR/n)
     lines(y ~ x, data = dfregion7_PC, col = "red", type="o", lwd = 3, pch = 16, cex = region7_may9$VC/n)
     lines(y ~ x, data = dfregion7_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = region7_may9$VE/n)
     lines(y ~ x, data = dfregion7_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = region7_may9$VT/n)
     lines(y ~ x, data = dfregion7_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = region7_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
           fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### Region 8
plot(y ~ x, data = dfregion8_PM , col = "darkorange", pch = 16, main = "Eastern Visayas Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = region8_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfregion8_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = region8_may9$VR/n)
     lines(y ~ x, data = dfregion8_PC, col = "red", type="o", lwd = 3, pch = 16, cex = region8_may9$VC/n)
     lines(y ~ x, data = dfregion8_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = region8_may9$VE/n)
     lines(y ~ x, data = dfregion8_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = region8_may9$VT/n)
     lines(y ~ x, data = dfregion8_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = region8_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
           fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### Region 9
plot(y ~ x, data = dfregion9_PM , col = "darkorange", pch = 16, main = "Northern Mindanao Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = region9_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfregion9_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = region9_may9$VR/n)
     lines(y ~ x, data = dfregion9_PC, col = "red", type="o", lwd = 3, pch = 16, cex = region9_may9$VC/n)
     lines(y ~ x, data = dfregion9_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = region9_may9$VE/n)
     lines(y ~ x, data = dfregion9_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = region9_may9$VT/n)
     lines(y ~ x, data = dfregion9_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = region9_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
           fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### Region 10
plot(y ~ x, data = dfregion10_PM , col = "darkorange", pch = 16, main = "Northern Mindanao Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = region10_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfregion10_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = region10_may9$VR/n)
     lines(y ~ x, data = dfregion10_PC, col = "red", type="o", lwd = 3, pch = 16, cex = region10_may9$VC/n)
     lines(y ~ x, data = dfregion10_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = region10_may9$VE/n)
     lines(y ~ x, data = dfregion10_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = region10_may9$VT/n)
     lines(y ~ x, data = dfregion10_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = region10_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
           fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### Region 11
plot(y ~ x, data = dfregion11_PM , col = "darkorange", pch = 16, main = "Davao Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = region11_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfregion11_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = region11_may9$VR/n)
     lines(y ~ x, data = dfregion11_PC, col = "red", type="o", lwd = 3, pch = 16, cex = region11_may9$VC/n)
     lines(y ~ x, data = dfregion11_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = region11_may9$VE/n)
     lines(y ~ x, data = dfregion11_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = region11_may9$VT/n)
     lines(y ~ x, data = dfregion11_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = region11_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
           fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### Region 12
plot(y ~ x, data = dfregion12_PM , col = "darkorange", pch = 16, main = "SOCCSKSARGEN Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = region12_may9$VM/n)
      daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
      axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
      axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
      lines(y ~ x, data = dfregion12_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = region12_may9$VR/n)
      lines(y ~ x, data = dfregion12_PC, col = "red", type="o", lwd = 3, pch = 16, cex = region12_may9$VC/n)
      lines(y ~ x, data = dfregion12_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = region12_may9$VE/n)
      lines(y ~ x, data = dfregion12_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = region12_may9$VT/n)
      lines(y ~ x, data = dfregion12_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = region12_may9$VH/n)
      legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
             fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### CARAGA
plot(y ~ x, data = dfcaraga_PM , col = "darkorange", pch = 16, main = "Caraga Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = caraga_may9$VM/n)
     daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
     lines(y ~ x, data = dfcaraga_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = caraga_may9$VR/n)
     lines(y ~ x, data = dfcaraga_PC, col = "red", type="o", lwd = 3, pch = 16, cex = caraga_may9$VC/n)
     lines(y ~ x, data = dfcaraga_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = caraga_may9$VE/n)
     lines(y ~ x, data = dfcaraga_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = caraga_may9$VT/n)
     lines(y ~ x, data = dfcaraga_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = caraga_may9$VH/n)
     legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
           fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)

### ARMM
plot(y ~ x, data = dfarmm_PM , col = "darkorange", pch = 16, main = "ARMM Votes Transmitted",
     xlab = "Date and Hour Reporting", ylab = "Share of Votes per Candidate (In Percent)",
     ylim = c(0,1), type = "o", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = armm_may9$VM/n)
    daterange=c(as.POSIXlt(min(vprace$Time3)), as.POSIXlt("2016-05-09 23:48:00 CEST"))
    axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
    axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), cex.axis = 1.5)
    lines(y ~ x, data = dfarmm_PR, col = "gold", type="o", lwd = 3, pch = 16, cex = armm_may9$VR/n)
    lines(y ~ x, data = dfarmm_PC, col = "red", type="o", lwd = 3, pch = 16, cex = armm_may9$VC/n)
    lines(y ~ x, data = dfarmm_PE, col = "lightblue", type="o", lwd = 3, pch = 16, cex = armm_may9$VE/n)
    lines(y ~ x, data = dfarmm_PT, col = "grey", type="o", lwd = 3, pch = 16, cex = armm_may9$VT/n)
    lines(y ~ x, data = dfarmm_PH, col = "lightgrey", type="o", lwd = 3, pch = 16, cex = armm_may9$VH/n)
    legend(locator(1), legend = c("Robredo", "Marcos", "Cayetano", "Escudero", "Trillanes ", "Honasan"), 
           fill = c("gold","darkorange","red","lightblue","grey","lightgrey"), box.lty=0,cex=1.5)



### Plot Figure 3. Robredo and Marcos Projected Votes from 6:45 PM, May 9 to Midnight of May 10
plot(y ~ x, data = dfVR , col = "gold", pch = 16, main = "Robredo was already leading right before midnight of May 10",
     xlab = "Date and Hour Reporting", ylab = "Projected Total Votes (in millions)",
     ylim = c(13.5,14.9), type = "p", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = 3)
     daterange=c(as.POSIXlt(min(trends$Time3)), as.POSIXlt(max(trends$Time3)))
     axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
     axis(2, at = seq(13.5,14.9,0.1), labels = seq(13.5,14.9,0.1), cex.axis = 1.5)
     
     lines(y ~ x, data = dfVM, col = "darkorange", type="p", lwd = 3, pch = 16, cex = 3)
     lines(y ~ x, data = dfVR_low, col = "gold", type="l", lty = 3, lwd = 6, pch = 16, cex = 2)
     lines(y ~ x, data = dfVR_high, col = "gold", type="l", lty = 3, lwd = 6, pch = 16, cex = 2)
     lines(y ~ x, data = dfVM_low, col = "darkorange", type="l", lty = 3, lwd = 6, pch = 16, cex = 2)
     lines(y ~ x, data = dfVM_high, col = "darkorange", type="l", lty = 3, lwd = 6, pch = 16, cex = 2)

     legend(locator(1), legend = c("Robredo", "Marcos"), fill = c("gold","darkorange"), box.lty=0,cex=1.9)
     legend(locator(1), legend = c("Confidence Interval",""), col = c("gold","darkorange"), lty = 3, lwd = 6, box.lty=0,cex=1.9)

### Plot (Supplementary) Figure 4. Changes in Projected Votes
plot(y ~ x, data = dfPR , col = "gold", pch = 16, main = "There is no proof of anomalies in transmissions",
     xlab = "Date and Hour Reporting", ylab = "Projected Vote Change (in Percent)",
     ylim = c(-1, 1), type = "p", lwd = "3", xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.8, cex = 3)
    daterange=c(as.POSIXlt(min(trends$Time3)), as.POSIXlt(max(trends$Time3)))
    axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="hour"), format="%d-%H", cex.axis = 1.5)
    axis(2, at = seq(-1, 1, 0.05), labels = seq(-1, 1, 0.05), cex.axis = 1.5)
    
    lines(y ~ x, data = dfPM, col = "darkorange", type="p", lwd = 3, pch = 16, cex = 3)
    lines(y ~ x, data = dfPC, col = "red", type="p", lwd = 3, pch = 16, cex = 3)
    lines(y ~ x, data = dfPE, col = "lightblue", type="p", lwd = 3, pch = 16, cex = 3)
    lines(y ~ x, data = dfPT, col = "darkgrey", type="p", lwd = 3, pch = 16, cex = 3)
    lines(y ~ x, data = dfPH, col = "lightgrey", type="p", lwd = 3, pch = 16, cex = 3)
    
    lines(y ~ x, data = dfPR_low, col = "gold", type="l", lty = 3, lwd = 6, pch = 16, cex = 2)
    lines(y ~ x, data = dfPR_high, col = "gold", type="l", lty = 3, lwd = 6, pch = 16, cex = 2)
    lines(y ~ x, data = dfPM_low, col = "darkorange", type="l", lty = 3, lwd = 6, pch = 16, cex = 2)
    lines(y ~ x, data = dfPM_high, col = "darkorange", type="l", lty = 3, lwd = 6, pch = 16, cex = 2)
    
    lines(y ~ x, data = dfPC_low, col = "red", type="l", lty = 3, lwd = 6, pch = 16, cex = 2)
    lines(y ~ x, data = dfPC_high, col = "red", type="l", lty = 3, lwd = 6, pch = 16, cex = 2)
    lines(y ~ x, data = dfPE_low, col = "lightblue", type="l", lty = 3, lwd = 6, pch = 16, cex = 2)
    lines(y ~ x, data = dfPE_high, col = "lightblue", type="l", lty = 3, lwd = 6, pch = 16, cex = 2)
    
    lines(y ~ x, data = dfPT_low, col = "darkgrey", type="l", lty = 3, lwd = 6, pch = 16, cex = 2)
    lines(y ~ x, data = dfPT_high, col = "darkgrey", type="l", lty = 3, lwd = 6, pch = 16, cex = 2)
    lines(y ~ x, data = dfPH_low, col = "lightgrey", type="l", lty = 3, lwd = 6, pch = 16, cex = 2)
    lines(y ~ x, data = dfPH_high, col = "lightgrey", type="l", lty = 3, lwd = 6, pch = 16, cex = 2)

    legend(locator(1), legend = c("Robredo", "Marcos","Cayetano","Escudero","Trillanes","Honasan"), 
                        fill = c("gold","darkorange", "red","lightblue","darkgrey","lightgrey"), box.lty=0,cex=1.8)
    legend(locator(1), legend = c("","","Confidence Interval","","",""), col = c("gold","darkorange", "red","lightblue","darkgrey","lightgrey"), lty = 3, lwd = 6, box.lty=0,cex=1.8)
