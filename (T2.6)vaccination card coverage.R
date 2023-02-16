#for all the states

data_imm_country=data[c("V024","V102","V106","V190","H1", "H2", "H3", "H5", "H7", "H9")]


#remove NA field
data_imm_country= data_imm_country[complete.cases(data_imm_country), ]

#children less than 9 yrs
subset_yesseen = subset(data_imm_country, data_imm_country$H1 == "Yes, seen")
sum(subset_yesseen$V190)/nrow(subset_yesseen)
#length(which(subset_yesseen$V190 == "No education"))/nrow(subset_yesseen)
#subset_yesseen = subset(subset_yesseen, subset_yesseen$V102 == "Urban")

# clean the matrix

subset_yesseen$H9 <- as.character(subset_yesseen$H9)
subset_yesseen$H9[subset_yesseen$H9 %in% "Vaccination date on card"] <- 0
subset_yesseen$H9[subset_yesseen$H9 %in% "Reported by mother"] <- 1
subset_yesseen$H9[subset_yesseen$H9 %in% "Vaccination marked on card"] <- 0
subset_yesseen$H9[subset_yesseen$H9 %in% "No"] <- 0
subset_yesseen$H9[subset_yesseen$H9 %in% "Don't know"] <- 0
subset_yesseen$H9 <- factor(subset_yesseen$H9)

subset_yesseen$H7 <- as.character(subset_yesseen$H7)
subset_yesseen$H7[subset_yesseen$H7 %in% "Vaccination date on card"] <- 0
subset_yesseen$H7[subset_yesseen$H7 %in% "Reported by mother"] <- 1
subset_yesseen$H7[subset_yesseen$H7 %in% "Vaccination marked on card"] <- 0
subset_yesseen$H7[subset_yesseen$H7 %in% "No"] <- 0
subset_yesseen$H7[subset_yesseen$H7 %in% "Don't know"] <- 0
subset_yesseen$H7 <- factor(subset_yesseen$H7)

subset_yesseen$H5 <- as.character(subset_yesseen$H5)
subset_yesseen$H5[subset_yesseen$H5 %in% "Vaccination date on card"] <- 0
subset_yesseen$H5[subset_yesseen$H5 %in% "Reported by mother"] <- 1
subset_yesseen$H5[subset_yesseen$H5 %in% "Vaccination marked on card"] <- 0
subset_yesseen$H5[subset_yesseen$H5 %in% "No"] <- 0
subset_yesseen$H5[subset_yesseen$H5 %in% "Don't know"] <- 0
subset_yesseen$H5 <- factor(subset_yesseen$H5)

subset_yesseen$H3 <- as.character(subset_yesseen$H3)
subset_yesseen$H3[subset_yesseen$H3 %in% "Vaccination date on card"] <- 0
subset_yesseen$H3[subset_yesseen$H3 %in% "Reported by mother"] <- 1
subset_yesseen$H3[subset_yesseen$H3 %in% "Vaccination marked on card"] <- 0
subset_yesseen$H3[subset_yesseen$H3 %in% "No"] <- 0
subset_yesseen$H3[subset_yesseen$H3 %in% "Don't know"] <- 0
subset_yesseen$H3 <- factor(subset_yesseen$H3)

subset_yesseen$H2 <- as.character(subset_yesseen$H2)
subset_yesseen$H2[subset_yesseen$H2 %in% "Vaccination date on card"] <- 0
subset_yesseen$H2[subset_yesseen$H2 %in% "Reported by mother"] <- 1
subset_yesseen$H2[subset_yesseen$H2 %in% "Vaccination marked on card"] <- 0
subset_yesseen$H2[subset_yesseen$H2 %in% "No"] <- 0
subset_yesseen$H2[subset_yesseen$H2 %in% "Don't know"] <- 0
subset_yesseen$H2 <- factor(subset_yesseen$H2)

subset_yesseen$V102 <- as.character(subset_yesseen$V102)
subset_yesseen$V102[subset_yesseen$V102 %in% "Rural"] <- 0
subset_yesseen$V102[subset_yesseen$V102 %in% "Urban"] <- 1
subset_yesseen$V102 <- factor(subset_yesseen$V102)

subset_yesseen$V106 <- as.character(subset_yesseen$V106)
subset_yesseen$V106[subset_yesseen$V106 %in% "No education"] <- 0
subset_yesseen$V106[subset_yesseen$V106 %in% "Primary"] <- 0.34
subset_yesseen$V106[subset_yesseen$V106 %in% "Secondary"] <- 0.67
subset_yesseen$V106[subset_yesseen$V106 %in% "Higher"] <- 1
subset_yesseen$V106 <- factor(subset_yesseen$V106)

subset_yesseen$V190 <- as.character(subset_yesseen$V190)
subset_yesseen$V190[subset_yesseen$V190 %in% "Poorest"] <- 0
subset_yesseen$V190[subset_yesseen$V190 %in% "Poorer"] <- 0.25
subset_yesseen$V190[subset_yesseen$V190 %in% "Middle"] <- 0.5
subset_yesseen$V190[subset_yesseen$V190 %in% "Richer"] <- 0.75
subset_yesseen$V190[subset_yesseen$V190 %in% "Richest"] <- 1
subset_yesseen$V190 <- factor(subset_yesseen$V190)

subset_yesseen[is.na(subset_yesseen)] <- 0 # replace all NA with 0

subset_yesseen$V190 <- as.numeric(as.character( subset_yesseen$V190 ))

subset_yesseen$H2 <- as.numeric(as.character( subset_yesseen$H2 ))
subset_yesseen$H3 <- as.numeric(as.character( subset_yesseen$H3 ))
subset_yesseen$H5 <- as.numeric(as.character( subset_yesseen$H5 ))
subset_yesseen$H7 <- as.numeric(as.character( subset_yesseen$H7 ))
subset_yesseen$H9 <- as.numeric(as.character( subset_yesseen$H9 ))
subset_yesseen$V102 <- as.numeric(as.character( subset_yesseen$V102 ))
subset_yesseen$V106 <- as.numeric(as.character( subset_yesseen$V106 ))


temp_length <- aggregate(subset_yesseen$H2, by=list(V024=subset_yesseen$V024), FUN =length)

#temp_sums are considering mother 

temp_sums <- aggregate(cbind(subset_yesseen$H2,subset_yesseen$H3,subset_yesseen$H5,subset_yesseen$H7,subset_yesseen$H9,subset_yesseen$V102,subset_yesseen$V106,subset_yesseen$V190), by=list(V024=subset_yesseen$V024), FUN=sum)

temp_sums$V1 <- (temp_sums$V1/temp_length$x)*100
temp_sums$V2 <- (temp_sums$V2/temp_length$x)*100
temp_sums$V3 <- (temp_sums$V3/temp_length$x)*100
temp_sums$V4 <- (temp_sums$V4/temp_length$x)*100
temp_sums$V5 <- (temp_sums$V5/temp_length$x)*100
temp_sums$V6 <- (temp_sums$V6/temp_length$x)*100
temp_sums$V7 <- (temp_sums$V7/temp_length$x)*100

temp_sums$V8 <- (temp_sums$V7/temp_length$x)*100

#write.csv(temp_sums,"temp.csv")

#temp_sums_2 are without considering

temp_sums_2 <- aggregate(cbind(subset_yesseen$H2,subset_yesseen$H3,subset_yesseen$H5,subset_yesseen$H7,subset_yesseen$H9), by=list(V024=subset_yesseen$V024), FUN=sum)

temp_sums_2$V1 <- (temp_sums_2$V1/temp_length$x)*100
temp_sums_2$V2 <- (temp_sums_2$V2/temp_length$x)*100
temp_sums_2$V3 <- (temp_sums_2$V3/temp_length$x)*100
temp_sums_2$V4 <- (temp_sums_2$V4/temp_length$x)*100
temp_sums_2$V5 <- (temp_sums_2$V5/temp_length$x)*100