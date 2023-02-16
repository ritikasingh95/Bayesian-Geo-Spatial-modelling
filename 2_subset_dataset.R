#2: subset required variables and factor them between 0 to 1

#selected cluster number, age of child, measles vaccination, highest education level, Urban/rural, wealth index
nigdata_subset= data.frame(nigdata$V001,nigdata$HW1,nigdata$H9,nigdata$V106,nigdata$V025,nigdata$V190)

#remove NA field
nigdata_subset= nigdata_subset[complete.cases(nigdata_subset), ]

subset_lessthan59 = subset(nigdata_subset, nigdata_subset$nigdata.HW1 <= 59)
#subset_lessthan59 = subset(subset_lessthan59, subset_lessthan59$nigdata.HW1 >=24)

# clean the dataframe
subset_lessthan59$nigdata.H9 <- as.character(subset_lessthan59$nigdata.H9)
subset_lessthan59$nigdata.H9[subset_lessthan59$nigdata.H9 %in% "Vaccination date on card"] <- 1
subset_lessthan59$nigdata.H9[subset_lessthan59$nigdata.H9 %in% "Reported by mother"] <- 1
subset_lessthan59$nigdata.H9[subset_lessthan59$nigdata.H9 %in% "Vaccination marked on card"] <- 1
subset_lessthan59$nigdata.H9[subset_lessthan59$nigdata.H9 %in% "No"] <- 0
subset_lessthan59$nigdata.H9[subset_lessthan59$nigdata.H9 %in% "Don't know"] <- 0
subset_lessthan59$nigdata.H9 <- factor(subset_lessthan59$nigdata.H9)
subset_lessthan59$nigdata.H9 <- as.numeric(as.character( subset_lessthan59$nigdata.H9 ))

subset_lessthan59$nigdata.V106 <- as.character(subset_lessthan59$nigdata.V106)
subset_lessthan59$nigdata.V106[subset_lessthan59$nigdata.V106 %in% "No education"] <- 0
subset_lessthan59$nigdata.V106[subset_lessthan59$nigdata.V106 %in% "Primary"] <- 0.34
subset_lessthan59$nigdata.V106[subset_lessthan59$nigdata.V106 %in% "Secondary"] <- 0.67
subset_lessthan59$nigdata.V106[subset_lessthan59$nigdata.V106 %in% "Higher"] <- 1
subset_lessthan59$nigdata.V106[subset_lessthan59$nigdata.V106 %in% "Missing"] <- 0
subset_lessthan59$nigdata.V106 <- factor(subset_lessthan59$nigdata.V106)
subset_lessthan59$nigdata.V106 <- as.numeric(as.character( subset_lessthan59$nigdata.V106 ))

subset_lessthan59$nigdata.V025 <- as.character(subset_lessthan59$nigdata.V025)
subset_lessthan59$nigdata.V025[subset_lessthan59$nigdata.V025 %in% "Rural"] <- 0
subset_lessthan59$nigdata.V025[subset_lessthan59$nigdata.V025 %in% "Urban"] <- 1
subset_lessthan59$nigdata.V025 <- factor(subset_lessthan59$nigdata.V025)
subset_lessthan59$nigdata.V025 <- as.numeric(as.character( subset_lessthan59$nigdata.V025 ))

subset_lessthan59$nigdata.V190 <- as.character(subset_lessthan59$nigdata.V190)
subset_lessthan59$nigdata.V190[subset_lessthan59$nigdata.V190 %in% "Poorest"] <- 0
subset_lessthan59$nigdata.V190[subset_lessthan59$nigdata.V190 %in% "Poorer"] <- 0.25
subset_lessthan59$nigdata.V190[subset_lessthan59$nigdata.V190 %in% "Middle"] <- 0.5
subset_lessthan59$nigdata.V190[subset_lessthan59$nigdata.V190 %in% "Richer"] <- 0.75
subset_lessthan59$nigdata.V190[subset_lessthan59$nigdata.V190 %in% "Richest"] <- 1
subset_lessthan59$nigdata.V190 <- factor(subset_lessthan59$nigdata.V190)
subset_lessthan59$nigdata.V190 <- as.numeric(as.character( subset_lessthan59$nigdata.V190 ))
