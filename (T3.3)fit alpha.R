###################
# This code containsfinding and plotting correlation between NFHS and WHO dataset.
# It can also be used to fit an alpha (as decribed in thesis).

#####################

####################################
#get state coverage
GetStateCoverage = function(data, state, vaccine){
  # GetStateCoverage:
  #   Get immunisation records for all individuals in a state and their district
  #
  # Args:
  #   data:     DHS data 
  #   state:    state
  #   vaccine:  vaccine(s) to select
  #
  # Returns:
  #  data_state: vaccine(s) for state with district tags.
  
  # get vaccines specified and districts ("SDISTRI)
  data_state = data[data$V024 == state, c("V024", "SDISTRI", vaccine)]
  return(data_state)
}
vaccination_list = c("H2", "H3", "H5", "H7", "H9")  # vaccines to choose

#function call for every state
nfhs4 = GetStateCoverage(data, "Tamil Nadu", vaccination_list)

###################################
x=1 #can be used as alpha to predict maternal recall. Can vary between 0-1.
# clean the matrix

nfhs4$H9 <- as.character(nfhs4$H9)
nfhs4$H9[nfhs4$H9 %in% "Vaccination date on card"] <- 1
nfhs4$H9[nfhs4$H9 %in% "Reported by mother"] <- x
nfhs4$H9[nfhs4$H9 %in% "Vaccination marked on card"] <- 1
nfhs4$H9[nfhs4$H9 %in% "No"] <- 0
nfhs4$H9[nfhs4$H9 %in% "Don't know"] <- 0
nfhs4$H9 <- factor(nfhs4$H9)

nfhs4$H7 <- as.character(nfhs4$H7)
nfhs4$H7[nfhs4$H7 %in% "Vaccination date on card"] <- 1
nfhs4$H7[nfhs4$H7 %in% "Reported by mother"] <- x
nfhs4$H7[nfhs4$H7 %in% "Vaccination marked on card"] <- 1
nfhs4$H7[nfhs4$H7 %in% "No"] <- 0
nfhs4$H7[nfhs4$H7 %in% "Don't know"] <- 0
nfhs4$H7 <- factor(nfhs4$H7)

nfhs4$H5 <- as.character(nfhs4$H5)
nfhs4$H5[nfhs4$H5 %in% "Vaccination date on card"] <- 1
nfhs4$H5[nfhs4$H5 %in% "Reported by mother"] <- x
nfhs4$H5[nfhs4$H5 %in% "Vaccination marked on card"] <- 1
nfhs4$H5[nfhs4$H5 %in% "No"] <- 0
nfhs4$H5[nfhs4$H5 %in% "Don't know"] <- 0
nfhs4$H5 <- factor(nfhs4$H5)

nfhs4$H3 <- as.character(nfhs4$H3)
nfhs4$H3[nfhs4$H3 %in% "Vaccination date on card"] <- 1
nfhs4$H3[nfhs4$H3 %in% "Reported by mother"] <- x
nfhs4$H3[nfhs4$H3 %in% "Vaccination marked on card"] <- 1
nfhs4$H3[nfhs4$H3 %in% "No"] <- 0
nfhs4$H3[nfhs4$H3 %in% "Don't know"] <- 0
nfhs4$H3 <- factor(nfhs4$H3)

nfhs4$H2 <- as.character(nfhs4$H2)
nfhs4$H2[nfhs4$H2 %in% "Vaccination date on card"] <- 1
nfhs4$H2[nfhs4$H2 %in% "Reported by mother"] <- x
nfhs4$H2[nfhs4$H2 %in% "Vaccination marked on card"] <- 1
nfhs4$H2[nfhs4$H2 %in% "No"] <- 0
nfhs4$H2[nfhs4$H2 %in% "Don't know"] <- 0
nfhs4$H2 <- factor(nfhs4$H2)

nfhs4<-na.omit(nfhs4) # replace all NA with 0

nfhs4$H2 <- as.numeric(as.character( nfhs4$H2 ))
nfhs4$H3 <- as.numeric(as.character( nfhs4$H3 ))
nfhs4$H5 <- as.numeric(as.character( nfhs4$H5 ))
nfhs4$H7 <- as.numeric(as.character( nfhs4$H7 ))
nfhs4$H9 <- as.numeric(as.character( nfhs4$H9 ))

#temp_length <- aggregate(nfhs4$H2, by=list(V024=nfhs4$V024), FUN =length)
temp_length <- aggregate(nfhs4$H2, by=list(SDISTRI=nfhs4$SDISTRI), FUN =length)


#nfhs4 are considering mother 

#nfhs4 <- aggregate(cbind(nfhs4$H2,nfhs4$H3,nfhs4$H5,nfhs4$H7,nfhs4$H9), by=list(V024=nfhs4$V024), FUN=sum)
nfhs4 <- aggregate(cbind(nfhs4$H2,nfhs4$H3,nfhs4$H5,nfhs4$H7,nfhs4$H9), by=list(SDISTRI=nfhs4$SDISTRI), FUN=sum)


nfhs4$V1 <- (nfhs4$V1/temp_length$x)*100
nfhs4$V2 <- (nfhs4$V2/temp_length$x)*100
nfhs4$V3 <- (nfhs4$V3/temp_length$x)*100
nfhs4$V4 <- (nfhs4$V4/temp_length$x)*100
nfhs4$V5 <- (nfhs4$V5/temp_length$x)*100
nfhs4 <- nfhs4[order(nfhs4$SDISTRI),]
#########################
#get states of WHO

# Loading
library("readxl")
# xlsx files
who <- read_excel("who_2016_india_subnational.xlsx")

who <- na.omit(who)
who<-who[(who$Admin1=="_Tamil Nadu"),]
who_bcg = who[(who$`Vaccine Type`=="BCG"),]
who_dtp1 = who[(who$`Vaccine Type`=="DTP1"),]
who_dtp3 = who[(who$`Vaccine Type`=="DTP3"),]
who_measles = who[(who$`Vaccine Type`=="MCV1"),]
who<-data.frame(DIST=who_bcg$Admin2,bcg=who_bcg$Coverage,
                dtp1=who_dtp1$Coverage,dtp3=who_dtp3$Coverage,
                measles=who_measles$Coverage)
who <- na.omit(who)
#merge with nfhs
dat<-merge(nfhs4,who,by.x='SDISTRI',by.y='DIST',fill=-1)

#######################
#data plot
nfhs4_val <- dat$V1
who_val <- dat$bcg
mydata<-data.frame(nfhs4_val,who_val)
plot(mydata, type = "p") #plot
#R2 LM
a11=summary(linear_model <- lm(who_val ~ nfhs4_val,mydata))$r.squared
c11=cor.test(nfhs4_val,who_val,method="pearson")$estimate
c12=cor.test(nfhs4_val,who_val,method="pearson")$p.value
abline(coef(linear_model), col="red")

#data plot
nfhs4_val <- dat$V2
who_val <- dat$dtp1
mydata<-data.frame(nfhs4_val,who_val)
plot(mydata, type = "p") #plot

#R2 LM
a12=summary(linear_model <- lm(who_val ~ nfhs4_val,mydata))$r.squared
c21=cor.test(nfhs4_val,who_val,method="pearson")$estimate
c22=cor.test(nfhs4_val,who_val,method="pearson")$p.value
abline(coef(linear_model), col="red")


#data plot
nfhs4_val <- dat$V4
who_val <- dat$dtp3
mydata<-data.frame(nfhs4_val,who_val)
plot(mydata, type = "p") #plot

#R2 LM
a13=summary(linear_model <- lm(who_val ~ nfhs4_val,mydata))$r.squared
c31=cor.test(nfhs4_val,who_val,method="pearson")$estimate
c32=cor.test(nfhs4_val,who_val,method="pearson")$p.value
abline(coef(linear_model), col="red")

#data plot
nfhs4_val <- dat$V5
who_val <- dat$measles
mydata<-data.frame(nfhs4_val,who_val)
plot(mydata, type = "p") #plot

a14=summary(linear_model <- lm(who_val ~ nfhs4_val,mydata))$r.squared
c41=cor.test(nfhs4_val,who_val,method="pearson")$estimate
c42=cor.test(nfhs4_val,who_val,method="pearson")$p.value
abline(coef(linear_model), col="red")

values <- rbind(values, c(x,c11, c12, c21, c22, c31, c32, c41, c42))

#write.csv(values,"pvaluesTN.csv")
