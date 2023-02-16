rm(list=ls())
#getwd()
setwd("C:/Users/HP/Desktop/DHS DATASET")  # set your working dir
data = foreign::read.spss(paste("C:/Users/HP/Desktop/DHS DATASET/KR/IAKR74SV/IAKR74FL.SAV", sep = ""), 
                          to.data.frame = T)  # read data

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

# h2: received BCG
# h3: received DTP1
# h5: received DTP2:
# h7: received DTP3
# h9: received MEASLES
vaccination_list = c("H2", "H3", "H5", "H7", "H9")  # vaccines to choose
data_imm_state = GetStateCoverage(data, "Uttar Pradesh", vaccination_list)

# clean the matrix

data_imm_state$H9 <- as.character(data_imm_state$H9)
data_imm_state$H9[data_imm_state$H9 %in% "Vaccination date on card"] <- 1
data_imm_state$H9[data_imm_state$H9 %in% "Reported by mother"] <- 1
data_imm_state$H9[data_imm_state$H9 %in% "Vaccination marked on card"] <- 1
data_imm_state$H9[data_imm_state$H9 %in% "No"] <- 0
data_imm_state$H9[data_imm_state$H9 %in% "Don't know"] <- 0
data_imm_state$H9 <- factor(data_imm_state$H9)

data_imm_state$H7 <- as.character(data_imm_state$H7)
data_imm_state$H7[data_imm_state$H7 %in% "Vaccination date on card"] <- 1
data_imm_state$H7[data_imm_state$H7 %in% "Reported by mother"] <- 1
data_imm_state$H7[data_imm_state$H7 %in% "Vaccination marked on card"] <- 1
data_imm_state$H7[data_imm_state$H7 %in% "No"] <- 0
data_imm_state$H7[data_imm_state$H7 %in% "Don't know"] <- 0
data_imm_state$H7 <- factor(data_imm_state$H7)

data_imm_state$H5 <- as.character(data_imm_state$H5)
data_imm_state$H5[data_imm_state$H5 %in% "Vaccination date on card"] <- 1
data_imm_state$H5[data_imm_state$H5 %in% "Reported by mother"] <- 1
data_imm_state$H5[data_imm_state$H5 %in% "Vaccination marked on card"] <- 1
data_imm_state$H5[data_imm_state$H5 %in% "No"] <- 0
data_imm_state$H5[data_imm_state$H5 %in% "Don't know"] <- 0
data_imm_state$H5 <- factor(data_imm_state$H5)

data_imm_state$H3 <- as.character(data_imm_state$H3)
data_imm_state$H3[data_imm_state$H3 %in% "Vaccination date on card"] <- 1
data_imm_state$H3[data_imm_state$H3 %in% "Reported by mother"] <- 1
data_imm_state$H3[data_imm_state$H3 %in% "Vaccination marked on card"] <- 1
data_imm_state$H3[data_imm_state$H3 %in% "No"] <- 0
data_imm_state$H3[data_imm_state$H3 %in% "Don't know"] <- 0
data_imm_state$H3 <- factor(data_imm_state$H3)

data_imm_state$H2 <- as.character(data_imm_state$H2)
data_imm_state$H2[data_imm_state$H2 %in% "Vaccination date on card"] <- 1
data_imm_state$H2[data_imm_state$H2 %in% "Reported by mother"] <- 1
data_imm_state$H2[data_imm_state$H2 %in% "Vaccination marked on card"] <- 1
data_imm_state$H2[data_imm_state$H2 %in% "No"] <- 0
data_imm_state$H2[data_imm_state$H2 %in% "Don't know"] <- 0
data_imm_state$H2 <- factor(data_imm_state$H2)

data_imm_state[is.na(data_imm_state)] <- 0 # replace all NA with 0

data_imm_state$H2 <- as.numeric(as.character( data_imm_state$H2 ))
data_imm_state$H3 <- as.numeric(as.character( data_imm_state$H3 ))
data_imm_state$H5 <- as.numeric(as.character( data_imm_state$H5 ))
data_imm_state$H7 <- as.numeric(as.character( data_imm_state$H7 ))
data_imm_state$H9 <- as.numeric(as.character( data_imm_state$H9 ))

temp_length <- aggregate(data_imm_state$H2, by=list(SDISTRI=data_imm_state$SDISTRI), FUN =length)
temp_sums <- aggregate(cbind(data_imm_state$H2,data_imm_state$H3,data_imm_state$H5,data_imm_state$H7,data_imm_state$H9), by=list(SDISTRI=data_imm_state$SDISTRI), FUN=sum)

temp_sums$V1 <- (temp_sums$V1/temp_length$x)*100
temp_sums$V2 <- (temp_sums$V2/temp_length$x)*100
temp_sums$V3 <- (temp_sums$V3/temp_length$x)*100
temp_sums$V4 <- (temp_sums$V4/temp_length$x)*100
temp_sums$V5 <- (temp_sums$V5/temp_length$x)*100

##########################################################################

install.packages("readxl")
# Loading
library("readxl")
# xlsx files
who_data <- read_excel("who_2016_india_subnational.xlsx")

temp_who_table <- who_data[who_data$Admin1 == "_Uttar Pradesh",]

#select specific vaccine
temp_who_table <- temp_who_table[temp_who_table$`Vaccine Type` == "BCG",]

#############################################################################

rch_data <- read_excel("rch_uttarpradesh_edited.xlsx")

###########################################################################

#plotting

#RCH VS WHO (UP and BCG)

RCH <- rch_data$`% Newborns given BCG to Reported live birth`

WHO <- temp_who_table$Coverage

library("MASS")
abc <- ginv(t(RCH) %*% RCH)
abc <- abc %*% t(RCH)
abc <- abc %*% WHO

Y <- abc %*% RCH

#calculation of R squared
cor(WHO,t(Y))^2

plot(RCH, (WHO), type = "p") #plot
lines(RCH,(Y))

#DHS VS WHO (UP and BCG)

temp_who_table$dhs_coverage <- rep(0,nrow(temp_who_table))

for(i in 1:75){
  for(j in 1:71){
    if(temp_sums$SDISTRI[j]== temp_who_table$Admin2[i])
      temp_who_table$dhs_coverage[i] <- temp_sums$V1[j]
  }
}

plot_dhs_who <- data.frame("who"=temp_who_table$Coverage,"dhs"=temp_who_table$dhs_coverage)

plot_dhs_who<-plot_dhs_who[!(plot_dhs_who$dhs==0),]

DHS <- plot_dhs_who$dhs
WHO <- plot_dhs_who$who
plot(DHS, WHO, type = "p") #plot

abc <- ginv(t(DHS) %*% DHS)
abc <- abc %*% t(DHS)
abc <- abc %*% WHO

Y <- abc %*% DHS

#calculation of R squared
cor(WHO,t(Y))^2

lines(DHS,Y)


#RCH VS DHS (UP AND BCG)

rch_data$dhs_coverage <- rep(0,nrow(rch_data))

for(i in 1:75){
  for(j in 1:71){
    if(rch_data$District[i] == temp_sums$SDISTRI[j])
      rch_data$dhs_coverage[i] <- temp_sums$V1[j]
  }
}

plot_rch_dhs <- data.frame("rch"=rch_data$`% Newborns given BCG to Reported live birth`,"dhs"=rch_data$dhs_coverage)

plot_rch_dhs<-plot_rch_dhs[!(plot_rch_dhs$dhs==0),]

DHS <- plot_rch_dhs$dhs
RCH <- plot_rch_dhs$rch
plot(DHS, RCH, type = "p") #plot

abc <- ginv(t(DHS) %*% DHS)
abc <- abc %*% t(DHS)
abc <- abc %*% RCH

Y <- abc %*% DHS

#calculation of R squared
cor(RCH,t(Y))^2

lines(DHS,Y)
lin_mod3 <- lm(DHS~RCH) #linear regression model
summary(lin_mod3)
abline(lin_mod3,col="blue") #best regression line
cor(DHS,RCH) #correlation
