#Load required libraries
library(spBayes)
library(raster)
library(parallel)
library(fields)
library(rgdal)

#loading the data
vaxdata <- gps_data_join[!(gps_data_join$SOURCE=='MIS'),] #File containing processed cluster-level vaccination data
#Each row contains the DHS cluster number, and for each age group,
#the number of children surveyed and the numbers who were vaccinated 

#Extract vaccination data for age group 0-59 months
Numvacc    <- (vaxdata$coverage * vaxdata$numbers)/100 #coverage
weight    <- vaxdata$numbers #total children surveyed

#Geographical coordinates of clusters
coords     <- cbind(vaxdata$LONGNUM,vaxdata$LATNUM)

#I added
#covariates
education_level <- vaxdata$edu
urban_rural <- vaxdata$urbrur
wealth_index <- vaxdata$wealth
# 
# #############################################################################################
# #For cross-validation. This can be repeated by creating a loop.
# #This part can be excluded if not needed. 
# ll <- length(weights)
# nc <- (10/100)*ll         #Take 10% for validation - this was used to estimate the cov prob.
# samp.c <- sample(1:nrow(coords), nc, replace=FALSE)
# coords.nc 	<- coords[samp.c,]
# Numvacc.nc	<- Numvacc[samp.c]
# weights.nc	<- weights[samp.c]
# education_level.nc	<- education_level[samp.c]
# urban_rural.nc	<- urban_rural[samp.c]
# wealth_index.nc	<- wealth_index[samp.c]
# 
# #Use the rest for model estimation
# coords  <- coords[-samp.c,]
# Numvacc <- Numvacc[-samp.c] 
# weights <- weights[-samp.c]
# education_level	<- education_level[-samp.c]
# urban_rural	<- urban_rural[-samp.c]
# wealth_index	<- wealth_index[-samp.c]
# #############################################################################################

#Formula
form 	 <- Numvacc ~  education_level  + urban_rural + wealth_index
form.2 <- (Numvacc/weight) ~ education_level  + urban_rural + wealth_index 

#Initial values of some parameters
fit 			<- glm(form.2, weights=weight, family="binomial")

beta.starting 	<- coefficients(fit)
beta.tuning 	<- t(chol(vcov(fit)))
# 
# ggplot(vax_data,aes(y=Numvacc/weight,x=education_level,color=factor(urban_rural)))+geom_point()+stat_smooth(method="glm",se=FALSE)
# xyplot(form.2, row.line="red")
# ggPredict(fit,se=TRUE,interactive=TRUE)

n.batch 		<- 100         ##100*1000 equals run length
batch.length 	<- 100
n.samples 		<- n.batch*batch.length
burn.in 		<- 0.1*n.samples + 1

##########
#knots_data=st_read("C:/Users/manoj/Documents/temp1/temp1.shp")

#########

#Sample knot locations using a geometric space-filling design 
#n.knots 	<- c(100,100)  #Can increase to 100 if need be
n.knots<-100
library("fields")
bb       	<- cover.design(coords, n.knots)  
knots 	<- as.matrix(bb$design)
weight=as.vector(weight)
# #COMPARE DIFFERENT COV MODELS LATER!
mod <- spGLM(form, family="binomial", weights = weight, 
             coords=coords, starting=list("beta"=beta.starting, "phi"=1,"sigma.sq"=1, "w"=0, "nu"=1),
             tuning=list("beta"=beta.tuning, "phi"=0.5, "sigma.sq"=0.5, "w"=0.5, "nu" = 0.1),
             priors=list("beta.Normal"=list(c(rep(0,4)),c(rep(10000,4))), "phi.Unif"=c(0.475, 10),
                         "sigma.sq.IG"=c(2, 1), "nu.Unif"=c(0.1,1)),
             amcmc=list("n.batch"=n.batch, "batch.length"=batch.length, "accept.rate"=0.43),
             knots=knots,
             cov.model="exponential", verbose=TRUE, n.report=10) # 0.475 - 700 km,  3.3 - 100 km
# 
# 
# #Save model for analysis
save(mod, file="model_3.rda")

############## PREDICTION

my.poly=st_read("C:/Users/manoj/Documents/CTech/Project/AfricaRep/NIR_outline.shp")
plot(my.poly$geometry)

grdpts <- sf::st_make_grid(my.poly, cellsize = 0.09, what = "centers")
my.points <- sf::st_sf(grdpts)
pointsInside <- sf::st_join(x = my.points, y = my.poly, left = FALSE)
plot(pointsInside$geometry)
#write.csv("coords_nig",pointsInside$geometry)
points_nig <- do.call(rbind, st_geometry(pointsInside)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
points_nig=as.matrix(points_nig)
pred_coord=points_nig

m_pred = spPredict(mod, pred_coord, pred.covars = matrix(1,nrow=nrow(pred_coord), ncol=1),
                   start=n.samples/10, thin=10)

y.hat.median <- apply(m_pred$p.y.predictive.samples, 1, median)
ff1=function(x) quantile(x,0.025)
ff2=function(x) quantile(x,0.975)
y.hat.low    <- apply(m_pred$p.y.predictive.samples, 1, ff1)
y.hat.up     <- apply(m_pred$p.y.predictive.samples, 1, ff2)
y.hat.sd     <- apply(m_pred$p.y.predictive.samples, 1, sd)
y.hat.mean     <- apply(m_pred$p.y.predictive.samples, 1, mean)
prediction  <- cbind(y.hat.median, y.hat.low, y.hat.up, y.hat.sd, y.hat.mean)

################# PLOT

vis1<- data.frame(cov=y.hat.median,pred_coord)
visual <- ggplot(vis1,aes(lon,lat)) +geom_point(aes(color=(cov)))
rainbox=c('red','orangered','orange','olivedrab4','olivedrab','darkgreen')
visual + scale_color_gradientn(colours = rainbox)


pointsInside$cov<-vis1$cov
ggplot(pointsInside, aes(reorder(NAME_1, cov, FUN = median), cov)) + 
  geom_boxplot(varwidth = TRUE, 
               fill = "white", colour = "#3366FF",
               outlier.colour = "red", outlier.shape = 1) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(colour="Coverage", x= "State", y="Coverage")

vis2<- data.frame(sd=y.hat.sd,pred_coord)
visual <- ggplot(vis2,aes(lon,lat)) +geom_point(aes(color=(sd)))
rainbox=c('lightblue','orange','orangered') #scheme1
visual + scale_color_gradientn(colours = rainbox)

print(summary(window(mod$p.beta.theta.samples, start=burn.in)))