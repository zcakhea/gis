# set working directory
setwd("~/Desktop/Postgraduate/CASA0005/assessment")

# library useful packages
library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
library(dplyr)
library(readxl)
library(maptools)
library(spatstat)
library(raster)
library(stars)
library(rsample)
library(broom)
library(boot)
library(tidypredict)
library(spatialreg)


library(here)
detach("package:here", unload=TRUE)
library("here")

############################### Preparation ###################################
### read in boundary msoa London data from https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london
Londonmsoa<-dir_info(here::here("statistical-gis-boundaries-london", 
                                "ESRI"))%>%
  #$ means exact match
  dplyr::filter(str_detect(path, 
                           "MSOA_2011_London_gen_MHW.shp$"))%>%
  dplyr::select(path)%>%
  pull()%>%
  #read in the file in
  st_read()%>%
  st_transform(., 27700)

# check the data
qtm(Londonmsoa)

### read in census data from https://data.gov.uk/dataset/d91901cb-13ee-4d00-906c-1ab36588943b/msoa-atlas
census <- read_excel("census.xlsx",
                       na = c("", "NA", "n/a"), 
                       col_names = TRUE)

census[,5] <- census[,5]/1000 

Datatypelist <- census %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

# merge boundaries and data
census_merged <- Londonmsoa%>%
  left_join(.,
            census, 
            by = c("MSOA11CD" = "MSOA11CD"))

# let's map our dependent variable to see if the join has worked:
tmap_mode("view")
qtm(Boots_data, 
    fill = "hoscount", 
    borders = NULL,  
    fill.palette = "Reds")


### read hospital data from https://data.gov.uk/dataset/f4420d1c-043a-42bc-afbc-4c0f7d3f1620/hospitals
### read clinic data from https://data.gov.uk/dataset/6d423b3b-c88b-4144-9908-0f18d67a92a2/clinics
hospital <- read_excel("hospital.xlsx")

hospital_sf <- st_as_sf(hospital, 
                        coords = c("Longitude","Latitude"), 
                        crs = 4326)
#%>% st_transform(.,27700)
qtm(hospital_sf)

############################ hospital spatial randomness? ###################
window1 <- as.owin(Londonwards)
plot(window1)

hospital_ppp<- hospital_sf %>%
  st_transform(.,27700)%>%
  as(., 'Spatial')

hospital_ppp <- ppp(x=hospital_ppp@coords[,1],
                    y=hospital_ppp@coords[,2],
                    window=window1)

plot(window1)
points(hospital_ppp)

hospital_ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="hospital")

hospital_ppp %>%
  density(., sigma=2000) %>%
  plot()

plot(hospital_ppp)
# plot(Kest(hospital_ppp))

quadrat.test(hospital_ppp)
plot(quadrat.test(hospital_ppp))

quadrat.test(hospital_ppp,metohd="MonteCarlo")

######################### exploratory  analysis #############################
### all balck prop
par(mfrow=c(2,2))

ggplot(census_merged, aes(x=hosdens)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

## log transformation
ggplot(census_merged, aes(x=log(hosdens))) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

##### hospital dens ######
hospital_sf1 <- st_as_sf(hospital, 
                        coords = c("Longitude","Latitude"), 
                        crs = 4326)%>% st_transform(.,27700)

points_sf_joined <- census_merged%>%
  st_join(hospital_sf1)%>%
  add_count(MSOA11CD)%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)%>%
  #select density and some other variables 
  dplyr::select(density,msoa11cd,n)

points_sf<- points_sf_joined %>%                    
  group_by(msoa11cd) %>%         
  summarise(density = first(density),
            hoscount= first(n))

census_merged<- census_merged %>% 
  mutate(hosdens= as.numeric(points_sf$density))%>%
  mutate(hoscount= points_sf$hoscount)

# check normal for hospitaldens
ggplot(census_merged, aes(x=hosdens)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

## log transfor hosdens
ggplot(census_merged, aes(x=log(hosdens))) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

#####  other covariates
ggplot(census_merged, aes(x=covid_19_deaths)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

## log transfor hosdens
ggplot(census_merged, aes(x=log(covid_19_deaths))) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)


#covid_19_deaths    better after log
#mean_annual_household_income      better
#popdens               not better 
#badhealth           better
#emission            better
#GCES                better
#hosdens             better
#hoscount          better
#bame              not better
 

census_merged <- census_merged %>% mutate(logcovid=log(covid_19_deaths))

#set -inf to 0
census_merged$logcovid[which(!is.finite(census_merged$logcovid))] <- 0

################### model preparation
model <- lm(log(hosdens) ~ log(all_black_prop) + popdens
            +logcovid +region
            +log(GCES)+bame+log(mean_annual_household_income), data=census_merged)
summary(model)
step(model)

model <- lm(log(hosdens) ~ popdens + log(all_black_prop)+
               logcovid + region +bame+log(mean_annual_household_income), 
             data = census_merged)

summary(model)
plot(model,c(1,3))

DW <- durbinWatsonTest(model2)
tidy(DW)

#now plot the residuals
tmap_mode("view")
qtm(census_merged, fill = "model2resids")

tm_shape(census_merged) +
  tm_polygons("model2resids",
              palette = "RdYlBu") +
  tm_shape(popdens) + tm_dots(col = "red")



vif(model)
model_data <- census_merged%>%
  st_drop_geometry()%>%
  dplyr::select(hosdens,mean_annual_household_income,
                all_black_prop,
                popdens,region,logcovid,bame,MSOA11CD)%>%
  mutate(income=log(mean_annual_household_income))%>%
  mutate(balckprop=log(all_black_prop))%>%
  mutate(hosdens=log(hosdens))


model_data <- model_data[,-c(2,3)]
cor(model_data[,-c(3,6)])

model2 <- lm(hosdens ~ balckprop +income +bame+logcovid+region+popdens
             , data = model_data)
summary(model2)

census_merged <- census_merged %>%
  mutate(model2resids = residuals(model2))

p <- ggplot(census_merged, 
            aes(x=mean_annual_household_income, 
                y=hosdens))
p + geom_point(aes(colour = region))

######################### bootstrap ##########################################
Boots_data <- census_merged%>%
  dplyr::select(mean_annual_household_income,
                all_black_prop,
                hosdens,bame,logcovid,region,popdens,MSOA11CD,geometry)%>%
  mutate(income=log(mean_annual_household_income))%>%
  mutate(balckprop=log(all_black_prop))%>%
  mutate(hosdens=log(hosdens))

Boots_data <- Boots_data[,-c(1,2)]

set.seed(99)

census_boot <-st_drop_geometry(Boots_data) %>%
  bootstraps(times = 1000, apparent = TRUE)

slice_tail(census_boot, n=5)

census_model <- census_boot %>%
  #make new column
  mutate(
    #column name is model that contains...
    model = map(splits, ~ lm(hosdens ~ balckprop +income +bame+logcovid+region+popdens, 
                             data = .)))

# let's look at the first model results
census_model$model[[1]]

census_models_tidy <- census_model %>%
  mutate(
    coef_info = map(model, tidy))

census_coef <- census_models_tidy %>%
  unnest(coef_info)
census_coef

int_pctl(census_models_tidy, coef_info, alpha = 0.05)

census_aug <- census_models_tidy %>%
  #sample_n(5) %>%
  mutate(augmented = map(model, augment))%>%
  unnest(augmented)

##################   Moran's I test on the residuals from the model###########
coordsM <- census_merged%>%
  st_centroid()%>%
  st_geometry()

#plot(coordsM)

LMSOA_nb <- census_merged %>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_msoa <-coordsM %>%
  knearneigh(., k=4)

LMSOA_knn <- knn_msoa %>%
  knn2nb()

#plot them
plot(LMSOA_nb, st_geometry(coordsM), col="red")
plot(LMSOA_knn, st_geometry(coordsM), col="blue")

#add a map underneath
#plot(census_merged)

LMSOA.queens_weight <- LMSOA_nb %>%
  nb2listw(., style="C")

LMSOA.knn_4_weight <- LMSOA_knn %>%
  nb2listw(., style="C")

Nearest_neighbour <- census_merged %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., LMSOA.knn_4_weight)%>%
  tidy()

Nearest_neighbour
#moderate to strong spatial autocorrelation in our residuals
slag_dv_model2_knn4 <- lagsarlm(log(hosdens) ~ popdens + log(all_black_prop)+
                                  logcovid + region +bame+log(mean_annual_household_income), 
                                data = census_merged, 
                                nb2listw(LMSOA_knn, 
                                         style="C"), 
                                method = "eigen")


#what do the outputs show?
tidy(slag_dv_model2_knn4)
summary(slag_dv_model2_knn4)
#############################################################################
#Correlation_myvars <- Boots_data %>%
  st_drop_geometry()%>%
  dplyr::select(-c(region,MSOA11CD))%>%
  cor()

model_final <- lm(log(hosdens) ~ popdens + log(all_black_prop)+
                    logcovid + region +bame+log(mean_annual_household_income), 
                    data=census_merged)

tidy(model_final)


par(mfrow=c(1,1))
plot(model_final)


census_merged <- census_merged %>%
  mutate(model_final_resids = residuals(model_final))

Nearest_neighbour1 <- census_merged %>%
  st_drop_geometry()%>%
  dplyr::select(model_final_resids)%>%
  pull()%>%
  moran.test(., LMSOA.knn_4_weight)%>%
  tidy()

Nearest_neighbour1
Nearest_neighbour

tmap_mode("plot")
qtm(census_merged, fill = "model_final_resids")

plot("plot")
ggplot(census_merged,aes(x=model2resids))+ 
  geom_histogram(aes(y=..density..)) 

final_model_Moran <- census_merged %>%
  st_drop_geometry()%>%
  dplyr::select(model_final_resids)%>%
  pull()%>%
  moran.test(., LMSOA.knn_4_weight)%>%
  tidy()

final_model_Moran


ggplot(census_merged, aes(x=log(covid_19_deaths))) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)






