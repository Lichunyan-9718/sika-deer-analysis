library(adehabitatLT)
library(adehabitatHR)
library(tidyverse)
library(move)
library(circular)
library(sp)
library(raster)
library(sf)
library(stars)
library(terra)
library(dplyr)
library(Rmpfr)
library(leaflet)
library(lubridate)
library(trip)
deer_data3 <- read.csv2("T30/T30data_5.csv",
                        header = TRUE,
                        sep = ",")
deer_move3 <- move(x = as.numeric(deer_data3$location_long),
                   y = as.numeric(deer_data3$location_lat),
                   time=as.POSIXct(deer_data3$t, format="%Y/%m/%d %H:%M"), 
                   data=deer_data3, 
                   proj=CRS("+proj=longlat +ellps=WGS84"),
                   animal=deer_data3$id,
                   sensor=deer_data3$sensor)
table(deer_data3$id)  # 查看每个季节有多少条记录

deer_move3 <- spTransform(deer_move3,
                          CRS("+proj=utm +zone=52 +datum=WGS84"))
deer_dbbmm3 <- brownian.bridge.dyn(object=deer_move3, 
                                   location.error=20, 
                                   window.size=31,
                                   margin=9, 
                                   dimSize=100,
                                   time.step=60,
                                   ext = 0.3
                                   )



cont1 <- raster2contour(deer_dbbmm3, level=c(0.95))
cont1_sf <- st_as_sf(cont1)
plot(deer_dbbmm3)
contour(deer_dbbmm3, levels=0.95, add=TRUE)

# 导出为线shp文件
st_write(cont1_sf, "D:/arcgisdata/T30/T30_8.shp")
# 利用分布 --------------------------------------------------------------------


deer_ud <- getVolumeUD(deer_dbbmm3)

par(mfrow=c(1,1))
plot(deer_ud, main="UD")
plot(deer_dbbmm3)
## also a contour can be added
plot(deer_ud, main="UD and contour lines")
contour(deer_ud, 
        levels=c(0.5, 0.95), 
        add=TRUE, 
        lwd=c(0.5, 0.5), 
        lty=c(2,1))
par(mfrow=c(1,1))

## mantaining the lower probabilities
ud95 <- deer_ud
ud95[ud95>.95] <- NA
plot(ud95, main="UD95")
contour(ud95, 
        levels=c(0.5, 0.94), 
        add=TRUE, 
        lwd=c(0.5, 0.5), 
        lty=c(2,1))
## or extracting the area with a given probability, where cells that belong to the given probability will get the value 1 while the others get 0
ud95 <- deer_ud<=.95
plot(ud95, main="UD95")

ud50 <- deer_ud<=.5
plot(ud50, main="UD50")
contour(ud95, 
        levels=c(0.5, 0.95), 
        add=TRUE, 
        lwd=c(0.5, 0.5), 
        lty=c(2,1))
par(mfrow=c(1,1))
# 将RasterLayer转换为SpatialPolygonsDataFrame
ud_polygons <- rasterToPolygons(ud95, 
                                dissolve = TRUE)


         

# 插值 ----------------------------------------------------------------------
deer_data3 <- read.csv2("T30/T30data_8new.csv",
                        header = TRUE,
                        sep = ",")
deer_move3 <- move(x = as.numeric(deer_data3$location_long),
                   y = as.numeric(deer_data3$location_lat),
                   time=as.POSIXct(deer_data3$t, format="%Y/%m/%d %H:%M"), 
                   data=deer_data3, 
                   proj=CRS("+proj=longlat +ellps=WGS84"),
                   animal=deer_data3$id,
                   sensor=deer_data3$sensor)
## 按照固定时间间隔插值
interp1hour <- interpolateTime(deer_move3, time=as.difftime(1, units="hours"), spaceMethod='rhumbline')
plot(deer_move3, col="red",pch=20, main="By time interval")
points(interp1hour)
lines(deer_move3, col="red")
legend("bottomleft", c("True locations", "Interpolated locations"), col=c("red", "black"), pch=c(20,1))
summary(timeLag(interp1hour, "hours"))
write.csv(interp1hour@coords, "T30/T30data_8inter1hour.csv")
         
         