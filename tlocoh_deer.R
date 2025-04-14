library(tlocoh)
library(sp)
library(sf)
library(raster)
library(terra)
library(ggplot2)

#1.准备数据
deer <- read.csv("D:/R/tlocoh/deer.csv")
plot(deer[ , c("long","lat")], pch=20)

deer.sf <- st_as_sf(deer,
                    coords = c("long", "lat"),
                    crs = 4326)# 创建 sf 对象
deer.sf.utm <- st_transform(deer.sf, crs = 32652)# 坐标转换
deer.mat.utm <- st_coordinates(deer.sf.utm)# 提取转换后的坐标
colnames(deer.mat.utm) <- c("x","y")
class(deer$t)
head(as.character(deer$t))
deer.utc <- as.POSIXct(deer$t, tz = "UTC")

#2.创建locoh-xy对象
deer.lxy <- xyt.lxy(xy=deer.mat.utm,
                    dt=deer.utc,
                    id="deer",
                    proj4string=CRS("+proj=utm +north +zone=52 +ellps=WGS84")) 

#3.参数选择
deer.lxy <- lxy.ptsh.add(deer.lxy)  # 计算s值参考范围
deer.lhs <- lhs.iso.add(deer.lhs)#①创建等值线
plot(deer.lhs,
     iso=T, 
     k=15,
     allpts=T, 
     cex.allpts=0.1,
     col.allpts="gray30",
     ufipt=F)
lhs.plot.isoarea(deer.lhs)#②等位面积曲线
lhs.plot.isoear(deer.lhs)#③isopleth edge:area ratios
deer.lhs.k15 <- lhs.select(deer.lhs, k=15)

s_value <- 0.01 
k_value <- 15    

#4.k方法
# 添加最近邻信息
lxy <- lxy.nn.add(lxy, s = s_value, k = k_value)
# 创建hullset
hullset <- lxy.lhs(lxy, k = k_value, s = s_value)
# 计算等密度线
hullset <- lhs.iso.add(hullset)
# 可视化家域（95%等密度线）
plot(hullset, iso = TRUE, k = k_value, allpts = TRUE, cex.allpts = 0.1)
# 评估模型质量 - 等密度线面积变化
lhs.plot.isoarea(hullset)
# 评估模型质量 - 边缘 - 面积比
lhs.plot.isoear(hullset)

#5.结果导出
lhs.exp.shp(deer.lhs, 
           dir = "D:/R/tlocoh/output",  
           iso = TRUE, 
           iso.level = 0.95)           



