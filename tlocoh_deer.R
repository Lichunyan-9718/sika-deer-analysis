library(tlocoh)
library(sp)
library(sf)
library(raster)
library(terra)
install.packages("ggplot2")
library(ggplot2)
??tlocoh


#1.准备数据
#加载数据
require(tlocoh)#加载包
deer <- read.csv("D:/R/tlocoh/T4data.csv")
class(deer)#查看数据类型
head(deer)
plot(deer[ , c("long","lat")], pch=20)#绘制经纬度散点图
#将动物位置数据从经纬度坐标转换为 UTM 坐标
deer.sf <- st_as_sf(deer,
                    coords = c("long", "lat"),
                    crs = 4326)# 创建 sf 对象
deer.sf.utm <- st_transform(deer.sf, crs = 32652)# 坐标转换
deer.mat.utm <- st_coordinates(deer.sf.utm)# 提取转换后的坐标
head(deer.mat.utm)# 查看转换后的坐标
colnames(deer.mat.utm) <- c("x","y")
head(deer.mat.utm)#改变列标签
# 将 UTC 时间戳转换为 POSIXct 对象，不指定时区
class(deer$t)
head(as.character(deer$t))
deer.utc <- as.POSIXct(deer$t, tz = "UTC")
deer.utc[1:3]
#2.创建locoh-xy对象（将移动数据的空间和时间信息整合到一个lxy对象中，同时处理重复数据）
#创建lxy对象
deer.lxy <- xyt.lxy(xy=deer.mat.utm,
                    dt=deer.utc,
                    id="deer",
                    proj4string=CRS("+proj=utm +north +zone=52 +ellps=WGS84")) 
#按日期、步长和采样间隔划分的位置分布
summary(deer.lxy)#查看摘要信息
plot(deer.lxy)#绘制轨迹
hist(deer.lxy)#绘制直方图
lxy.plot.freq(deer.lxy, deltat.by.date=T)#绘制频率图
pdf("s_value_distribution.pdf")
lxy.plot.sfinder(deer.lxy)
dev.off()
file.show("s_value_distribution.pdf")
graphics.off()
lxy.plot.sfinder(deer.lxy)
deer.lxy <- lxy.thin.bursts(deer.lxy, thresh=0.2)
lxy.plot.sfinder(deer.lxy, delta.t=3600*c(12,24,36,48,54,60))
#4.k方法
#确定最近的邻点
#获得邻近点的信息
deer.lxy <- lxy.nn.add(deer.lxy, s=0.01, k=25)
summary(deer.lxy)
#创建 Hullsets
deer.lhs <- lxy.lhs(deer.lxy, k=3*2:8, s=0.01)
summary(deer.lhs, compact=T)
#决定k值
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

#5.Compute Additional Hull Metrics
#①边界椭球体的偏心度
deer.lhs.k15 <- lhs.ellipses.add(deer.lhs.k15)
summary(deer.lhs.k15)
plot(deer.lhs.k15,
     hulls=T,
     ellipses=T, 
     allpts=T, 
     nn=T, 
     ptid="auto")
#②Time-Use Metrics
deer.lhs.k15 <- lhs.visit.add(deer.lhs.k15, 
                              ivg=3600*12)
summary(deer.lhs.k15)
lhs.save(deer.lhs.k15, dir="D:/R/tlocoh")












