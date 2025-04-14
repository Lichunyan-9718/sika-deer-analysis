library(tlocoh)
library(tlocoh.dev)
library(sp)
library(sf)
library(raster)
library(terra)
library(shiny)
library(pbapply)

setwd("D:/R/HR/HR2")  # 设置工作目录

#加载示例数据集
deer <-read.csv("all/T1data.csv")
class(deer$t)
head(as.character(deer$t))
deer.utc <- as.POSIXct(deer$t, tz = "UTC")
deer.utc[1:3]
# 确保时间列 t 是 POSIXct 格式
deer$t <- as.POSIXct(deer$t)  # 若时间格式特殊，需指定格式，如 format="%Y-%m-%d %H:%M:%S"

# 转换为 tlocoh 的 Lxy 对象（关键！）
lxy <- xyt.lxy(xy = deer[, c("x", "y")], 
               dt = deer$t, 
               id = "T1", 
               proj4string = "+proj=longlat +datum=WGS84")
# 生成规则网格并计算重访指标
lxy <- lxy.tumap(
  lxy,
  gridtype = "square",       # 或 "hexagon" 生成六边形网格
  cellsize = 0.01,           # 网格大小（单位与坐标系一致，此处为经纬度度）
  ivg = ivg_seconds,         # 关键参数：Inter-Visit Gap 秒数
  status = FALSE             # 关闭进度条（大数据时加速）
)

# 提取结果（每个网格的重访次数和平均访问定位点数）
tumap_df <- lxy$tumap$T1$grid@data  # 假设个体名为 "T1"

# 将网格转换为sf对象用于可视化
grid_sf <- st_as_sf(lxy$tumap$T1$grid)
grid_sf <- cbind(grid_sf, tumap_df[, c("nvisits", "mnlv")])

#重访次数 (nvisits) 热力图
ggplot() +
  geom_sf(data = grid_sf, aes(fill = nvisits), color = NA) +
  scale_fill_viridis_c(name = "Number of Visits", option = "C") +
  labs(title = paste0("Revisitation Rate (IVG = ", ivg_seconds/3600, " hours)"),
       x = "Longitude", y = "Latitude") +
  theme_minimal()









# 示例代码 --------------------------------------------------------------------

mycon <- url("http://tlocoh.r-forge.r-project.org/toni.n5775.2005-08-22.2006-04-23.lxy.RData")
load(mycon); close(mycon) 

summary(toni.lxy)


toni.tumap1 <- lxy.tumap(toni.lxy, ivg=10*3600, grid="square")
plot(toni.tumap1, cex.axis=0.8, cex=0.8, legend="topright")
toni.tumap2 <- lxy.tumap(toni.lxy, ivg=10*3600, gridtype="hex")
plot(toni.tumap2, cex.axis=0.8, cex=0.8, legend="topright")
toni.tumap3 <- lxy.tumap(toni.lxy, ivg=10*3600, gridtype="hex", mindim=40)
plot(toni.tumap3, cex.axis=0.8, cex=0.8, legend="topright")

require(rgdal)
kruger_bnd <- readOGR(dsn=".", layer="knp_boundary_36s", verbose=FALSE)
kruger_water <- readOGR(dsn=".", layer="drinking_troughs_36s", verbose=FALSE)
kruger_roads <- readOGR(dsn=".", layer="roads_36s", verbose=FALSE)

plot(toni.tumap3, mnlv=FALSE, nsv=TRUE, cex.axis=0.8, cex=0.8, legend="topright")
plot(kruger_bnd, add=TRUE, border="green")
plot(kruger_water, add=TRUE, pch=24, col="black", bg="yellow", cex=0.6)
plot(kruger_roads, add=TRUE, lty=3, col="black")

require(rgdal)
writeOGR(toni.tumap3[["toni"]], ".", "toni_timeuse", overwrite_layer = TRUE, driver="ESRI Shapefile")
list.files(".", pattern = "^toni") 

#sbsbsbsbsbsbsbsbsbsbsb
#跑不出跑不出跑不出跑不出跑不出跑不出跑不出跑不出
