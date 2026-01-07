setwd("C:/Users/nsaby/OneDrive/fac/bourdinière")

library(terra)
library(sf)

zc <- read_sf("SIG/zone_cultivee.shp") %>%
  st_transform(crs= 2154)

zcb <- st_buffer(zc, -5)

lb <- read_sf("SIG/limite_Bourdaisiere.shp")
lbb <- st_buffer(lb, dist = 15)

r <- rast("SIG/RGE_alti_1m.tif")
r <- mask(r,lbb)
plot(r)
lines(lb)


# le rgalti
alt37 <- rast("d:/database/mnts/mnt_5m_dep371.tif")
lbb2 <- lbb %>% st_transform(crs = crs(alt37))
r4 <- crop(alt37,lbb2,mask = TRUE ,extend = TRUE)
plot(r4)




r1m <- rast("SIG/RGE_alti_1m.tif")
r3m <- aggregate(r1m,10)

r <- rast("SIG/RGE_alti_25mres.tif")
r <- resample(r, r3m, method='cubic')
mnt <- mask(r,lbb)
plot(mnt)

slp <- terra::terrain(mnt)


plot(c(mnt,slp))


# solmnt# solutoin 1----

slp <- terra::terrain(r)
plot(slp)

mask <- ifel(slp < 10,slp,NA)
plot(mask)
lines(zc , col=2)
lines(lb)

mnt <- mask(r,mask)
plot(mnt)


r10  <- aggregate(mnt, 10)
y <- resample(mnt, r10, method='cubicspline')

slp <- terra::terrain(y)


plot(slp)
lines(zc , col=2)
lines(lb)


mask2 <- ifel(slp < 10,slp,NA)

mnt2 <- mask(r10,mask2)
slp2 <- terra::terrain(mnt2)

plot(slp2)
lines(zc , col=2)
lines(lb)


# solution 2 ---------

r10  <- aggregate(r, 25)
y <- resample(r, r10, method='cubicspline')

slp <- terra::terrain(y)

mask <- ifel(slp < 10,slp,NA)
plot(mask)
lines(zc , col=2)
lines(lb)

altm <- mask(y,mask)

tt = as.data.frame(c(mnt,slp), xy=TRUE,  na.rm=TRUE)



# cluster --------
mnt = r4
slp <- terra::terrain(mnt)
plot(c(mnt,slp))

tt = as.data.frame(c(mnt,slp), xy=TRUE,  na.rm=TRUE)

#Set number of sampling locations to be selected
n<-4

#Compute clusters
set.seed(314)
myClusters <- kmeans(scale(tt[,c(3:4)]),
                     centers=n,
                     iter.max=100,
                     nstart=10)

tt$clusters <- myClusters$cluster
tt$id <- 1:nrow(tt)


# #Select locations closest to the centers of the clusters
# rdist.out <- rdist(x1=myClusters$centers,
#                    x2=scale(grdHunterValley[,c(3,4,5,6,7)])
#                    )
# ids.mindist <- apply(rdist.out,MARGIN=1,which.min)
# mySample <- grdHunterValley[ids.mindist,]

library(foreach)

sel <- foreach(cl = 1:n,
               .combine = c ) %do% {
  
  pix <- tt[tt$clusters == cl , ]
  sample( pix$id , 13 )
               }


mysample <- tt[sel, ]
resolution = 5
mysample$x1<-jitter(mysample$x,resolution[1]/2)
mysample$y1<-jitter(mysample$y,resolution[1]/2)

library(ggplot2)

ggplot(tt) +
  geom_tile(mapping = aes(x = x, y = y, 
                          fill = factor(clusters))) +
  scale_fill_discrete(name = "cluster") +
   geom_point(data=mysample,mapping=aes(x=x1,y=y1),size=2) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  coord_fixed() +
  theme(legend.position="none")


ggplot(tt) +
  geom_point(mapping=aes(y=RGE_alti_25mres      ,
                         x=slope,
                         colour=factor(clusters)),
             size = .91) +
   geom_point(data=mysample,mapping=aes(y=RGE_alti_25mres ,x=slope),size=1.5) +
  scale_y_continuous(name = "Elevation") +
  scale_x_continuous(name = "Slope") +
  theme(legend.position="none")
