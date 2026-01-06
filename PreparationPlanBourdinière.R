setwd("C:/Users/nsaby/OneDrive/fac/bourdinière")

library(terra)
library(sf)
library(foreach)

# les vecteurs 
zc <- read_sf("SIG/zone_cultivee.shp") %>%
  st_transform(crs= 2154)

zcb <- st_buffer(zc, -5)

lb <- read_sf("SIG/limite_Bourdaisiere.shp")
lbb <- st_buffer(lb, dist = -5)

# l'altitude
r <- rast("SIG/RGE_alti_25mres.tif")

r1m <- rast("SIG/RGE_alti_1m.tif")
r3m <- aggregate(r1m,10)

r <- resample(r, r3m, method='cubic')

mnt <- crop(r,
            lbb,
            mask = TRUE ,
            extend = TRUE)

writeRaster(mnt,
            filename = "SIG/mnt_final.tiff",
            overwrite = TRUE)

# calcul de la pente
slp <- terra::terrain(mnt , 
                      v="slope"
                      )


plot(c(mnt,slp))

# cluster --------

tt = as.data.frame(c(mnt,slp), xy=TRUE,  na.rm=TRUE)

#Set number of sampling locations to be selected
n<-4

#Compute clusters
set.seed(314)
myClusters <- kmeans(scale(tt[,c(3:4)]),
                     centers=n,
                     iter.max=1000,
                     nstart=20)

tt$clusters <- myClusters$cluster
tt$id <- 1:nrow(tt)




sel <- foreach( cl = 1:n,
               .combine = c ) %do% {
                 
                 pix <- tt[tt$clusters == cl , ]
                 sample( pix$id , 
                         n = 13
                         )
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
