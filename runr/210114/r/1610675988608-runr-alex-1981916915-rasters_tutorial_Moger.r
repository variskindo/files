


#working with rasters!

#first, load the necessary packages:
library(raster)
library(rgdal)
library(rgeos)
library(sp)



#A raster is a spatial grid, where each pixel represents a square area (i.e., 30 x 30 m; called resolution)
#And the value assigned to that pixel characterizes that area

#Let's create our own raster. We will specify a resolution of 30 x 30 m:

r <- raster(res = 30)

#if we call r:
r
#we see its structure. But it's empty. We didn't assign values. So let's fill it with ascending numbers
ncell(r) #what is the total number of cells in the raster we made?
fill <- 1:ncell(r)

#you can set values in a raster two different ways.
#1 using a function in the raster package:
r1 <- setValues(r, fill)
#2 using matrix notation:
r2 <- r
r2[] <- fill

#plot them to compare
plot(r) #throws error, it's empty
plot(r1)
plot(r2)

#we can formally compare rasters too:
compareRaster(r1, r2)

#we can do simple raster calculations just like we would any other math in R:

#we can add rasters. This is performed pixel by pixel, so pixel 1 in r1 is added to pixel 1 in r2, etc
r_sum <- r1 + r2
plot(r_sum)

#we can also just multiply by some value, which will be applied pixel by pixel as well:
r_mult <- r_sum * 10

#we can replace specific pixels by index:
r1[1:10] <- 100
plot(r1)

#think of a raster as a matrix or table but with spatial information included!

#let's call r1 again and check out all the attributes:
r1
#you can see our res is 30 X 30 like we specified.
#the crs (Coordinate Reference System) is also provided (raster defaults to WGS84)
#source tells us this raster is stored in memory. Some large rasters are not!
#values is useful to tell us what our range of values is. I use this frequently as a gut-check
#to see if an operation I performed worked properly


#let's work with a raster that contains real data
#this is a burn severity raster for the entire continental US in 2012 from Monitoring Trends in Burn Severity (MTBS) data

#we can also use the raster() command to read in a raster
#provide the full filepath to the raster, with extension
burn <- raster("C:/Users/croth/Documents/rasters_tutorial_moger/mtbs_sev_2012.tif")

#let's check it out:
burn
#note it's not in memory this time!
plot(burn)
#you see a LOT of black
#this is value 255, which is a "no data" value
#we will deal with this in a moment.

#let's say we only want to check out the CA and NV fires in 2012
#we can bring in a CANV shapefile and clip the raster to our area of interest
CANV <- readOGR("C:/Users/croth/Documents/rasters_tutorial_moger/CA_NV", "CA_NV")

#now, before we clip, we need to make sure they have the same projections so they line up correctly!

projection(CANV) == projection(burn)

#They don't, so let's fix that
CANV <- spTransform(CANV, CRS(projection(burn))) #transform CA to our burn raster's crs

#we can "clip" in two ways:
burn_crop <- crop(burn, CANV) #this clips by the "bounding box" of the shapefile
plot(burn_crop)
plot(CANV, add = T)

####NOT RUN:
burn_mask <- raster::mask(burn, CANV) #this clips by the outline of the shapefile
plot(burn_mask)
plot(CANV, add = T)

#ok, let's tackle those NA values (255, the black pixels)
#we don't want it to count in our calculations (it's not a "real" value)
#but R will treat is as a true number, so let's assign it as NA!
burn_mask[burn_mask == 255] <- NA #this mean inside the raster where any pixel is 255, change it to NA
#it takes a while....get a snack lol

plot(burn_mask)
#This is important! it's an annoying step but very important to perform or you will end up with very inflated calculations!
#It's good practice to look at the metadata for a raster and see what the no data value is, then clean it up :)



