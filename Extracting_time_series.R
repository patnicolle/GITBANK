#extracting a time series 

library(survival)
library(RNetCDF)
library(date)
library(ncdf4)



my.data <- "/Volumes/P_Harddrive/GIMMS3g_NDVI_Australia_1982_2015/Australia_NDVI3g_bimonthly_1982_2015.nc"
my.time <- var.get.nc(my.data, "z")

n.latitudes  <- length(var.get.nc(my.data, "lat"))
n.longitudes <- length(var.get.nc(my.data, "lon"))

n.dates <- trunc(length(my.time))
n.dates

my.object <- var.get.nc(my.data, "my.variable")

my.array  <- array(my.object, dim = c(n.latitudes, n.longitudes, n.dates))
my.array[,,1:5]
my.vector <- my.array[1, 2, 1:n.dates]  # first latitude, second longitude
head(my.vector)

baseDate <- att.get.nc(my.data, "NC_GLOBAL", "base_date")
bdInt    <- as.integer(baseDate[1])

year     <- date.mdy(seq(mdy.date(1, 1, bdInt), by = 1,
                         length.out = length(my.vector)))$year 

head(year)
