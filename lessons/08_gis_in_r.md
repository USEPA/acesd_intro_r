
# Geospatial Analysis (aka GIS) in R

Believe it or not, you can meet all your GIS and geospatial analysis needs using only R! We will introduce this today and we will cover the basics of how to read in and write spatial data with R, geospatial data packages, basic geospatial analysis, and options for making maps.  This will be a very high level overview but should at least get you started on the right path to learn more.

Like all things in the R community, there are many options for learning about geospatial work in R.  One that I find particularly good is [Geocomputation with R](https://r.geocompx.org/index.html).  For an annotated list of spatial packages, the [CRAN Task View: Analysis of Spatial Data](https://cran.r-project.org/web/views/Spatial.html) is still a good, if a bit overwhelming, resource.

None of these capabilities are part of base R, as such you will need to make sure that all of the packages have been installed.  The code block below will take care of that for you.


```r
install.packages("sf")
```

```
## package 'sf' successfully unpacked and MD5 sums checked
## Warning in install.packages :
##   cannot remove prior installation of package 'sf'
## Warning in install.packages :
##   problem copying C:\Program Files\R\R-4.2.1\library\00LOCK\sf\libs\x64\sf.dll to C:\Program Files\R\R-4.2.1\library\sf\libs\x64\sf.dll: Permission denied
## Warning in install.packages :
##   restored 'sf'
## 
## The downloaded binary packages are in
## 	C:\Users\JHollist\AppData\Local\Temp\Rtmpg3J4MO\downloaded_packages
```

```r
install.packages("terra")
```

```
## 
##   There is a binary version available but the source version is
##   later:
##       binary source needs_compilation
## terra  1.7-3 1.7-18              TRUE
```

```
## installing the source package 'terra'
```

```
## Warning in install.packages :
##   installation of package 'terra' had non-zero exit status
```

```r
install.packages("tidycensus")
```

```
## also installing the dependency 'tigris'
```

```
## package 'tigris' successfully unpacked and MD5 sums checked
## package 'tidycensus' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\JHollist\AppData\Local\Temp\Rtmpg3J4MO\downloaded_packages
```

```r
install.packages("elevatr")
```

```
## package 'elevatr' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\JHollist\AppData\Local\Temp\Rtmpg3J4MO\downloaded_packages
```

```r
install.packages("USAboundaries")
```

```
## package 'USAboundaries' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\JHollist\AppData\Local\Temp\Rtmpg3J4MO\downloaded_packages
```

```r
install.packages("mapview")
```

```
## package 'mapview' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\JHollist\AppData\Local\Temp\Rtmpg3J4MO\downloaded_packages
```

```r
install.pacakges("ggspatial")
```

```
## Error in install.pacakges("ggspatial"): could not find function "install.pacakges"
```

## Vector data with `sf`

## Raster data with `terra`

## Geospatial data packages

## Static maps with `ggplot2` and `ggspatial`

## Interactive maps with `mapview`
