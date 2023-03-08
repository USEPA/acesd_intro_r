---
output: html_document
editor_options: 
  chunk_output_type: console
---

# Geospatial Analysis (aka GIS) in R

Believe it or not, you can meet all your GIS and geospatial analysis needs using only R! We will introduce this today and we will cover the basics of how to read in and write spatial data with R, geospatial data packages, basic geospatial analysis, and options for making maps.  This will be a very high level overview but should at least get you started on the right path to learn more.

Like all things in the R community, there are many options for learning about geospatial work in R.  One that I find particularly good is [Geocomputation with R](https://r.geocompx.org/index.html).  For an annotated list of spatial packages, the [CRAN Task View: Analysis of Spatial Data](https://cran.r-project.org/web/views/Spatial.html) is still a good, if a bit overwhelming, resource.

I also won't be spending too much time on covering the basics of spatial data and the rest of this lesson assumes you know the basics of vector data (e.g.,  points, lines, and polygons) and raster data (e.g., pixels, images, etc.)

None of these capabilities are part of base R, as such you will need to make sure that all of the packages have been installed.  The code block below will take care of that for you.


```r
install.packages("sf")
install.packages("terra")
install.packages("tidycensus")
install.packages("elevatr")
install.packages("USAboundaries")
install.packages("mapview")
install.pacakges("ggspatial")
```

## Vector data with `sf`

The main package for working with vectore data in the R Spatial ecosystem is the `sf` package.  With `sf` we can read in a wide variety of vector data types.  We will see one common example that folks at EPA will encounter:  the venerable shapefile.  We will also see how to turn data (e.g., x and y locations) into an `sf` object.  

A shapefile is actually a collection of files.  We can get one from <https://www.epa.gov/system/files/other-files/2022-07/NLA17_polgons.zip>.  This is the 2017 lakes  The code below includes a function that will download these files for you.


```r
# Download data
download.file("https://www.epa.gov/system/files/other-files/2022-07/NLA17_polgons.zip", "nla17_lake_polygons.zip")

# Unzip file
unzip("nla17_lake_polygons.zip", junkpaths = T)

# Read in Vector
nla17_lakes <- st_read("NLA_2017_Lake_Polygon.shp")
```

```
## Reading layer `NLA_2017_Lake_Polygon' from data source 
##   `C:\Users\JHollist\projects\acesd_intro_r\lessons\NLA_2017_Lake_Polygon.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 1096 features and 2 fields
## Geometry type: MULTIPOLYGON
## Dimension:     XY
## Bounding box:  xmin: -2294144 ymin: 336576.5 xmax: 2204029 ymax: 3128293
## Projected CRS: Albers
```

```r
nla17_lakes
```

```
## Simple feature collection with 1096 features and 2 fields
## Geometry type: MULTIPOLYGON
## Dimension:     XY
## Bounding box:  xmin: -2294144 ymin: 336576.5 xmax: 2204029 ymax: 3128293
## Projected CRS: Albers
## First 10 features:
##           SITE_ID     COMID                       geometry
## 1  NLA17_VA-HP007  10062553 MULTIPOLYGON (((1671056 176...
## 2  NLA17_SC-10002 166756746 MULTIPOLYGON (((1412918 130...
## 3  NLA17_NY-10025  22026848 MULTIPOLYGON (((1579017 237...
## 4  NLA17_LA-10004  15108649 MULTIPOLYGON (((309181.1 77...
## 5  NLA17_SD-10053 120053557 MULTIPOLYGON (((-275133.1 2...
## 6  NLA17_TX-10004  19980072 MULTIPOLYGON (((-500846.9 1...
## 7  NLA17_NM-10049  17843236 MULTIPOLYGON (((-939685 157...
## 8  NLA17_ME-10019    806169 MULTIPOLYGON (((2105577 291...
## 9  NLA17_ME-10040   4290227 MULTIPOLYGON (((1997377 283...
## 10 NLA17_ME-10039   4287237 MULTIPOLYGON (((2041918 297...
```

Another common thing is to have a tabular dataset with Longitude and Latitude columns.  We can take that data and turn it into an `sf` object that we can then do spatial analysis with.  We have this with the 2017 NLA that we used as part of the `acesd_analysis.R` script.  Here is that code (with a few small changes).


```r
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

nla_2017_chem <- read_csv("data/nla_2017_water_chemistry_chla-data.csv", 
                          guess_max = 23000)
```

```
## Rows: 22873 Columns: 23
## ── Column specification ──────────────────────────────────────────────────────────────
## Delimiter: ","
## chr (17): PUBLICATION_DATE, SITE_ID, DATE_COL, STUDY, STATE, LAB, SAMPLE_TYPE, MAT...
## dbl  (6): UID, VISIT_NO, MDL, RL, HOLDING_TIME, LAB_SAMPLE_ID
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
nla_2017_chem_clean <- nla_2017_chem |>
  rename_all(tolower) |>
  select(uid:visit_no, state, analyte, result, nars_flag) |>
  filter(is.na(nars_flag), visit_no == 1) |>
  select(-nars_flag) |>
  mutate(date_col = dmy(date_col), 
         analyte = tolower(analyte),
         result = as(result, "numeric")) |>
  select(uid:date_col, year, month, day, state:result)
```

```
## Warning: There was 1 warning in `mutate()`.
## ℹ In argument: `result = as(result, "numeric")`.
## Caused by warning in `asMethod()`:
## ! NAs introduced by coercion
```

```
## Error in `select()`:
## ! Can't subset columns that don't exist.
## ✖ Column `year` doesn't exist.
```

```r
nla_2017_sites <- read_csv("https://www.epa.gov/sites/default/files/2021-04/nla_2017_site_information-data.csv")
```

```
## Rows: 5721 Columns: 80
## ── Column specification ──────────────────────────────────────────────────────────────
## Delimiter: ","
## chr (60): PUBLICATION_DATE, SITE_ID, DATE_COL, SITESAMP, UNIQUE_ID, STUDY, AG_ECO3...
## dbl (20): UID, VISIT_NO, AREA_HA, COMID, ELEVATION, FCODE, FEOW_ID, FRAME17_ID, GN...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
nla_wq_sites <- nla_2017_sites |>
  select(uid = UID, site_id = SITE_ID, cntyname = CNTYNAME, lon_dd83 = LON_DD83,
         lat_dd83 = LAT_DD83) |>
  filter(!is.na(uid)) |>
  left_join(nla_2017_chem_clean, by = c("uid" = "uid"), multiple = "all") |>
  pivot_wider(names_from = "analyte", values_from = "result")
```

### Note on coordinate reference systems

I gloss over the details of a coordinate reference system (CRS) in this lesson, but choosing and managing the CRS for a given analysis is something that should not be taken lightly.  The CRS will impact all of your spatial measurements (area, distance, shape, etc.) and whichever you choose will need to be appropriate to the goals of your analysis.  Furthermore, you will need to manage the CRS such that all of your spatial data have one defined and have been transformed into the same CRS.  I suggest you consult a GIS person for advice on this and I am happy to provide options and walk you through managing the CRS in R.

## Raster data with `terra`

## Geospatial data packages

## Static maps with `ggplot2` and `ggspatial`

## Interactive maps with `mapview`
