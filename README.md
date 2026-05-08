# myutils

## Description
`myutils` is a personal R helper package to store and use functions related to my animal movement research and workflows. It is currently a work in progress, and will be regularly updated with new functionality.
The package is still evolving, but functions are grouped below by workflow so the structure stays readable.

## Installation
Install from GitHub with:

```r
remotes::install_github("nedschofield/myutils")
library(myutils)
```

## Movement tools
Functions for coercing SPOT gps data into R and filtering data.
1. read SPOT exports into R 
2. verify no duplicates or missing values
3. inspect and remove possible outliers with `ctmm`
4. convert back to `data.table`
5. optionally export a Movebank-ready table

A typical workflow would look something like:

```r
gps <- spot_multi_to_dt(paths)

lapply(gps, dt_is_time_ordered_no_duplicates)
lapply(gps, dt_has_no_empty_points)

tel <- dt_to_ctmm(gps, crs = "EPSG:7852")
# inspect possible outliers with ctmm::outlie(). See ctmm documentation for details
gps_clean <- ctmm_to_dt(tel)

dt_to_movebank_tab(
  dt = gps_clean,
  out.path = "outputs/table/movebank_upload.txt"
)
```

## Spatial tools
tba
