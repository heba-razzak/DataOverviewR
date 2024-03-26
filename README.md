# createDataDict R package

The goal of createDataDict is to easily create a data dictionary for any
data frame.

## Installation

You can install the development version of createDataDict from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("heba-razzak/createDataDict")
library(createDataDict)
```

## Example

``` r
descriptions = descriptions_df(airquality)

descriptions <- update_description(descriptions,
                                   c("Ozone",
                                     "Solar.R"),
                                   c("numeric   Ozone (ppb)",
                                     "numeric   Solar R (lang)"))

print_data_dict(airquality, data_title = "Air Quality", descriptions = descriptions)
```

## **Air Quality**

**Number of rows:** `153`

| Variable | Type    |  NA |    %NA | Unique | Description            |
|:---------|:--------|----:|-------:|-------:|:-----------------------|
| Ozone    | integer |  37 | 24.18% |     68 | numeric Ozone (ppb)    |
| Solar.R  | integer |   7 |  4.58% |    118 | numeric Solar R (lang) |
| Wind     | numeric |   0 |  0.00% |     31 |                        |
| Temp     | integer |   0 |  0.00% |     40 |                        |
| Month    | integer |   0 |  0.00% |      5 |                        |
| Day      | integer |   0 |  0.00% |     31 |                        |
