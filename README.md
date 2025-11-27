
# NeTExR
The goal of NeTExR is to read and work with NeTEx data.

This package is under development and I am not a NeTEx expert. **Please use with caution!**

## Installation

You can install the development version of NeTExR from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("HeidiSeibold/NeTExR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library("NeTExR")

bus_371 <- xml2::read_xml(netexr_example("NX-PI-01_DE_NAP_LINE_mvv_d-REGBU-85707123_20251117.xml"))
ns <- xml2::xml_ns(bus_371)

## Times
# 1. Get the line info (since there is only one line)
line_info <- get_line_info(bus_371, ns)

# 2. Get all journeys
journeys <- xml2::xml_find_all(bus_371, ".//d1:ServiceJourney", ns)

# 3. Build the times dataframe
times_df <- build_times_df(journeys, line_info$line, line_info$line_type, ns)
head(times_df)

## Stop point journey
spjp_df <- build_spjp_df(bus_371)
head(spjp_df)

## Stop
stop_df <- build_stop_df(bus_371)
head(stop_df)

## Join all
result <- merge(times_df, spjp_df, by = "spjp_id", all.x = TRUE, sort = FALSE)
result <- merge(result,   stop_df, by = "scheduled_stop_id", all.x = TRUE, sort = FALSE)
head(result)
```

