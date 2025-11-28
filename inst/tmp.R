library("devtools")
library("xml2")

document()
build()
install()
# check()

bus_371 <- xml2::read_xml(netexr_example("NX-PI-01_DE_NAP_LINE_mvv_d-REGBU-85707123_20251117.xml"))
ns <- xml2::xml_ns(bus_371)

unique(xml_name(xml_find_all(bus_371, ".//*")))

xml_find_all(bus_371, ".//d1:ServiceFrame", ns)

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

b <- subset(result, stop_point_name == "Baiernrain")


get_stop_times_spjp(bus_371, spjp_id = b$spjp_id[1],
                    target_date = "2025-11-28")
get_stop_times_spjp(bus_371, spjp_id = b$spjp_id[3],
                    target_date = "2025-11-28")
st_target_date <- lapply(unique(b$spjp_id),
                         function(spjp_id)
                           get_stop_times_spjp(bus_371,
                                               spjp_id,
                                               target_date = "2025-11-28")
)
stop_times_target_date <- do.call(rbind, )

