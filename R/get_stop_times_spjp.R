#' Build a data.frame mapping DayType to UicOperatingPeriods
#'
#' @param x NeTEx XML document
#' @param ns Namespace vector
#' @returns data.frame with columns: daytype_id, uic_id, from, to
build_daytype_map_df <- function(x, ns) {
  daytype_nodes <- xml_find_all(x, ".//d1:ServiceCalendarFrame//d1:ServiceCalendar//d1:dayTypes/d1:DayType", ns)

  df_list <- lapply(daytype_nodes, function(dt) {
    daytype_id <- xml_attr(dt, "id")
    parent_cal <- xml_parent(xml_parent(dt))
    uops <- xml_find_all(parent_cal, ".//d1:operatingPeriods/d1:UicOperatingPeriod", ns)

    if (length(uops) == 0) return(NULL)

    do.call(rbind, lapply(uops, function(u) {
      data.frame(
        daytype_id = daytype_id,
        uic_id = xml_attr(u, "id"),
        from = as.Date(substr(xml_text(xml_find_first(u, "d1:FromDate", ns)), 1, 10)),
        to   = as.Date(substr(xml_text(xml_find_first(u, "d1:ToDate", ns)), 1, 10)),
        valid_bits = xml_text(xml_find_first(u, "d1:ValidDayBits", ns)),
        stringsAsFactors = FALSE
      )
    }))
  })

  do.call(rbind, df_list)
}


#' Check if a date is active in a ValidDayBits string
#'
#' @param date Date to check
#' @param from_date FromDate of UicOperatingPeriod (Date)
#' @param valid_bits string of 0/1
#' @returns TRUE if date is active, FALSE otherwise
date_in_valid_bits <- function(date, from_date, valid_bits) {
  index <- as.integer(date - from_date) + 1
  if (index < 1 || index > nchar(valid_bits)) return(FALSE)
  substr(valid_bits, index, index) == "1"
}



#' Check if a ServiceJourney operates on a given date
#'
#' @param sj ServiceJourney xml_node
#' @param daytype_map_df Data.frame from build_daytype_map_df()
#' @param target_date Date object
#' @param ns Namespace vector
#' @returns TRUE if the journey operates on target_date, FALSE otherwise
journey_operates_on_date <- function(sj, daytype_map_df, target_date, ns) {
  dt_refs <- xml_attr(xml_find_all(sj, ".//d1:DayTypeRef", ns), "ref")
  if (length(dt_refs) == 0) return(TRUE) # valid every day if no DayTypeRef

  any(sapply(dt_refs, function(dt) {
    periods <- subset(daytype_map_df, daytype_id == dt)
    any(mapply(function(from, to, valid_bits) {
      date_in_valid_bits(target_date, from, valid_bits)
    }, periods$from, periods$to, periods$valid_bits))
  }))
}





#' Filter ServiceJourneys by target date
#'
#' @param journeys list of ServiceJourney nodes
#' @param daytype_map_df Data.frame from build_daytype_map_df()
#' @param target_date Date object
#' @param ns Namespace vector
#' @returns filtered list of journeys
filter_journeys_by_date <- function(journeys, daytype_map_df, target_date, ns) {
  Filter(function(sj) journey_operates_on_date(sj, daytype_map_df, target_date, ns), journeys)
}



#' Get stop times for a specific StopPointInJourneyPattern (modular & data.frame calendar)
#'
#' @param x NeTEx XML document
#' @param spjp_id StopPointInJourneyPattern ID
#' @param target_date Optional date (YYYY-MM-DD)
#' @returns data.frame with columns: spjp_id, stop_time, line, line_type
#'
#' @export
get_stop_times_spjp <- function(x, spjp_id, target_date = NULL) {
  ns <- xml_ns(x)

  # 1. Get line info
  line_info <- get_line_info(x, ns)

  # 2. Get all ServiceJourneys
  journeys <- xml_find_all(x, ".//d1:ServiceJourney", ns)

  # 3. Filter by date if provided
  if (!is.null(target_date)) {
    target_date <- as.Date(target_date)
    daytype_map_df <- build_daytype_map_df(x, ns)
    journeys <- filter_journeys_by_date(journeys, daytype_map_df, target_date, ns)
  }

  if (length(journeys) == 0) return(data.frame())

  # 4. Build times_df for all journeys
  times_df <- build_times_df(journeys, line_info$line, line_info$line_type, ns)

  # 5. Filter rows for the specific spjp_id
  times_df <- times_df[times_df$spjp_id == spjp_id, ]

  # 6. Sort by stop_time
  times_df[order(times_df$stop_time), ]
}

