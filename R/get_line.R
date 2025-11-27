#' Get the line information from a NeTEx file
#'
#' This function retrieves the Line, Name and TransportMode.
#' It works under the assumption that the `x` only contains information on one
#' line.
#'
#' @param x NeTEx XML file
#' @param ns Optionally, a named vector giving prefix-url pairs, as produced
#' by xml_ns().
#'
#' @returns a list of `line` and `line_type`.
#' @export
#'
#' @examples
#' bus_371 <- xml2::read_xml(netexr_example("NX-PI-01_DE_NAP_LINE_mvv_d-REGBU-85707123_20251117.xml"))
#' get_line_info(bus_371)
get_line_info <- function(x, ns = xml_ns(x)) {
  line_node <- xml_find_first(x, ".//d1:Line", ns)
  line_name <- gettext_ns(line_node, "./d1:Name", ns)
  line_type <- gettext_ns(line_node, "./d1:TransportMode", ns)
  list(line = line_name, line_type = line_type)
}

#' Extract one passing-time node
#'
#' @param p_node Passing Node
#' @param line_name Line Name
#' @param line_type Line Type
#' @param ns Namespace
#'
#' @returns data.frame
#' @export
#'
extract_one_passing <- function(p_node, line_name, line_type, ns) {

  spjp_id <- gettext_ns(p_node, "./d1:StopPointInJourneyPatternRef/@ref", ns)
  arr <- gettext_ns(p_node, "./d1:ArrivalTime", ns)
  dep <- gettext_ns(p_node, "./d1:DepartureTime", ns)
  stop_time <- ifelse(!is.na(arr), arr, dep)

  data.frame(
    spjp_id = spjp_id,
    stop_time = stop_time,
    line = line_name,
    line_type = line_type,
    stringsAsFactors = FALSE
  )
}


#' Process one ServiceJourney
#'
#' @param journey Journey
#' @param line_name Line Name
#' @param line_type Line Type
#' @param ns Namespace
#'
#' @returns data.frame
#' @export
#'
process_one_journey <- function(journey, line_name, line_type, ns) {
  # Find all passing nodes
  passing_nodes <- xml_find_all(journey, ".//d1:TimetabledPassingTime", ns)

  # Apply extract_one_passing to each node and combine results
  do.call(rbind, lapply(passing_nodes, function(node) {
    extract_one_passing(node, line_name, line_type, ns)
  }))
}



#' Build times_df for all journeys
#'
#' @param journeys Journeys
#' @param line_name Line Name
#' @param line_type Line Type
#' @param ns Namespace
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' bus_371 <- xml2::read_xml(netexr_example("NX-PI-01_DE_NAP_LINE_mvv_d-REGBU-85707123_20251117.xml"))
#' ns <- xml2::xml_ns(bus_371)
#'
#' # 1. Get the line info (since there is only one line)
#' line_info <- get_line_info(bus_371, ns)
#'
#' # 2. Get all journeys
#' journeys <- xml2::xml_find_all(bus_371, ".//d1:ServiceJourney", ns)
#'
#' # 3. Build the times dataframe
#' times_df <- build_times_df(journeys, line_info$line, line_info$line_type, ns)
build_times_df <- function(journeys, line_name, line_type, ns) {

  # Apply process_one_journey to each journey and combine the results
  do.call(rbind, lapply(journeys, function(journey) {
    process_one_journey(journey, line_name, line_type, ns)
  }))
}
