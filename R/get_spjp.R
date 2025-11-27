#' Extract one stop point StopPointInJourneyPattern
#'
#' Extract information about the StopPointInJourneyPattern (`spjp_id` and `scheduled_stop_id`)
#'
#' @param spjp_node StopPointInJourneyPattern xml node
#' @param ns Namespace
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' bus_371 <- xml2::read_xml(netexr_example("NX-PI-01_DE_NAP_LINE_mvv_d-REGBU-85707123_20251117.xml"))
#'
#' spjp <- xml2::xml_find_first(bus_371, ".//d1:StopPointInJourneyPattern",
#'                              ns = xml2::xml_ns(bus_371))
#'
#' xml2::xml_name(spjp)
#'
#' extract_one_spjp(spjp)
#'
#' spjp_df <- build_spjp_df(bus_371)
#' head(spjp_df)
#'
extract_one_spjp <- function(spjp_node, ns = xml_ns(spjp_node)) {
  data.frame(
    spjp_id           = xml_attr(spjp_node, "id"),
    scheduled_stop_id = gettext_ns(spjp_node, "./d1:ScheduledStopPointRef/@ref", ns),
    stringsAsFactors = FALSE
  )
}


#' Stop point information for all scheduled stop points
#'
#' @param x NeTEx XML file
#' @param ns Namespace
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' bus_371 <- xml2::read_xml(netexr_example("NX-PI-01_DE_NAP_LINE_mvv_d-REGBU-85707123_20251117.xml"))
#'
#' # One stop point
#' spjp <- xml2::xml_find_first(bus_371, ".//d1:StopPointInJourneyPattern",
#'                              ns = xml2::xml_ns(bus_371))
#' xml2::xml_name(spjp)
#' extract_one_spjp(spjp)
#'
#' # All stop points
#' spjp_df <- build_spjp_df(bus_371)
#' head(spjp_df)
build_spjp_df <- function(x, ns = xml_ns(x)) {
  spjp <- xml_find_all(x, ".//d1:StopPointInJourneyPattern", ns)

  do.call(rbind, lapply(spjp, function(node) {
    extract_one_spjp(node, ns)
  }))
}
