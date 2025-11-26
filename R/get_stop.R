#' Extract one stop point
#'
#' @param s_node ScheduledStopPoint XML node
#' @param ns Namespace
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' bus_371 <- xml2::read_xml(netexr_example("NX-PI-01_DE_NAP_LINE_mvv_d-REGBU-85707123_20251117.xml"))
#'
#' ssp <- xml_find_all(
#'   bus_371,
#'   './/d1:ScheduledStopPoint[d1:Name = "Baiernrain"]',
#'   ns = xml_ns(bus_371)
#' )
#' extract_one_stop(ssp[[1]])
#' extract_one_stop(ssp[[2]])
#'
#' stop_df <- build_stop_df(bus_371)
#' head(stop_df)
#'
extract_one_stop <- function(s_node, ns = xml_ns(s_node)) {
  data.frame(
    scheduled_stop_id = xml_attr(s_node, "id"),
    stop_point_name   = gettext_ns(s_node, "./d1:Name", ns),
    stop_type         = gettext_ns(s_node, "./d1:StopType", ns),
    latitude          = gettext_ns(s_node, ".//d1:Location/d1:Latitude | .//d1:Centroid/d1:Location/d1:Latitude", ns),
    longitude         = gettext_ns(s_node, ".//d1:Location/d1:Longitude | .//d1:Centroid/d1:Location/d1:Longitude", ns),
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
#' stop_df <- build_stop_df(bus_371)
#' head(stop_df)
build_stop_df <- function(x, ns = xml_ns(x)) {
  ssp <- xml_find_all(x, ".//d1:ScheduledStopPoint", ns)

  do.call(rbind, lapply(ssp, function(s_node) {
    extract_one_stop(s_node, ns)
  }))
}
