#' Read TrackMate XML into an aniframe
#'
#' Parses a TrackMate XML file and returns spot data from filtered tracks
#' as an aniframe.
#'
#' @param path Path to the TrackMate XML file.
#' @param slim If TRUE, return only essential columns (default TRUE).
#'
#' @return An aniframe with columns including time, x, y, z, frame, and track_id.
#' @export
read_trackmate <- function(path, slim = TRUE) {
  # Check the file
  validate_files(path, expected_suffix = "xml")

  # Check that the xml2 package is installed
  check_xml2()

  # Read file
  xml <- xml2::read_xml(path)

  track_nodes <- xml2::xml_find_all(xml, ".//Track")
  if (length(track_nodes) == 0) {
    cli::cli_abort("No tracks found in XML file.")
  }

  filtered_ids <- xml2::xml_find_all(xml, ".//TrackID") |>
    xml2::xml_attr("TRACK_ID")

  if (length(filtered_ids) == 0) {
    cli::cli_abort("No filtered tracks found in XML file.")
  }

  # Report units
  model_node <- xml2::xml_find_first(xml, ".//Model")
  spatial_units <- xml2::xml_attr(model_node, "spatialunits")
  spatial_units <- if (spatial_units == "pixel") {
    "px"
  }
  time_units <- xml2::xml_attr(model_node, "timeunits")
  time_units <- if (time_units == "sec") {
    "s"
  }

  # if (spatial_units == "pixel") {
  # 	cli::cli_warn(
  # 		"Spatial units are in pixels. Consider transforming to real units."
  # 	)
  # }
  # cli::cli_alert_info("Units: {spatial_units}, {time_units}")

  # Extract all spot attributes in one call
  spot_nodes <- xml2::xml_find_all(xml, ".//AllSpots//SpotsInFrame//Spot")
  spots <- extract_spot_attrs(spot_nodes, slim)

  # Build spot-to-track mapping (only for filtered tracks)
  spot_track_map <- build_spot_track_map(track_nodes, filtered_ids)

  # Join and arrange
  result <- spot_track_map |>
    dplyr::inner_join(spots, by = "spot_id") |>
    dplyr::select(-"spot_id")

  # Check for duplicates
  dupe_count <- sum(duplicated(result[, c("individual", "frame")]))
  if (dupe_count > 0) {
    cli::cli_warn(
      "Detected {dupe_count} duplicate track-frame combinations."
    )
  }

  cli::cli_alert_success(
    "Loaded {nrow(result)} spots from {dplyr::n_distinct(result$individual)} tracks."
  )

  data <- aniframe::as_aniframe(result) |>
    aniframe::set_metadata(
      source = "trackmate",
      filename = basename(path),
      unit_time = "s", #time_units,
      unit_space = "mm" #spatial_units
    )

  if (length(unique(data$z)) == 1) {
    data <- data |>
      dplyr::select(-"z") |>
      aniframe::set_metadata(
        coordinate_system = "cartesian_2d"
      )
  } else {
    data <- data |>
      aniframe::set_metadata(
        coordinate_system = "cartesian_3d"
      )
  }

  # Remove the frame column if there are time stamps
  if (!all(is.na(data$time))) {
    data <- data |>
      dplyr::select(-frame)
  }

  data
}


#' Extract spot attributes efficiently
#'
#' @param spot_nodes XML nodeset of Spot elements.
#' @param slim If TRUE, extract only essential columns.
#'
#' @return A data.frame of spot attributes.
#' @noRd
extract_spot_attrs <- function(spot_nodes, slim) {
  # Pull all attributes at once - much faster than multiple xml_attr calls
  all_attrs <- xml2::xml_attrs(spot_nodes)

  # Define columns to extract
  core_cols <- c(
    "ID",
    "POSITION_X",
    "POSITION_Y",
    "POSITION_Z",
    "POSITION_T",
    "FRAME"
  )
  extra_cols <- c("RADIUS", "QUALITY")
  cols <- if (slim) core_cols else c(core_cols, extra_cols)

  # Extract as matrix then convert
  mat <- vapply(all_attrs, function(x) x[cols], character(length(cols)))
  spots <- as.data.frame(t(mat), stringsAsFactors = FALSE)
  names(spots) <- c(
    "spot_id",
    "x",
    "y",
    "z",
    "time",
    "frame",
    if (!slim) c("radius", "quality")
  )

  # Type conversion
  num_cols <- setdiff(names(spots), "spot_id")
  spots[num_cols] <- lapply(spots[num_cols], as.numeric)
  spots$frame <- as.integer(spots$frame)

  spots
}


#' Build a mapping from spot IDs to track IDs
#'
#' @param track_nodes XML nodeset of Track elements.
#' @param filtered_ids Character vector of filtered track IDs to include.
#'
#' @return A data.frame with spot_id and track_id columns.
#' @noRd
build_spot_track_map <- function(track_nodes, filtered_ids) {
  # Pre-filter to only process tracks we care about
  track_ids <- xml2::xml_attr(track_nodes, "TRACK_ID")
  keep <- track_ids %in% filtered_ids
  track_nodes <- track_nodes[keep]
  track_ids <- track_ids[keep]

  # Process each track
  lapply(seq_along(track_nodes), function(i) {
    edge_nodes <- xml2::xml_find_all(track_nodes[[i]], ".//Edge")
    source_ids <- xml2::xml_attr(edge_nodes, "SPOT_SOURCE_ID")
    target_ids <- xml2::xml_attr(edge_nodes, "SPOT_TARGET_ID")
    spot_ids <- unique(c(source_ids, target_ids))

    data.frame(
      spot_id = spot_ids,
      individual = track_ids[[i]], # This should be changed to "track" once the tidy movement syntax is implemented in aniframe
      keypoint = "centroid",
      stringsAsFactors = FALSE
    )
  }) |>
    dplyr::bind_rows()
}
