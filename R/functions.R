library(sf)
library(dplyr)

convert_points_to_lines <- function(points_df) {
  # Function to convert the geometry string to coordinates
  parse_coords <- function(geom_str) {
    # The geometry is already in R format, so we can evaluate it directly
    coords <- eval(parse(text = geom_str))
    return(coords)
  }

  # Convert to sf points and group by row_id
  lines_sf <- points_df %>%
    # Create coordinate pairs
    mutate(coords = lapply(geometry, parse_coords)) %>%
    # Split into begin and end points
    group_by(row_id) %>%
    summarize(
      begin_coords = coords[point_type == "begin"][[1]],
      end_coords = coords[point_type == "end"][[1]]
    ) %>%
    ungroup() %>%
    # Create line geometries
    mutate(geometry = mapply(function(start, end) {
      # Create matrix of coordinates for the line
      coords_matrix <- matrix(c(start, end), ncol = 2, byrow = TRUE)
      # Create linestring
      st_linestring(coords_matrix)
    }, begin_coords, end_coords, SIMPLIFY = FALSE)) %>%
    # Convert to sf object
    st_sf(geometry = .) %>%
    # Add back the row_id
    mutate(line_id = row_id)

  # Set the CRS to WGS84 since these are lon/lat coordinates
  st_crs(lines_sf) <- 4326

  return(lines_sf)
}

# Usage example with your dataframe
# lines_sf <- convert_points_to_lines(your_dataframe)
