#' delineate_watershed_cities
#'
#' This function discretize a numeric vector into an interval factor
#'
#' @param smooth_rast_file a path to the input smoothed raster file.
#' @keywords delineation, watershed
#' @export
#' @examples
#' To be added
#'

delineate_watershed_cities <- function( smooth_rast_file ) {

  require(sf)
  require(terra)
  require(whitebox)

  # Invert the surface data ----
  inv_smooth_rast_file <- tempfile(fileext=".tif")
  wbt_negate(smooth_rast_file, inv_smooth_rast_file)

  # Find pour points
  d8_pntr_inverted_smoothed_file <- tempfile(fileext = ".tif")
  wbt_d8_pointer(dem = inv_smooth_rast_file, output=d8_pntr_inverted_smoothed_file)

  d8_pntr_smoothed_file <- tempfile(fileext = ".tif")
  wbt_d8_pointer(dem = smooth_rast_file, output=d8_pntr_smoothed_file)

  pour_points <- (rast(d8_pntr_inverted_smoothed_file) == 0 & rast(d8_pntr_smoothed_file) != 0)
  pour_points[!pour_points] <- NA
  pour_points <- pour_points |> as.points() |> st_as_sf() |> st_geometry()

  pour_points_file <- tempfile(fileext=".shp")
  st_write(pour_points, pour_points_file, append=F, quiet=T)

  # Delineate the watershed boundaries ----

  ws_rast_file <- tempfile(fileext = ".tif")
  wbt_watershed(d8_pntr = d8_pntr_inverted_smoothed_file,
                pour_pts = pour_points_file,
                output = ws_rast_file)

  # Convert the boundary rasters to polygons ----

  ws_poly <- rast(ws_rast_file) |> as.polygons() |> st_as_sf()

  # Add pour point locations to each watershed polygon. ----
  # These are used when we merge watershed polygons stretching across tile boundaries.
  ws_poly <- ws_poly |> st_join(pour_points |> mutate(
    pp_X = st_coordinates(pour_points)[,1] |> round(digits=1),
    pp_Y = st_coordinates(pour_points)[,2] |> round(digits=1)))

  ws_poly[c('pp_X', 'pp_Y')]
}
