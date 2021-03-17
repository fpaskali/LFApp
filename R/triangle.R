#' Triangle algorithm
#'
#' @export

triangle <- function(image, offset = 0.2, breaks = 256) {
  image_hist <- hist(image, breaks = breaks, plot = FALSE)
  hist_counts <- image_hist$counts
  hist_breaks <- image_hist$breaks

  # Finds the locaton of peak, the peak value.
  arg_peak_height <- which.max(hist_counts)
  peak_height <- hist_counts[arg_peak_height]

  # Flip is true if the right tail is shorter.
  flip <- arg_peak_height > breaks / 2
  if (flip == TRUE) {
    hist_counts <- rev(hist_counts)
    arg_peak_height <- which.max(hist_counts)
  }

  # Width is the length from the position of the highest peak, to the end.
  width <- breaks - arg_peak_height
  heights <- hist_counts[arg_peak_height:breaks]
  # Scaling the heights to be equal to the width.
  normalizer <- peak_height / width
  heights <- heights / normalizer

  # The distance from highest peak and the each height. Used to measure the distance.
  triangle_side <- width - heights
  distances <- sqrt(2*triangle_side**2)/2
  # Finding the position of the maximal distance.
  arg_max_distance <- which.max(distances)
  # Adding the fixed offset to the maximal distance position.
  arg_threshold <- arg_peak_height + arg_max_distance + width*offset

  return(hist_breaks[arg_threshold])
}
