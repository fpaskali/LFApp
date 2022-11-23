threshold_li <- function(image, tolerance=NULL, initial_guess=NULL, iter_callback=NULL) {
  # For Li's algorithm to work, the image should be positive
  image_min <- min(image)
  image <- image - image_min
  # Tolerance has to be positive or there is a risk of while loop to be infinite.
  if (is.null(tolerance)) {
    tolerance <- abs(min(diff(as.vector(image))) / 2)
  }
  # Initial estimate for iterations
  if (is.null(initial_guess)) {
    t_next <- mean(image)
  } else if (is(initial_guess,"function")){
    t_next <- initial_guess(image)
  } else if (is.numeric(initial_guess)) {
    t_next <- initial_guess - image_min
    image_max <- max(image) + image_min
    if (!t_next>0 & !t_next<max(image)) {
      stop(sprintf("The threshold_li must be greater than 0 and lesser than max
                   value of the image. threshold_li is %s", threshold_li))
    } else {
      stop("The initial_guess has incorrect class. It should be numeric or a 
           function that returns numeric value.")
    }
  }
  # The difference between t_next and t_curr should be equal to the tolerance.
  t_curr <- tolerance * (-2)
  
  if (!is.null(iter_callback)) {
    iter_callback(t_next + image_min)
  } 
  
  # Stop the iteration when the difference between the new and old threshold
  # value is less than the tolerance
  while(abs(t_next - t_curr) > tolerance) {
    t_curr <- t_next
    foreground <- (image > t_curr)
    mean_fore <- mean(image[foreground])
    mean_back <- mean(image[!foreground])

    t_next <- ((mean_back - mean_fore) /
               (log(mean_back) - log(mean_fore)))

    if (!is.null(iter_callback)) {
      iter_callback(t_next + image_min)
    }
  }
  threshold <- t_next + image_min
  threshold
}
