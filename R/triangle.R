triangle <- function(image, offset = 0.2, breaks = 256) {
  if(!is(image, "Image"))
    stop("'image' must be of class 'Image'!")
  breaks <- as.integer(breaks)
  stopifnot(offset >= 0)
  stopifnot(breaks > 0)
  
  ## compute histogram and extract counts and breaks
  rg <- range(image)
  bins <- breaks
  breaks <- seq(rg[1], rg[2], length = breaks + 1L)
  image.hist <- hist.default(imageData(image), breaks = breaks, plot = FALSE)
  hist.counts <- image.hist$counts
  hist.breaks <- image.hist$breaks
  
  ## centers of bins
  delta <- hist.breaks[2]-hist.breaks[1]
  hist.bins <- hist.breaks[-(bins+1)] + delta/2
  
  ## location of peaks and peak value
  ind.peaks <- which(hist.counts == max(hist.counts))
  ind.first.peak <- ind.peaks[1]
  ind.last.peak <- ind.peaks[length(ind.peaks)]
  peak.height <- hist.counts[ind.first.peak]
  
  ## fist and last bin with positive count
  pos.counts <- which(hist.counts > 0)
  ind.low <- pos.counts[1]
  ind.high <- pos.counts[length(pos.counts)]
  
  if((ind.first.peak - ind.low) < (ind.high - ind.last.peak)){
    ## right tail is longer
    sel <- ind.last.peak:ind.high
    norm.counts <- hist.counts[sel]/peak.height
    norm.bins <- (1:length(sel))/length(sel)
    distances <- (1-norm.counts)*(1-norm.bins)/sqrt((1-norm.counts)^2 + (1-norm.bins)^2)
  }else{
    ## left tail is longer
    sel <- ind.low:ind.first.peak
    norm.counts <- hist.counts[sel]/peak.height
    norm.bins <- (1:length(sel))/length(sel)
    distances <- (1-norm.counts)*norm.bins/sqrt((1-norm.counts)^2 + norm.bins^2)
  }
  ind.max <- which.max(distances)
  
  hist.bins[sel[ind.max]] + offset
}
