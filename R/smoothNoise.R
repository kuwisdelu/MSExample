
#' Smooth mass spectra
#'
#' @param object A mass spectrum
#' @param halfWindow Smoothing window half-size
#' @param ... Additional arguments
#
#' @return A smoothed \code{MassSpectrum} object
#'
#' @aliases smoothNoise
#' @export
setMethod("smoothNoise", "MassSpectrum",
	function(object, halfWindow = 2, ...) {
		s <- .gaussianFilter(object@intensity, halfWindow=halfWindow)
		object@intensity <- s
		if ( validObject(object) )
			object
	})

.gaussianFilter <- function(x, halfWindow, ...) {
	sd <- ((halfWindow * 2) + 1) / 4
	kernel <- dnorm((-halfWindow):halfWindow, sd=sd)
	x2 <- c(rep(x[1], halfWindow), x, rep(x[length(x)], halfWindow))
	convolve(x2, kernel, type="filter")
}
