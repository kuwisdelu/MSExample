
#' Find peaks in mass spectra
#'
#' @param object A mass spectrum
#' @param SNR Signal-to-noise ratio
#' @param ... Additional arguments
#
#' @return A \code{MassSpectrum} object with peaks
#'
#' @export
setMethod("findPeaks", "MassSpectrum",
	function(object, SNR = 6, ...) {
		p <- .findPeaks(object@intensity, SNR=SNR, ...)
		object@peaks <- p
		if ( validObject(object) )
			object
	})

.findPeaks <- function(x, SNR, ...) {
	p1 <- locmax(x, ...)
	i <- seq_along(x)
	noise <- supsmu(i, x)$y
	p2 <- which(x / noise > SNR)
	intersect(p1, p2)
}

