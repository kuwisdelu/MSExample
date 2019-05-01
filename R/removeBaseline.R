
#' Remove baseline from mass spectra
#'
#' @param object A mass spectrum
#' @param ... Additional arguments
#
#' @return A \code{MassSpectrum} object with baseline removed
#'
#' @export
setMethod("removeBaseline", "MassSpectrum",
	function(object, ...) {
		b <- .estimateBaseline(object@intensity)
		object@intensity <- pmax(object@intensity - b, 0)
		if ( validObject(object) )
			object
	})

.estimateBaseline <- function(x, ...) {
	i <- locmax(-x, ...)
	b <- x[i]
	if ( i[1L] != 1L ) {
		b <- c(b[1L], b)
		i <- c(1L, i)
	}
	if ( i[length(i)] != length(x) ) {
		b <- c(b, b[length(b)])
		i <- c(i, length(x))
	}
	approx(i, b, seq_along(x))$y
}

