
#' Bin peaks in mass spectra
#'
#' @param object A mass spectrum
#' @param binwidth The bin width
#' @param units The bin width untis
#' @param ... Additional arguments
#
#' @return A binned \code{MassSpectrum} object
#'
#' @export
setMethod("binPeaks", "MassSpectrum",
	function(object, binwidth = 1000, units=c("ppm", "mz"), ...)
	{
		units <- match.arg(units)
		mzr <- range(object@mz)
		if ( units == "ppm" ) {
			m <- seq_ppm(from=mzr[1], to=mzr[2], ppm=binwidth / 2)
			tol <- (1e-6 * binwidth / 2)
			bins <- c(m[1] - m[1] * tol, m + m * tol)
		} else {
			m <- seq(from=mzr[1], to=mzr[2], by=binwidth)
			bins <- c(m[1] - binwidth / 2, m + binwidth / 2)
		}
		i <- object@peaks
		if ( length(i) == 0L ) {
			p <- numeric(length(m))
			out <- MassSpectrum(mz=m, intensity=p, isCentroided=TRUE)
		} else {
			p <- .binPeaks(object@intensity[i], object@mz[i], bins)
			out <- MassSpectrum(mz=m, intensity=p, isCentroided=TRUE)
			out@peaks <- which(p != 0)
		}
		out
	})

.binPeaks <- function(x, t, bins) {
	y <- numeric(length(bins) - 1L)
	i <- findInterval(t, bins)
	y[i] <- x
	y
}

