
#' Mass spectrum objects
#'
#' \code{MassSpectrum} is a simple S4 class designed to
#' encapsulate basic information about a mass spectrum.
#'
#' @param object A \code{MassSpectrum} object
#' @param value A replacement value
#' @param x A \code{MassSpectrum} object
#' @param i indices specifying elements to extract
#' @param j indices specifying elements to extract
#' @param drop ignored
#' @param ... Additional arguments
#' 
#' @slot mz The m/z-values.
#' @slot intensity The intensities observed
#' 					at the associated m/z values.
#' @slot isCentroided Whether the spectrum has
#'						been centroided or not.
#' @slot peaks The indices of any detected peaks
#'
#' @name MassSpectrum-class
#' @rdname MassSpectrum-class
#' 
#' @export
setClass("MassSpectrum",
	contains = "Vector",
	slots = c(
		mz = "numeric",
		intensity = "numeric",
		isCentroided = "logical",
		peaks = "integer"))

.valid_MassSpectrum <- function(object) {
	errors <- NULL
	if ( is.unsorted(object@mz) )
		errors <- c(errors , "mz must be sorted in increasing order")
	if ( length(object@mz) != length(object@intensity) ) {
		errors <- c(errors , paste0("length of mz [",
			length(object@mz), "] must match length of intensity [",
			length(object@intensity), "]"))
	}
	outofrange <- object@peaks < 1L | object@peaks > length(object@mz)
	if ( length(object@peaks) > 0L && any(outofrange) ) {
		errors <- c(errors , paste0("out-of-range peak indices [",
			object@peaks[outofrange]))
	}
	if ( is.null(errors) ) TRUE else errors
}

setValidity("MassSpectrum", .valid_MassSpectrum)

#' Create a \code{MassSpectrum} object.
#'
#' @param mz A \code{numeric} vector of m/z values.
#' @param intensity A \code{numeric} vector of intensities.
#' @param isCentroided A \code{logical} value indicating whether
#' 					the spectrum has been centroided or not.
#'
#' @return object A \code{MassSpectrum} object.
#' @export
MassSpectrum <- function(mz, intensity, isCentroided = FALSE) {
	if ( is.unsorted(mz) ) {
		i <- order(mz)
		intensity <- intensity[i]
		mz <- mz[i]
	}
	new("MassSpectrum", mz=mz, intensity=intensity,
		isCentroided=isCentroided, peaks=integer())
}

#' @export
#' @describeIn MassSpectrum Get m/z values
setMethod("mz", "MassSpectrum",
	function(object, ...) object@mz)

#' @export
#' @describeIn MassSpectrum Set m/z values
setReplaceMethod("mz", "MassSpectrum",
	function(object, value) {
		object@mz <- value
		if ( validObject(object) )
			object
	})

#' @export
#' @describeIn MassSpectrum Get intensities
setMethod("intensity", "MassSpectrum",
	function(object, ...) object@intensity)

#' @export
#' @describeIn MassSpectrum Get indices of peaks
setMethod("peaks", "MassSpectrum",
	function(object, ...) object@peaks)

#' @export
#' @describeIn MassSpectrum Get whether the spectrum is centroided
setMethod("isCentroided", "MassSpectrum",
	function(object, ...) object@isCentroided)

#' @export
#' @describeIn MassSpectrum Get the total ion count
setMethod("tic", "MassSpectrum",
	function(object, ...) {
		sum(intensity(object))
	})

#' @export
#' @describeIn MassSpectrum Get the length of the spectrum
setMethod("length", "MassSpectrum",
	function(x) {
		length(mz(x))
	})

#' @export
#' @describeIn MassSpectrum Subset a mass spectrum
setMethod("[", "MassSpectrum",
	function(x, i, ...) {
		len <- length(x)
		if ( length(peaks(x)) > 0L ) {
			p <- replace(logical(len), peaks(x), TRUE)[i]
			x@peaks <- which(p)
		}
		x@mz <- x@mz[i]
		x@intensity <- x@intensity[i]
		if ( validObject(x) )
			x
	})

setMethod("show", "MassSpectrum",
	function(object) {
		cat("Mass Spectrum\n")
		mzr <- paste0(range(mz(object)), collapse=" to ")
		cat("  m/z range:", mzr, "\n")
		cat("  centroided:", isCentroided(object), "\n")
		cat("  length:", length(object), "\n")
		if ( length(peaks(object)) > 0L )
			cat("  num peaks:", length(peaks(object)), "\n")
		cat("  tic:", tic(object), "\n")
	})


