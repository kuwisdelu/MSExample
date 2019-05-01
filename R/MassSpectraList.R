
#' Mass spectra list objects
#'
#' \code{MassSpectraList} is a simple S4 class designed to
#' contain a list of \code{MassSpectrum} objects.
#'
#' @name MassSpectraList-class
#' @rdname MassSpectraList-class
#' 
#' @export
setClass("MassSpectraList", contains = "SimpleList")

setAs("ANY", "MassSpectraList",
	function(from) {
		to <- as(from, "SimpleList")
		to <- new("MassSpectraList", to,
			elementType="MassSpectrum")
		if ( validObject(to) )
			to
	})

