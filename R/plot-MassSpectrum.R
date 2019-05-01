
#' Plot a mass spectrum
#' 
#' Plots one or two mass spectra.
#' 
#' @param x A mass spectrum to plot
#' @param y A second mass spectrum to plot
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param type The type of plot to draw
#' @param col The plot color
#' @param peaklabels Number of peaks to label
#' @param add Add to an existing plot?
#' @param ... Arguments passed to plot function
#' 
#' @export
#' @rdname plot-MassSpectrum
setMethod("plot", c("MassSpectrum", "missing"),
	function(x,
		xlab = expression(italic(m/z)),
		ylab = expression(italic(Intensity)),
		type = if (isCentroided(x)) 'h' else 'l',
		col = "darkcyan",
		peaklabels = 10,
		add = FALSE, ...)
	{
		# set up blank canvas
		if ( !add ) {
			plot(range(mz(x)), range(intensity(x)),
				xlab=xlab, ylab=ylab, type='n', ...)
			# draw reference line
			abline(h=0, lwd=0.5)
		}
		# draw spectrum
		lines(mz(x), intensity(x), type=type, col=col, ...)
		p <- peaks(x)
		# draw peaks
		if ( length(p) > 0L ) {
			lines(mz(x)[p], intensity(x)[p],
				type='h', col="red", lwd=1.5)
			plab <- p[order(intensity(x)[p], decreasing=TRUE)]
			plab <- sort(head(plab, n=peaklabels))
			text(mz(x)[plab], intensity(x)[plab],
				labels=round(mz(x)[plab], digits=4),
				pos=3, offset=0.1, cex=0.5)
		}
	})

#' @export
#' @rdname plot-MassSpectrum
setMethod("plot", c("MassSpectrum", "MassSpectrum"),
	function(x, y,
		xlab = expression(italic(m/z)),
		ylab = expression(italic(Intensity)),
		type = if (isCentroided(x)) 'h' else 'l',
		col = c("darkcyan", "darkred"),
		peaklabels = 10,
		add = FALSE, ...)
	{
		# set up blank canvas
		if ( !add ) {
			plot(range(mz(x), mz(y)), range(intensity(x), -intensity(y)),
				xlab=xlab, ylab=ylab, type='n', ...)
			# draw reference line
			abline(h=0, lwd=0.5)
		}
		# draw spectrum
		col <- rep_len(col, 2)
		lines(mz(x), intensity(x), type=type, col=col[1], ...)
		lines(mz(y), -intensity(y), type=type, col=col[2], ...)
		p1 <- peaks(x)
		p2 <- peaks(y)
		# draw peaks
		if ( length(p1) > 0L ) {
			lines(mz(x)[p1], intensity(x)[p1],
				type='h', col="red", lwd=1.5)
			p1lab <- p1[order(intensity(x)[p1], decreasing=TRUE)]
			p1lab <- sort(head(p1lab, n=peaklabels))
			text(mz(x)[p1lab], intensity(x)[p1lab],
				labels=round(mz(x)[p1lab], digits=4),
				pos=3, offset=0.1, cex=0.5)
		}
		if ( length(p2) > 0L ) {
			lines(mz(y)[p2], -intensity(y)[p2],
				type='h', col="red", lwd=1.5)
			p2lab <- p2[order(intensity(y)[p2], decreasing=TRUE)]
			p2lab <- sort(head(p2lab, n=peaklabels))
			text(mz(y)[p2lab], -intensity(y)[p2lab],
				labels=round(mz(y)[p2lab], digits=4),
				pos=1, offset=0.1, cex=0.5)
		}
	})

