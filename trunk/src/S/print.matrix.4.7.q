# $Id$

"print.matrix.4.7" <- 
function(x, digits = NULL, quote = T, right = F, abbreviate.labels = F, ...)
{
	if(!missing(digits)) {
		if((length(digits) != 1. || digits < 1.) || digits > 20.)
			warning("Bad value for digits argument, options(\"digits\") used instead"
				)
		else {
			d <- options(digits = digits)
			on.exit(options(d))
		}
	}
	class(x) <- NULL
	if(is.atomic(x)) {
		dn <- dimnames(x)
		clab <- dn[[2.]]
		if(length(clab)) {
			nd <- switch(mode(x),
				numeric = if(missing(digits)) .Options$digits
					 else digits,
				character = max(nchar(x)) + 2.,
				complex = 2. * (if(missing(digits)) .Options$
						digits else digits),
				2.)
			if(abbreviate.labels && max(nchar(clab)) > nd + 3.)
				clab <- abbreviate(clab, c(nd, nd + 3.))
		}
		prmatrix(x, rowlab = dn[[1.]], collab = clab, quote = quote,
			right = right)
	}
	else {
		class(x) <- NULL
		n <- length(x)
		ot <- xm <- character(n)
		xl <- numeric(n)
		for(i in seq(length = n)) {
			xx <- x[[i]]
			ll <- length(xx)
			if(ll == 1.)
				ot[i] <- format(xx)
			else ot[i] <- paste(data.class(xx), ll, sep = ", ")
		}
		dim(ot) <- dim(x)
		dimnames(ot) <- dimnames(x)
		prmatrix(ot, quote = F, right = right)
	}
	invisible(x)
}

