# $Id$

"print.listof" <- 
function(x, show.names = T, ...)
{
	nn <- names(x)
	ll <- length(x)
	iseq <- seq(length = ll)
	if(length(nn) != ll)
		nn <- paste("Component", iseq)
	for(i in iseq) {
		if(show.names)
			cat(nn[i], "\n")
		print(x[[i]], ...)
		cat("\n")
	}
	invisible(x)
}

