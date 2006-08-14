# $Id$

"rep.4.7" <- 
function(x, times, length.out, each)
{
	#
	# SDC 1999.05.24, 2002.08.07
	# Includes patch from Bill Dunlap at end (see S-news,  16 June 1994)
	#
	names(x) <- NULL
	if(!missing(each)) {
		x <- rep(x, rep.int(each, length(x)))
		if(missing(length.out) && missing(times))
			return(x)
	}
	xlen <- length(x)
	if(missing(length.out))
		if(xlen) {
			if(storage.mode(x) == "integer" && is.null(class(x)))
				rep.int(x, times)
			else x[rep.int(1.:xlen, times)]
		}
		else x
	else if(xlen) {
		if(length.out > xlen) {
			out <- x
			length(out) <- length.out
			.Options$warn <- -1.
			out[] <- x
			return(out)
		}
		else if(length.out == xlen)
			return(x)
		else {
			length(x) <- length.out
			return(x)
		}
	}
	else {
		#	PATCH: else Recall(NA, length.out)
		# length(x)==0 and length.out is supplied
		if(length.out) x[1.:length.out] else x
	}
}

