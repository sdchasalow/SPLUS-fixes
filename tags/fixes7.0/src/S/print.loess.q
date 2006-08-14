# $Id$

"print.loess" <- 
function(x, ...)
{
	print(summary(x), ...)
	invisible(x)
}

