# $Id$

"print.summary.varcomp" <- 
function(x, ...)
{
	cat("Call:\n")
	print(x$call, ...)
	cat("\nVariance Estimates:\n")
	print(x$varsum, ...)
	cat("\nMethod: ", x$method, "\n")
	if(length(x$cov.ran)) {
		cat("\nApproximate Covariance Matrix of Variance Estimates:\n")
		print(zapsmall(x$cov.ran), ...)
	}
	cat("\nCoefficients:\n")
	if(ns <- attr(x$coefficients, "singular"))
		cat(paste("(", ns, 
			" coefficient(s) not estimated because of singularity)\n",
			sep = ""))
	print(x$coefficients, ...)
	if(length(x$cov.fix)) {
		cat("\nApproximate Covariance Matrix of Coefficients:\n")
		print(zapsmall(x$cov.fix), ...)
	}
	if(!is.null(x$na.action))
		cat("\n", naprint(x$na.action), "\n")
	invisible(x)
}

