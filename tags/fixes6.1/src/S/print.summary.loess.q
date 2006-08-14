# $Id$

"print.summary.loess" <- 
function(x, ...)
{
	c1 <- "Number of Observations:"
	c2 <- x$num.observation
	if(!is.null(x$num.level)) {
		c1 <- c(c1, "Number per Level:")
		c2 <- c(c2, x$num.level)
	}
	c1 <- c(c1, "Equivalent Number of Parameters:")
	if(x$family == "gaussian")
		c1 <- c(c1, "Residual Standard Error:")
	else c1 <- c(c1, "Residual Scale Estimate:")
	c2 <- c(c2, format(round(x$enp, 1)), format(signif(x$inf.s, 4)))
	if(!is.null(x$covariance)) {
		c1 <- c(c1, "Multiple R-squared:")
		c2 <- c(c2, format(round(x$covariance, 2)))
	}
	sum <- cbind(c1, c2)
	dimnames(sum) <- list(rep("", dim(sum)[1]), rep("", dim(sum)[2]))
	res.quantile <- x$res.quantile
	names(res.quantile) <- c("min", "1st Q", "median", "3rd Q", "max")
	cat("Call:\n")
	dput(x$call)
	print(sum, quote = F)
	cat(" Residuals:", "\n")
	print(signif(res.quantile, 4), ...)
	invisible(x)
}

