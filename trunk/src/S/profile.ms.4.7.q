# $Id$

"profile.ms.4.7" <- 
function(fitted, which = 1.:npar, npoints = 10., zetamax = 10., delta = (0.5 *
	abs(.pars))/npoints, trace = F, data)
{
	S.hat <- fitted$value
	if(S.hat != 0.)
		zetamax <- zetamax * sqrt(abs(S.hat))
	.pars <- fitted$parameters
	pn <- names(.pars)
	npar <- length(.pars)
	f.class <- class(fitted)
	class(fitted) <- NULL
	scale <- fitted$scale
	if(!length(scale))
		stop("fitted model missing scale")
	formula <- fitted$formula
	asgn <- fitted$assign
	.parameters <- names(asgn)
	# Now, some code almost like ms(): make a new frame from the data, 
	# analyze the trace argument and call setup_min. Differences noted
	if(missing(data)) {
		data <- fitted$data
		if(is.null(data) && !is.null(data <- fitted$call$data))
			data <- eval(data)
		if(is.null(data))
			data <- sys.parent()
	}
	if(is.numeric(data))
		data <- sys.frame(data)
	if(inherits(data, "pframe")) {
		class(data) <- NULL
		pp <- parameters(data)
		if(length(pp)) {
			np <- names(pp)
			if(any(match(np, names(data), 0.)))
				stop("can't have variables, parameters with same name"
					)
			data[np] <- pp
		}
	}
	else class(data) <- NULL
	data[[".parameters"]] <- .parameters
	for(i in seq(along = .parameters))
		data[[.parameters[i]]] <- .pars[asgn[[i]]]
	nl.frame <- new.frame(data, F)
	#Trace, if any
	trace.fun <- ""
	if(!missing(trace)) {
		if(is.logical(trace)) {
			if(trace)
				trace.fun <- "trace.ms"
		}
		else if(mode(trace) == "function") {
			trace.fun <- substitute(trace)
			if(!is.name(trace.fun)) {
				assign("trace.ms", frame = 1., trace)
				trace.fun <- "trace.ms"
			}
			else trace.fun <- as.character(trace.fun)
			trace <- T
		}
		else trace.fun <- as.character(trace)
	}
	z <- .C("setup_min",
		n = integer(3.),
		list(formula),
		as.integer(trace),
		as.character(trace.fun),
		as.integer(nl.frame))$n
	npar <- z[1.]
	n <- npar - 1.
	nderiv <- z[2.]
	nobs <- z[3.]
	fval <- double(nobs)
	if(nderiv == 0.) {
		nwork <- 78. + (n * (n + 17.))/2.
		gval <- hval <- double(1.)
	}
	else {
		gpattern <- array(T, c(nobs, npar))
		nn <- nobs * n
		gval <- double(nn)
		gg <- double(n)
		nwork <- 78. + (n * (n + 15.))/2.
		hval <- double(1.)
	}
	if(nderiv > 1.) {
		nn <- nobs * n^2.
		hval <- double(nn)
		hh <- array(0., c(n, n))
		storage.mode(hh) <- "double"
		nwork <- 78. + n * (n + 12.)
	}
	controlvals <- fitted$control
	if(any(which > npar) || any(which < 1.))
		stop(paste("which must be in the range 1:", npar))
	out <- list()
	nseq <- 1.:npar
	nmax <- 2. * npoints + 1.
	#clear out to avoid false convergence
	.C("min_free_cache")
	for(par in which) {
		.pars <- fitted$parameters
		sgn <- -1.
		count <- 1.
		varying <- rep(T, npar)
		varying[par] <- F
		.C("nl_set_varying",
			as.integer(varying))
		if(nderiv) {
			gpattern[,  ] <- T
			gpattern[, par] <- F
			.C("nl_set_patterns",
				gpattern,
				NULL)
		}
		zeta <- rep(NA, nmax)
		par.vals <- array(NA, c(nmax, npar), list(NULL, pn))
		zeta[1.] <- 0.
		par.vals[1.,  ] <- .pars
		base <- .pars[par]
		profile.inc <- delta * (nseq == par)
		pp <- .pars[ - par]
		if(nderiv > 0.)
			names(gg) <- names(pp)
		while(count <= npoints) {
			.pars <- .pars - profile.inc
			count <- count + 1.
			for(i in seq(along = .parameters))
				assign(.parameters[i], .pars[asgn[[i]]], frame
					 = nl.frame)
			min.pars <- min.setpars(controlvals, nwork)
			z <- switch(nderiv + 1.,
				.C("do_minf",
					parameters = pp,
					as.integer(n),
					scale = scale,
					value = double(1.),
					pieces = fval,
					flags = min.pars$flags,
					opt.parameters = min.pars$parameters),

					.C("do_ming",
					parameters = pp,
					as.integer(n),
					scale = scale,
					value = double(1.),
					gradient = gg,
					pieces = fval,
					slopes = gval,
					flags = min.pars$flags,
					opt.parameters = min.pars$parameters),

					.C("do_minh",
					parameters = pp,
					as.integer(n),
					scale = scale,
					value = double(1.),
					gradient = gg,
					hessian = hh,
					pieces = fval,
					slopes = gval,
					curves = hval,
					flags = min.pars$flags,
					opt.parameters = min.pars$parameters))[
				-2.]
			if(z$flags[1.] > 6.) {
				count <- count - 1.
				break
			}
			zeta[count] <- (sgn * sqrt(z$value - S.hat))
			.pars[ - par] <- z$parameters
			par.vals[count,  ] <- .pars
			if(abs(zeta[count]) > zetamax)
				break
		}
		zeta[1.:count] <- zeta[count:1.]
		par.vals[1.:count,  ] <- par.vals[count:1.,  ]
		sgn <- 1.
		.pars <- par.vals[count,  ]
		pp <- .pars[ - par]
		if(nderiv > 0.)
			names(gg) <- names(pp)
		while(count < nmax) {
			.pars <- .pars + profile.inc
			count <- count + 1.
			for(i in seq(along = .parameters))
				assign(.parameters[i], .pars[asgn[[i]]], frame
					 = nl.frame)
			min.pars <- min.setpars(controlvals, nwork)
			z <- switch(nderiv + 1.,
				.C("do_minf",
					parameters = pp,
					as.integer(n),
					scale = scale,
					value = double(1.),
					pieces = fval,
					flags = min.pars$flags,
					opt.parameters = min.pars$parameters),

					.C("do_ming",
					parameters = pp,
					as.integer(n),
					scale = scale,
					value = double(1.),
					gradient = gg,
					pieces = fval,
					slopes = gval,
					flags = min.pars$flags,
					opt.parameters = min.pars$parameters),

					.C("do_minh",
					parameters = pp,
					as.integer(n),
					scale = scale,
					value = double(1.),
					gradient = gg,
					hessian = hh,
					pieces = fval,
					slopes = gval,
					curves = hval,
					flags = min.pars$flags,
					opt.parameters = min.pars$parameters))[
				-2.]
			if(z$flags[1.] > 6.) {
				count <- count - 1.
				break
			}
			zeta[count] <- (sgn * sqrt(z$value - S.hat))
			.pars[ - par] <- z$parameters
			par.vals[count,  ] <- .pars
			if(abs(zeta[count]) > zetamax)
				break
		}
		out[[pn[par]]] <- data.frame(zeta = zeta[1.:count], par.vals = 
			I(par.vals[1.:count,  , drop = F]))
	}
	class(fitted) <- f.class
	attr(out, "original.fit") <- fitted
	class(out) <- c("profile.ms", "profile")
	out
}

