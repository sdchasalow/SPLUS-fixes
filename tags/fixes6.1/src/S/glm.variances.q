# $Id$

"glm.variances" <- 
matrix(list("Constant: 1"
, "variance" = function(mu)
1
, "constant" = function(mu, y, w, residuals = F)
if(residuals) sqrt(w) * (y - mu) else sum(w * (y - mu)^2)
, "Binomial: mu(1-mu)"
, "variance" = function(mu)
mu * (1 - mu)
, "mu(1-mu)" = function(mu, y, w, residuals = F)
{
	devy <- y
	nz <- y != 0
	devy[nz] <- y[nz] * log(y[nz])
	nz <- (1 - y) != 0
	devy[nz] <- devy[nz] + (1 - y[nz]) * log(1 - y[nz])
	devmu <- y * log(mu) + (1 - y) * log(1 - mu)
	if(any(small <- mu * (1 - mu) < .Machine$double.eps)) {
		warning("fitted values close to 0 or 1")
		smu <- mu[small]
		sy <- y[small]
		smu <- ifelse(smu < .Machine$double.eps, .Machine$double.eps,
			smu)
		onemsmu <- ifelse((1 - smu) < .Machine$double.eps, .Machine$
			double.eps, 1 - smu)
		devmu[small] <- sy * log(smu) + (1 - sy) * log(onemsmu)
	}
	devi <- 2 * (devy - devmu)
	if(residuals)
		sign(y - mu) * sqrt(abs(devi) * w)
	else sum(w * devi)
}
, "Identity: mu"
, "variance" = function(mu)
mu
, "mu" = function(mu, y, w, residuals = F)
{
	nz <- y > 0
	devi <-  - (y - mu)
	devi[nz] <- devi[nz] + y[nz] * log(y[nz]/mu[nz])
	if(residuals)
		sign(y - mu) * sqrt(2 * abs(devi) * w)
	else 2 * sum(w * devi)
}
, "Square: mu^2"
, "variance" = function(mu)
mu^2
, "mu^2" = function(mu, y, w, residuals = F)
{
	nz <- y > 0
	devi <- (y - mu)/mu + log(mu)
	devi[nz] <- devi[nz] - log(y[nz])
	if(residuals)
		sign(y - mu) * sqrt(2 * abs(devi) * w)
	else 2 * sum(w * devi)
}
, "Cube: mu^3"
, "variance" = function(mu)
mu^3
, "mu^3" = function(mu, y, w, residuals = F)
{
	devi <- (2 * (y - mu))/(mu^2 * y)
	if(residuals)
		sign(y - mu) * sqrt(w * abs(devi))
	else sum(w * devi)
}
)
, nrow = 3, ncol = 5
,  dimnames = list(c("name", "variance", "deviance")
, c("constant", "mu(1-mu)", "mu", "mu^2", "mu^3")
)
)

