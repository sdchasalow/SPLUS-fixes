# $Id$

"find2" <- 
function(what, mode = "any", numeric. = T, whichFrames = 0:1, ...)
{
	#   DATE REVISED:  19 June 1994, 31 Oct 2003
	#   1.  Change default for arg numeric. to T.
	#   2.  Modify code so that find(blah) finds blah, not the object
	#       given by the value of blah, if blah is a named object of
	#       mode character.
	#   3.  Fix bug in new version: argument "mode" was ignored!
	#       (NOTE: mode still does not seem to work quite right.
	#       For example, if x <- 1, x is found with mode = "numeric", 
	#       "character", or "logical"; not with mode = "list" or 
	#       "function".  I expected it would be found only with mode
	#       = "numeric".  This is a "feature" of function exists().)
	#
	attached <- search()
	n <- length(attached)
	#
	# The new bit:
	what <- substitute(what)
	if(is.name(what))
		what <- as.character(what)
	else if(!is.character(what))
		stop("find needs an object name or a quoted object name")
	#
	# if(!is.character(what))
	#	what <- as.character(substitute(what))
	where <- logical(n)
	pos <- 1:n
	for(i in pos)
		where[i] <- exists(what, where = i, mode = mode, ...)
	if(numeric.) {
		names(pos) <- attached
		whichPos <- pos[where]
	}
	else whichPos <- attached[where]
	if(length(whichFrames) > 0) {
		# Also search specified frames
		frameWhere <- logical(length(whichFrames))
		for(i in seq(along = frameWhere))
			frameWhere[i] <- exists(what, frame = whichFrames[
				i], mode = mode, ...)
		if(numeric.) {
			names(whichFrames) <- paste("frame", whichFrames)
			frameWhich <- ( - whichFrames[frameWhere])
		}
		else frameWhich <- paste("frame", whichFrames)[frameWhere]
		if(length(frameWhich) > 0)
			whichPos <- c(rev(frameWhich), whichPos)
	}
	return(whichPos)
}

