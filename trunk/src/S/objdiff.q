# $Id$

"objdiff" <- 
function(x, y, file = tempfile("diff"), command = "diff -c", window = T)
{
	# Revised by Scott Chasalow on 20 Feb 2004:
	# -  added argument window = T
	# -  added call to unlinkTemporaryFile().
	old <- tempfile("old")
	new <- tempfile("new")
	on.exit(unlink(c(old, new)))
	dput(x, old)
	dput(y, new)
	if(missing(file)) {
		if(!window)
			on.exit(unlink(file), add = T)
		else unlinkTemporaryFile(file, "diff")
	}
	status <- unix(paste(command, old, new, ">", file), output = F)
	page(file = file, window = window)
	if(!missing(file))
		file
	else invisible(status)
}

