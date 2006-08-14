# $Id$

"prompt.default.4.7" <- 
function(object, dir, where = 1.)
{
	#
	# Bug fix: Scott Chasalow, 29 July 1997
	# Added argument where, used in calls to get() and true.file.name().
	# Now maybe prompt() won't crash my computer (GPF) when called for
	# a function not in position 1?
	# Modified: 12 Oct 1997
	# (1) Changed default for argument "dir" to be the _Help directory under the
	# directory indicated by argument "where". 
	# (2) Prevent overwriting of an existing file.
	#
	where <- where[1.]
	name <- substitute(object)
	if(is.language(name) && !is.name(name))
		name <- eval(name)
	name <- as.character(name)
	fn <- get(name, where = where)
	oldopts <- options(width = 80.)
	on.exit(options(oldopts))
	if(missing(dir)) {
		dir <- if(is.numeric(where)) search()[where] else where
		dir <- paste(dir, "\\_Help", sep = "")
	}
	if(!is.function(fn)) {
		file <- c("\n \t", 
			"\t Replace this line with 1-line descr of data object \n \t E.g. Halibut Data\n",
			"DESCRIPTION: \n", 
			"\t, Replace this line with a more complete description",
			)
	}
	else {
		n <- length(fn) - 1.
		if(n > 0.) {
			s <- 1.:n
			args <- fn[s]
			arg.names <- names(args)
		}
		else s <- integer(0.)
		file <- c("\n \t", 
			"\tReplace with function to do ??? header \n \tE.g. \"Print a data object\"\n",
			"DESCRIPTION:\n", 
			"\tReplace this line with brief description \n", 
			"USAGE:")
		call <- paste(name, "(", sep = "")
		for(i in s) {
			if(mode(args[[i]]) == "missing")
				call <- paste(call, arg.names[i], sep = "")
			else call <- paste(call, arg.names[i], "=", deparse(
					args[[i]]), sep = "")
			if(i != n)
				call <- paste(call, ", ", sep = "")
		}
		file <- c(file, paste("\t", call, ")", sep = ""), "\n", 
			"REQUIRED ARGUMENTS:\n", "OPTIONAL ARGUMENTS:", 
			"\tMove the above line to just above the first optional argument\n"
			)
		for(i in s)
			file <- c(file, arg.names[i], paste("\tDescribe", 
				arg.names[i], "here\n"))
		file <- c(file, "VALUE:", "\tDescribe the value returned\n",
			"SIDE EFFECTS:", 
			"\tDescribe any side effects if they exist\n", 
			"DETAILS:", "\tExplain details here.\n", "REFERENCES:",
			"\tPut references here, make other sections like NOTE and\nWARNING as needed\n",
			"SEE ALSO:", "\tPut functions to SEE ALSO here\n",
			"EXAMPLES:", "# The function is currently defined as",
			deparse(fn))
	}
	whatname <- character(2.)
	whatname[1.] <- name
	whatname[2.] <- dir
	filename <- true.file.name(name, where = where)
	tfilename <- paste(dir, filename, sep = "\\")
	if(file.exists(tfilename))
		stop(paste("may not overwrite existing file,", tfilename))
	cat(file, file = tfilename, sep = "\n")
	if(name != filename)
		cat(name, "mapped to DOS file name", filename, "\n")
	cat("created file named ", filename, " in the ", dir, "directory",
		"\n edit the file as desired.\n")
}

