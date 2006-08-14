# $Id$

"page.unix" <- 
function(x, pager = options()$pager, filename, window = T, display = getenv(
	"DISPLAY"), title, remove.file = F, ...)
{
	# SDC, 20 Feb 2004:
	# 1. changed default of arg window to T
	# 2. added arg "..." to pass to print()
	#
	QUOTE <- ifelse(getPlatform() == "WIN386", "\"", "'")
	if(!missing(filename) && missing(x)) {
		qfilename <- paste(sep = "", QUOTE, filename, QUOTE)
		if(!interactive())
			return(invisible(unix(paste("cat", qfilename), output
				 = F)))
		cmd <- paste(pager, qfilename)
		if(remove.file)
			cmd <- paste(cmd, "; rm -f", qfilename)
		if(missing(title))
			title <- filename
	}
	else {
		# S expression
		if(!interactive()) return(invisible(print(x, ...)))
		filename <- tempfile("page")
		qfilename <- paste(sep = "", QUOTE, filename, QUOTE)
		sink.n <- sink(filename)
		on.exit({
			sink(unsink.to = sink.n)
			unlink(filename)
		}
		)
		print(x, ...)
		sink(unsink.to = sink.n)
		on.exit()
		cmd <- paste(pager, qfilename, "; rm -f", qfilename)
		if(missing(title))
			title <- deparse(substitute(x))
	}
	if(getenv("S_REMOTE_SESSION_ID") != "") {
		pagestr <- .C("S_readfile",
			as.character(filename),
			as.integer(0),
			as.integer(-1),
			out = character(1),
			as.logical(F))$out
		cmd <- paste("xterm -title", title, "-display", display, "-e",
			pager)
		invisible(.JavaMethod("com.insightful.splus.SplusEditManager",
			"page", "(Ljava/lang/String;Ljava/lang/String;)V",
			cmd, pagestr))
		unix(paste("rm -f", filename))
	}
	else {
		if(window || is.ui.app("s+java"))
			if(display != "" && using.X(display)) {
				title <- paste("\"'", title, "'\"", sep = "")
				cmd <- paste("(unix2 xterm -title", title,
					"-display", display, "-e", cmd, 
					")>/dev/null&")
			}
			else warning(paste(sep = "", "Unable to open display '",
					display, "'"))
		invisible(unix(cmd, output = F))
	}
}

