# first and last
.First.lib <- function(lib, pkg)
{
require("epitools","epicalc",quietly=TRUE,warn.conflicts=FALSE)
# require("tcltk",quietly=TRUE,warn.conflicts=FALSE)   # to be moved
# if (.Platform$OS.type=="windows")
#	require("xlsReadWrite",quietly=TRUE,warn.conflicts=FALSE)
see <- packageDescription(pkg,fields="Version")
cat("'DiagnosisMed' library",see," loaded\n",sep=" ")
}

.Last.lib <- function(libpath)
{
# nothing so far
}
