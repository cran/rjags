#  R package rjags file R/utilities.R
#  Copyright (C) 2006-2018 Martyn Plummer and Matt Denwood
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License version
#  2 as published by the Free Software Foundation.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

# Note: all functions in here are intended to be exported

# Get the version of JAGS to which we are currently linked:
jags.version <- function(){
	vers <- .Call("get_version", PACKAGE="rjags")	
	return(package_version(vers))	
}
