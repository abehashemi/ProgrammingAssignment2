## Accelerate matrix inversion, by cacheing the inverse of a matrix the first time it 
## is calculated and using it when needed as long as the original matrix has not changed.

## Encapsulate definition of a matrix so that its inverse can be cached and the matrix 
## cannot change without invalidating possibly cached inverse.
makeCacheMatrix <- function(x = numeric()) {
	m <- NULL
	set <- function(y){					#Set matrix
		x <<- y
		m <<- NULL						# and its inverse to NULL
	}
	get <- function() x					# get matrix
	setInv <- function(n) m <<- n				# Cache Inverse
	getInv <- function() m					# Get from cache
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}
##Returned cached inverse of a matrix, x, if it is cached, otherwise calculate it.
cacheSolve <- function(x,...) {
	y <- x$getInv()
	if(!is.null(y)){
		message("Getting cached inverse")
		return(y)						#Return cached inverse.
	}
	m <- x$get()
	n <- solve(m)						#Invert m
	x$setInv(n)							#Save in cache
	n								#Return inverse.

}