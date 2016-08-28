## These are functions meant to cache the inverse of the input
## matrix x, and to retrieve it from the cache if it exists
## rather than recalculate it.

## Create a list of functions that will set the value of the 
## matrix, get the value of the matrix, set the inverse of
## the matrix, and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse, 
		getinverse = getinverse)
}


## If passed in the output of the makeCacheMatrix (which is
## a list), this will either calculate or look up the
## inverse of the matrix x.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
