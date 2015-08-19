## A pair of functions allowing the create a matrix that caches its inverse
## and access it if the matrix has not changed, avoiding its computation.

## Creates a special matrix object that can cahe its inverse.
makeCacheMatrix <- function(x = matrix()) {
	
	i <- NULL
	
	set <- function(y){
		x <<- y
		i <<- NULL
	}

	get <- function() x

	set_inverse <- function(inverse) i <<- inverse

	get_inverse <- function() i

	list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## If the inverse of 'x' is cached and 'x' has not changed
## the cached value is returned, if it's not cached then
## it's computed, cached and returned.
cacheSolve <- function(x, ...) {
	
	i <- x$get_inverse()

	if(!is.null(i)){
		message("Returning cached value")
		return(i)
	}

	data <- x$get()
	i <- solve(data)
	x$set_inverse(i)

	i
}
