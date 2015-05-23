## 'makeCacheMatrix()' is a function that creates a
## special inverse matrix object that can be cached for
## retrieval later on. This is helpful as recalculating
## the inverse of a matrix using solve() is very
## costly to processing power.

## 'cachesolve()' is a function that computes the inverse
## of a matrix. If the matrix has not changed then R
## will search through the cache and retrieve the stored
## value. If no cached value is found then R will need to 
## use the solve() function to calculate the inverse.


## <<- is being used to assign a value to an object in
## a different environment to the current one, ie. outside
## of the environment in which it was called



makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function(s)
	list(set = set, get = get, 
		setsolve = setsolve,
		getsolve = getsolve)
}
	
cacheSolve <- function(x, ...) {
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
}


