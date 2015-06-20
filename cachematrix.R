## This is a set of functions that will allow you to generate and cache the inverse
## of a matrix.

## The makeChacheMatrix creates a set of functions to be used in the cacheSolve function
## for the creating and storing of the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
                    x <<- y
                    i <<- NULL
        }
	get <- function() x
	setInverse <- function(inv) i <<- inv
	getInverse <- function() i
	list(set = set, get = get,
                 setInverse = setInverse,
	         getInverse = getInverse
	    )

}


## The cacheSolve function takes a list of makeCacheMatrix functions and either returns the cached
## inverse of a matrix or generates the inverse of the matrix if it is not cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
	                message("getting cached data")
	                return(i)
	        }
	        data <- x$get()
	        i <- solve(data, ...)
	        x$setInverse(i)
        i
}
