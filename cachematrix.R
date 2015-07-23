## set of functions that memoize the inverse of a matrx
## memoizable matrix has to be initiated via makeCacheMatrix
## then the inverse has to be calculated with cacheSolve
## subsequent calls to cacheSolve will return memoized (cached) inverse value

# creates a special type of matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get, 
		setinverse = setinverse,
		getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting data from cache")
		return (m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}