## The following 2 functions will utilize the <<- operator to cache time consuming
## operations such as calculating the inverse of matrix so it can be reused

## makeCacheMatrix creates a special "matrix" which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(...)	{
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- mean
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix. If the
## matrix has already been caluclated (and the matrix is unchanged) it will retrieve
## the inverse from cache

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m))  {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
