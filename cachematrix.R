## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix inputs a matrix, sets the value, sets the inverse value.
## it gets the matrix and its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
 	set<- function (y) 
	{x <<- y
 		inv <<-NULL }
 	get <- function () x
 	setinv <- function (inverse) m <<-inverse
 	getinv <- function() m
 	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function calculates the inverse matrix and stores its value to the function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
 	if ( !is.null(m)) {
 	message ("getting cached data")
 	return (m) }

 	data <- x$get()
  	m <- solve(data)
 	x$setinv(m)
 	m
}
