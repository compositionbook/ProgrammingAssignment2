 ## makeCacheMatrix function creates a special matrix object that can cache its inverse.
 ## I create makeCacheMatrix to input a matrix, set the value and the inverse value.
 ## it gets the matrix and its inverse matrix.
 
 makeCacheMatrix <- function (x = matrix () ) {  ##default as matrix
 	m <- NULL                                     ## initialize m as NULL
 	set<- function (y) {                          ## initialize set function to create new matrix
 		x <<- y
		inv <<-NULL
 	}
 
 	get <- function () x                       ## define get with functon to return matrix value
 	setinv <- function (inverse) m <<-inverse  ## set value of the matrix
 	getinv <- function() m                     ##get value of the matrix
 	list(set = set, get = get, setinv = setinv, getinv = getinv)
 }
 
## this cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix. 
## if the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache.
 
 cacheSolve <- function (x) {   ## define a new function
 	m <- x$getinv()
 	if ( !is.null(m)) { 
 		message ("getting cached data")   ## generate simple diagnostic message
 		return (m)
 	}
 	data <- x$get()
         m <- solve(data)
 	x$setinv(m)
	m
}
