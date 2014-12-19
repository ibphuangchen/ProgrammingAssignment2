## This pair of functions is to solve the inverse of a matrix in the "cached" manner.
## The first one creates a special "matrix" object that can cache its inverse.
## The second one computes the inverse of this special "matrix". 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
	# store the original matrix and its inverse matrix
	# provide various method to set and retrieve the matrix and its inverse
	InverseMatrix <- NULL 
	set <- function (y) {
		x <<- y
		InverseMatrix <<- NULL #reset the InverseMatrix back to 
	}				#NULL each time modifying the matrix
	get <- function() x
	setinverse <- function(my_inverse) InverseMatrix <<- my_inverse 
	getinverse <- function() InverseMatrix
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	InverseMatrix <- x$getinverse()
	if(!is.null(InverseMatrix)){
		message("getting cached data")
		return(InverseMatrix)
	}
	data <- x$get()
	InverseMatrix <- solve(data, ...)
	x$setinverse(InverseMatrix)
	InverseMatrix
}
