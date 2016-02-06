## The program includes two functions, makeCacheMatrix and CacheSolve.  
## Given an inversible matrix as input, these functions calculate the 
## inverse of the matrix.  Once the inverse is calculated, it is stored
## in cache for future reference.

## This function takes a matrix as an argument and creates a vector comprised 
## of a list of functions which gets the current matrix, sets the new matrix and clears the cache, stores the 
## inverse matrix in cache, and retrieves the inverse from cache.

makeCacheMatrix <- function(m = matrix()) {
	cache <- NULL
	set <- function(y) {
		m <<-y
		cache <<- NULL
	}
	  get <- function() m
        setinverse <- function(inverse) cache <<- inverse
        getinverse <- function() cache
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function takes an inversible matrix as an argument, and calls getinverse() from
## makeCacheMatrix above, to retrieve the inverse from cache, if it exists.  If it does
## exist, a message indicating that the inverse matrix is being retrieved from cache is 
## presented, and the inverse is returned.  Otherwise, the matrix is retrieved from 
## the get function above, and sent as an argument to the solve function, which produces
## the inverse matrix. The inverse is stored in cache using the setinverse function, and 
## the inverse is returned.

cacheSolve <- function(m) {
	inverse <- m$getinverse()
	if (!is.null(inverse)) {
		message("getting cached matrix")
		return(inverse)
	}
	mat <- m$get()
	inverse <- solve(mat)
	m$setinverse(inverse)
	inverse
}
