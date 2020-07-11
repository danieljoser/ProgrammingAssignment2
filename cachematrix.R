
#The makeCacheMatrix function creates a matrix object, and includes multiple functions that are accessed
#internally by the other function. 
#It has the set function that sets the value of the matrix,the get function gets the values
#already in the matrix. While the setinv and getinv functions can get and set the values of
#the inverse of the matrix that was already calculated by the cacheSolve function.

#The cacheSolve function it's a function that takes a matrix object created using the makeCacheMatrix
#function and returns the inverse of that matrix. If the inverse was already calculated for the matrix,
#then it would return the cached value without the need of doing the computation again.


# This makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)


}


#This cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves
#sthe inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("Getting already cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
