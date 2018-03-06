## The following is a pair of functions that cache the inverse of a matrix.
## The following shows how to cache potentially time-consuming computations. 

## This function creates a special "matrix" object and it caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
        matrixInv <- NULL
        set <- function(y) {
                x <<- y
                matrixInv <<- NULL
        }
        get <- function() x
        setInverse <- function(invertMatrix) matrixInv <<- invertMatrix
        getInverse <- function() matrixInv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)){
                message("getting cached Inverse Matrix")
                ## go ahead and return the cached data
                return(inverseMatrix)
        }
        inverseMatrix <- solve(x$get())
        x$setInverse(inverseMatrix)
        inverseMatrix      
}
