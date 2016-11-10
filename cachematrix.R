
## Make a list of function and save 'x' and 'matrixInv' in a parent enviroment 
makeCacheMatrix <- function(x = matrix()) {
        matrixInv <- NULL
        set <- function(y) {
                x <<- y      
                matrixInv <<- NULL
        }
        get <- function() x
        setmatrixInv <- function(inver) matrixInv <<- inver
        getmatrixInv <- function() matrixInv
        list(set = set, get = get,
             setmatrixInv = setmatrixInv,
             getmatrixInv = getmatrixInv)
}

## This function calculates the inverse matrix of a given matrix. 
## In case the matrix has already been calculated it only displays it.
cacheSolve <- function(x, ...) {
        matrixInv <- x$getmatrixInv()
        if(!is.null(matrixInv)) {
                message("getting cached data")
                return(matrixInv)
        }
        data <- x$get()
        matrixInv <- solve(data, ...)
        x$setmatrixInv(matrixInv)
        matrixInv
}
