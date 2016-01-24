## Function "makeCacheMatrix" creates a matrix object that can cache its inverse 
## 1. setMatrix for setting the value of the basic matrix
## 2. getMatrix for getting the value of the basic matrix
## 3. setInverse for setting the the value of inverse matrix
## 4. getInverse for getting the value of inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        setMatrix <- function(y) {
                x <<- y
                invMatrix <<- NULL
		   message("inverse matrix null")
        }
        getMatrix <- function() x
        setInverse <- function(inverse) invMatrix <<- inverse
        getInverse <- function() invMatrix
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}



##  Function "cacheSolve" creates the inverse matrix of matrix created by "makeCacheMatrix" 
## If the inverse matrix has been already calculated (!is.null(invMatrix) then only dipslay inverse matrix from the cache

cacheSolve <- function(x, ...) {
        invMatrix <- x$getInverse()
        if (!is.null(invMatrix)) {
                message("getting cached data, disply inverse matrix")
                return(invMatrix)
        }
        mat <- x$getMatrix()
        invMatrix <- solve(mat, ...)
        x$setInverse(invMatrix)
         message("disply inverse matrix for the first time")
        invMatrix
}
