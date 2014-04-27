## Follows assignment guidelines, creating a set of functions to set and store
## a matrix and its inverse.
## cacheSolve returns matrix from cache if available, solves and stores if not


## creates functions to set and store a matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
        InverseMatrix <- NULL
        setMatrix <- function(y){
                x <<- y
                InverseMatrix <<- NULL
        }
        getMatrix <- function()x
        setInverseMatrix <- function(solve) InverseMatrix <<- solve
        getInverseMatrix <- function() InverseMatrix
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setInverseMatrix = setInverseMatrix, 
             getInverseMatrix = getInverseMatrix)
}


## prints the inverse matrix from the cache if available
## if not available, function solves the matrix, caches and prints inverse

cacheSolve <- function(x, ...) {
        InverseMatrix <- x$getInverseMatrix()
        if(!is.null(InverseMatrix)){
                message("trying to remember...")
                return(InverseMatrix)
        }
        data <- x$getMatrix()
        InverseMatrix <- solve(data,...)
        x$setInverseMatrix(InverseMatrix)
        InverseMatrix
}

