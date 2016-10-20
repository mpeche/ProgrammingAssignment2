## These two functions work together to create a data object that contains a given matrix, 
##    as well as the inverse of the matrix in a cache memory. For simplicity, we are allowed
##    to assume that the supplied matrix is always invertable.

##    We can use cbind(c(2,-1),c(-5,3)) as test data
##    The inverse of above is cbind(c(3,1),c(5,2)) 

## Creates a Matrix-object with a matrix and the inverse of the matrix in cache.
## Receives	A inverable matrix
## Returns	A list of functions associated with this class.
makeCacheMatrix <- function(x = matrix()) {

    inv_x <- NULL		##Create empty cache

    ## Internal function that updates the matrix associated with the object;
    ##    it also resets the cache to empty
    ## Receives		A new matrix to assign
    ## Returns		Nothing
    setMatrix <- function(newMatrix) {
        ## x and inv_x both desides in the environment of makeCacheMatrix
        x <<- newMatrix	
	inv_x <<- NULL
    }

    ## Internal function that returns the matrix associated with the object
    ## Receives		Nothing
    ## Returns		The matrix
    getMatrix <- function() x

    ## Internal function that
    ## Receives		A
    ## Returns		A
    setInverseMatrix <- function(newInverse) inv_x <<- (newInverse) 

    ## Internal function that returns the cached inverse of the matrix associated
    ##    with the object
    ## Receives		Nothing
    ## Returns		The inverse matrix
    getInverseMatrix <- function(){ inv_x }

    ##Return the list of functions.
    list(
       setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}



##Retrieve if available, else calculate and store the inverse of a given matrix.
## Receives	A makeCacheMatrix Object
## Returns	A matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    ##Check cache  
    result <- x$getInverseMatrix()

    if (is.null(result)){
        ##If cache is not populated, calculate and save inverse before returning it.
        ##Note: inverse_mtx == solve(mtx)
        result <- solve(x$getMatrix())
        x$setInverseMatrix(result)
        message("Saving inverse to cache")
        return(result)
    }

    ## Else return cache
    message("Retrieving inverse from cache")
    result
}

