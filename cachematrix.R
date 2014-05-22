## This source code allows to calculate the inverse of a given matrix,
## making sure it is squared and assuming it is inversible. The inverse is
## calculated with the solve() function. Once the matrix solved, the given
## matrix and its inverse are kept in an environment for future calls.
## Whenever called again for another inversion, the function checks if the given
## matrix is identical to the one stored in cache and that its inverse exists,
## before trying to solve it.


## This function creates a special "matrix" object
## that can cache its content and its inverse.

makeCacheMatrix <- function(x = matrix())
{
        ## Initialize the cached inversed matrix
        cacheMatrix <- NULL

        ## This function stores the given matrix to be inversed
        ## and resets cacheMatrix to NULL
        set <- function(y)
        {
                x <<- y
                cacheMatrix <<- NULL
        }

        ## This function returns the stored given matrix to be inversed
        get <- function() x

        ## This function sets the cacheMatrix to cache the inverse
        setInverse <- function(y) cacheMatrix <<- y

        ## This function get the matrix inverse from the cache
        getInverse <- function() cacheMatrix

        ## The makeCacheMatrix() function returns the list of callable
        ## functions previously declared above
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'

        ## In case this function has already been called, 
        ## get the matrix to be solved from the cache.
        ## NULL is returned in case of a first call
        savedMatrix <- x$get()

        ## In case this function has already been called, 
        ## get the inverse matrix from the cache.
        inversedMatrix <- x$getInverse()

        ## Check in the inverse has already been calculated
        ## and the matrix has not changed, and return from the cache, if so
        if (!is.null(inversedMatrix) && identical(savedMatrix, x))
        {
                message("getting cached data")
                return(inversedMatrix)
        }

        ## Solve the matrix, if possible (square matrix object)
        inversedMatrix <- solveMatrix(savedMatrix, ...)

        ## Save the given matrix in cache for future comparison
        x$set(x)

        ## Save the inverse in cache for future calls
        x$setInverse(inversedMatrix)

        ## Return return the inverse matrix to the caller
        inversedMatrix
}


## This function solves a matrix given as argument. It makes sure the given
## object is a matrix and that is squared. The given matrix is assumed to be
## inversible.

solveMatrix <- function(x, ...)
{
        ## Make sure the given object is a matrix, and return if not
        if (!is.matrix(x))
        {
                return(message("The given object is not a matrix"))
        }

        ## Make sure the given matrix is squared, and return if not
        if (dim(x)[1] != dim(x)[2])
        {
                return(message("The given object is not a square matrix"))
        }

        ## Solve and return the inverse matrix, assuming it is inversible
        solve(x, ...)
}
