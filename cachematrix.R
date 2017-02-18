# Matrix inversion costs a lot of computation effort.There may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
                x <<- y
                inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
                list(set=set, 
                     get=get, 
                     setinverse=setinverse, 
                     getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
            message("retrieving your cached data.")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}

# Example Matrix setup            
# > x = rbind(c(1, -1/4), c(-1/4, 1))
# > mp = makeCacheMatrix()
# > mp = makeCacheMatrix(x)
# > mp$get()
#      [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00
            
# No cache in the first run
# > cacheSolve(mp)
#          [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
            
# > cacheSolve(mp)
# retrieving your cached data.
#         [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# > 
