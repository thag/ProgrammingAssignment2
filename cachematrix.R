## Programmer: Ram A, April 26 2015

## The following two functions allow users to avoid calculating
## inverse of a matrix by caching the result in a global variable

## The following function makeCacheMatrix creates an inverse of a 
## given matrix as long as the inverse was already not calculated
## or if the input matrix is empty
## It assumes that the matrix supplied is always invertible.

makeCacheMatrix <- function(inp = matrix()) {
    #function to see if the matrices are equal
    isMatrixEqual <- function(a, b) is.matrix(a) && is.matrix(b) && dim(a) == dim(b) && all(a == b)
    
    #if the cache is null or different matrix, calculate inverse and cache
    if(is.null(matrixOriginal) | (!(isMatrixEqual(matrixOriginal, inp)))){
        message("calculated as empty cache or new matrix")
        matrixOriginal <<- inp             #store inp as global variable 
        inverseMatrixCache <<- solve(inp)
    }
    else message ("same matrix no need to re-calculate")
    
}

## The following function cacheSolve returns inverse of a 
## given matrix by accessing cache location names inverseMatrixCache
## as long as the inverse was already calculated.
## If not it calls the makeCacheMatrix with the supplied matrix.
## It assumes that the matrix supplied is always invertible.

cacheSolve <- function(inp, ...) {
    #function to see if the matrices are equal
    isMatrixEqual <- function(a, b) is.matrix(a) && is.matrix(b) && dim(a) == dim(b) && all(a == b)
    
    #if the cache is null or different matrix, calculate inverse by calling makeCacheMatrix 
    if(is.null(matrixOriginal) | (!(isMatrixEqual(matrixOriginal, inp)))){
        message ("different matrix, calling makeCacheMatrix()")
        makeCacheMatrix(inp)
    }
    else message ("same matrix retrieve from cache")
    
    inverseMatrixCache #return the inverse from the cache
}

## Test cases
# m=rbind(c(1, 1), c(-1, 2)) 
# solve(m)
# makeCacheMatrix(m)
# makeCacheMatrix(m)
# cacheSolve(m)
# 
# m=rbind(c(1, -1), c(1, 2)) 
# solve(m)
# cacheSolve(m)
# cacheSolve(m)
# makeCacheMatrix(m)
# cacheSolve(m)
# 
## http://www.purplemath.com/modules/mtrxinvr.htm
# m=cbind(c(1,1,1),c(3,4,3),c(3,3,4))
# cacheSolve(m)
# cacheSolve(m)