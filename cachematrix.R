## As the calculation of the inverse of a very large matrix (say 10 by 10) can
## take some time, the following pair of functions allow the result of a 
## previous calculation to be stored and returned if it is requested a second or
## subsequent time.
## 

## The makeCacheMatrix function takes a matrix as input and returns a list of
## functions for getting and setting the values of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        # create a location in which to store the inverse
        inv <- NULL
        # The set function for the matrix can be used to change its value
        set <- function(y) {
                ## If it has changed, store the new value and null out the inverse
                ## (otherwise do nothing, there is no point in recalculating 
                ## the previous inverse if it was really hard to calculate)
                if(!identical(x,y)) {
                        x <<- y
                        inv <<- NULL
                }
        }
        # The get function simply returns the current value of the matrix
        get <- function() x
        # The setinv function stores a value into the inv location
        setinv <- function(a) inv <<- a
        # The getinv function simply returns the current value of the inverse 
        getinv <- function() inv
        # The return value is a list of the four functions, named after the functions
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## The input value of the cachesolve function must be a list
## that has been returned by the makeCacheMatrix function
## If the associated inv is null, it will 'solve' it,
## otherwise it will return it

cacheSolve <- function(CachedMatrix, ...) {
        ## Return a matrix that is the inverse of the x supplied to makeCachedMatrix
        inv <- CachedMatrix$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        } else { 
                ## Calculate the inverse (no error handling here,
                ## as per instructions assuming that the matrix is invertible)
                inv <- solve(CachedMatrix$get())
                CachedMatrix$setinv(inv)
                return(inv)
        }        
        
}

#######################################################################
## Sample output to prove it works, because I had a really hard time
## getting my head around this one!!!! I hope it was a lot easier for
## you, peer reviewer.
## 
# 
# 
# > x<-matrix(c(1:4,5:1),3,3)
# > x
# [,1] [,2] [,3]
# [1,]    1    4    3
# [2,]    2    5    2
# [3,]    3    4    1
# 
# > source('~/git/ProgrammingAssignment2/cachematrix.R')
# > a=makeCacheMatrix(x)
# > cacheSolve(x)    <<< This will not work, the special list must be supplied
# Error in CachedMatrix$getinv : $ operator is invalid for atomic vectors
# > cacheSolve(a)
# [,1] [,2]   [,3]
# [1,]  0.375   -1  0.875
# [2,] -0.500    1 -0.500
# [3,]  0.875   -1  0.375
# > cacheSolve(a)
# getting cached data
# [,1] [,2]   [,3]
# [1,]  0.375   -1  0.875
# [2,] -0.500    1 -0.500
# [3,]  0.875   -1  0.375
# > cacheSolve(a)
# getting cached data
# [,1] [,2]   [,3]
# [1,]  0.375   -1  0.875
# [2,] -0.500    1 -0.500
# [3,]  0.875   -1  0.375
# > a$set(x)                 << If set to the same matrix, no recalculation
# > cacheSolve(a)
# getting cached data
# [,1] [,2]   [,3]
# [1,]  0.375   -1  0.875
# [2,] -0.500    1 -0.500
# [3,]  0.875   -1  0.375
# > x<-matrix(c(1:15,50:1),5,5)
# > a$set(x)                 << If you change the matrix, the inv is recalculated
# > cacheSolve(a)
# [,1]        [,2]       [,3]        [,4]
# [1,]  8.252327e-17 -0.36363636  7.7272727 -6.36363636
# [2,] -1.982541e-18  0.09090909 -0.1818182  0.09090909
# [3,] -8.333333e-02 -0.38636364  9.0227273 -7.55303030
# [4,] -8.333333e-02  0.06818182  0.1136364 -0.09848485
# > cacheSolve(a)
# getting cached data
# [,1]        [,2]       [,3]        [,4]
# [1,]  8.252327e-17 -0.36363636  7.7272727 -6.36363636
# [2,] -1.982541e-18  0.09090909 -0.1818182  0.09090909
# [3,] -8.333333e-02 -0.38636364  9.0227273 -7.55303030
# [4,] -8.333333e-02  0.06818182  0.1136364 -0.09848485
# > 
