## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
###################################################
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1)set the value of the matrix
## 2)get the value of the matrix
## 3)set the inverse of the matrix
## 4)get the inverse of the matrix
###################################################

makeCacheMatrix <- function(x=matrix()){
    
    inversematrix <- NULL
    
    ## set the value of the matrix
    set <- function(y){
        x <<- y
        inversematrix <<- NULL
    }
    
    ## get the value of the matrix
    get <- function() x
    
    ## set the inverse of the matrix
    setinverse <- function(inverse) inversematrix <<- inverse
    
    ## get the inverse of the matrix
    getinverse <- function() inversematrix
    
    ## returns the list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
###################################################
## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the inverse in the cache via the setinverse function.
###################################################

cacheSolve <- function(x, ...) {
    
    ## get the inverse of x from the list
    inversematrix <- x$getinverse()
    
    ## If we already have the inverse of x calculated and saved in the list, return that inverse
    if(!is.null(inversematrix)) {
        message("getting cached data")
        return(inversematrix)
    }
    
    ## get the matrix from the list
    data <- x$get()
    
    ## calculate the inverse by "solve" function
    inversematrix <- solve(data)
    
    ## save the inverse in the list
    x$setinverse(inversematrix)
    
    ## Return a matrix that is the inverse
    inversematrix
}


## Example
###################################################
## > M=makeCacheMatrix(x)
## > M$get()
## [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    4    5
## [3,]    1    0    6
## > x=matrix(c(1,0,1,2,4,0,3,5,6),3,3)
## > x
## [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    4    5
## [3,]    1    0    6
## > M=makeCacheMatrix(x)
## > M$get()
## [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    4    5
## [3,]    1    0    6
## > cacheSolve(M)
## [,1]        [,2]        [,3]
## [1,]  1.0909091 -0.54545455 -0.09090909
## [2,]  0.2272727  0.13636364 -0.22727273
## [3,] -0.1818182  0.09090909  0.18181818
## > cacheSolve(M)
## getting cached data
## [,1]        [,2]        [,3]
## [1,]  1.0909091 -0.54545455 -0.09090909
## [2,]  0.2272727  0.13636364 -0.22727273
## [3,] -0.1818182  0.09090909  0.18181818
###################################################
