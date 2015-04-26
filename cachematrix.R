## Put comments here that give an overall description of what your
## functions do:

## makeCacheMatrix(): to create a list object that stores a numeric vector.

## cacheSolve(): to solve the inverted matrix that *** and cache's its inverted matrix.


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ans <- NULL
        set <- function(y) {
            x <<- y 
            ans <<- NULL
        }
        set_ans <- function(x) {
            ans <<- x
        }
        get_ori_matrix <- function() {
            x
        }
        get_ans <- function() {
            ans
        }
        list(set = set, 
             get = get_ori_matrix,
             set_ans = set_ans,
             get_ans = get_ans)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverted_m <- x$get_ans()
        if(!is.null(inverted_m)) {
            message("getting cached data")
            return(inverted_m)
        }
        data <- x$get()
        inverted_m <- solve(data, ...)
        x$set_ans(inverted_m)
        inverted_m
}
