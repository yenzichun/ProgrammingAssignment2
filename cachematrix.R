## Put comments here that give an overall description of what your
## functions do:

## makeCacheMatrix(): to create a list object that stores a numeric vector (ie. the "cache").
## cacheSolve(): to solve the inverted matrix and return its inverted matrix from cache.


## Write a short comment describing this function

## This function will create/return a list object that stores a numeric vector (i.e the "cache")
## The cache is defined by 4 element:
## 1.set_o_m: sets the original matrix using the anonymous function set_ori_mtx().
## 2.set_i_m: sets the inverted matrix using the anonymous function set_inv_mtx().
## 3.get_o_m: returns the original matrix using the anonymous function get_ori_mtx().
## 4.get_i_m: returns the inverted matrix using the anonymous function get_inv_mtx().
## At the end of this function, it returns the list object.


makeCacheMatrix <- function(x = matrix()) {
    inv_mtx <- NULL ## initialize the inv_mtx variable
    set_ori_mtx <- function(y) {
        x <<- y ## use <<- operator so that variable x can be accessed by cacheSolve(), whose environment is different from the current environment.
        inv_mtx <<- NULL
    }
    set_inv_mtx <- function(x) {
        inv_mtx <<- x ## use <<- operator so that variable inv_mtx can be accessed by cacheSolve().
    }
    get_ori_mtx <- function() x ## this function simply returns x. So short that doesn't need braces.
    get_inv_mtx <- function() inv_mtx
    list(set_o_m = set_ori_mtx, 
         set_i_m = set_inv_mtx,
         get_o_m = get_ori_mtx,
         get_i_m = get_inv_mtx)
}


## Write a short comment describing this function

## First the function will try to access inverted matrix(i.e get_i_m) from the numeric vector in makeCacheMatrix(),
## If the inverted matrix does exist in cache, then return the inverted matrix (i.e inv_mtx).
## If it's null, then access the original matrix (i.e get_o_m) from cache, solve the inverted matrix,
## and set it back to cache.
## At last, return the inverted matrix (i.e inv_mtx) which is cached in makeCacheMatrix().

cacheSolve <- function(x, ...) {
    inv_mtx <- x$get_i_m()
    if(!is.null(inv_mtx)) {
        message("getting cached data")
        return(inv_mtx)
    }
    data <- x$get_o_m()
    inv_mtx <- solve(data, ...)
    x$set_i_m(inv_mtx)
    inv_mtx
}
