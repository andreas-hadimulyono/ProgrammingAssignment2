## makeCacheMatrix: creates a list which contains function to get,set matrix and get,set the inverse
## cacheSolve: get inverse matrix from cache if exists, otherwise calculate inverse and store in cache

## return list of functions:
## 1. set the matrix value (set)
## 2. get the matrix value (get)
## 3. set the matrix inverse (set)
## 4. get the matrix inverse (get)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## returns the matrix inverse.
## get from matrix inverse cache if it exists, otherwise calculate inverse

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
