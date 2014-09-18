## This script contains two functions, makeCacheMatrix  and CacheSolve. The objective is to calculate the inverse of a matrix and save it in memory the first time it is calculated, to avoid unnecessary expense of CPU time.

## Function "makeCacheMatrix" builds a matrix in a special form that allows the user to retrieve the values, calculate and save (cache) the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) { ##creates an object with four properties in the form of a list
			inv <- NULL
            set <- function(y) { ##property "set" allows the user to define a matrix and set its inverse to NULL
                    x <<- y
                   inv <<- NULL
            }
            get <- function() x ##property "get" allows the user to retrieve the value of the matrix
            setinv <- function(inverse) inv <<- inverse ##property "setinv" allows the user to define the inverse of a matrix
            getinv <- function() inv ##property "getinv" allows the user to retrieve the value of the inverse of a matric
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv) ##define matrix object with the properties described above

}


## Function "CacheSolve" returns the inverse of the matrix  by checking whether it has already been calculated and saved (cached). If not, the inverse is calculated and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getinv()		##define inv as the content of the property "getinv"
            if(!is.null(inv)) { ##if the inverse has been already calculated, just...
                    message("getting cached data")
                    return(inv) ##print a message and return the inverse;
            }
            data <- x$get() ##otherwise retrieve the matrix to be inverted...
            inv <- solve(data, ...) ##compute the inverse...
            x$setinv(inv)	##set the value of the matrix's inverse so it's in memory for the next time and...
            inv     		##return the inverse        

}
