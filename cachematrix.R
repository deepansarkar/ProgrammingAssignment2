# This program is able to cache the time consuming process of calculating
# the inverse of a matrix. The program will display the previously
# calculated value of the matrix inverse if the data matrix is unchanged.
#
# Created by  : Deepan Sarkar
# Created on  : 27 July 2014
# Modified on : 27 July 2014

# Write a short comment describing this function
# This function creates special 'matrix' which is hust a list of functions
# which can :
#   Set the value of the matrix
#   Get the value of the matrix
#   Set the value of the inverse of the matrix
#   Get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse<- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# This function calculates the inverse of the special 'matrix' created
# but if the inverse has already been calculated then it returns the
# previous calcluated inversee
cacheSolve <- function(x, ...)
{
  inv <- x$getInverse()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
