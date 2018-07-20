makeCacheMatrix <- function(x = matrix()) {
  # set init values and define functions
  m.inv <- NULL
  set.matrix <- function(y) { x <<- y; m.inv <<- NULL }
  get.matrix <- function() x
  setMI <- function(inv.m) m.inv <<- inv.m
  getMI <- function() m.inv
  
  # return list with functions as result
  list(set.matrix = set.matrix, get.matrix = get.matrix, setMI = setMI, getMI = getMI)
}


cacheSolve <- function(x, ...) {
  # make an invers x-matrix
  m.inv <- x$getMI()
  if(!is.null(m.inv)) { message("getting cached matrix inverse"); return(m.inv) }
  
  # find inverse data and cache it
  data <- x$get.matrix()
  m.inv <- solve(data, ...)
  x$setMI(m.inv)
  m.inv
}