library(hdf5)
## Read the values as integers (reals are the default).
v <- scan ("sugarspace.pgm", what=integer(0), skip=3)

## Construct a matrix out of the vector, by telling it the number
## of rows and that it should load row major order (by default it does
## column major like FORTRAN).

m <- matrix (v, nrow=50, byrow=TRUE)

## ivars <- data.frame(xsize=50,ysize=50)

maxSugarDiscrete2d <-list(lattice=m)
attr(maxSugarDiscrete2d, "type") <- "Discrete2d"

hdf5save ("sss.hdf", "maxSugarDiscrete2d")
