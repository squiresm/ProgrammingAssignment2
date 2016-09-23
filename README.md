### Summary

This git project addresses the assignment requirements for the Coursera R Programming course.
The file cachematrix.R contains two functions required, namely:
1. makeCacheMatrix and
2. cacheSolve

Note that there is no implementation of ensuring that the matrix is invertible, as per the 
assignment assumption

>For this assignment, assume that the matrix supplied is always invertible.

That said, I have implemented some rudimentary checks to ensure that the passed matrix
has the following characteristics:
1. It is a numeric matrix and
2. It has a square structure (non-square matrices not offering inversion)

