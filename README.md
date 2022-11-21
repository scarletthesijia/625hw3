# Package: mylr

2022/11/17

### A package of R for linear regression

### Version: 0.1.0

## Author: Scarlett He

## Description: There are 4 functions in this package, mylm.basic, mylm.detailed, describe.data and mylm.predict.
​    mylm.basic is the most basic linear regression function, which returns the regressed model.
​    mylm.detailed returns the same value as basic, but plots a diagnostic plot of the linear regression, printing each parameter of the model.
​    describe.data can plot a histogram of the data and print the common statistical indicators of the data.
​    mylm.predict uses the incoming model and independent variables to make predictions about the dependent variable and plots scatter plots of the independent and dependent variables.

## Suggests: 

* knitr
* rmarkdown
* testthat (>= 3.0.0)
* VignetteBuilder: knitr
* Config/testthat/edition: 3
