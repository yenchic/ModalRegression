# Modal Regression

Performing nonparametric modal regression based on kernel density estimator.
- Paper reference: Chen, Yen-Chi, et al. "Nonparametric modal regression." The Annals of Statistics 44.2 (2016): 489-514.
- Contact: yenchic@uw.edu

## ModalRegression.R:
This contains two functions: RegMS1d and RegMS2d.

### RegMS1d
`RegMS1d = function(X, Y, G.x=X, G.y=Y, h.x, h.y, iter=100, tolerance=1e-8)`

- Input: 
  - X: covariate, 1 dimension
  - Y: response, 1 dimension
  - G.x: grid of covariate
  - G.y: grid of response
  - h.x: smoothing parameter for covariate
  - h.y: smoothing parameter for response
  - iter: maximal number of iteration
  - tolerance: the tolerance level of shifting step

- Output:
  - Estimated local modes from each point of (G.x, G.y)


### RegMS2d
`RegMS2d = function(X,Y, G.x=X, G.y=Y, h.x, h.y, iter=100,tolerance=1e-8)`

- Input: 
  - X: covariate, 2 dimension
  - Y: response, 1 dimension
  - G.x: grid of covariate
  - G.y: grid of response
  - h.x: smoothing parameter for covariates
  - h.y: smoothing parameter for response
  - iter: maximal number of iteration
  - tolerance: the tolerance level of shifting step

- Output:
  - Estimated local modes from each point of (G.x, G.y)


## Ex1_DualCurve.R:
A script on the dual curve simulation data. The analysis includes estimating the modal curves, prediction sets and comparison to the local regression.

- Section 1: Generate Data
- Section 2: Modal regression
- Section 3: Representing by curves 
- Section 4: Prediction set (non-optimized)
- Section 5: Comparison to local regression


## Ex2_3Mixture.R:
A script on the three mixture simulation data. The analysis is essentially the same as dual curve but we also present bandwidth selection and comparison to mixture regression.

- Section 1: Generate Data
- Section 2: Modal Regression
- Section 3: Bandwidth selection via minizming prediction set
- Section 4: Optimal result
- Section 5: Prediction set
- Section 6: Local regression
- Section 7: Mixture regression
