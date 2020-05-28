# DMCfun
R/Cpp implementation of the diffusion process model (Diffusion Model for
Conflict Tasks, DMC) presented in Automatic and controlled stimulus processing
in conflict tasks: Superimposed diffusion processes and delta functions
(https://www.sciencedirect.com/science/article/pii/S0010028515000195)

## Installation

``` r
# install version from  GitHub
# install.packages("devtools")
devtools::install_github("igmmgi/DMCfun")
```

---
## Basic Examples DMC Simulation
``` r
dmc <- dmcSim(fullData = TRUE)
plot(dmc)
```
![alt text](figures/figure1.png)     

``` r
dmc$means
  Comp   rtCor sdRtCor perErr rtErr sdRtErr
1 comp    440.   105.   0.633  479.   104. 
2 incomp  459.    94.8  1.38   406.    95.2
```

``` r
dmc <- dmcSim(fullData = TRUE, tau = 150)
plot(dmc)
```
![alt text](figures/figure2.png)     

``` r
dmc$means
  Comp   rtCor sdRtCor perErr rtErr sdRtErr
1 comp    421.    90.4  0.259  504.   119. 
2 incomp  484.   103.   2.37   425.    82.7
```

``` r
params <- list(tau = seq(20, 170, 10))
dmc <- dmcSims(params)
plot(dmc, ncol = 2, col = c("red", "green"))
```
![alt text](figures/figure4.png)     

## Basic Examples DMC Fit: Real data
``` r
fit <- dmcFitAgg(flankerData) # flanker data from Ulrich et al. (2015)
plot(fit, flankerData)
```
![alt text](figures/figure5.png)     

``` r
summary(fit)
    amp   tau    mu  bnds resMean resSD aaShape spShape sigm  rmse
1  19.3  98.8 0.593  55.8    325.  28.4    2.26    2.84     4  8.91
```

``` r
fit <- dmcFitAgg(simonData) # simon data from Ulrich et al. (2015)
plot(fit, simonData)
```
![alt text](figures/figure6.png)     

``` r
    amp   tau    mu  bnds resMean resSD aaShape spShape sigm  rmse
1  17.5  33.8 0.577  58.7    311.  29.9    2.31    3.41     4  11.5
```
