---
output:
  html_document:
    df_print: paged
---
# DMCfun
R/Cpp implementation of the diffusion process model (Diffusion Model for Conflict Tasks, DMC) presented in Automatic and controlled stimulus processing in conflict tasks: Superimposed diffusion processes and delta functions (https://www.sciencedirect.com/science/article/pii/S0010028515000195)

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
![alt text](/figures/figure1.png)     

``` r
dmc <- dmcSim(fullData = TRUE, tau = 150)
plot(dmc)
```
![alt text](/figures/figure2.png)     

``` r
params <- list(tau = seq(20, 170, 10))
dmc <- dmcSims(params)
plot(dmc, ncol = 2, col = c("red", "green"))
```
![alt text](/figures/figure4.png)     

## Basic Examples DMC Fit: Real data
``` r
fit <- dmcFitAgg(flankerData) # flanker data from Ulrich et al. (2015)
plot(fit, flankerData)
```

![alt text](/figures/figure5.png)     

``` r
fit <- dmcFitAgg(simonData) # simon data from Ulrich et al. (2015)
plot(fit, simonData)
```

![alt text](/figures/figure6.png)     
