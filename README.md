# DMCfun
R/Cpp implementation of the diffusion process model (Diffusion Model for Conflict Tasks, DMC) presented in Automatic and controlled stimulus processing in conflict tasks: Superimposed diffusion processes and delta functions (https://www.sciencedirect.com/science/article/pii/S0010028515000195)

## Installation

``` r
# install version from  GitHub
# install.packages("devtools")
devtools::install_github("igmmgi/DMCfun")
```

## Basic Examples

``` r
# Example 1 
dmc <- dmcSim(fullData = TRUE)
plot(dmc)

# Example 2 
dmc <- dmcSim(fullData = TRUE, tau = 130)
plot(dmc)
```

