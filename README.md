# BIOMASSapp

## Description
BIOMASSapp is the Shiny application of the R BIOMASS package. It allows the user to estimate above-ground biomass and its uncertainty in tropical forests. This application  allows to (1) retrieve and correct tree taxonomy, (2) estimate wood density and its uncertainty, (3) build height-diameter models, (4) estimate above-ground biomass at stand level with associated uncertainty. To cite ‘BIOMASS’, please use citation(‘BIOMASS’). For more information, see [Réjou-Méchain et al. (2017)](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12753).

## Website link

You can access the application at the following adress (hosted by the CIRAD institution thanks to Guillaume CORNU): 

[https://amap-apps.cirad.fr/apps/biomass-app/](https://amap-apps.cirad.fr/apps/biomass-app/)


## Installation (development version) & Lauch

In a R session: 

```r
install.packages("remotes")
remotes::install_github('umr-amap/BIOMASSapp')

BIOMASSapp::run_app()
```

## Citation

Please cite this package as:

*Réjou-Méchain M, Tanguy A, Piponiot C, Chave J, Herault B* (2017). “BIOMASS : an R package for estimating above-ground biomass and its uncertainty in tropical forests.” _Methods in Ecology and Evolution_, *8*(9). ISSN 2041210X, [doi:10.1111/2041-210X.12753](https://doi.org/10.1111/2041-210X.12753).

Or you can also run 

``` r
citation("BIOMASS")
```
