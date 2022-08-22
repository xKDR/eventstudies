# eventstudies

[![Build Status](https://travis-ci.org/nipfpmf/eventstudies.svg?branch=master)](https://travis-ci.org/nipfpmf/eventstudies)

An R package for conducting event studies and a platform for
methodological research on event studies.
Laptops are waving. 
## Installation

### Stable version
**Release notes**: https://github.com/nipfpmf/eventstudies/releases/tag/v1.2.2

Current stable version of `eventstudies` is `v1.2.2`. It can be
installed directly from CRAN or via GitHub.

* CRAN URL: https://cran.r-project.org/web/packages/eventstudies/index.html
```R
install.packages("eventstudies")                                                    
```

* GitHub
```R                                                                  
devtools::install_github("nipfpmf/eventstudies", ref="v1.2.2") 
```

### Latest version

Latest/unstable version of the package can be installed via GitHub:
```R
devtools::install_github("nipfpmf/eventstudies", ref="master")
```

## Usage

```R
data("SplitDates", package = "eventstudies")
data("StockPriceReturns", package = "eventstudies")
data("OtherReturns", package = "eventstudies")

es <- eventstudies::eventstudy(firm.returns = StockPriceReturns,
         event.list = SplitDates,
         event.window = 7,
         type = "marketModel",
         to.remap = TRUE,
         remap = "cumsum",
         inference = TRUE,
         inference.strategy = "bootstrap",
         model.args = list(
             market.returns = OtherReturns[, "NiftyIndex"]
             )
         )
plot(es)
```

## Help
```R
?eventstudy
vignette("eventstudies", package = "eventstudies")
examples("eventstudy", package = "eventstudies")
```

## Contributing

1. Fork it!
2. Create your feature branch: `git checkout -b my-new-feature`
3. Commit your changes: `git commit -am 'Add some feature'`
4. Push to the branch: `git push origin my-new-feature`
5. Submit a pull request

## History

This repository was moved from R-forge before the release of v1.2:

<https://r-forge.r-project.org/projects/eventstudies>

## License

GPL-2
