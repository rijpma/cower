# cower

R-implementation of [COW](https://github.com/CLARIAH/COW) to convert csv files to nquads using json-ld data description and `data.table` for speed.

## Installation.
```
library("devtools")
devtools::install_github("rijpma/cower")
```

You may need to manually install V8 as an external dependency, see the [V8 package](https://cran.r-project.org/package=V8).