# PayneProfileR

An R package for calculating kill-off profiles of caprines using tooth wear stages, based on the method proposed by Sebastian Payne (1973). The package includes graphical outputs and options to visualize variants proposed by Helmer et al. (2007) and compare survival curves to idealized models (meat, milk, wool strategies).

## Installation

You can install the development version from GitHub using:

``` r
# install.packages("remotes") if needed
remotes::install_github("your-username/PayneProfileR")
```

## Usage

``` r
library(PayneProfileR)

# Example dataset
data <- data.frame(
  stage = c("A", "AB", "B", "C", "DEF"),
  count = c(5, 4, 6, 3, 8)
)

result <- payne_profile(data, ef_merge = TRUE, add_ideal_curves = TRUE)

# Plots
print(result$barplot)
print(result$curve)
print(result$barplot_helmer)
print(result$ideal_curve)
```

## Outputs

-   Table of absolute and relative frequencies by stage
-   Classical barplot (Payne 1973)
-   Cumulative survival curve
-   Helmer-style barplot with proportional widths (Helmer et al. 2007)
-   Comparative survival curve with idealized meat/milk/wool models

## References

-   Payne, S. (1973). Kill-off patterns in sheep and goats: the mandibles from Asvan Kale. Anatolian Studies, 23, 281–303.
-   Helmer, D., Gourichon, L., & Vila, E. (2007). The development of the exploitation of products from Capra and Ovis (meat, milk and fleece) from the PPNB to the Early Bronze in the northern Near East (8700 to 2000 BC). Anthropozoologica, 42(2), 41–69.
