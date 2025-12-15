# Custom ggplot Theme

A minimalist custom ggplot theme with configurable font size and font
family.

## Usage

``` r
theme_baseR(font_size = 10, font_family = "sans")
```

## Arguments

- font_size:

  Numeric. The base font size for text elements in the plot. Default is
  12.

- font_family:

  Character. The font family to use for all text elements. Default is
  "sans".

## Value

A ggplot2 theme object.

## Examples

``` r
library(ggplot2)

# Example plot with the default theme
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  theme_baseR()


# Custom font size and font family
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  theme_baseR(font_size = 14, font_family = "serif")

```
