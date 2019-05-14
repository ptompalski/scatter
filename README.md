# scatter

This package contains functions that allow to quickly calculate measures of model prediction accuracy (e.g. rmse), create scatterplots that contain text informing on the agreement between the two plotted variables. 

## Installation 
```
install.packages("devtools")
devtools::install_github("ptompalski/scatter")
library(scatter)
```


## Brief description of selected funcions

Function `calc.error()` is useful for quickly calculating bias and RMSE (and other) between observed and predicted values. This function also check the statistical significance of the differences. For example:

```
ref <- iris$Sepal.Length
est <- predict(lm(data=iris,iris$Sepal.Length~iris$Petal.Width))
calc.error(reference = ref, estimate = est)
```
Optionally a grouping variable can be added:
```
grouping_var <- iris$Species
calc.error(reference = ref, estimate = est,by = grouping_var)
```
I recommend setting the parameter `noinfo = F` during the first run and see how the absolute and relative bias are calculated.

## Plot templates

There are two plot templates: one for scatterplot (`scatter()`) and one for histogram (`h()`). 


Using:
```
ref <- iris$Sepal.Length
est <- predict(lm(data=iris,iris$Sepal.Length ~ iris$Petal.Width))
scatter(x = ref, y = est)
```
will produce a scatter plot with additional information (text box) on the bias and RMSE. Optionally you can disable this by setting `info = F`. 

The default behaviour of `h()` is to plot a histogram with chosen descriptive statistics added in a corner. You can turn them off by setting `info = F`. You can also very easly define the width of the histogram bins (much easier than with standard `hist()`) with:
```
h(ref,0.1)
```


Both `scatter()` and `h()` use ggplot2 package to produce plots. The returned ggplot objects can be 
further modified with additional options:

```
h(iris$Sepal.Length) + theme_minimal()
```
