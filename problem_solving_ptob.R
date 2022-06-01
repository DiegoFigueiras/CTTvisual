library(ggplot2)

coeficients <- read.csv("pvalue_to_b_estimates.csv")

ggplot(data = coeficients, aes(x = intercept)) + geom_histogram(bins = 50) + facet_grid(simulation~.)
ggplot(data = coeficients, aes(x = slope)) + geom_histogram(bins = 500) + facet_grid(simulation~.)

ggplot(data = coeficients, aes(x = scrubbedn)) + geom_histogram() + xlim(0, 100) + facet_grid(simulation~.)

psych::describeBy(coeficients, coeficients$simulation)
