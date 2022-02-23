# install.packages(c("knitr", "bookdown"))

library(lubridate)
library(dplyr)
library(tidyverse)
library(reshape2)
library(corrplot)
library(ggplot2)
library(ggpmisc)
library(sf)
library(rnaturalearth)
library(fastICA)
library(FactoMineR)
library(factoextra)
library(knitr)

options(encoding = "UTF-8")

SEED = 360
set.seed(SEED)

theme_plot = theme(title = element_text(),
                  plot.title = element_text(margin = margin(5,5,5,5), size = 16, hjust = 0.5))

id_sondes_list = c(825, 827, 828, 830)

# load sonde coordinates
coordinates_df = as.data.frame(read.csv(file="sensors_coordinates.csv", header=TRUE, sep=",", dec=".", na.strings=NA))
coordinates_df = coordinates_df[coordinates_df$TopoOH == "La Touques", c("id_sonde", "lib_sonde", "lat_wgs84", "lon_wgs84")]
coordinates_df$color = rep("black", nrow(coordinates_df))
coordinates_df$color[coordinates_df$id_sonde %in% id_sondes_list] = "red"

df = as.data.frame(read.csv(file="base.csv", header=TRUE, sep=";", dec=",", row.names=1, na.strings=NA))
df$date = ymd(df$date)
df$t = ymd_hms(df$t)
df$id_sonde = as.factor(df$id_sonde)


custom_summary = function(df) {
    quantitative_summary = function(column, epsilon=1e-20) {
        col_min = min(column)
        col_max = max(column)
        col_first_quantile = quantile(column, 0.25)
        col_median = median(column)
        col_third_quantile = quantile(column, 0.75)
        col_mean = mean(column)
        col_geometric_mean = exp(mean(log(column - col_min + epsilon))) + col_min - epsilon
        col_etendue = max(column) - min (column)
        col_iqr = col_third_quantile - col_first_quantile
        col_var = var(column)
        col_sd = sd(column)
        col_coeff_vaiation = col_sd / col_mean
        
        mat = matrix(c(col_min, col_max, col_first_quantile, col_median, col_third_quantile, col_mean, col_geometric_mean, col_etendue, col_iqr, col_var, col_sd, col_coeff_vaiation), byrow=TRUE, nrow=1)
        colnames(mat) = c("Min", "Max", "1st quantile", "Median", "3rd quantile", "Mean", "Geometric mean", "Value range", "IQR", "Variance", "Standard deviation", "Variation coefficient")
        return(mat)
    }

    qualitative_summary = function(column) {
        return(summary(column))
    }

    datetime_summary = function(column) {
        return(list(Min = min(column), Max = max(column)))
    }

    date_summary = function(column) {
        return(list(Min = min(column), Max = max(column)))
    }

    l = list()
    for(n in names(df)) {
        if(class(df[[n]]) == "Date") {
            l[[n]] = date_summary(df[[n]])
        } else if(class(df[[n]]) == "POSIXct" || class(df[[n]]) == "POSIXt") {
            l[[n]] = datetime_summary(df[[n]])
        } else if(class(df[[n]]) == "factor") {
            l[[n]] = qualitative_summary(df[[n]])
        } else if(class(df[[n]]) == "numeric") {
            l[[n]] = quantitative_summary(df[[n]])
        } else {
            l[[n]] = summary(df[[n]])
        }
    }
    return(l)
}



rmarkdown::render(input = "report.rmd", output_file = "report.pdf")
