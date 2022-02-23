# install.packages(c("lubridate", "dplyr", "tidyverse", "reshape2", "corrplot", "ggplot2", "ggpmisc", "sf", "rnaturalearth", "fastICA", "FactoMineR", "factoextra"))

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

set.seed(360)

theme_plot = theme(title = element_text(),
                  plot.title = element_text(margin = margin(20,20,20,20), size = 18, hjust = 0.5))

if(dir.exists("graphics")){
  unlink("graphics", recursive=TRUE)
}
dir.create("graphics")

id_sondes_list = c(825, 827, 828, 830)

# load sonde coordinates
coordinates_df = as.data.frame(read.csv(file="sensors_coordinates.csv", header=TRUE, sep=",", dec=".", na.strings=NA))
coordinates_df = coordinates_df[coordinates_df$TopoOH == "La Touques", c("id_sonde", "lib_sonde", "lat_wgs84", "lon_wgs84")]
coordinates_df$color = rep("black", nrow(coordinates_df))
coordinates_df$color[coordinates_df$id_sonde %in% id_sondes_list] = "red"

# load map and center of the zoomed map
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
france = worldmap[worldmap$name == 'France',]
df_caen_coord = data.frame(lon = -0.370679, lat = 49.182863)

zoom_level = 7
lon_span = 360 / 2^zoom_level
lat_span = 180 / 2^zoom_level
lon_bounds <- c(df_caen_coord$lon - lon_span / 2, df_caen_coord$lon + lon_span / 2)
lat_bounds <- c(df_caen_coord$lat - lat_span / 2, df_caen_coord$lat + lat_span / 2)

attach(coordinates_df)
png("graphics/map.png")
ggplot() + 
    geom_sf(data = france) + 
    geom_point(aes(data = df_caen_coord, x = df_caen_coord$lon, y = df_caen_coord$lat, label = "Caen"), color = "blue", size = 5) + 
    geom_text(aes(data = df_caen_coord, x = df_caen_coord$lon, y = df_caen_coord$lat, label = "Caen"), hjust = -0.5, vjust = 0.25) +
    geom_point(data = coordinates_df, aes(x = lon_wgs84, y = lat_wgs84, label = id_sonde), color = color) + 
    geom_text(aes(x = lon_wgs84, y = lat_wgs84, label = id_sonde), hjust = -0.5, vjust = 0.25) +
    coord_sf(xlim = lon_bounds, ylim = lat_bounds) + 
    theme_bw() +
    labs(x = "Longitude", 
        y = "Latitude", 
        title = "Sondes de la Touques") +
    theme_plot
dev.off()
detach(coordinates_df)

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

concatenated_summary = custom_summary(df)
concatenated_summary

df$year = year(df$date)
df$month = month(df$date)
df$day = day(df$date)
df$hour = hour(df$t)

sub_df = df[df$id_sonde %in% id_sondes_list,][,c("Teau", "Tair.EOBS", "Rainf.EOBS", "year", "month", "day", "hour", "id_sonde")]

cor_matrix = cor(df[,c("year", "month", "day", "hour", "Tair.EOBS", "Teau", "Rainf.EOBS")])
png("graphics/corr_variables.png")
color = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor_matrix, method = "color", diag = FALSE, type = "upper",
         col = color(200), order = "hclust", title = "Corrélations entre variables",
         addCoef.col = "black", sig.level = 0.05, insig = "blank", mar = c(0,0,1,0)) +
  theme_plot
dev.off()


new_df = df[df$year %in% 2014:2017,]
cor_matrix_new_df = cor(new_df[,c("year", "month", "day", "hour", "Tair.EOBS", "Teau", "Rainf.EOBS")])
png("graphics/corr_variables_fixed.png")
color = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor_matrix_new_df, method = "color", diag = FALSE, type = "upper",
         col = color(200), order = "hclust", title = "Matrice de corrélations corrigée",
         addCoef.col = "black", sig.level = 0.05, insig = "blank", mar = c(0,0,1,0)) +
  theme_plot
dev.off()


attach(sub_df)
png("graphics/histogram_Rainf.png")
ggplot(sub_df, aes(x = Rainf.EOBS)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 20) +
  geom_density(lwd = 1.2, alpha = 0.25, colour = "darkblue", fill = "darkblue") + 
  geom_vline(aes(xintercept = mean(Rainf.EOBS)),
            color = "red", size = 1) + 
  labs(title = "Histogramme sur les\nvolumes de pluie tombés",
       x = "Volumétrie de pluie (en mm)", y = "Densités") +
  geom_text(aes(x = mean(Rainf.EOBS) + 3.5, label = paste0("Moyenne :\n", format(round(mean(Rainf.EOBS), 3), nsmall = 3), " (mm)"), y = 1)) + 
  theme_plot
dev.off()
png("graphics/histogram_Teau.png")
ggplot(sub_df, aes(x = Teau)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 30) +
  geom_density(lwd = 1.2, alpha = 0.25, colour = "darkblue", fill = "darkblue") + 
  geom_vline(aes(xintercept = mean(Teau)),
            color = "red", size = 1) + 
  labs(title = "Histogramme sur les\ntempératures prises par l'eau",
       x = "Température de l'eau (en °C)", y = "Densités") +
  geom_text(aes(x = mean(Teau) + 3.5, label = paste0("Moyenne :\n", format(round(mean(Teau), 3), nsmall = 3), " (°C)"), y = 0.115)) + 
  theme_plot
dev.off()
png("graphics/histogram_Tair.png")
ggplot(sub_df, aes(x = Tair.EOBS)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 30) +
  geom_density(lwd = 1.2, alpha = 0.25, colour = "darkblue", fill = "darkblue") + 
  geom_vline(aes(xintercept = mean(Tair.EOBS)),
            color = "red", size = 1) + 
  labs(title = "Histogramme sur les\ntempératures prises par l'air",
       x = "Température de l'air (en °C)", y = "Densités") +
  geom_text(aes(x = mean(Tair.EOBS) + 3.5, label = paste0("Moyenne :\n", format(round(mean(Tair.EOBS), 3), nsmall = 3), " (°C)"), , y = 0.0675)) + 
  theme_plot
dev.off()

png("graphics/density_Teau_each_sensors.png")
ggplot(sub_df, aes(x = Teau, fill = id_sonde, colour = id_sonde)) +
  geom_density(aes(y = ..density..), lwd = 1.2, alpha = 0.25) + 
  labs(title = "Densités des températures\nprises par l'eau",
       x = "Température de l'eau (en °C)", y = "Nombre de valeurs", colour = "ID sonde", fill = "ID sonde") +
  theme_plot
dev.off()
detach(sub_df)


# boîtes à moustaches pour la température de l'air
month_by_year = sub_df$year * 100 + sub_df$month
# boites moustache : par sonde
attach(sub_df)
png("graphics/boxplot_Tair_by_sensors.png")
ggplot(sub_df, aes(x = id_sonde, y = Tair.EOBS)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4) + 
  labs(title = "Boîtes à moustache pour chaque sonde",
       x = "Sondes", y = "Température de l'air (en °C)") +
  theme_plot
dev.off()

# boites moustache : par années
png("graphics/boxplot_Tair_by_years.png")
ggplot(sub_df, aes(x = as.factor(year), y = Tair.EOBS)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4) + 
  labs(title = "Boîtes à moustache pour chaque années",
       x = "Années", y = "Température de l'air (en °C)") +
  theme_plot
dev.off()

# boites à moustache par mois
png("graphics/boxplot_Tair_by_months.png")
ggplot(sub_df, aes(x = as.factor(month), y = Tair.EOBS)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4) + 
  labs(title = "Boîtes à moustache pour chaque mois",
       x = "Mois", y = "Température de l'air (en °C)") +
  theme_plot
dev.off()

# boites à moustache par mois et années
png("graphics/boxplot_Tair_by_year_and_months.png", width = 1200, height = 800)
ggplot(sub_df, aes(x = as.factor(month_by_year), y = Tair.EOBS)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title = "Boîtes à moustache pour chaque mois et années",
       x = "Année/Mois (AAAAMM)", y = "Température de l'air (en °C)") +
  theme_plot
dev.off()
detach(sub_df)

# boîtes à moustaches pour la volumétrie
# boites moustache : par sonde
attach(sub_df)
png("graphics/boxplot_Rainf_by_sensors.png")
ggplot(sub_df, aes(x = id_sonde, y = Rainf.EOBS)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4) + 
  labs(title = "Boîtes à moustache pour chaque sonde",
       x = "Sondes", y = "Volume de pluie (en mm)") +
  theme_plot
dev.off()

# boites moustache : par années
png("graphics/boxplot_Rainf_by_years.png")
ggplot(sub_df, aes(x = as.factor(year), y = Rainf.EOBS)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4) + 
  labs(title = "Boîtes à moustache pour chaque années",
       x = "Années", y = "Volume de pluie (en mm)") +
  theme_plot
dev.off()

# boites à moustache par mois
png("graphics/boxplot_Rainf_by_months.png")
ggplot(sub_df, aes(x = as.factor(month), y = Rainf.EOBS)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4) + 
  labs(title = "Boîtes à moustache pour chaque mois",
       x = "Mois", y = "Volume de pluie (en mm)") +
  theme_plot
dev.off()

# boites à moustache par mois et années
png("graphics/boxplot_Rainf_by_year_and_months.png", width = 1200, height = 800)
ggplot(sub_df, aes(x = as.factor(month_by_year), y = Rainf.EOBS)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title = "Boîtes à moustache pour chaque mois et années",
       x = "Mois", y = "Volume de pluie (en mm)") +
  theme_plot
dev.off()
detach(sub_df)


test_df = sub_df[,c("Rainf.EOBS", "Tair.EOBS")]
attach(test_df)
png("graphics/linear_regression_Tair_Rainf.png")
y = Rainf.EOBS
x = Tair.EOBS
my.formula = y ~ x
ggplot(data = test_df, aes(x = Tair.EOBS, y = Rainf.EOBS)) +
  geom_point() + 
  geom_smooth(method = lm, formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
               parse = TRUE) +
  labs(title = "Volumétrie de pluie en fonction\nde la température de l'air entre 2013-2018",
       x = "Température de l'air (en °C)", y = "Volumétrie de pluie (en mm)", colour = "Sonde ID") +
  theme_plot
dev.off()
detach(test_df)
test_df = sub_df[,c("Rainf.EOBS", "Teau")]
attach(test_df)
png("graphics/linear_regression_Teau_Rainf.png")
y = Rainf.EOBS
x = Teau
my.formula = y ~ x
ggplot(data = test_df, aes(x = Teau, y = Rainf.EOBS)) +
  geom_point() + 
  geom_smooth(method = lm, formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
               parse = TRUE) +
  labs(title = "Volumétrie de pluie en fonction\nde la température de l'eau entre 2013-2018",
       x = "Température de l'eau (en °C)", y = "Volumétrie de pluie (en mm)", colour = "Sonde ID") +
  theme_plot
dev.off()
detach(test_df)
test_df = sub_df[,c("Rainf.EOBS", "Tair.EOBS", "id_sonde")]
attach(test_df)
png("graphics/linear_regression_Tair_Rainf_by_sensor.png")
y = Rainf.EOBS
x = Tair.EOBS
my.formula = y ~ x
ggplot(data = test_df, aes(x = Tair.EOBS, y = Rainf.EOBS, color = id_sonde)) +
  geom_point() + 
  geom_smooth(method = lm, formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
               parse = TRUE) +
  labs(title = "Volumétrie de pluie en fonction de la\ntempérature de l'air entre 2013-2018",
       x = "Température de l'air (en °C)", y = "Volumétrie de pluie (en mm)", colour = "Sonde ID") +
  theme_plot
dev.off()
detach(test_df)

test_df = sub_df[,c("month", "Rainf.EOBS", "id_sonde")]
attach(test_df)
png("graphics/dispersion_by_months_Rainf.png")
ggplot(data = test_df, aes(x = month, y = Rainf.EOBS, colour = id_sonde)) +
  geom_point() + 
  labs(title = "Volumétrie de pluie en fonction\ndes mois de l'année entre 2013-2018",
       x = "Mois de l'année", y = "Volumétrie de pluie (en mm)", colour = "Sonde ID") +
  theme_plot
dev.off()
detach(test_df)


# regression pour chaque sonde
attach(sub_df)
png("graphics/linear_regression_Tair_Teau_by_sensors.png")
y = Teau
x = Tair.EOBS
my.formula = y ~ x
ggplot(data = sub_df, aes(x = x, y = y, colour = id_sonde)) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE, formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
               parse = TRUE) +
  labs(title = "Température de l'eau en fonction\nde celle de l'air entre 2013-2018",
       x = "Température de l'air (en °C)", y = "Température de l'eau (en °C)", colour = "Sonde ID") +
  theme_plot
dev.off()
detach(sub_df)


# ACI
# preparation données : Teau
base_825 = df %>% filter(df$id_sonde == 825)
base3_825 = aggregate(Teau~date + id_sonde, data = base_825, FUN = mean, na.rm = TRUE)
base3_825 = mutate(base3_825, Teau825 = Teau)

base_827 = df %>% filter(df$id_sonde == 827)
base3_827 = aggregate(Teau~date + id_sonde, data = base_827, FUN = mean, na.rm = TRUE)
base3_827 = mutate(base3_827, Teau827 = Teau)

base_828 = df %>% filter(df$id_sonde == 828)
base3_828 = aggregate(Teau~date + id_sonde, data = base_828, FUN = mean, na.rm = TRUE)
base3_828 = mutate(base3_828, Teau828 = Teau)

base_830 = df %>% filter(df$id_sonde == 830)
base3_830 = aggregate(Teau~date + id_sonde, data = base_830, FUN = mean, na.rm = TRUE)
base3_830 = mutate(base3_830, Teau830 = Teau)

base_f = merge(merge(base3_825, base3_827, by="date"), merge(base3_828, base3_830, by="date"), by="date")
base_f = select(base_f, date, Teau825, Teau827, Teau828, Teau830)

custom_summary(base_f)

# utilisation de fastICA
base_ica = select(base_f, -date)
set.seed(360)
ica = fastICA(base_ica, 2, alg.typ = "parallel", fun = "logcosh",
              alpha = 1, method = "R", row.norm = FALSE, maxit = 200,
              tol = 1e-4, verbose = FALSE)
ica$A # à afficher sur le rapport pour analyse

S = data.frame(ica$S)
B = cbind(base_f, S)

# pour les 4 sondes
png("graphics/ica_2components_for_all_sensors.png")
attach(B)
ggplot(B, aes(date)) +
    geom_line(aes(y = sign(mean(ica$A[1,])) * X1, colour = "1ere")) + 
    geom_line(aes(y = sign(mean(ica$A[2,])) * X2 - 6, colour = "2eme (centrée à -6)")) + 
    labs(title = "ACI plot",
        y = "Température de l'eau (en °C) normalisées", x = "Date", colour = "Composantes\nindépendantes") +
    theme_plot
detach(B)
dev.off()

# pour la sonde 825
B$comp1_825 = ica$A[1,1] * ica$S[,1]
B$comp2_825 = ica$A[2,1] * ica$S[,2]
png("graphics/ica_2components_for_sensor825.png")
attach(B)
ggplot(B, aes(date)) +
    geom_line(aes(y = comp1_825, colour = "1ere")) + 
    geom_line(aes(y = comp2_825 - 10, colour = "2eme (centrée à -10)")) + 
    labs(title = "ACI plot (sonde 825)",
        y = "Température de l'eau (en °C)", x = "Date", colour = "Composantes\nindépendantes") +
    theme_plot
detach(B)
dev.off()

# pour la sonde 827
B$comp1_827 = ica$A[1,2] * ica$S[,1]
B$comp2_827 = ica$A[2,3] * ica$S[,2]
png("graphics/ica_2components_for_sensor827.png")
attach(B)
ggplot(B, aes(date)) +
    geom_line(aes(y = comp1_827, colour = "1ere")) + 
    geom_line(aes(y = comp2_827 - 10, colour = "2eme (centrée à -10)")) + 
    labs(title = "ACI plot (sonde 827)",
        y = "Température de l'eau (en °C)", x = "Date", colour = "Composantes\nindépendantes") +
    theme_plot
detach(B)
dev.off()

# pour la sonde 828
B$comp1_828 = ica$A[1,3] * ica$S[,1]
B$comp2_828 = ica$A[2,3] * ica$S[,2]
png("graphics/ica_2components_for_sensor828.png")
attach(B)
ggplot(B, aes(date)) +
    geom_line(aes(y = comp1_828, colour = "1ere")) + 
    geom_line(aes(y = comp2_828 - 10, colour = "2eme (centrée à -10)")) + 
    labs(title = "ACI plot (sonde 828)",
        y = "Température de l'eau (en °C)", x = "Date", colour = "Composantes\nindépendantes") +
    theme_plot
detach(B)
dev.off()

# pour la sonde 830
B$comp1_830 = ica$A[1,4] * ica$S[,1]
B$comp2_830 = ica$A[2,4] * ica$S[,2]
png("graphics/ica_2components_for_sensor830.png")
attach(B)
ggplot(B, aes(date)) +
    geom_line(aes(y = comp1_830, colour = "1ere")) + 
    geom_line(aes(y = comp2_830 - 10, colour = "2eme (centrée à -10)")) + 
    labs(title = "ACI plot (sonde 830)",
        y = "Température de l'eau (en °C)", x = "Date", colour = "Composantes\nindépendantes") +
    theme_plot
detach(B)
dev.off()

# set.seed(360)
# ica2 = fastICA(base_ica, 3, alg.typ = "parallel", fun = "logcosh",
#               alpha = 1, method = "R", row.norm = FALSE, maxit = 200,
#               tol = 1e-4, verbose = FALSE)
# ica2$A # à afficher sur le rapport pour analyse

# S = data.frame(ica2$S)
# B2 = cbind(base_f, S)

# # pour les 4 sondes
# png("graphics/ica_3components_for_all_sensors.png")
# attach(B2)
# ggplot(B2, aes(date)) +
#     geom_line(aes(y = sign(mean(ica2$A[1,])) * X1, colour = "1ere")) + 
#     geom_line(aes(y = sign(mean(ica2$A[2,])) * X2 - 6, colour = "2eme (centrée à -6)")) + 
#     geom_line(aes(y = sign(mean(ica2$A[3,])) * X3 - 12, colour = "3eme (centrée à -12)")) + 
#     labs(title = "ACI plot",
#         y = "Température de l'eau (en °C) normalisées", x = "Date", colour = "Composantes\nindépendantes") +
#     theme_plot
# detach(B2)
# dev.off()

# # pour la sonde 825
# B2$comp1_825 = ica2$A[1,1] * ica2$S[,1]
# B2$comp2_825 = ica2$A[2,1] * ica2$S[,2]
# B2$comp3_825 = ica2$A[3,1] * ica2$S[,3]
# png("graphics/ica_3components_for_sensor825.png")
# attach(B2)
# ggplot(B2, aes(date)) +
#     geom_line(aes(y = comp1_825, colour = "1ere")) + 
#     geom_line(aes(y = comp2_825 - 10, colour = "2eme (centrée à -10)")) + 
#     geom_line(aes(y = comp3_825 - 20, colour = "3eme (centrée à -20)")) + 
#     labs(title = "ACI plot (sonde 825)",
#         y = "Température de l'eau (en °C)", x = "Date", colour = "Composantes\nindépendantes") +
#     theme_plot
# detach(B2)
# dev.off()

# # pour la sonde 827
# B2$comp1_827 = ica2$A[1,2] * ica2$S[,1]
# B2$comp2_827 = ica2$A[2,3] * ica2$S[,2]
# B2$comp3_827 = ica2$A[3,2] * ica2$S[,3]
# png("graphics/ica_3components_for_sensor827.png")
# attach(B2)
# ggplot(B2, aes(date)) +
#     geom_line(aes(y = comp1_827, colour = "1ere")) + 
#     geom_line(aes(y = comp2_827 - 10, colour = "2eme (centrée à -10)")) + 
#     geom_line(aes(y = comp3_827 - 20, colour = "3eme (centrée à -20)")) + 
#     labs(title = "ACI plot (sonde 827)",
#         y = "Température de l'eau (en °C)", x = "Date", colour = "Composantes\nindépendantes") +
#     theme_plot
# detach(B2)
# dev.off()

# # pour la sonde 828
# B2$comp1_828 = ica2$A[1,3] * ica2$S[,1]
# B2$comp2_828 = ica2$A[2,3] * ica2$S[,2]
# B2$comp3_828 = ica2$A[3,3] * ica2$S[,3]
# png("graphics/ica_3components_for_sensor828.png")
# attach(B2)
# ggplot(B2, aes(date)) +
#     geom_line(aes(y = comp1_828, colour = "1ere")) + 
#     geom_line(aes(y = comp2_828 - 10, colour = "2eme (centrée à -10)")) + 
#     geom_line(aes(y = comp3_828 - 20, colour = "3eme (centrée à -20)")) + 
#     labs(title = "ACI plot (sonde 828)",
#         y = "Température de l'eau (en °C)", x = "Date", colour = "Composantes\nindépendantes") +
#     theme_plot
# detach(B2)
# dev.off()

# # pour la sonde 830
# B2$comp1_830 = ica2$A[1,4] * ica2$S[,1]
# B2$comp2_830 = ica2$A[2,4] * ica2$S[,2]
# B2$comp3_830 = ica2$A[3,4] * ica2$S[,3]
# png("graphics/ica_3components_for_sensor830.png")
# attach(B2)
# ggplot(B2, aes(date)) +
#     geom_line(aes(y = comp1_830, colour = "1ere")) + 
#     geom_line(aes(y = comp2_830 - 10, colour = "2eme (centrée à -10)")) + 
#     geom_line(aes(y = comp3_830 - 20, colour = "3eme (centrée à -20)")) + 
#     labs(title = "ACI plot (sonde 830)",
#         y = "Température de l'eau (en °C)", x = "Date", colour = "Composantes\nindépendantes") +
#     theme_plot
# detach(B2)
# dev.off()

# ACP
variance_to_save = 80
# pour sonde 825
df_base_pca = df %>% filter(df$id_sonde == 825)
base1 = aggregate(Teau~date, data = df_base_pca, FUN = mean, na.rm = TRUE)
base2 = aggregate(Tair.EOBS~date, data = df_base_pca, FUN = mean, na.rm = TRUE)
base3 = aggregate(Rainf.EOBS~date, data = df_base_pca, FUN = mean, na.rm = TRUE)
base4 = aggregate((Tair.EOBS - Teau)~date, data = df_base_pca, FUN = mean, na.rm = TRUE)
colnames(base4) = c("date", "diff825")
base = merge(base1, base2, by = "date")
base = merge(base, base3, by = "date")
base = merge(base, base4, by = "date")
base_pca = merge(base, B[,c("comp1_825", "comp2_825", "date")], by = "date")
base_pca = mutate(base_pca, C2 = comp2_825, Ta = Tair.EOBS, Tw = Teau, 
                  D = diff825, C1 = comp1_825, Rain = Rainf.EOBS)
base_pca$year = year(base_pca$date)
base_pca$month = month(base_pca$date)
base_pca$day = day(base_pca$date)
base_pca$hour = hour(base_pca$date)
base_pca = select(base_pca, -date)
base_pca = select(base_pca, -c(Teau, Tair.EOBS, Rainf.EOBS, diff825, comp1_825, comp2_825))

res.pca = PCA(base_pca, scale.unit = TRUE, ncp = 5, graph = FALSE, quanti.sup = 6:10)
res.pca$var
eig.val = get_eigenvalue(res.pca)
eig.val

# scree plot (explained variance)
df_eig = as.data.frame(eig.val)
attach(df_eig)
max_lim = 80
ratio = 100 / max_lim
var_to_save = variance_to_save / ratio

png("graphics/pca_explained_variance_sensor825.png")
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, max_lim), 
         barcolor = "#E7B800", barfill = "#E7B800", linecolor = "#00AFBB", choice = "variance") +
  geom_point(data = df_eig, aes(x = 1:nrow(df_eig), y = cumulative.variance.percent/ratio), size = 2, color = "#00AFBB") + 
  geom_line(data = df_eig, aes(x = 1:nrow(df_eig), y = cumulative.variance.percent/ratio), color = "#00AFBB") +
  scale_y_continuous(sec.axis = sec_axis(~ . * ratio, 
                                   name = "Cumulative proportion of explained variance")) +
  geom_hline(yintercept = var_to_save, col = 'red') +
  annotate("text", x = 4.8, y = var_to_save, label = paste(var_to_save * ratio, " %"), vjust = 1.2, col = "red") +
  theme_plot
dev.off()
detach(df_eig)

png("graphics/pca_kaiser_rule_sensor825.png")
res.pca$eig %>%
  as_tibble() %>%
  rownames_to_column("comp") %>%
  ggplot(aes(x = comp, y = eigenvalue, colour = eigenvalue > 1)) +
  geom_hline(yintercept = 1, col = "red") +
  geom_line(size = 1, col = "black") +
  geom_point(size = 3) +
  labs(x = "Composantes principales", y = "Valeurs propres", title = "Application règle de Kaiser") +
  theme_plot
dev.off()

# cercle des corrélations
png("graphics/pca_variables_sensor825.png")
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
             ) +
  theme_plot
dev.off()

png("graphics/pca_components_correlation_to_vars_sensor825.png")
corrplot(res.pca$var$cos2, is.corr = FALSE, title = "Corrélations entre composantes et variables",
         mar = c(0,0,1,0)) +
  theme_plot
dev.off()

# pour sonde 830
df_base_pca = df %>% filter(df$id_sonde == 830)
base1 = aggregate(Teau~date, data = df_base_pca, FUN = mean, na.rm = TRUE)
base2 = aggregate(Tair.EOBS~date, data = df_base_pca, FUN = mean, na.rm = TRUE)
base3 = aggregate(Rainf.EOBS~date, data = df_base_pca, FUN = mean, na.rm = TRUE)
base4 = aggregate((Tair.EOBS - Teau)~date, data = df_base_pca, FUN = mean, na.rm = TRUE)
colnames(base4) = c("date", "diff830")
base = merge(base1, base2, by = "date")
base = merge(base, base3, by = "date")
base = merge(base, base4, by = "date")
base_pca = merge(base, B[,c("comp1_830", "comp2_830", "date")], by = "date")
base_pca = mutate(base_pca, C2 = comp2_830, Ta = Tair.EOBS, Tw = Teau, 
                  D = diff830, C1 = comp1_830, Rain = Rainf.EOBS)
base_pca$year = year(base_pca$date)
base_pca$month = month(base_pca$date)
base_pca$day = day(base_pca$date)
base_pca$hour = hour(base_pca$date)
base_pca = select(base_pca, -date)
base_pca = select(base_pca, -c(Teau, Tair.EOBS, Rainf.EOBS, diff830, comp1_830, comp2_830))

res.pca = PCA(base_pca, scale.unit = TRUE, ncp = 5, graph = FALSE, quanti.sup = 6:10)
res.pca$var
eig.val = get_eigenvalue(res.pca)
eig.val

# scree plot (explained variance)
df_eig = as.data.frame(eig.val)
attach(df_eig)
max_lim = 70
ratio = 100 / max_lim
var_to_save = variance_to_save / ratio

png("graphics/pca_explained_variance_sensor830.png")
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, max_lim), 
         barcolor = "#E7B800", barfill = "#E7B800", linecolor = "#00AFBB", choice = "variance") +
  geom_point(data = df_eig, aes(x = 1:nrow(df_eig), y = cumulative.variance.percent/ratio), size = 2, color = "#00AFBB") + 
  geom_line(data = df_eig, aes(x = 1:nrow(df_eig), y = cumulative.variance.percent/ratio), color = "#00AFBB") +
  scale_y_continuous(sec.axis = sec_axis(~ . * ratio, 
                                   name = "Cumulative proportion of explained variance")) +
  geom_hline(yintercept = var_to_save, col = 'red') +
  annotate("text", x = 4.8, y = var_to_save, label = paste(var_to_save * ratio, " %"), vjust = 1.2, col = "red") +
  theme_plot
dev.off()
detach(df_eig)

png("graphics/pca_kaiser_rule_sensor830.png")
res.pca$eig %>%
  as_tibble() %>%
  rownames_to_column("comp") %>%
  ggplot(aes(x = comp, y = eigenvalue, colour = eigenvalue > 1)) +
  geom_hline(yintercept = 1, col = "red") +
  geom_line(size = 1, col = "black") +
  geom_point(size = 3) +
  labs(x = "Composantes principales", y = "Valeurs propres", title = "Application règle de Kaiser") +
  theme_plot
dev.off()

# cercle des corrélations
png("graphics/pca_variables_sensor830.png")
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
             ) +
  theme_plot
dev.off()

png("graphics/pca_components_correlation_to_vars_sensor830.png")
corrplot(res.pca$var$cos2, is.corr = FALSE, title = "Corrélations entre composantes et variables",
         mar = c(0,0,1,0)) +
  theme_plot
dev.off()

# ACP pour toutes les sondes
base1 = aggregate(Teau~date, data = df, FUN = mean, na.rm = TRUE)
base2 = aggregate(Tair.EOBS~date, data = df, FUN = mean, na.rm = TRUE)
base3 = aggregate(Rainf.EOBS~date, data = df, FUN = mean, na.rm = TRUE)
base4 = aggregate((Tair.EOBS - Teau)~date, data = df, FUN = mean, na.rm = TRUE)
colnames(base4) = c("date", "D")
base = merge(base1, base2, by = "date")
base = merge(base, base3, by = "date")
base = merge(base, base4, by = "date")
base_pca = merge(base, B[,c("X1", "X2", "date")], by = "date")
base_pca = mutate(base_pca, C2 = sign(mean(ica$A[2,])) * X2, Ta = Tair.EOBS, Tw = Teau, 
                  C1 = sign(mean(ica$A[1,])) * X1, Rain = Rainf.EOBS)
base_pca$year = year(base_pca$date)
base_pca$month = month(base_pca$date)
base_pca$day = day(base_pca$date)
base_pca$hour = hour(base_pca$date)
base_pca = select(base_pca, -date)
base_pca = select(base_pca, -c(Teau, Tair.EOBS, Rainf.EOBS, X1, X2))
res.pca = PCA(base_pca, scale.unit = TRUE, ncp = 5, graph = FALSE, quanti.sup = 6:10)
res.pca$var
eig.val = get_eigenvalue(res.pca)
eig.val

# scree plot (explained variance)
df_eig = as.data.frame(res.pca$eig)
attach(df_eig)
max_lim = 75
ratio = 100 / max_lim
var_to_save = variance_to_save / ratio
colnames(df_eig) = c("eigenvalue", "percentage of variance", "cumulative.variance.percent")

png("graphics/pca_explained_variance.png")
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, max_lim), 
         barcolor = "#E7B800", barfill = "#E7B800", linecolor = "#00AFBB", choice = "variance") + 
  geom_point(data = df_eig, aes(x = 1:nrow(df_eig), y = cumulative.variance.percent/ratio), size = 2, color = "#00AFBB") + 
  geom_line(data = df_eig, aes(x = 1:nrow(df_eig), y = cumulative.variance.percent/ratio), color = "#00AFBB") +
  scale_y_continuous(sec.axis = sec_axis(~ . * ratio, 
                                   name = "Cumulative proportion of explained variance")) +
  geom_hline(yintercept = var_to_save, col = 'red') +
  annotate("text", x = 4.8, y = var_to_save, label = paste(var_to_save * ratio, " %"), vjust = 1.2, col = "red") +
  theme_plot
dev.off()
detach(df_eig)

png("graphics/pca_kaiser_rule.png")
res.pca$eig %>%
  as_tibble() %>%
  rownames_to_column("comp") %>%
  ggplot(aes(x = comp, y = eigenvalue, colour = eigenvalue > 1)) +
  geom_hline(yintercept = 1, col = "red") +
  geom_line(size = 1, col = "black") +
  geom_point(size = 3) +
  labs(x = "Composantes principales", y = "Valeurs propres", title = "Application règle de Kaiser") +
  theme_plot
dev.off()

png("graphics/pca_variables.png")
# cercle des corrélations
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
             ) +
  theme_plot
dev.off()

png("graphics/pca_components_correlation_to_vars.png")
corrplot(res.pca$var$cos2, is.corr = FALSE, title = "Corrélations entre composantes et variables",
         mar = c(0,0,1,0)) +
  theme_plot
dev.off()


# ACP pour toutes les sondes (sans ACI et toutes les variables)
res.pca = PCA(sub_df[,-which(names(sub_df) == "id_sonde")], scale.unit = TRUE, ncp = 5, graph = FALSE)
res.pca$var
eig.val = get_eigenvalue(res.pca)
eig.val

# scree plot (explained variance)
df_eig = as.data.frame(eig.val)
attach(df_eig)
max_lim = 40
ratio = 100 / max_lim
var_to_save = variance_to_save / ratio

png("graphics/all_sensors_and_vars_pca_explained_variance.png")
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, max_lim), 
         barcolor = "#E7B800", barfill = "#E7B800", linecolor = "#00AFBB", choice = "variance") +
  geom_point(data = df_eig, aes(x = 1:nrow(df_eig), y = cumulative.variance.percent/ratio), size = 2, color = "#00AFBB") + 
  geom_line(data = df_eig, aes(x = 1:nrow(df_eig), y = cumulative.variance.percent/ratio), color = "#00AFBB") +
  scale_y_continuous(sec.axis = sec_axis(~ . * ratio, 
                                   name = "Cumulative proportion of explained variance")) +
  geom_hline(yintercept = var_to_save, col = 'red') +
  annotate("text", x = 7, y = var_to_save, label = paste(var_to_save * ratio, " %"), vjust = 1.2, col = "red") +
  theme_plot
dev.off()
detach(df_eig)

png("graphics/all_sensors_and_vars_pca_kaiser_rule.png")
res.pca$eig %>%
  as_tibble() %>%
  rownames_to_column("comp") %>%
  ggplot(aes(x = comp, y = eigenvalue, colour = eigenvalue > 1)) +
  geom_hline(yintercept = 1, col = "red") +
  geom_line(size = 1, col = "black") +
  geom_point(size = 3) +
  labs(x = "Composantes principales", y = "Valeurs propres", title = "Application règle de Kaiser") +
  theme_plot
dev.off()

png("graphics/all_sensors_and_vars_pca_variables.png")
# cercle des corrélations
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
             ) +
  theme_plot
dev.off()

png("graphics/all_sensors_and_vars_pca_individus.png")
fviz_pca_ind(res.pca,
             geom.ind = "point", # Montre les points seulement (mais pas le "text")
             col.ind = sub_df$id_sonde, # colorer by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "#00FCFC"),
             addEllipses = TRUE, # Ellipses de concentration
             legend.title = "Sondes"
             ) +
  theme_plot
dev.off()

png("graphics/all_sensors_and_vars_pca_variables_individus.png")
fviz_pca_biplot(res.pca,
                col.ind = sub_df$id_sonde, palette = "jco",
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Sondes") +
  theme_plot
dev.off()

png("graphics/all_sensors_and_vars_pca_biplot_contribs.png")
fviz_pca_biplot(res.pca, 
                # Individus
                geom.ind = "point",
                fill.ind = sub_df$id_sonde, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                legend.title = list(fill = "Sondes", color = "Contrib",
                                    alpha = "Contrib")
                ) +
  theme_plot
dev.off()

png("graphics/all_sensors_and_vars_pca_components_correlation_to_vars.png")
corrplot(res.pca$var$cos2, is.corr = FALSE, title = "Corrélations entre composantes et variables",
         mar = c(0,0,1,0)) +
  theme_plot
dev.off()
