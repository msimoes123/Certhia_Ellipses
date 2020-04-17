library(ellipsenm)
setwd('C:\\Users\\mari1\\Desktop\\Certhia\\Certhia\\M_certhia\\Un_Dil')
d<-as.data.frame(fread("coordinatesCerthiaOnlyUniqueDiluted.tsv"))
head(d)
d <- cbind(d$species, d$decimalLongitude, d$decimalLatitude)
colnames(d) <- c('species', 'long', 'lat')
write.csv(d,"coordinatesCerthiaOnlyUniqueDiluted2.tsv", row.names = F) 
d <- read.csv("coordinatesCerthiaOnlyUniqueDiluted2.tsv")
dim(d)
u <- unique(d$species)
i=1
cols = 1:3 # columns extracted from dataset - species, long, lat
for (i in 1:length(u)) {
  sptable <- d[d$species == u[i], cols]
  write.csv(sptable, paste(paste("C:\\Users\\mari1\\Desktop\\Certhia\\Certhia\\M_certhia\\Un_Dil\\", u[i], collapse = '_'), ".csv", sep = ""), row.names = FALSE)
}
#Create M for each species------------
#Shapefiles for all except tia (buffer of 5km)
path <- "C:\\Users\\mari1\\Desktop\\Certhia\\Certhia\\M_certhia\\PCA\\used\\"
varaibles_list <-list.files(path = path, pattern = ".asc", full.names = TRUE) #variables 
variables <- stack(varaibles_list) #create a stack

shape <- readOGR(dsn = "C:\\Users\\mari1\\Desktop\\Certhia\\Certhia\\BirdLife ranges Certhiidae",
                 layer = "Certhia_familiaris_32439_BL") 
var_mask <- mask(crop(variables, shape), shape)
rnames <- paste0("C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/ellipsenm/familiaris/", names(variables), ".asc") # 
## saving layers in new folder
sav <- lapply(1:nlayers(var_mask), function(x) {
  writeRaster(var_mask[[x]], filename = rnames[x], format = "ascii", overwrite=T) # change format accordingly
})

#tia - the only species I uused the data from UniqueDiluted-----------

occ <- read.csv('C:\\Users\\mari1\\Desktop\\Certhia\\Certhia\\M_certhia\\Un_Dil\\Certhia_tianquanensis.csv')
M_tia <- buffer_area(occ, longitude = "long", latitude = "lat",
                        buffer_distance = 5)
var_mask <- mask(crop(variables, M_tia), M_tia)
rnames <- paste0("C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/ellipsenm/tianquanensis/", names(variables), ".asc") # 
## saving layers in new folder
sav <- lapply(1:nlayers(var_mask), function(x) {
  writeRaster(var_mask[[x]], filename = rnames[x], format = "ascii", overwrite=T) # change format accordingly
})

#Fixing occ of familiris bcs I changed the shapefile - excluding Nas
occurrences <- read.csv("C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/Un_Rng_Dil/Certhia_familiaris.csv") # occurrences
continents <- raster("C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/ellipsenm/familiaris/pc_1.asc") # representing the continents
occ_inout <- data.frame(occurrences, inside = extract(continents, occurrences[,2:3]))
fam <- na.omit(occ_inout)
write.csv(fam[,1:3], 'C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/Un_Rng_Dil/Certhia_familiaris_Correct.csv', row.names = F)

#Creating stack of variables for each species-----------
#Stack of 4 Pcs with the shape of their M, from where we will sample the env. space
#All occ come from Un_Rng_Dil, excepet tia which comes from Uni_Dil (9 occ)
occ1 <- read.csv('C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/Un_Rng_Dil/Certhia_brachydactyla.csv')
path <- "C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/ellipsenm/brachydactyla/"
varaibles_list <-list.files(path = path, pattern = ".asc", full.names = TRUE) #variables 
vars1 <- stack(varaibles_list) #create a stack
crs(vars1) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

occ2 <- read.csv('C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/Un_Rng_Dil/Certhia_americana.csv')
varaibles_list <- stack(list.files(path = "C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/ellipsenm/americana/", pattern = ".asc", full.names = TRUE)) #variables 
crs(vars2) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

occ3 <- read.csv('C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/Un_Rng_Dil/Certhia_discolor.csv')
vars3 <-stack(list.files(path = "C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/ellipsenm/discolor/", pattern = ".asc", full.names = TRUE)) #variables 
crs(vars3) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

occ4 <- read.csv('C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/Un_Rng_Dil/Certhia_familiaris_Correct.csv')
varaibles_list <- stack(list.files(path = "C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/ellipsenm/familiaris/", pattern = ".asc", full.names = TRUE)) #variables 
vars4 <- stack(varaibles_list) #create a stack
crs(vars4) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

occ5 <- read.csv('C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/Un_Rng_Dil/Certhia_himalayana.csv')
vars5<-stack(list.files(path = "C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/ellipsenm/himalayana/", pattern = ".asc", full.names = TRUE)) #variables 
crs(vars5) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

occ6 <- read.csv('C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/Un_Rng_Dil/Certhia_hodgsoni.csv')
vars6<-stack(list.files(path = "C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/ellipsenm/hodgsoni/", pattern = ".asc", full.names = TRUE)) #variables 
crs(vars6) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

occ7 <- read.csv('C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/Un_Rng_Dil/Certhia_manipurensis.csv')
vars7<-stack(list.files(path = "C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/ellipsenm/manipurensis/", pattern = ".asc", full.names = TRUE)) #variables 
crs(vars7) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

occ8 <- read.csv('C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/Un_Rng_Dil/Certhia_nipalensis.csv')
vars8<-stack(list.files(path = "C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/ellipsenm/nipalensis/", pattern = ".asc", full.names = TRUE)) #variables 
crs(vars7) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'

occ9 <- read.csv('C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/Un_Dil/Certhia_tianquanensis.csv')
vars9<-stack(list.files(path = "C:/Users/mari1/Desktop/Certhia/Certhia/M_certhia/ellipsenm/tianquanensis/", pattern = ".asc", full.names = TRUE)) #variables 
plot(vars9)
crs(vars9) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '

# preparing overlap objects to perform analyses----------
am <- overlap_object(occ2, species =  "species", longitude = "long", 
                     latitude = "lat", method = "covmat", level = 95, 
                     variables = vars2)

bra <- overlap_object(occ1, species =  "species", longitude = "long", 
                         latitude = "lat", method = "covmat", level = 95, 
                         variables = vars1)
dis <- overlap_object(occ3, species =  "species", longitude = "long", 
                     latitude = "lat", method = "covmat", level = 95, 
                     variables = vars3)
fam <- overlap_object(occ4, species =  "species", longitude = "long", 
                     latitude = "lat", method = "covmat", level = 95, 
                     variables = vars4)
him <- overlap_object(occ5, species =  "species", longitude = "long", 
                     latitude = "lat", method = "covmat", level = 95, 
                     variables = vars5)
hod <- overlap_object(occ6, species =  "species", longitude = "long", 
                     latitude = "lat", method = "covmat", level = 95, 
                     variables = vars6)
man <- overlap_object(occ7, species =  "species", longitude = "long", 
                     latitude = "lat", method = "covmat", level = 95, 
                     variables = vars7)
nip <- overlap_object(occ8, species =  "species", longitude = "long", 
                      latitude = "lat", method = "covmat", level = 95, 
                      variables = vars8)
tia <- overlap_object(occ9, species =  "species", longitude = "long", 
                      latitude = "lat", method = "covmat", level = 95, 
                      variables = vars9)

# niche overlap analysis
overlap <- ellipsoid_overlap(man, bra, n_points = 1000000, significance_test = TRUE, replicates = 1000,
                             confidence_limit = 0.05)

#overlap_Eu - Europe sp; overlap_M - Mountain sp; overlap_1 (man, dis); -------
#overlap_2 (tia,nip); overlap_3 (hod, fam); overlap_4 (am, bra)-------
overlap_Eu <- ellipsoid_overlap(fam, bra, n_points = 1000000,significance_test = TRUE, replicates = 1000, confidence_limit = 0.05)
overlap_1 <- ellipsoid_overlap(man,dis, n_points = 1000000, 
                               significance_test = TRUE, replicates = 1000, confidence_limit = 0.05)
overlap_2 <- ellipsoid_overlap(tia,nip, n_points = 1000000, 
                               significance_test = TRUE, replicates = 1000, confidence_limit = 0.05)
overlap_3 <- ellipsoid_overlap(hod,fam, n_points = 1000000,
                               significance_test = TRUE, replicates = 1000,confidence_limit = 0.05)
overlap_4 <- ellipsoid_overlap(am,bra, n_points = 1000000, 
                               significance_test = TRUE, replicates = 1000,confidence_limit = 0.05)
overlap_M <- ellipsoid_overlap(dis,man, hod, nip, n_points = 1000000,
                               significance_test = TRUE, replicates = 1000,confidence_limit = 0.05)



#Overap_M - Niche_1_vs_2------------
hist(overlap_M@significance_results$union_random$Niche_1_vs_2$overlap, 
     main = "Certhia: discolor & manupurensis", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 300))
abline(v = quantile(overlap_M@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "red", lwd = 2, lty = 2)
abline(v = overlap_M@union_overlap$overlap[1], col = "lightblue", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("lightblue", "red"), lty = c(1, 2), lwd = 2)
dev.off()

#Save images------------
# plotting ellispodis and background for full overlap
plot_overlap(overlap_3, background = TRUE, proportion = 0.6, background_type = "full")

# plotting ellispodis and background for overlap based on accessible environments
plot_overlap(overlap_Eu, background = TRUE,  proportion = 1, background_type = "back_union")

library(rgl)
rgl.snapshot( "Fam&Brady", fmt = "png", top = TRUE )
browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), width=500), sep=""))
rgl.viewpoint( theta = 0, phi = 15, fov = 60, zoom = 0, interactive = TRUE )

#Overap_M - Niche_1_vs_3---------

hist(overlap_M@significance_results$union_random$Niche_1_vs_3$overlap, 
     main = "Certhia: discolor & hodgsoni", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 300))
abline(v = quantile(overlap_M@significance_results$union_random$Niche_1_vs_3$overlap, 0.05),
       col = "red", lwd = 2, lty = 2)
abline(v = overlap_M@union_overlap$overlap[1], col = "lightblue", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("lightblue", "red"), lty = c(1, 2), lwd = 2)

#Overap_M - Niche_1_vs_4-----------

hist(overlap_M@significance_results$union_random$Niche_1_vs_4$overlap, 
     main = "Certhia: discolor & nipalensis", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 300))
abline(v = quantile(overlap_M@significance_results$union_random$Niche_1_vs_4$overlap, 0.05),
       col = "red", lwd = 2, lty = 2)
abline(v = overlap_M@union_overlap$overlap[1], col = "lightblue", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("lightblue", "red"), lty = c(1, 2), lwd = 2)

#Overap_M - Niche_2_vs_3---------
hist(overlap_M@significance_results$union_random$Niche_2_vs_3$overlap, 
     main = "Certhia: manupurensis & hodgsoni", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 400))
abline(v = quantile(overlap_M@significance_results$union_random$Niche_2_vs_3$overlap, 0.05),
       col = "red", lwd = 2, lty = 2)
abline(v = overlap_M@union_overlap$overlap[1], col = "lightblue", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("lightblue", "red"), lty = c(1, 2), lwd = 2)

#Overap_M - Niche_2_vs_4---------
hist(overlap_M@significance_results$union_random$Niche_2_vs_4$overlap, 
     main = "Certhia: manupurensis & nipalensis", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 300))
abline(v = quantile(overlap_M@significance_results$union_random$Niche_2_vs_4$overlap, 0.05),
       col = "red", lwd = 2, lty = 2)
abline(v = overlap_M@union_overlap$overlap[1], col = "lightblue", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("lightblue", "red"), lty = c(1, 2), lwd = 2)

#Overap_M - Niche_2_vs_4---------
hist(overlap_M@significance_results$union_random$Niche_3_vs_4$overlap, 
     main = "Certhia: hodgsoni & nipalensis", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 300))
abline(v = quantile(overlap_M@significance_results$union_random$Niche_3_vs_4$overlap, 0.05),
       col = "red", lwd = 2, lty = 2)
abline(v = overlap_M@union_overlap$overlap[1], col = "lightblue", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("lightblue", "red"), lty = c(1, 2), lwd = 2)

#Overap_1 - Man x Dis------------
hist(overlap_1@significance_results$union_random$Niche_1_vs_2$overlap, 
     main = "Certhia: manupurensis & discolor", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 300))
abline(v = quantile(overlap_1@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "red", lwd = 2, lty = 2)
abline(v = overlap_1@union_overlap$overlap, col = "lightblue", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("lightblue", "red"), lty = c(1, 2), lwd = 2)
dev.off()
overlap_1@full_overlap
#Overap_2 - Niche_1_vs_2------------
hist(overlap_2@significance_results$union_random$Niche_1_vs_2$overlap, 
     main = "Certhia: tianquanensis & nipalensis", xlab = "Overlap", xlim = c(0, 1), ylim = c(0, 300))
abline(v = quantile(overlap_2@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "red", lwd = 2, lty = 2)
abline(v = overlap_2@union_overlap$overlap, col = "lightblue", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("lightblue", "red"), lty = c(1, 2), lwd = 2)
dev.off()

#Overap_4 - Niche_1_vs_2------------
hist(overlap_4@significance_results$union_random$Niche_1_vs_2$overlap, 
     main = "Certhia: americana & brachydactyla", xlab = "Overlap", xlim = c(0.1, 0.3), ylim = c(0, 300))
abline(v = quantile(overlap_4@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "red", lwd = 2, lty = 2)
abline(v = overlap_4@union_overlap$overlap, col = "lightblue", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("lightblue", "red"), lty = c(1, 2), lwd = 2)
dev.off()

#Overap_Eu - Niche_1_vs_2------------
hist(overlap_Eu@significance_results$union_random$Niche_1_vs_2$overlap, 
     main = "Certhia: familiaris & brachydactyla", xlab = "Overlap", xlim = c(0.1, 0.3), ylim = c(0, 300))
abline(v = quantile(overlap_Eu@significance_results$union_random$Niche_1_vs_2$overlap, 0.05),
       col = "red", lwd = 2, lty = 2)
abline(v = overlap_4@union_overlap$overlap, col = "lightblue", lwd = 2)
legend("topright", bty = "n", legend = c("Observed", "5% CL"),
       col = c("lightblue", "red"), lty = c(1, 2), lwd = 2)
dev.off()

#Save images------------
# plotting ellispodis and background for full overlap
plot_overlap(overlap_Eu, background = TRUE, proportion = 0.6, background_type = "full")

# plotting ellispodis and background for overlap based on accessible environments
plot_overlap(overlap_Eu, background = TRUE,  proportion = 1, background_type = "back_union")

library(rgl)
rgl.snapshot( "Fam&Brady", fmt = "png", top = TRUE )
browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), width=500), sep=""))
rgl.viewpoint( theta = 0, phi = 15, fov = 60, zoom = 0, interactive = TRUE )


git

plot_overlap(overlap_1, niches = c(1, 2), niche_col = c("green", "red"),
                           data = TRUE, data_col = c("green", "red"),
                           background = FALSE, background_type = "back_union", proportion = .3,
                           background_col = viridis::viridis, change_labels = T,
                           xlab = "PC1", ylab = "PC2", zlab = "PC3", legend = TRUE)
str(overlap_1)

el3d <- ellipse3d(mean_pred@covariance_matrix[1:3, 1:3],
                  centre = mean_pred@centroid[1:3], 
                  level = mean_pred@level / 100)

vals <- cbind(r_values[, 1:3], suit@mahalanobis)
vals <- vals[order(vals[, 4], decreasing = F), 1:3]

plot3d(data[, 4:6], col = "blue3", size = 5)
plot3d(vals, col = col1, add = TRUE)
wire3d(el3d, col = "darkgreen", alpha = 0.5)
rglwidget()

#niche evolution 
if(!require(devtools)){
  install.packages("devtools")
}
if(!require(nichevol)){
  devtools::install_github("marlonecobos/nichevol")
}
library(nichevol)
library(raster) # for reading environmental layers
library(rgdal) # for reading shapefiles
install.packages('ape')
library(ape) # for plotting phylogenetic trees and node labels
install.packages('geiger')
library(geiger) #
directory <- "C:\\Users\\mari1\\Desktop\\Certhia\\Certhia\\M_certhia\\nichevol" # change the characters accordingly
setwd(directory) 
help(stats_evalues)
# variable at coarse resolution to be used as example only
temp <- getData("worldclim", var = "bio", res = 10)[[1]]

# examples of species accessible areas
d <- data("m_list", package = "nichevol")

# examples of species occurrences
data("occ_list", package = "nichevol")

# a simple phylogenetic tree for demonstrations
data("tree", package = "nichevol")

# a table of charaters representing species ecological niches derived from bin_table
data("character_table", package = "nichevol")

# a list that matches the tree with the character table 
data("tree_data", package = "nichevol")

stats <- stats_evalues(stats = c("mean", "sd", "median", "range", "quantile"), 
                       M_folder = "Folder_with_Ms", M_format = "shp", 
                       occ_folder = "Folder_with_occs", longitude = "lon_column", 
                       latitude = "lat_column", var_folder = "Folder_with_vars", 
                       var_format = "GTiff", percentage_out = 90)
stat <- stats_eval(stats = c("mean", "sd", "median", "range", "quantile"), 
                   Ms = m_list, occurrences = occ_list, species = "species",
                   longitude = "x", latitude = "y", variable = temp, 
                   percentage_out = 0)
