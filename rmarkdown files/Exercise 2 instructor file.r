# "Systematic Conservation Prioritization: Exercise 2"
# A more realistic example

## Loading packages: only run if opening R anew. If still loaded from last time this is not required.

# specify packages
packList <- c("raster","rgdal","sp","devtools","prioritizr")

# check if package is installed; if not, install them
for(i in 1:length(packList)){
  if(!is.element(noquote(packList[i]),installed.packages()[,1])){
    install.packages(packList[i],dependencies = T)
  }
}
# load packages
sapply(packList, require, character.only=T)

# load lpsymphony (reinstall if you need to)
 source("https://bioconductor.org/biocLite.R") 
 biocLite("lpsymphony") # on one computer it asked for updating some other packages --> "n" is
                   #  a possible option
require(lpsymphony)

# specify working directory - change this to your own!
wdr <-'T:/el/NADC_prioritizr/inputdata/'

## Loading the data

# load the 10km raster data
forestbirds <- stack(paste0(wdr,'forestbirds10.tif'))
otherbirds <- stack(paste0(wdr,'otherbirds10.tif'))
mammals <- stack(paste0(wdr,'mammals10.tif'))
protectedareas <- raster(paste0(wdr,'protectedareas10.tif'))
areakm <- raster(paste0(wdr, 'areakm10.tif'))
intensiveareas <- raster(paste0(wdr,'intensiveareas10.tif'))
soyprofit <- raster(paste0(wdr,'soyprofit10.tif')) 
smallholderdensity <- raster(paste0(wdr,'smallholderdensity10.tif'))
carbon <- raster(paste0(wdr,'carbon10.tif'))

# check import
print(forestbirds); plot(forestbirds)
print(otherbirds); plot(otherbirds)
print(mammals); plot(mammals)
print(protectedareas); plot(protectedareas)
print(intensiveareas); plot(intensiveareas)
print(soyprofit); plot(soyprofit)
print(smallholderdensity); plot(smallholderdensity)

# load species names and examine data
speciesnames <- read.csv(paste0(wdr,'speciesnames.csv'))
print(speciesnames)

# allocate the names to the species rasters
names(forestbirds) <- speciesnames$SpeciesName[speciesnames$Type=='ForestBird']
names(otherbirds) <- speciesnames$SpeciesName[speciesnames$Type=='OtherBird']
names(mammals) <- speciesnames$SpeciesName[speciesnames$Type=='Mammal']

## Gap analysis

# how much area already in reserves?
# protectedareas>50 gives us a binary raster (true/false) that we need to times by the planning unit area
reserved <- protectedareas>=50
plot(reserved)
reservearea <- cellStats(areakm*reserved, sum)
totalarea <- cellStats(areakm, sum)
reservearea/totalarea*100

# species totals
speciestotals <- c( round(cellStats(forestbirds, sum)),
                    round(cellStats(otherbirds, sum)),
                    round(cellStats(mammals, sum)))
speciesprotected <- c( round(cellStats(forestbirds*reserved, sum)),
                       round(cellStats(otherbirds*reserved, sum)),
                       round(cellStats(mammals*reserved, sum)))
speciesnames <- c( names(forestbirds),
                   names(otherbirds),
                   names(mammals))
speciesprotected <- as.data.frame(cbind( 
                           speciesnames, 
                           speciestotals, 
                           speciesprotected, 
                           percentprotected = round(speciesprotected/speciestotals*100, 2)))
rownames(speciesprotected) <- NULL
print(speciesprotected)

## Prioritizr examples

### Lock in, lock out

# set up the problem
p0 <- problem(soyprofit, stack(forestbirds, otherbirds, mammals)) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.17) %>%
  add_locked_in_constraints(protectedareas>50) %>%
  add_locked_out_constraints(intensiveareas>50) %>%
  add_binary_decisions()  %>%
  add_lpsymphony_solver(gap=0.1) 

s0 <- solve(p0)

# some post-processing options:
plot(s0, main = paste("p0 cost =", s0@objective), 
     breaks = c(0, 0.5, 1),  col = c("grey70", "#0A5640"))

cellStats(s0*soyprofit, sum)    # cost in lost soy profit
cellStats(s0*areakm, sum)     # selected area 
cellStats(s0*forestbirds[[1]], sum) # amount of Cacicus chrysopterus selected (forest bird 1)


### Adding connectivity

# lets create a little test raster
test <- crop(soyprofit, extent(-284816.9, -254816.9, 1123636, 1153636))
test
# create labels to plot
puidtest <- c(1,2,'NA',3,4,5,6,7,8)
# identify their locations
xytest <- xyFromCell(test, 1:9)
# plot with labels
plot(test, main = "test, puid") 
text(xytest[,1], xytest[,2], puidtest, col='gray')

# then run the boundary matrix
blmtest <- boundary_matrix(test)
print(blmtest)

# what exactly does this mean? Let's look at the help file, under 'details'
?boundary_matrix

# create boundary matrix
blm <- boundary_matrix(soyprofit)

# add this into the problem from before
p1 <- p0 %>% 
      add_boundary_penalties(penalty = 0.05, edge_factor = 0.5, boundary_data = blm)

s1 <- solve(p1)


plot(s1, main = paste("p1 cost =", s1@objective), 
     breaks = c(0, 0.5, 1),  col = c("grey70", "#0A5640"))

cellStats(s1*soyprofit, sum)    # cost in lost soy profit
cellStats(s1*areakm, sum)     # area in km2
cellStats(s1*forestbirds[[1]], sum) # amount of Cacicus chrysopterus selected (forest bird 1)

# plot the selection frequency
plot(s1+s0)

# plot the selections, using the key below

# Key:
# 0 = not selected
# 1 = selected in s0 only
# 2 = selected in s1 only
# 3 = selected in both

sf <- s0
sf[s1==1] <- 2
sf[(s0+s1)==2] <- 3

plot(sf)
