### NADC Prioritizr Course Instructor code

# This is exactly the code in the instruction files, just all copied over to one file for easy demonstration.

# It includes exercise 1 and 2.



### Simple R demonstration

# assign the numbers 1 through 5 to an object called 'numbers1to5, and then view on screen
numbers1to5 <- 1:5
numbers1to5

#  add 3 to each element of numbers1to5, assign to 'newnumbers' 
newnumbers <- numbers1to5 + 3
newnumbers

# plot numbers1to5 agains newnumbers
# what format to i use? check the help file:
?plot
# plot(x, y, ...)
plot(numbers1to5, newnumbers)




### Loading packages

# specify what packages we want
packList <- c("raster","rgdal","sp","devtools","prioritizr")

# for each package, check if package is installed; if not, install them
for(i in 1:length(packList)){
  if(!is.element(noquote(packList[i]),installed.packages()[,1])){
    install.packages(packList[i],dependencies = T)
  }
}
# load packages
sapply(packList, require, character.only=T)

# install and load lpsymphony (this comes from a different source)
source("https://bioconductor.org/biocLite.R") 
biocLite("lpsymphony") # on one computer it asked for updating some other packages --> "n" is
#  a possible option
library(lpsymphony)




### Loading and looking at data

# specify your working directory - you will need to change this code to your folder path!!!
wdr <-'T:/el/NADC_prioritizr/inputdata/'

# load data
simple_cost <- raster(paste0(wdr,'simple_cost.tif'))
simple_feat <- stack(paste0(wdr,'simple_feat.tif'))

# check file we are trying to access:
paste0(wdr,'simple_cost.tif')

# is this where the data is and what it is called? If not, you might want to fix your wdr (or make sure folder is unzipped)

# explore cost data
print(simple_cost)
plot(simple_cost, main='Cost')

# explore feature data
print(simple_feat)
plot(simple_feat, main='Features')




### Prioritizing using prioritizr

library(prioritizr)

# specify the basic problem
p1 <- problem(simple_cost, simple_feat) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.17) %>%
  add_binary_decisions()  %>%
  add_lpsymphony_solver(gap=0.1) 
s <- solve(p1)

# explore solution
print(s)
plot(s, main='Solution p1')

# the objective value attribute
s@objective

# the status of the optimization (currently a bug here, sorry, it should read 'OPTIMAL', we can read this in the output that the progress log gives)
s@status

# number of cells selected (we sum all the cells in the solution raster)
cellStats(s, sum)

# cost of the solution (all the selected cells * the cost, summed over the raster)
cellStats(s*simple_cost, sum)

# amount of feature 1 selected (change the number in the square brackets to find the others)
cellStats(s*simple_feat[[1]], sum)

# generate a list of pu id to also plot
puid <- (1:ncell(simple_cost))[!is.na(values(simple_cost))]
xy <- xyFromCell(simple_cost, puid)

# then plot the solution and add the puid numbers on top
plot(s, main = "p1")
text(xy[,1], xy[,2], puid, cex=0.5)