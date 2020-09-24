##2019.9.2

##https://github.com/chrisamiller/fishplot

library(devtools)
install_github("chrisamiller/fishplot")

# If you prefer to build the package by hand, follow these steps:
#   Make sure that you have the dependencies from CRAN ("Hmisc","plotrix","png")
# Download and build from source:
# git clone git@github.com:chrisamiller/fishplot.git
# R CMD build fishplot
# R CMD INSTALL fishplot_0.2.tar.gz

library(fishplot)

#provide a list of timepoints to plot
#You may need to add interpolated points to end up with the desired
#visualization. Example here was actually sampled at days 0 and 150
timepoints=c(0,30,75,150)      

#provide a matrix with the fraction of each population
#present at each timepoint
frac.table = matrix(
  c(100, 45, 00, 00,
    02, 00, 00, 00,
    02, 00, 02, 01,
    98, 00, 95, 40),
  ncol=length(timepoints));frac.table

#provide a vector listing each clone's parent
#(0 indicates no parent)
parents = c(0,1,1,3)

#create a fish object
fish = createFishObject(frac.table,parents,timepoints=timepoints)

#calculate the layout of the drawing
fish = layoutClones(fish)

#draw the plot, using the splining method (recommended)
#and providing both timepoints to label and a plot title
fishPlot(fish,shape="spline",title.btm="Sample1",
         cex.title=0.5, vlines=c(0,150), 
         vlab=c("day 0","day 150"))
