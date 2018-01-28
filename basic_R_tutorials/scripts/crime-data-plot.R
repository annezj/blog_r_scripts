# Animate pre-prepared crime data
# over a Googlemaps background
rm(list=ls())

library(animation)
library(ggplot2) 
library(ggmap)

# Read the data: Crimes in the Liverpool area during 2016 
# (Contains public sector information licensed under the Open Government Licence v3.0)
# Data for Merseyside Police, January to December 2016 obtained from https://data.police.uk/
# Grab my edit for Liverpool area and with extraneous columns removed:
df=read.csv("https://github.com/annezj/basic_R_tutorials/raw/master/data/Liverpool-01-2016-12-2016.csv")

# Get googlemaps background for the dataset location
latmin=min(df$Latitude)
latmax=max(df$Latitude)
lonmin=min(df$Longitude)
lonmax=max(df$Longitude)
mymap<-get_map(location=c(lonmin,latmin,lonmax,latmax)) 

# Process the date string
df$monthnum=as.integer(substr(df$Month, 6, 7))
monthstrings=c("January", "February", "March", "April", "May", "June", "July", "August", "September",
               "October", "November", "December")
df$monthname=factor(df$monthnum, levels=1:12,labels=monthstrings)

# Quick plot to check
ggplot(df)+
  geom_bar(aes(x=monthname, fill=Crime.type), position="stack")+
  theme_bw()+ylab("Number of crimes")+
  xlab("")
ggsave("/Users/annejones/Documents/blog/blog_r_scripts/basic_R_tutorials/output/crime_animation/bar.png",
       height=4, width=10, units="in")

# Create an animation with one frame per month
nframes=12

# Define plotting function which must must create one plot per frame
create.plots <-function()
{
  # Set an frame rate of 1 per second
  ani.options(interval=1, verbose=FALSE)
  # animvec is used to loop over frames
  animvec=1:nframes

  # loop over months/frames
  for(iframe in animvec)
  {
    # Pick up crimes occurring this month
    dfi=df[which(df$monthnum==iframe),]
    
    # Print the base map and title, implement various appearance tweaks
    titlestr=paste(monthstrings[iframe],'\n',2016,sep='')
    p=ggmap(mymap)+ggtitle(titlestr)+theme_bw()+
      theme(plot.title = element_text(hjust = 0.5),text = element_text(size=16))+
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
      theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
    
    # Plot all crimes for this frame
    p=p+geom_point(data=dfi,aes(x=Longitude,y=Latitude,color=Crime.type), alpha=0.4)
    
    # Fix the color scale across all frames so no categories are dropped
    p=p+scale_color_discrete("", drop=F)
    
    # Fix transparency, size of points and spacing on the legend
    p=p+guides(colour = guide_legend(override.aes = list(alpha = 1, size=4), keyheight = 1.5))
    print(p)  
  }
}

# Animate by calling the function to create an html animation from the plotting function
# The html file will appear in the current working directory
# and open automatically in the default web brower
testdir="/Users/annejones/Documents/blog/blog_r_scripts/basic_R_tutorials/output/crime_animation/"
setwd(testdir)
saveHTML(create.plots(),
         img.name="crime_animation",
         autoplay=T,
         outdir=getwd(),
         htmlfile=paste("crime_animation.html", sep=""),
         ani.height=800,ani.width=800,
         title="Crime Animation",
         description=c("none")
)


# generate code for embedding animmations
library(Rmdanimation)
cat(animatedHTML(create.plots(), "anim-embed",ani.height=600,ani.width=600))

