rm(list=ls())
library(data.table)
library(ggplot2)
datadir="/Users/annejones/Documents/blog/datasets/crime/"
# load data for 2016 and merseyside
df=data.frame()
for(year in c(2016))
{
  for(month in 1:12)
  {
    if(month==12)
    {
      df1<-fread(paste("/Users/annejones/Documents/blog/datasets/crime/2016-12-merseyside-street.csv",sep=''), data.table=F)
    }else
    {
      monthstr=paste(year,sprintf("%02i",month),sep='-')
      print(monthstr)
      location="merseyside"
      df1<-fread(paste("/Users/annejones/Documents/blog/datasets/crime/crimedata/",monthstr,"/",
                         monthstr,"-",location,"-street.csv",sep=''), data.table=F)
    }
    df=rbind(df,df1)
  }
}
rm(df1)

# clean up data and save
df.save=df[,c(2,5,6,10)]
write.csv(df.save, file="/Users/annejones/Documents/blog/datasets/crime/crimedata/Merseyside-01-2016-12-2016.csv", row.names = F)
df.crime=df.save

# zoom and trim, then save again
xlim=c(-3.05, -2.84)
ylim=c(53.32,53.50)

df.trim=df.crime[which((df.crime$Latitude>=ylim[1])&(df.crime$Latitude<=ylim[2])&
                         (df.crime$Longitude>=xlim[1])&(df.crime$Longitude<=xlim[2])),]
write.csv(df.trim, file="/Users/annejones/Documents/blog/blog_r_scripts/basic_R_tutorials/data/Liverpool-01-2016-12-2016.csv", row.names = F)

df=df.trim
rm(df.trim)
rm(df.save)
colnames(df)=c("date","x","y", "type")
df$month=as.integer(substr(df$date, 6, 7))
ggplot(df)+geom_bar(aes(x=month, fill=type), position="stack")+theme_bw()+scale_x_continuous(breaks=1:12)
