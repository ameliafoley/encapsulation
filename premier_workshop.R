library(here) #to set paths
#path to data
#data_location3 <- here::here("demo","genus_taxaAsColumns.txt", sep="\t", header=TRUE,row.names=1)

#load data. 
#myT <- read.table(data_location3)

rm(list=ls()) #empties R environment, start with blank slate

#we removed a line where the code set the working directory 

myT <- read.table("genus_taxaAsColumns.txt", sep="\t", header=TRUE,row.names=1) #input the file we wish to normalize
#this says 1st row and column are special
hist(rowSums(myT),breaks=20)
#row sums are number of sequences in sample = gives sequencing depth; breaks gives you more bins in the histogram
#could also do in log10 hist(log10(...)

myTNorm <- sweep(myT, 1, rowSums(myT), '/') #divide each cell in spreadsheet by row sums

myTNorm <- cbind(row.names(myT), myTNorm)
names(myTNorm)[[1]] = "SampleID" #give name to first column

write.table(myTNorm, file="taxaAsColumnsNorm.txt",sep="\t",row.names=FALSE) #write spreadsheet back out

#new tool

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ggtree")
BiocManager::install("rhdf5")


library(shiny)
library(shinyjs)
runApp('~/plotmicrobiome')
runApp('~/plotmicrobiome', display.mode = "showcase")

#PCO Plot

#install.packages("vegan")
library("vegan")

#rm(list=ls())

#setwd("C:\\Users\\afodor\\git\\afodor.github.io\\classes\\ChapelHill2018")

myT <- read.table("genusWithMeta.txt", sep="\t", header=TRUE,row.names=1)

myT <- myT[ myT$readNumber ==1, ]

myTData <- myT[,5:ncol(myT)]

myPCOA <- capscale(myTData~1,distance="bray")

plot(myPCOA$CA$u[,1], myPCOA$CA$u[,2],col = ifelse(myT$ruralUrban == "rural", "red", "blue"))
legend( "topleft", legend=c("urban", "rural"), col=c("red","blue"), pch=19,cex=1.3 )


#slide 63
rm(list=ls())

#setwd("C:\\Users\\afodor\\git\\afodor.github.io\\classes\\ChapelHill2018")

myT <- read.table("taxaAsColumnsLogNormPlusMDS.txt", sep="\t", header=TRUE,row.names=1)

mds1Col <- which(names(myT)=="MDS1") #hwat is the column number of this column
mds5Col <- which(names(myT)=="MDS5")
firstTaxaCol <- which(names(myT)=="timepoint") + 1

dataCols <- c( mds1Col:mds5Col , firstTaxaCol :(ncol(myT)) ) #graphs that you want to make by column number

pdf("pdfBall.pdf")
par(mfrow=c(2,2))

patientIDS <- myT$patientID 
timepoints <- myT$timepoint
ruralUrban <- myT$ruralUrban

for( i in dataCols ) 
{
  yData <- myT[,i]
  
  if( sum(yData > 0) >= nrow(myT)/4 ) 
  {
    aName <- names(myT)[i]
    myFrame <- data.frame( patientIDS ,timepoints, ruralUrban, yData )
    
    boxplot( yData ~ patientIDS , main = paste(aName,"by patient"), ylab = aName, las=2 ) 
    stripchart( yData ~ patientIDS , data = myFrame,vertical = TRUE, pch = 21, add=TRUE )		
    
    boxplot( yData ~ timepoints, main = paste(aName,"by timepoint"), ylab = aName, las=2 ) 
    stripchart( yData ~ timepoints, data = myFrame,vertical = TRUE, pch = 21, add=TRUE )		
    
    boxplot( yData ~ ruralUrban , main = paste(aName,"by rural/urban"), ylab = aName, las=2 ) 
    stripchart( yData ~ ruralUrban , data = myFrame,vertical = TRUE, pch = 21, add=TRUE )	
    
    plot(0,type='n',axes=FALSE,ann=FALSE)
  }
}

dev.off()

#can't get this to generate a PDF for some reason
#supposed to give a better view of the data, something we can interpret. basically graphed out data by metadata



