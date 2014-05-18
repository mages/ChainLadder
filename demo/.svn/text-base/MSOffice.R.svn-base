## Demo which shows how to use R and ChainLadder for auto reporting with MS Office
## Markus Gesmann, March 2010
## See also the R2PPT package
## Of course you need MS Office, and also the 'rcom' package
R <- MackChainLadder(RAA)
myfile=tempfile()
win.metafile(file=myfile)
plot(R)
dev.off()
#
require(rcom)
ppt<-comCreateObject("Powerpoint.Application")
comSetProperty(ppt,"Visible",TRUE)
myPresColl<-comGetProperty(ppt,"Presentations")
myPres<-comInvoke(myPresColl,"Add")
mySlides<-comGetProperty(myPres,"Slides")
mySlide<-comInvoke(mySlides,"Add",1,12)
myShapes<-comGetProperty(mySlide,"Shapes")
myPicture<-comInvoke(myShapes,"AddPicture",myfile,0,1,100,10)

## See also the packages R2PPT and R2wd which provide wrapper functions dealing 
## with PowerPoint and Word

## Also see the SWord example file:
# system(paste("open ", system.file("SWord", package="ChainLadder"),"/ChainLadder_SWord_Example.doc", sep=""))

## RExcel example file:
# system(paste("open ", system.file("Excel", package="ChainLadder"),"/ChainLadder_in_Excel.xls", sep=""))