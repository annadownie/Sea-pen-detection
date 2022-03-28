# Libraries and working directory  ----

require(sf)
require(data.table)
require(lubridate)
require(dplyr)
require(openxlsx)
require(nngeo)


setwd("C:/Users/ad06/OneDrive - CEFAS/Seapens/AutomaticDetection")


# Load Data From BIIGLE Annotations  ----

# Load Data From BIIGLE Annotations and convert to spatial ----

## Load data ----
bgl <- as.data.table(read.xlsx("Annotations/BIIGLE Annotations/Annotations_2016_2020.xlsx",sheet=1))
bgl
names(bgl)
sum(duplicated(bgl[,video_annotation_label_id]))

### This bit needs to be changed when multiple videos ----

bgl.json <- bgl[,c('video_filename','video_annotation_label_id','label_name','points','frames')]
bgl.json$video_filename <- gsub('-R.mp4','',bgl.json$video_filename)
bgl.json

## Split JSON arrays into x,y,frame
ldat <- NULL

for (i in 1:nrow(bgl.json)) {
  
  tw <- cbind(bgl.json$video_filename[[i]],
              bgl.json$video_annotation_label_id[[i]],
              bgl.json$label_name[[i]],
              jsonlite::fromJSON(bgl.json$points[[i]]),
              jsonlite::fromJSON(bgl.json$frames[[i]]))
  ldat <- rbind(ldat,tw)
  
}

## Convert into data table
ldat <- as.data.table(ldat)
names(ldat) <- c('video_filename','video_annotation_label_id','label_name','x','y','Frame.s')

# change coordinate and frame fields to numeric
ldat <- ldat %>% mutate_at(c('x','y','Frame.s'),as.numeric)

# round the frame.s field to match the 1 decimal place of VIAME annotations
ldat[,Frame.s := round(Frame.s,1)]

summary(ldat)
sum(duplicated(ldat[,video_annotation_label_id]))
# remove duplicates
ldat <- ldat[!duplicated(ldat$video_annotation_label_id),]


## Convert to sf ----
# make the data frame spatial (as points)
bgl.pa <- st_as_sf(ldat, coords = c('x', 'y'))
plot(bgl.pa[1:4,1])

save(bgl.pa,file = "R Code/BIIGLE_Annotations_16_20_SF.RData")

# Load the VIAME header ----

vtrhdr = as.data.table(read.csv('R Code/VIAME_header.csv',
                                header = FALSE))[1:2,]

# Export square annotations by video  ----

# Video file list from folder
vidlist = list.files(path = 'CEND1216Video10fps/')

# List of video names
vidlist <- gsub('.mp4','',filelist.vid)
# Check video list matches with BIIGLE video names - which don't match?
vidlist[!vidlist %in% bgl.pa$video_filename]
# Only keep videos with matching annotations
vidlist <- vidlist[vidlist %in% bgl.pa$video_filename]

# Loop through video list
# 
pdf(file = 'Annotations/BIIGLE Annotations/Boxes2016/AnnotationPoints.pdf',width = 10, height = 7)

for (vid in vidlist) {
  
  if (!vid %in% bgl.pa$video_filename) {
    
    next
    
  }
  
  # Create boxes for the annotations for the video
  buf <- st_buffer(bgl.pa[bgl.pa$video_filename==vid,], dist = 30,endCapStyle = 'SQUARE')
  # Extract the coordinates
  buf.coord <- as.data.table(st_coordinates(buf))
  # Get the top left and bottom right corners
  buf.coord2 <- buf.coord[,.(TLx=min(X),TLy=max(Y),BRx=max(X),BRy=min(Y)),by=L2]
  
  ttl <- paste('Video: ',vid)
  
  plot(st_geometry(bgl.pa[bgl.pa$video_filename==vid,]), pch = 19, col = 'red',
       axes = TRUE,xlim=c(0,1280),ylim=c(0,736),main=ttl)
  plot(st_geometry(buf), border = 'black',
       add = TRUE)

  ## create the right format of table for VIAME
  d <- data.table(TrackID=seq(0,nrow(buf)-1,1),Frame.ms='01:00.0',FrameID=buf$Frame.s*10,buf.coord2[,2:5],DLC=1,TL=0,Label=buf$label_name,CPA=1)
  d.e <- rbind(vtrhdr,d,use.names=FALSE)
  
  
 
    
  ## Export the data
    write.table(d.e,file = paste0('Annotations/BIIGLE Annotations/Boxes2016/',vid,'_Boxes.csv'),
                row.names = FALSE,col.names = FALSE, sep=",",quote = FALSE)
    
  }

dev.off()
