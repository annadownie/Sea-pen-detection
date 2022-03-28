# Libraries and working directory  ----

require(sf)
require(data.table)
require(lubridate)
require(dplyr)
require(openxlsx)
require(nngeo)


setwd("C:/Users/ad06/OneDrive - CEFAS/Seapens/AutomaticDetection")

# Load the VIAME header ----

vtrhdr = as.data.table(read.csv('R Code/VIAME_header.csv',
                                header = FALSE))[1:2,]

# Load Data From BIIGLE Annotations and convert to spatial ----

load("R Code/BIIGLE_Annotations_16_20_SF.RData")

# Match points to tracks for each video ----

## File list from folder ----
# Track file list
filelist.t = list.files(path = 'Annotations/GenericTracker_CEND1216/GenericTrackerOutput/',pattern="*tracks.csv$")
# Detection file list
filelist.d = list.files(path = 'Annotations/GenericTracker_CEND1216/GenericTrackerOutput/',pattern="*detections.csv$")

# List of video names
vidlist <- gsub('_tracks.csv','',filelist.t)
# Check video list matches with BIIGLE video names - which don't match?
vidlist[!vidlist %in% bgl.pa$video_filename]
# Only keep videos with matching annotations
vidlist <- vidlist[vidlist %in% bgl.pa$video_filename]

pdf(file = 'Annotations/Run1_CEND1216/MatchupPlots.pdf',width = 10, height = 7)

for (vid in vidlist) {
  
  if (!vid %in% bgl.pa$video_filename) {
    
    next
    
  }
  

  ## Load Data From VIAME Annotations and convert to spatial ----
  
  ### Load data ----
  
  ## load tracks
  v = as.data.table(read.csv(paste0('Annotations/GenericTracker_CEND1216/GenericTrackerOutput/',vid,'_tracks.csv')))
  v = v[-1,]
  setnames(v, c('TrackID','Frame.ms','FrameID','TLx','TLy','BRx','BRy','DLC','TL','Label','CPA'))
  ## Add Unique ID column
  v[ ,TrackFrameID := do.call(paste, c(.SD, sep = "_")),.SDcols= c('TrackID','FrameID')]
  summary(v)
  
  ## subset the track data for analysis
  d = as.data.table(v)[,c(1:7,12)]
  
  ## Convert and add time and coordinate fields ----
  # There are ten frames per second, hence the time in seconds for matching with 
  # BIIGLE annotations can be calculated by dividing it by ten. The time in the Frame.ms
  # column from the project run pipelines is not correct for some reason. 
   d[, Frame.s := FrameID/10]
  ## Check the data frame
  summary(d)
  
  d <- d %>% mutate_at(c('TrackID','FrameID'),as.integer)
  
  
  d[,BLx := TLx]
  d[,BLy := BRy]
  d[,TRx := BRx]
  d[,TRy := TLy]
  
  d <- d[complete.cases(d),]
  
  
  d[,RID := rownames(d)]
  d
  
  names(d)
  
  dm <- melt(d,id.vars = c("TrackID","Frame.ms","FrameID","Frame.s","RID" ),
             measure.vars =  c("TLx","TLy","BRx","BRy","BLx","BLy","TRx","TRy"))
  dm[, Corner := substr(variable,1,2)]
  dm[, Coord := substr(variable,3,3)]
  
  dc <- dcast(dm, TrackID+Frame.ms+FrameID+Frame.s+RID+Corner ~Coord)
  dc
  
  ## Convert to sf ----
  # make the data frame spatial (as points)
  src <- st_as_sf(dc, coords = c('x', 'y'))
  
  # intermediary check
  #plot(src[1:4,1], pch = 4)
  
  # make into polygons
  res <- src %>%
    group_by(RID) %>%
    summarise() %>%
    st_cast("POLYGON")%>%
    st_convex_hull()
  
  # intermediary check
  #plot(res[1:4,1])
  
  # merge attributes by original row id
  res.dt <- merge(res,d[,c(1:3,8,9,14)])
  res.dt
  
  # Spatial Overlay Analysis for Annotations ----
  
  ## step through each frame with a BIIGLE annotation
  
  # BIIGLE annotations for the video
  bgl.vid <- bgl.pa[bgl.pa$video_filename==vid,]
  
  ann.stjoin <- NULL
  bgl.frames <- unique(bgl.vid$Frame.s)
  # frmpal <- RColorBrewer::brewer.pal(7,'PRGn')
  # frmpal[4] <- '#000000'
  
  for (i in bgl.frames) {
    
    frs <- c(i-0.1,i,i+0.1)
    bgl.sub <- bgl.vid[bgl.vid$Frame.s==i,]
    res.sub <- res.dt[res.dt$Frame.s %in% frs,]
    
    ttl <- paste('Video: ',vid,'\nBIIGLE Frame: ',i,'\nVIAME Frames: ',paste0(frs,collapse=','))
    #frmcols <-frmpal[as.factor(res.sub$Frame.s)]
    
    
    plot(st_geometry(bgl.sub), pch = 19, col = 'red',
         main=ttl,axes = TRUE,xlim=c(0,1250),ylim=c(0,700)) 
    plot(st_geometry(res.sub), border = 'black', 
          add = TRUE)
    
    if (nrow(res.sub)>0) {
    out <- st_join(res.sub,bgl.sub, join = st_nn, k = 1, maxdist = 20)
    out <- out[st_area(out)<7000 & !is.na(out$label_name),]
    
    plot(st_geometry(out),border='red', add = TRUE)
    
    if (nrow(out)>0) {
     ann.stjoin <- rbind(ann.stjoin,out) 
    }
    
    } else {
      out = NULL
    }
    
  }
  
  
  ## Extract those rectangles with a match
  
  ann.match <- ann.stjoin[!is.na(ann.stjoin$label_name),]
  
  ## Export matched output
  
  if (!is.null(ann.match)) {

    ann.match.df <- as.data.table(merge(st_drop_geometry(ann.match),d)[,c(1:5,7,15,9,11:14,16:19)])
    ann.match.df

    ## Convert matches back to VIAME format
    
    # Track IDs with matches with Pennatula
    pTr <- unique(ann.match$TrackID[ann.match$label_name == 'Pennatula phospohorea'])
    vTr <- unique(ann.match$TrackID[ann.match$label_name == 'Virgularia mirabilis'])
    
    
    # Match data with track ID's with one or more match with points and 
    # assign the label to all of them
    vtr <- v
    vtr[TrackID %in% pTr,Label := 'Pennatula']
    vtr[TrackID %in% vTr,Label := 'Virgularia']
    vtr[TrackID %in% ann.match$TrackID,DLC := 1]
    vtr[TrackID %in% ann.match$TrackID,CPA := 1]
    vtrhdr = as.data.table(read.csv(paste0('Annotations/GenericTracker_CEND1216/GenericTrackerOutput/',vid,'_tracks.csv'),
                                  header = FALSE))[1:2,]
    vtr.e <- rbind(vtrhdr,vtr[,-12],use.names=FALSE)
    
   
    
    # Edit into a detection format
    vdt <- vtr[Label %in% c('Pennatula','Virgularia'),-12]
    
    ## Add the Boxes around points
    # Create boxes for the annotations for the video
    buf <- st_buffer(bgl.pa[bgl.pa$video_filename==vid,], dist = 30,endCapStyle = 'SQUARE')
    # Extract the coordinates
    buf.coord <- as.data.table(st_coordinates(buf))
    # Get the top left and bottom right corners
    buf.coord2 <- buf.coord[,.(TLx=min(X),TLy=max(Y),BRx=max(X),BRy=min(Y)),by=L2]
    
    ## create the right format of table for VIAME
    bdf <- data.table(TrackID=seq(0,nrow(buf)-1,1),Frame.ms='01:00.0',FrameID=buf$Frame.s*10,buf.coord2[,2:5],DLC=1,TL=0,Label=buf$label_name,CPA=1)
    
    # join tables
    vdt <- rbind(vdt,bdf)
    
    # Order for output
    setorder(vdt,FrameID)
    setnames(vdt,'TrackID','DetectionID')
    vdt[,DetectionID := seq(0,nrow(vdt)-1,1)]
    vdt[,Frame.ms := '01:00.0']

    vdt.e <- rbind(vtrhdr,vdt,use.names=FALSE)
    
    vtr.e2 <- rbind(vtr.e,bdf,use.names=FALSE)
    names(vtr.e2) <- names(vtr.e)
    newr <- nrow(vtr.e2[V10 %in% c('Pennatula phospohorea','Virgularia mirabilis')])
    vtr.e2[V10 %in% c('Pennatula phospohorea','Virgularia mirabilis'),V1:=max(as.numeric(vtr$TrackID))+1:newr]
    
    ## Export the data
    write.table(vtr.e2,file = paste0('Annotations/Run1_CEND1216/',vid,'_Tracks.csv'),
                row.names = FALSE,col.names = FALSE, sep=",",quote = FALSE)
    write.table(vdt.e,file = paste0('Annotations/Run1_CEND1216/',vid,'_Detections.csv'),
                row.names = FALSE,col.names = FALSE, sep=",",quote = FALSE)
    
  } else {
    
    ### Export boxes only
    ## Add the Boxes around points
    # Create boxes for the annotations for the video
    buf <- st_buffer(bgl.pa[bgl.pa$video_filename==vid,], dist = 30,endCapStyle = 'SQUARE')
    # Extract the coordinates
    buf.coord <- as.data.table(st_coordinates(buf))
    # Get the top left and bottom right corners
    buf.coord2 <- buf.coord[,.(TLx=min(X),TLy=max(Y),BRx=max(X),BRy=min(Y)),by=L2]
    
    ## create the right format of table for VIAME
    bdf <- data.table(TrackID=seq(0,nrow(buf)-1,1),Frame.ms='01:00.0',FrameID=buf$Frame.s*10,buf.coord2[,2:5],DLC=1,TL=0,Label=buf$label_name,CPA=1)
    
    vdt <- bdf
    # Order for output
    setorder(vdt,FrameID)
    setnames(vdt,'TrackID','DetectionID')
    vdt[,DetectionID := seq(0,nrow(vdt)-1,1)]
    vdt[,Frame.ms := '01:00.0']
    
    vdt.e <- rbind(vtrhdr,vdt,use.names=FALSE)
    
    vtr.e2 <- bdf
    setnames(vtr.e2,'DetectionID','TrackID')
    vtr.e2[,TrackID:= TrackID+1]
    
    ## Export the data
    write.table(vtr.e2,file = paste0('Annotations/Run1_CEND1216/',vid,'_Tracks.csv'),
                row.names = FALSE,col.names = FALSE, sep=",",quote = FALSE)
    write.table(vdt.e,file = paste0('Annotations/Run1_CEND1216/',vid,'_Detections.csv'),
                row.names = FALSE,col.names = FALSE, sep=",",quote = FALSE)
  }

}

dev.off()

# Copy matched videos and tracks to a folder for manual editing in DIVE  ----

filelist.d <- list.files(path = 'Annotations/Run1_CEND1216',pattern="*Tracks.csv$")
# Detection file list
matchlist <- gsub('_Tracks.csv','',filelist.d)
new.folder <- 'AnnotationEdits/'

for (m in matchlist) {
  # find the files that you want
  detections <- list.files("Annotations/Run1_CEND1216", paste0(m,'_Tracks.csv'),full.names = TRUE)
  videos <- list.files("CEND1216Video10fps", m,full.names = TRUE)
  # copy the files to the new folder
  file.copy(videos, new.folder)
  file.copy(detections, new.folder)
}

# List of video names


