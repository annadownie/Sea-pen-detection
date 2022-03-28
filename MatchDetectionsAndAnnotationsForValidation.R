##### Libraries and working directory  --------------------------------------------------------------------

require(sf)
require(data.table)
require(lubridate)
require(dplyr)
require(openxlsx)
require(nngeo)
require(ggplot2)
require(stringr)
require(wesanderson)

# set working directory
setwd("C:/Users/ad06/OneDrive - CEFAS/Seapens/AutomaticDetection/Validation")

# Shortcut for project cdp folder
cdp <- '//corp.cefas.co.uk/LOW/cdp/C8480_Sea_pens_IEG/Working_Area'

##### Detection matching ----------------------------------------------------------------------------------

# Load Data From DIVE Annotations ----

## File list from folder ----
# all csv files in the cdp folder
# Find the station folders containing annotations
dirs <- grep("Station",list.dirs(cdp,
                                  recursive=FALSE),value=TRUE)
# list csv files in those directories
filelist.dive <- list.files(dirs, pattern=".csv$", recursive = TRUE,full.names = TRUE)

# List files in model prediction folder
# list csv files in those directories
filelist.cnn <- list.files('DetectionsRCNN3', pattern=".csv$", recursive = TRUE,full.names = TRUE)
# List of video names
vidlist.a <- str_split(filelist.dive,"_[pP]en|_[vV]i|_none|/",simplify = TRUE)[,10]
vidlist.m <- unique(str_split(filelist.cnn,"_tra|_det|/",simplify = TRUE)[,2])

vidlist <- vidlist.a[vidlist.a %in% vidlist.m]

# List the models to test
modlist <- 'RCNN3'

# Data table for collecting number of frames in the track, confidence and match stats
sumv <- NULL
overlap <- NULL
vmatches <- NULL
n.annot <- NULL

# Match single frame annotations to tracks for each video ----
for (vid in vidlist) {
  
  # Loop through models
  for (mod in modlist) {
    
    # Capture matches in pdf
    pdf(file = paste0('MatchupPlots_',vid,'_',mod,'.pdf'),width = 10, height = 7)
    
    ## Load data from model track predictions and convert to spatial ----
    
    ### Load data ----
    
    ## load tracks
    v = as.data.table(
      read.table(paste0('Detections',mod,'/',vid,'_tracks.csv'),
                                 header = FALSE, sep = ",", 
                                 col.names = paste0("V",seq_len(13)), fill = TRUE)[,1:11])
    setnames(v, c('TrackID','Frame.ms','FrameID','TLx','TLy','BRx','BRy','DLC','TL','Label','CPA'))
    ## Add Unique ID column
    v[ ,TrackFrameID := do.call(paste, c(.SD, sep = "_")),.SDcols= c('TrackID','FrameID')]
    v <- v %>%
      na.omit()
    summary(v)
    
    ## subset the track data for analysis removing unnecessary fields
    ## The fields that are needed are:
    ## TrackID - identifies the collection of detections that form a track
    ## FrameID - identifies the frame and will be used to match annotations for a video
    ## Top right and bottom left coordinate fields - TLx,TLy,BRx,BRy
    ## Label - predicted label
    ## CPA - prediction confidence
    ## TrackFrameID - combined track and frame ids
    d = copy(v[,c(1,3:8,10,12)])
    
    ## Check the data frame
    summary(d)
    
    # Make both IDs integers
    d <- d %>% mutate_at(c("TrackID","FrameID","TLx","TLy","BRx","BRy"),as.integer)
    
    # Add missing corner coordinates
    d[,BLx := TLx]
    d[,BLy := BRy]
    d[,TRx := BRx]
    d[,TRy := TLy]
    
    # Make sure there are no NAs
    d <- d[complete.cases(d),]
    
    # Add rowID for converting to spatial
    d[,RID := rownames(d)]
    d
    
    names(d)
    
    # Make into long form table for creating polygons
    dm <- melt(d,id.vars = c("TrackFrameID","TrackID","FrameID","RID" ),
               measure.vars =  c("TLx","TLy","BRx","BRy","BLx","BLy","TRx","TRy"))
    # Create variables for which corner and x or y
    dm[, Corner := substr(variable,1,2)]
    dm[, Coord := substr(variable,3,3)]
    
    # Format into corner coordinate table for converting to polygons
    dc <- dcast(dm, TrackFrameID+TrackID+FrameID+RID+Corner ~Coord)
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
    mres.dt <- merge(res,d[,c(1:2,7,8,9,14)],by="RID")
    mres.dt
    # Calculate area of detections
    mres.dt <- mres.dt  %>% 
      mutate(area = st_area(.))
  
    ## Load data from analyst annotations and convert to spatial ----
    
    ### Load data ----
    
    ## load tracks
        v = as.data.table(
      read.table(grep(vid,filelist.dive,value = TRUE),
                 header = FALSE, sep = ",", 
                 col.names = paste0("V",seq_len(13)), fill = TRUE)[,1:11])
    setnames(v, c('TrackID','Frame.ms','FrameID','TLx','TLy','BRx','BRy','DLC','TL','Label','CPA'))
    ## Add Unique ID column
    v[ ,TrackFrameID := do.call(paste, c(.SD, sep = "_")),.SDcols= c('TrackID','FrameID')]
    v <- v %>%
      na.omit()
    summary(v)
    
    if (nrow(v)>0) {
    
      ## Change labelling to match models
      
      v[grep('[vV]i',Label),Label:='Virgularia']
      v[grep('[pP]e',Label),Label:='Pennatula']
      
      ## subset the track data for analysis removing unnecessary fields
      ## The fields that are needed are:
      ## TrackID - identifies the collection of detections that form a track
      ## FrameID - identifies the frame and will be used to match annotations for a video
      ## Top right and bottom left coordinate fields - TLx,TLy,BRx,BRy
      ## Label - predicted label
      ## CPA - prediction confidence
      ## TrackFrameID - combined track and frame ids
      d = copy(v[,c(1,3:8,10,12)])
      
      ## Check the data frame
      summary(d)
      
      # Make both IDs integers
      d <- d %>% mutate_at(c("TrackID","FrameID","TLx","TLy","BRx","BRy"),as.integer)
      
      # Add missing corner coordinates
      d[,BLx := TLx]
      d[,BLy := BRy]
      d[,TRx := BRx]
      d[,TRy := TLy]
      
      # Make sure there are no NAs
      d <- d[complete.cases(d),]
      
      # Add rowID for converting to spatial
      d[,RID := rownames(d)]
      d
      
      names(d)
      
      # Tabulate Number of sea pens annotated in the video
      
      
      # Make into long form table for creating polygons
      dm <- melt(d,id.vars = c("TrackFrameID","TrackID","FrameID","RID" ),
                 measure.vars =  c("TLx","TLy","BRx","BRy","BLx","BLy","TRx","TRy"))
      # Create variables for which corner and x or y
      dm[, Corner := substr(variable,1,2)]
      dm[, Coord := substr(variable,3,3)]
      
      # Format into corner coordinate table for converting to polygons
      dc <- dcast(dm, TrackFrameID+TrackID+FrameID+RID+Corner ~Coord)
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
      ares.dt <- merge(res,d[,c(1:2,7,8,9,14)],by="RID")
      ares.dt
      
      
      # Spatial Overlay Analysis for Annotations ----
      
      ## step through each frame with a analyst annotation
  
      # list frames with annotations
      anno.frames <- unique(ares.dt$FrameID)
      # frmpal <- RColorBrewer::brewer.pal(7,'PRGn')
      # frmpal[4] <- '#000000'
      
      for (i in anno.frames) {
        
        # Allow for previous of next frame
        frs <- c(i-1,i,i+1)
        anno.sub <- ares.dt[ares.dt$FrameID==i,]
        
        # Plot overlaps for checking
        res.sub <- mres.dt[mres.dt$FrameID %in% frs,]
        # Title
        ttl <- paste('Video: ',vid,'\nAnnotated Frame: ',i,'\nModel Frames: ',paste0(frs,collapse=','))
        #frmcols <-frmpal[as.factor(res.sub$Frame.s)]
        # Plot
        plot(st_geometry(anno.sub), pch = 19, col = 'red',
             main=ttl,axes = TRUE,xlim=c(0,1250),ylim=c(0,700)) 
        plot(st_geometry(res.sub), border = 'black', 
             add = TRUE)
        
        # Calculate area overlap with each true annotation for all predicted
        is_pct <- mres.dt %>% 
          filter(FrameID %in% frs) %>%  # select frames
          st_intersection(anno.sub) %>% # Intersect with known annotation
          mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
          dplyr::select(TrackFrameID,TrackID.1,intersect_area,Label.1) %>%   # only select columns needed to merge
          st_drop_geometry() %>% # drop geometry as we don't need it
          rename(LabelTrue=Label.1, # rename the label column from analyst annotations
                 AnnoID=TrackID.1) # rename the TrackID (i.e. individual annotation ID)
        
        # Add rows to a table that collects the intersections
        overlap <- rbind(overlap, is_pct)
      
      }
      
      annt <- as.data.table(ares.dt %>%
        st_drop_geometry() %>% 
        mutate(Label = factor(Label, levels = c('Pennatula','Virgularia'))) %>%
        group_by(Label) %>%
        count(Label, .drop = FALSE) %>%
        tidyr::spread(Label, n, fill = 0))
      
    } else {
      
      overlap <- data.table(TrackFrameID=character(),
                            AnnoID= character(),
                            intersect_area=numeric(),
                            LabelTrue=character())
      
      annt <- data.table(Pennatula=0, Virgularia=0)
      
    }
    

  dev.off()
  
  # Make a data table with all model predicted track and matches  
  vmchs <- mres.dt %>%
    st_drop_geometry() %>% 
    left_join(overlap, by = 'TrackFrameID') %>% 
    tidyr::replace_na(list(intersect_area=0, LabelTrue='None'))
  
  vmatches <- rbind(vmatches,data.table(video=vid,model=mod,vmchs))
  
  # Tabulate Number of sea pens annotated by the analyst in the video

  n.annot <- rbind(n.annot,data.table(video=vid,annt))
   
  }
}

# Find matched frames - label must be the same for match
vmatches[,Match:= if_else(Label==LabelTrue,1,0)]
# Calculate the percentage overlap between detection and analyst annotation
vmatches[,OverlapPct:= (intersect_area/area)*100]
# Summarise by track
sumv <-as.data.table(vmatches %>%
                        group_by(video,model,Label,TrackID) %>%
                        summarise(maxDLC = max(DLC), 
                                  n = n(),
                                  Matched=max(Match),
                                  MaxOverlapPct=max(OverlapPct),
                                  NumberMatched=n_distinct(AnnoID, na.rm = TRUE)) %>%
                        mutate(Station = str_extract(video, "[^6-]+$"),
                               Year=as.numeric(paste0(20,str_sub(video,7,8)))))
# Some DL station videos have an R at the end which gives wrong station code, replacing with DL
sumv[Station=='R',Station:= 'DL']

# Add Station and year to Analyst annotation counts
n.annot <- n.annot %>%
            mutate(Station = str_extract(video, "[^6-]+$"),
            Year=as.numeric(paste0(20,str_sub(video,7,8))))
n.annot

# Replace R with DL
n.annot[Station=='R',Station:= 'DL']

# check ovelap
vids.sumv <- unique(sumv$video)
vids.n.annot <- unique(n.annot$video)

sum(vids.sumv %in% vids.n.annot)



##### Other data ----

### Read in station information and pivot visibility variables into one factor variable

# Read excel sheet
tab <- as.data.table(openxlsx::read.xlsx('StationInfo.xlsx'))
tab <- tab[!is.na(Cruise)]

# Only keep distinct rows and necessary columns
tab <- tab[,c(1,3:5,10:12)] %>%
            distinct() %>%
            mutate(Station = str_replace(TVID, "6-",""),
            Year=as.numeric(paste0(20,str_sub(Cruise,4,5))))

# convert to tibble for printing
tab <- tibble::as_tibble(tab) %>%
  mutate(VisGood = replace(VisGood, VisGood ==TRUE & (VisMod==TRUE|VisPoor==TRUE), FALSE)) %>%
  mutate(VisMod = replace(VisMod, VisMod==TRUE&VisPoor==TRUE, FALSE))

# transform to long format the dummy columns
tab_long <- tidyr::pivot_longer(tab, 
                                cols = 5:7,
                                names_to = c("groups", "levels"),
                                names_sep=3)
# get the groups name for column selection after
groups <- unique(tab_long$groups)
# keep only non dummy value and do not keep temp value col
tab_filter <- dplyr::select(
  dplyr::filter(tab_long, value == TRUE),
  -value)

# tranform to wide format for final table
StInfo <- as.data.table(tidyr::pivot_wider(
  tab_filter,
  names_from = groups, 
  values_from = levels)[,-1])



save(sumv, n.annot, StInfo,file='SeaPenValidationData_240222.RData')

##### Datasets -----------------------------------------------------------------------------------------------------

load('SeaPenValidationData_240222.RData')

##### sumv - Table summarising the attributes of each predicted sea pen track (group sea pen          ###
##### detections considered to be one sea pen moving through the field of view in consequtive frames) ###

  ## Fields:
  ## video - video name
  ## model - machine learning model used for prediction
  ## Label - model predicted sea pen name
  ## TrackID - unique ID for a track in a video (not unique across videos)
  ## maxDLC - maximum detection label confidence score (how sure is the model) for detections that form the track
  ## n - number of detections forming the track
  ## Matched - binary (1/0) indication of whether the track intersects an analyst annotation (1=yes)
  ## MaxOverlapPct - Maximum proportion of the area of model detections that overlaps with an anlyst annotation
  ##                 (this can be more than 100 percent if the detection overlaps more than one annotation
  ##                 which can happen where multiple annotations are close to each other)
  ## NumberMatched - number of analyst annotations matched with track (see above)
  ## Station - station code
  ## Year - year sampled

summary(sumv)
View(sumv)

##### n.annot - Table with the number of sea pens annotated by the analyst per video               ###

  ## Fields:
  ## video - video name
  ## Pennatula - number of Pennatula
  ## Virgularia - number of Virgularia
  ## Station - station code
  ## Year - year sampled

summary(n.annot)
View(n.annot)

##### StInfo - Table visibility record for each year               ###

## Fields:
## Cruise - cruise code
## Stno - cruise specific station number (for double check comparison with video names)
## Virgularia - number of Virgularia
## TVID - Nephrops survey station ID
## Station - station code without leading 6 (for FU6), for comparability with other data
## Year - year sampled
## Vis - visibility in 3 classes (Good, Moderate, Bad)

summary(StInfo)
View(StInfo)


##### PLOTS --------------------------------------------------------------------------------------------------------


# plot numbers of frames and confidence with whether matched or not for individual videos
pp <- ggplot(sumv[video== 'CEND0919_ST_089_TVID_6-L' & Label=='Pennatula'], 
             aes(x=as.factor(n), y=maxDLC,fill=as.factor(Matched))) + 
      geom_dotplot(binaxis='y',binwidth = 0.02, stackdir='center') +
      xlab('Number of detections per track') +
      ylab('Detection label confidence') +
      scale_fill_manual(values=c('#4f8686','#a90a26'),name='Matched',labels = c("No", "Yes"))
pp + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                  geom="pointrange",shape=5, color="black")

pv <- ggplot(sumv[video== 'CEND0919_ST_089_TVID_6-L' & Label=='Virgularia'], 
             aes(x=as.factor(n), y=maxDLC,fill=as.factor(Matched))) +
  geom_dotplot(binaxis='y',binwidth = 0.02, stackdir='center') +
  xlab('Number of detections per track') +
  ylab('Detection label confidence') +
  scale_fill_manual(values=c('#4f8686','#a90a26'),name='Matched',labels = c("No", "Yes"))
pv + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                  geom="pointrange", shape=5,color="black")

# Hexbin plots summarising for all videos
pph <-  ggplot(sumv[Label=='Pennatula'], aes(n, maxDLC, z = Matched)) +
        stat_summary_hex(fun = mean) +
        scale_x_log10(breaks=c(1,5,25,50,100,500,1000)) +
        annotation_logticks() +
        scale_fill_gradientn(colours = wes_palettes$Zissou1, 
                             guide = "colourbar",
                             name='Proportion\nmatched') +
        xlab('Number of detections per track') +
        ylab('Detection label confidence') +
        theme(legend.position=c(0.9,0.2),
              legend.title = element_text(vjust = 1.2),
              legend.margin =margin(r=10,l=10,t=10,b=10))
pph

pvh <-  ggplot(sumv[Label=='Virgularia'], aes(n, maxDLC, z = Matched)) +
        stat_summary_hex(fun = mean) +
        scale_x_log10(breaks=c(1,5,25,50,100,500,1000)) +
        annotation_logticks() +
        scale_fill_gradientn(colours = wes_palettes$Zissou1, 
                             guide = "colourbar",
                             name='Proportion\nmatched') +
        xlab('Number of detections per track') +
        ylab('Detection label confidence') +
        theme(legend.position=c(0.9,0.2),
              legend.title = element_text(vjust = 1.2),
              legend.margin =margin(r=10,l=10,t=10,b=10))
pvh


