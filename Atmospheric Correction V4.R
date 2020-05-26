library(raster)
library(landsat)
library(magrittr)
library(tidyverse)
library(RStoolbox)
library(rgdal)
library(sf)
library(gridExtra)
library(sp)
library(ncdf4)
library(rlist)
library(utils)
library(tcltk)


############
#  http://bleutner.github.io/RStoolbox/rstbx-docu/radCor.html
############

c1_path <- "E:/University College London/O'Sullivan, Aidan - SDG6/Landsat data/Cropped level 1 data/"
cor2_path <- "E:/University College London/O'Sullivan, Aidan - SDG6/Landsat data/Corrected data V2/"

setwd(c1_path)


# Create lists of scene and site ids for processing -----------------------


### create list of dowloaded files
dir_files <- list.files(pattern = '.TIF')
sids <- str_split(dir_files,"__",simplify = T)[,1]

## list of sid__eid
eids1 <- str_split(dir_files,"__",simplify = T)[,2] %>% str_trunc(43,"right") %>% str_remove_all('\\.')
sid_eids <- paste0(sids,'__',eids1) %>% unique()

### list of processed files
dircor_files <- list.files(path = cor2_path,pattern = '.B2_sre.tif|.B2_tre.tif')
cors <- str_split(dircor_files,"__",simplify = T)[,1]

## list of processed sid__eid
eids2 <- str_split(dircor_files,"__",simplify = T)[,2] %>% str_trunc(43,"right") %>% str_remove_all('\\.')
cor_eid <- paste0(cors,'__',eids2) %>% unique()

## subset files to process on already processed
sid_eid_tp <- sid_eids[!(sid_eids %in% cor_eid)] %>% unique()

## list metadata files
mtl_files <- list.files(pattern = 'MTL')


# For loop ----------------------------------------------------------------


exceptions <- NULL

## range of sids to process
sid_eid_tpf <- sid_eid_tp[50000:60000]

## progress bar setup
total = length(sid_eid_tpf)
pb <- tkProgressBar(title = "progress bar",min = 0, max = total)
times <- data.frame(t1=NA,t2=NA,t3=NA,t4=NA)

for (sid_eid in sid_eid_tpf) {
  
  ptm <- proc.time()
  pos <- match(sid_eid,sid_eid_tpf)
  tt <- data.frame(t1=NA,t2=NA,t3=NA,t4=NA)
  setTkProgressBar(pb, pos,label=paste0(pos,"/",total,
                                      " done ",
                                      round(mean(times$t4,na.rm=T),0),
                                      "s/iter"))
  ## set sid and eid
  sid <- str_split(sid_eid,'__')[[1]][1]
  eid <- str_split(sid_eid,'__')[[1]][2]
  
  #~~~~ List tif and meta_data files ~~~~#
  
  ## list tifs for sid eid
  tifs <- dir_files[str_detect(dir_files,sid_eid)]
  
  ## find meta_data file and read
  mtl_file <- mtl_files[str_detect(mtl_files,eid)]
  
  if (length(mtl_file)!=0) {meta_data <- readMeta(mtl_file)}
  else {
    temp <- data.frame(sid = sid,eid=eid,call = 'read meta',error='No metadata')
    exceptions <- rbind(exceptions,temp)
  }
  
  #~~~~ If meta file read, proceed to process rasters ~~~~#
  
  if (exists("meta_data")==T){

    ## create stack from bands
    ras_stack <- do.call(stack, lapply(tifs, raster))

    ## rename bands
    blist <- str_remove_all(tifs,str_c('__',sid,str_c(eid,'_'),'.tif','.TIF',sep = '|'))
    blist %<>% str_replace_all('BQA','QA')
    blist <- lapply(blist,function(x){str_c(x,'_dn')}) %>% unlist()
    names(ras_stack) <-  blist
    
    
    t1 <- proc.time() - ptm
    tt$t1 <- t1[[3]]
    
    
    # ~~~ Create cloud mask from QA ~~~ #

    if ('QA_dn' %in% names(ras_stack)) {

      if (meta_data$SATELLITE=="LANDSAT8"){
      cloud_mask <- classifyQA(ras_stack$QA_dn,type  = c("cloud", "cirrus"),
                             sensor = "OLI",legacy = "collection1",confLayers=T)
      } else if (meta_data$SATELLITE=="LANDSAT7"){
        cloud_mask <- classifyQA(ras_stack$QA_dn,type = c("cloud", "cirrus"),
                                 sensor = "ETM+",legacy = "collection1",confLayers=T)}
      else if (meta_data$SATELLITE=="LANDSAT5"){
        cloud_mask <- classifyQA(ras_stack$QA_dn,type = c("cloud", "cirrus"),
                                 sensor = "TM",legacy = "collection1",confLayers=T)}

      ## Write cloud mask to file
      writeRaster(cloud_mask,paste0(cor2_path,sid,'__',eid,'_',names(cloud_mask),'.tif'),
                  overwrite = T, bylayer=TRUE,format="GTiff")

    }

    t2 <- proc.time() - ptm
    tt$t2 <- t2[[3]]
    
    
    # ~~~ Convert DN to surface reflectance ~~~ #

           
    tryCatch( expr = {
      
      errorm <- NULL
      errorc <- NULL
      
      ## correction
      lsat_costz <- radCor(ras_stack, metaData = meta_data, method = "costz")
      
      t3 <- proc.time() - ptm
      tt$t3 <- t3[[3]]
      
      ## write raster to file as seperate tifs
      writeRaster(lsat_costz,filename=paste0(cor2_path,sid,'__',eid,'_',names(lsat_costz)),
                overwrite = T, bylayer=TRUE,format="GTiff")

      },error = function(e){message()
        errorc <<- conditionCall(e)
        errorm <<- conditionMessage(e)} )
      
      ## Add erros from call to df
      errorm <- ifelse(is.null(errorm),'none',errorm)
      
      temp <- data.frame(sid = sid,eid=eid,call = toString(errorc),error=errorm)
      exceptions <- rbind(exceptions,temp)

    
    if (exists("ras_stack")==T) {rm(ras_stack)}
    if (exists("blist")==T) {rm(blist)}
  }

  if (exists("meta_data")==T) {rm(meta_data)}
  if (exists("mtl_file")==T) {rm(mtl_file)}

  t4 <- proc.time() - ptm
  tt$t4 <- t4[[3]]
  times <- rbind(times,tt)
  
  removeTmpFiles(h=0)
  
}


mean(times$t1/times$t4,na.rm = T)
mean(times$t2/times$t4,na.rm = T)
mean(times$t3/times$t4,na.rm = T)

## Write exceptions to file
setwd(cor2_path)
write.csv(exceptions,'ac exceptions.csv')

View(exceptions[exceptions$eid=='LC08_L1TP_198017_20130712_20170503_01_T1',])

exceptions$error %<>% as.character()

exceptions[str_detect(exceptions$call,'Lhaze[1]'),] %>% group_by(error) %>% summarise(n=n())
