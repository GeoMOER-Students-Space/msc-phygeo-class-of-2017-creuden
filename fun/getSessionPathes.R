# rs-ws-05-1
#' @description  MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#' getSessionPathes
#'@return 
#' defines and creates (if necessary) all folders variables
#' set the SAGA path variables and other system variables
#' exports all variables to the global environment
#'
#'@param filepath_git  project github root directory (your github name)
#'@param csess= current session "01",
#'@param courseCode current course options are "gi", "rs", "da" you may only use one course per session
#'@param moc=TRUE creates a folder structure according to the needs of the MOC courses, FALSE creates a simple project structure
#'@param dataWorkingFolder default c("/scripts/", "/rmds/"),
#'@param sessionWorkingFolder default ("RData/","temp/","run/","input/","output/"))
#'
#'\preformatted{
#'   If moc=TRUE the following folderstructure is exported. If folders do not exist thesy will be created.
#'.
#'├── data
#'│   ├── data_analysis
#'│   │   ├── csv
#'│   │   └── raw
#'│   ├── gis
#'│   │   ├── input
#'│   │   ├── output
#'│   │   ├── RData
#'│   │   ├── run
#'│   │   └── temp
#'│   └── remote_sensing
#'│       ├── aerial
#'│       ├── aerial_croped
#'│       ├── aerial_merged
#'│       ├── input
#'│       ├── RData
#'│       ├── run
#'│       └── temp
#'└── MOC
#'    ├── data_analysis
#'    │   └── da-ws-01
#'    │       └── rmds
#'    │       └── scripts
#'    ├── fun
#'    ├── gis
#'    │   └── gi-ws-01
#'    │       └── rmds
#'    │       └── scripts
#'    └── remote_sensing
#'        └── rs-ws-01
#'    │       └── rmds
#'            └── scripts
#'   } 
#'
#'@author Thomas Nauss, Chris Reudenbach
#'
#'@return  getSessionPathes< creates if necessary the directories and export the corresponding pathes as global variables\cr

getSessionPathes<- function(filepath_git,
                            filepath_data,
                            sessNo=1,
                            courseCode="gi",
                            sessionWorkingFolder=c("/scripts/", "/rmds/"),
                            dataWorkingFolder=c("RData/","temp/","run/","input/","output/")) {
  
  # switch backslash to slash and expand path to full path
  filepath_git<-gsub("\\\\", "/", path.expand(filepath_git))  
  filepath_data<-gsub("\\\\", "/", path.expand(filepath_data))  
  # check  tailing / and if not existing append
  if (substr(filepath_git,nchar(filepath_git),nchar(filepath_git)) != "/") {
    filepath_git<-paste0(filepath_git,"/")
  }
  if (substr(filepath_data,nchar(filepath_data),nchar(filepath_data)) != "/") {
    filepath_data<-paste0(filepath_data,"/")
  }
  
  # script and function folder for each course session can be adapted 
  sessionWorkingFolder<-sessionWorkingFolder
  # currently implemented data folders can be adapted 
  dataWorkingFolder<-dataWorkingFolder
                       
  

    # static course structure - better keep the below folders
    proj_root_git<-c(path.expand(filepath_git))
    proj_root_data<-filepath_data
    #proj_root_data<-paste0(substr(proj_root_git,1,gregexpr(pattern ='/',proj_root_git)[[1]][as.numeric(lengths(gregexpr(pattern ='/',proj_root_git))[[1]]-2)]),"data/")
    
    if (courseCode == "rs") {
    sub_root<-c("remote_sensing/")
    session_ID<-c("rs-ws-")
    
    } else if (courseCode == "gi") {
    sub_root<-c("gis/")
    session_ID<-c("gi-ws-")
    
    } else if  (courseCode == "da") {
    sub_root<-c("data_analysis/")
    session_ID<-c("da-ws-")
    
    }
    # create sessionstring
    
    session_number<- sapply(sessNo, function(no){
      if (no<10) {no<-paste0("0",no)}
      return(no)
    })
    
    # create folder and varibales 
    # function folder for all courses
    name<-paste0("fun")
    value<-paste0(filepath_git,"fun/")
    makGlobalVar(name, value)
    # and the rest
      
    for (i in 1:length(proj_root_git)) {
#      for (j in 1:length(sub_root)) {
        #for (k in 1:length(session_ID)) {
          for (l in 1:length(session_number)) {
            for (m in 1:length(sessionWorkingFolder)) {
              name<-paste0( substr(session_ID,1,2),"_",as.character(gsub("/", "", session_number[l])),"_",as.character(gsub("/", "",sessionWorkingFolder[m])))
              value<- paste0(proj_root_git[i],sub_root,session_ID,session_number[l],sessionWorkingFolder[m])
               makGlobalVar(name, value)
              }
            }
#          }
#        }sub_root<-3
      }
    
    
    # data structure NOTE it is outside the proj_root_git folder
    for (i in 1:length(proj_root_data)){
#      for (j in 1:length(sub_root)) {
        for (k in 1:length(dataWorkingFolder)) {
          name<-paste0(substr(session_ID,1,2),"_",as.character(gsub("/", "",dataWorkingFolder[k]))) #add prefix
          value<- paste0(proj_root_data[i],sub_root,dataWorkingFolder[k])
           makGlobalVar(name, value)
          if (courseCode==substr(session_ID,1,2) && dataWorkingFolder[k]=="run/"){
            path_temp<- value
          }
        }
#      }
  } # end of moc=TRUE

}
