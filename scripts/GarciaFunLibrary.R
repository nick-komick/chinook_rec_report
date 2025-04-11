####################
# GarciaFunLibrary #
####################

###########################################
# readHRJ
#
# Description
# ------------------
# reads in a. HRJ file and returns a list object with the n 
#
# Argument(s)
# ------------------
# filename
# nFisheries
# straysinescap
# Age6
#
# Output(s)
# ------------------
# list object 
###########################################
#readHRJ
readHRJ <- function(filename, nFisheries=69, straysinescap=TRUE, Age6=c("DNE","ignore","include"), zipped=FALSE) {
  #Read in the file by line
   #if file is not in a zipped directory (default)
    if(!zipped) {
     rawHRJ <- readFILE(filename)
   #else
    } else {
     rawHRJ <- readFILEinZIP(filename) #note filename is a vector of length 2: c(zip file name, file within zip file name)
    }
  #Determine if there are strays in escapement
    #best let user specify... #if((length(strsplit(rawHRJ[1]," ")[[1]][nchar(strsplit(rawHRJ[1], split=" ")[[1]])!=0])-3) %% 3 == 0) 
  #Determine the number of years of data in the HRJ
   nYears <- length(rawHRJ)/(nFisheries+1) #+1 b/c first row always contains escapement
   if(I(nYears-floor(nYears))!=0) cat("WARNNG: number of fisheries in the HRJ and what's user specified mismatch\n")
  #First, subset the data to escapRows and fishRows
   escRows <- rawHRJ[seq(1, length(rawHRJ), by=nFisheries+1)]
   fshRows <- rawHRJ[-seq(1, length(rawHRJ), by=nFisheries+1)]
  #Second, set up the vectors for the access database dependent on;
   #1)if strays are present or not
   #2)what to do with 5 age HRJs (2:6 vs. 2:5)
   hrjNamFish = c("AEQCat","AEQTot","NomCat","NomTot","Pop")
   if(straysinescap) {
     hrjNamEsc = c("All_Esc", "CA_Esc", "US_Esc")
   } else hrjNamEsc = "Esc"
   #given the straysinescap and Age6 option specified
    if(Age6=="DNE" || Age6=="ignore") {
      if(straysinescap) {
         hrjEscVec = sort(paste(hrjNamEsc, sort(rep(2:5,3)), sep=""))
      } else hrjEscVec = paste(hrjNamEsc, 2:5, sep="")
      hrjFshVec = sort(paste(hrjNamFish, sort(rep(2:5,5)), sep=""))
    } else if(Age6=="include") {
      if(straysinescap) {
        hrjEscVec = sort(paste(hrjNamEsc, sort(rep(2:6,3)), sep=""))
      } else hrjEscVec = paste(hrjNamEsc, 2:6, sep="")
      hrjFshVec = sort(paste(hrjNamFish, sort(rep(2:6,5)), sep=""))     
    }  else cat("WARNING: user-specified Age6 option not recognized\n")
  #Third, set up the number of escapement and fishery data matrices
   escMat <- as.data.frame(matrix(NA, ncol=length(hrjEscVec)+3, nrow=length(escRows)))
   names(escMat) = c("brood", "fishery", "oldestage", hrjEscVec)
   fshMat <- as.data.frame(matrix(NA, ncol=length(hrjFshVec)+3, nrow=length(fshRows)))
   names(fshMat) = c("brood", "fishery", "oldestage", hrjFshVec)
  #Fourth, load the escapement data
   for(i in 1:length(escRows)) {
    tmp = strsplit(escRows[i]," ")[[1]][nchar(strsplit(escRows[i], split=" ")[[1]])!=0]
    escMat[i,"brood"] = as.numeric(tmp[1])
    escMat[i,"fishery"] = as.numeric(tmp[2])
    escMat[i,"oldestage"] = as.numeric(tmp[3])
    #if length of the HRJ row is 3, there's no data, so do nothing
    if(length(tmp)!=3) {
      #oldestage = as.numeric(tmp[3]) (stocks that have age 5 & 6 collapsed reports the oldestage as 6, not 5, so i can't reference that value)
      oldestage = as.numeric(tmp[3]) #length(tmp[4:length(tmp)])/ifelse(straysinescap,3,1)+1
      minage = as.numeric(tmp[3]) - length(tmp[4:length(tmp)])/ifelse(straysinescap,3,1)+1
      hrjEscTmp = sort(paste(hrjNamEsc, sort(rep(minage:oldestage,ifelse(straysinescap,3,1))), sep=""))
      tmp2 = as.numeric(tmp[4:length(tmp)])
      names(tmp2) = hrjEscTmp[1:length(tmp2)]
      #IFFF YOU WANT TO IGNORE AGE6, YOU HAVE TO READ THE DATA IN, BUT IT IS THEN DROPPED HERE!!!!
      #Case only applies if age6 is present
      if(Age6=="ignore" && length(-grep(6,names(tmp2)))!=0) {
        escMat[i,names(tmp2)[-grep(6,names(tmp2))]] = tmp2[-grep(6,names(tmp2))]
      } else escMat[i,names(tmp2)] = tmp2     
    }
   }
  #Fifth, load the fishery data
   for(i in 1:length(fshRows)) {
     tmp = strsplit(fshRows[i]," ")[[1]][nchar(strsplit(fshRows[i], split=" ")[[1]])!=0]
     fshMat[i,"brood"] = as.numeric(tmp[1])
     fshMat[i,"fishery"] = as.numeric(tmp[2])
     fshMat[i,"oldestage"] = as.numeric(tmp[3])
     #if length of the HRJ row is 3, there's no data, so do nothing
     if(length(tmp)!=3) {
       #oldestage = as.numeric(tmp[3]) (stocks that have age 5 & 6 collapsed reports the oldestage as 6, not 5, so i can't reference that value)
       oldestage = as.numeric(tmp[3]) #length(tmp[4:length(tmp)])/5 + 1
       minage = as.numeric(tmp[3]) - length(tmp[4:length(tmp)])/5 + 1
       
       hrjFshTmp = sort(paste(hrjNamFish, sort(rep(minage:oldestage,5)), sep=""))
       tmp2 = as.numeric(tmp[4:length(tmp)])
       names(tmp2) = hrjFshTmp[1:length(tmp2)]
       #IF YOU WANT TO IGNORE AGE6, YOU HAVE TO READ THE DATA IN, BUT IT IS THEN DROPPED HERE!!!!
       #Case only applies if age6 is present
       if(Age6=="ignore" && length(-grep(6,names(tmp2)))!=0) {
         fshMat[i,names(tmp2)[-grep(6,names(tmp2))]] = tmp2[-grep(6,names(tmp2))]
       } else fshMat[i,names(tmp2)] = tmp2      
     }
   }
  #Sixth, return results to user
 return(list(ESC=escMat,HRJ=fshMat,strays=straysinescap,nFisheries=nFisheries))
}

###########################################
# .convertHRJ_BYtoCY_bytable
#
# Description
# ------------------
# Internal function that manipulates an HRJ standardized R format object
# from brood year data to calendar year data and
# determines if a brood is complete
#
# Dependent(s)
# ------------------
# .convertHRJ_BYtoCY_bytable - internal function that 
#
# Argument(s)
# ------------------
#
# Output(s)
# ------------------
# 
###########################################
.convertHRJ_BYtoCY_bytable <- function(x) {
 #subset the data by age
  headings=x[,match(c("brood","fishery","oldestage","stock"), names(x), nomatch=0)]
  x.age2 = cbind(headings,x[,c(grep("2", names(x)))])
  x.age3 = cbind(headings,x[,c(grep("3", names(x)))])
  x.age4 = cbind(headings,x[,c(grep("4", names(x)))])
  x.age5 = cbind(headings,x[,c(grep("5", names(x)))])
  x.age6 = cbind(headings,x[,c(grep("6", names(x)))])
 #add cy date to each
  x.age2$cy <- x.age2$brood+2
  x.age3$cy <- x.age3$brood+3
  x.age4$cy <- x.age4$brood+4
  x.age5$cy <- x.age5$brood+5
  x.age6$cy <- x.age6$brood+6
 #create blank matrix
  minCY <- min(x.age2$cy,x.age3$cy,x.age4$cy,x.age5$cy,x.age6$cy)
  maxCY <- max(x.age2$cy,x.age3$cy,x.age4$cy,x.age5$cy,x.age6$cy)
  cy.HRJ = matrix(NA,nrow=max(x$fishery)*(maxCY-minCY+1),ncol=ncol(x)+1)
  colnames(cy.HRJ) = c(names(x),"cy")
  cy.HRJ = as.data.frame(cy.HRJ)
  cy.HRJ$cy = sort(rep(minCY:maxCY,max(x$fishery)))
 #if the max fishery number is 0, assume that we're dealing with escapement. 
  if(max(x$fishery)==0) {
   cy.HRJ = matrix(NA,nrow=1*(maxCY-minCY+1),ncol=ncol(x)+1)
   colnames(cy.HRJ) = c(names(x),"cy")
   cy.HRJ = as.data.frame(cy.HRJ)
   cy.HRJ$cy = sort(rep(minCY:maxCY,1))
  }
 #subset data to each cy and recombine
  for(i in minCY:maxCY) {
    tmp = subset(cy.HRJ,cy==i)
    age2.temp = subset(x.age2, cy==i)
    
   # subset(x.age2, brood==i&fishery==2)
   # subset(x.age3, brood==i&fishery==2)
   # subset(x.age4, brood==i&fishery==2)
   # subset(x.age5, brood==i&fishery==2)
   # i=1982
   # subset(x.age2, cy==i&fishery==2)
   # subset(x.age3, cy==i&fishery==2)
   # subset(x.age4, cy==i&fishery==2)
   # subset(x.age5, cy==i&fishery==2)
    
    if(nrow(age2.temp)!=0) tmp[,names(age2.temp)] = age2.temp
    age3.temp = subset(x.age3, cy==i)
    if(nrow(age3.temp)!=0) tmp[names(age3.temp)] = age3.temp
    age4.temp = subset(x.age4, cy==i)
    if(nrow(age4.temp)!=0) tmp[names(age4.temp)] = age4.temp
    age5.temp = subset(x.age5, cy==i)
    if(nrow(age5.temp)!=0) tmp[names(age5.temp)] = age5.temp
    age6.temp = subset(x.age6, cy==i)
    if(nrow(age6.temp)!=0) tmp[names(age6.temp)] = age6.temp
    cy.HRJ[cy.HRJ$cy %in% i,] = tmp
  }
 #cleanup and rearrange the matrix
  cy.HRJ = cy.HRJ[,c(1,ncol(cy.HRJ),2:(ncol(cy.HRJ)-1))] 
  cy.HRJ = cy.HRJ[,-match("brood",names(cy.HRJ),nomatch=0)] #drop column brood, and put the column cy in its place
 #determine if it's a 4 age or 5 age HRJ file...
  #this is determined by case: if the input is escapement or fishery
  if(max(x$fishery)==0) {
    nvars <- 3 #escap, ca escap strays, us escap strays
  } else {
    nvars <- 5 #nom lc, nom tm, aeq lc, aeq tm, population
  }
  #if it's a 4 age HRJ file, then all values in 3 (escapement) or 5 (fishery) columns will be NA
  if(sum(colSums(apply(cy.HRJ[,-c(1:3)], 2, is.na))==nrow(cy.HRJ))>=nvars) {
    #if it's a 4 age HRJ, a cy with incomplete BY data will have 2 or more columns/nvars NA values
    #for example, a 4 age escapement HRJ input that's missing 3 of 4 age classes in a catch year will have 12 NA values and 12/3=4 NA values
    #and for a year with complete information it'll have 3 NA values for a total of 3/3=1 NA values
    #and for the same example but its fishery HRJ, it'll have 20 NA values and 20/5=4 NA values
    #and fir a year with complete information it'll have 5 NA values for a total of 5/5=1 NA values
    cy.HRJ$inc = rowSums(apply(cy.HRJ[,-c(1:3)], 2, is.na))/nvars>1
  } else {
    #and if it's a 5 age HRJ, a cy with incomplete BY data will have no NA values in a row!
    cy.HRJ$inc = rowSums(apply(cy.HRJ[,-c(1:3)], 2, is.na))>0   
  }
 #return object
  cy.HRJ
}

###########################################
# convertHRJ_BYtoCY
#
# Description
# ------------------
# Manipulates an HRJ standardized R format object  from brood year data to calendar year data
# Determine if a brood is complete
#
# Dependent(s)
# ------------------
# .convertHRJ_BYtoCY_bytable - interal function that 
#
# Argument(s)
# ------------------
#
# Output(s)
# ------------------
# 
###########################################
convertHRJ_BYtoCY <- function(x) {
  if(x$HRJformat=="calendar") cat("ERROR: data is already in calendar year format\n")
  for(i in 1:x$nstocks) {
    #convert fishery data
    x[[i]]$HRJ_BY = .convertHRJ_BYtoCY_bytable(x[[i]]$HRJ_BY)
    x[[i]]$HRJ_CY = .convertHRJ_BYtoCY_bytable(x[[i]]$HRJ_CY)
    #convert escapement data
    x[[i]]$ESC_BY = .convertHRJ_BYtoCY_bytable(x[[i]]$ESC_BY)
    x[[i]]$ESC_CY = .convertHRJ_BYtoCY_bytable(x[[i]]$ESC_CY)
  }
  x$HRJformat="calendar"
  x
}

###########################################
# addPTableHRJ
#
# Description
# ------------------
# Adds the "preferred" table to any HRJ object. Use the 'BY' method data when the brood is incomplete, else use the 'CY" method
#
# Dependent(s)
# ------------------
# na
#
# Argument(s)
# ------------------
#
# Output(s)
# ------------------
# 
###########################################
addPTableHRJ <- function(x, hrjclass=c("R","Access")) {
  if(hrjclass=="R") {
    cat("not implemented\n")
    stop()
  }
  if(hrjclass=="Access" && !is.null(x$HRJ_BY$inc) && x$HRJformat=="calendar") {
    #copy the HRJ_C shaker method results into a tmp object
     tmp = x$HRJ_CY
     tmp2 = x$ESC_CY
    #for rows where the brood is not complete, copy the HRJ_B shaker method results over
     tmp[tmp$inc,] <- x$HRJ_BY[tmp$inc,]
     tmp2[tmp2$inc,] <- x$ESC_CY[tmp2$inc,]
    #hence the HRJ_C shaker method results are referenced when the brood year is complete
    #add a field named inc inc to state where the data is from
     tmp$inc <- ifelse(tmp$inc,"by","cy")
     tmp2$inc <- ifelse(tmp2$inc,"by","cy")
    #and add the 'p' method results to the HRJ object
     x$HRJ_P = tmp
     x$ESC_P = tmp2
  }
  if(hrjclass=="Access" && x$HRJformat=="brood") {
   #create a blank list object to store the stock-specific HRJ_P results
    hrj_p = list()
    esc_p = list()
   #for each stock in the HRJ object
    for(i in 1:x$nstocks) {
     #1. subset the ERA result's escapement estimates to just one stock
      hrj_tmp = subset(x$ESC_BY, stock==i)
     #2. preliminary steps
      #determine if it's a 4 age or 5 age structured HRJ file
      #if it's a 4 age HRJ, it'll have all blanks across all years in a given age
       nAges = colSums(apply(hrj_tmp[,match(paste("All_Esc",2:6,sep=""), names(hrj_tmp))], 2, is.na))
       nRows = nrow(hrj_tmp)
       nAgesInHRJ = length(2:6) - sum(nAges==nRows) #to illustrate the implicit assumption that the world of CoShak is 2-5, 3-6, or 2-6 (and nothing but!)
      #determine the range of ages present
       rangeAge = as.numeric(right(names(nAges[!(nAges==nRows)]),1))
     #3. determine if the B or C shaker method results should be used in multiple steps
      #a. transform the data
       hrj_tmp_pivot = pivotMatrix(x=hrj_tmp, repeatcolumns=match("brood",names(hrj_tmp)), movecolumns=match(paste("All_Esc", 2:6,sep=""), names(hrj_tmp)))
       hrj_tmp_pivot = data.frame(hrj_tmp_pivot)
       colnames(hrj_tmp_pivot) = c("brood","age","value")
       hrj_tmp_pivot$brood = as.numeric(hrj_tmp_pivot$brood)
       hrj_tmp_pivot$age = as.numeric(right(hrj_tmp_pivot$age,1))
       hrj_tmp_pivot$cy = hrj_tmp_pivot$brood + hrj_tmp_pivot$age
      #b. switch from by to cy data layout
       hrj_tmp_pivot2 = with(hrj_tmp_pivot, tapply(value, list(cy,age), length))
       hrj_tmp_pivot3 = data.frame(hrj_tmp_pivot2)
       names(hrj_tmp_pivot3) = colnames(hrj_tmp_pivot2)
       hrj_tmp_pivot3[,!(colnames(hrj_tmp_pivot3) %in% rangeAge)] = NA
      #c. where the method is determined
       #copy 
        hrj_tmp_pivot4 = hrj_tmp_pivot3
       #for each catch year...
        for(j in 1:nrow(hrj_tmp_pivot3)) {
         whichmethod = ifelse(sum(hrj_tmp_pivot3[j,],na.rm=TRUE)==nAgesInHRJ,"cy","by")
         hrj_tmp_pivot4[j,!is.na(hrj_tmp_pivot3[j,])] = whichmethod
        }
       #re-add field cy
       hrj_tmp_pivot4$cy = as.numeric(rownames(hrj_tmp_pivot4))
      #d. "fold" data back
       #
        hrj_tmp_pivot5 = list()
        for(j in 1:(ncol(hrj_tmp_pivot4)-1)) { 
         hrj_tmp_pivot5[[j]] = hrj_tmp_pivot4[,c(j,grep("cy",names(hrj_tmp_pivot4)))]
         hrj_tmp_pivot5[[j]]$age = as.numeric(names(hrj_tmp_pivot4)[j])
         names(hrj_tmp_pivot5[[j]]) = c("inc","cy","age")
        }
       #
        hrj_tmp_pivot5 = do.call("rbind",hrj_tmp_pivot5)
       #re-add field brood
        hrj_tmp_pivot5$brood = hrj_tmp_pivot5$cy - hrj_tmp_pivot5$age
      #e. transform the data from cy to by layout
       hrj_tmp_pivot6 = data.frame(with(hrj_tmp_pivot5, tapply(inc, list(brood,age), unique)))
       names(hrj_tmp_pivot6) = paste("method_age",2:6,sep="")
       hrj_tmp_pivot6$brood = as.numeric(rownames(hrj_tmp_pivot6))
     #4. cobble together the p method results
      #add the HRJ_C shaker method results into a tmp object
       hrj_p[[i]] = subset(x$HRJ_CY, stock==i)
       hrj_p[[i]] = merge(hrj_p[[i]], hrj_tmp_pivot6, by.x="brood", by.y="brood")
       hrj_p[[i]] = hrj_p[[i]][,match(c(names(x$HRJ_CY),paste("method_age",2:6,sep="")),names(hrj_p[[i]]))]
       esc_p[[i]] = subset(x$ESC_CY, stock==i)
       esc_p[[i]] = merge(esc_p[[i]], hrj_tmp_pivot6, by.x="brood", by.y="brood")
       esc_p[[i]] = esc_p[[i]][,match(c(names(x$ESC_CY),paste("method_age",2:6,sep="")),names(esc_p[[i]]))]
      #temp obj
       hrj_by = subset(x$HRJ_BY, stock==i)
      #
       SELECT_hrj = hrj_p[[i]][,paste("method_age",2:6,sep="")]
       SELECT2_hrj = ifelse(SELECT_hrj=="cy",TRUE,FALSE)
      #
       datablock = 5:29
       for(k in 1:nrow(hrj_p[[i]])) {
            MYSELECT = unlist(ifelse(rep(SELECT2_hrj[k,],5)==TRUE, hrj_p[[i]][k,datablock], hrj_by[k,datablock]))
            hrj_p[[i]][k,datablock] = MYSELECT
       }
       #temp obj
       esc_by = subset(x$ESC_BY, stock==i)
       #
       SELECT_esc = esc_p[[i]][,paste("method_age",2:6,sep="")]
       SELECT2_esc = ifelse(SELECT_esc=="cy",TRUE,FALSE)
       #
       datablock = 5:19
       for(k in 1:nrow(esc_p[[i]])) {
         MYSELECT = unlist(ifelse(rep(SELECT2_esc[k,],3)==TRUE, esc_p[[i]][k,datablock], esc_by[k,datablock]))
         esc_p[[i]][k,datablock] = MYSELECT
       }
     #5. add a field named inc inc to state where the data is from
      hrj_p[[i]]$inc <- NA
      hrj_p[[i]]$inc <- ifelse(apply(SELECT_hrj, 1, function(x) all(x=="cy",na.rm=TRUE)),"cy","mixed") #options are 'by', 'cy', 'mixed' 
      hrj_p[[i]]$inc <- ifelse(apply(SELECT_hrj, 1, function(x) all(x=="by",na.rm=TRUE)),"by",hrj_p[[i]]$inc) #options are 'by', 'cy', 'mixed'
      esc_p[[i]]$inc <- NA
      esc_p[[i]]$inc <- ifelse(apply(SELECT_esc, 1, function(x) all(x=="cy",na.rm=TRUE)),"cy","mixed")
      esc_p[[i]]$inc <- ifelse(apply(SELECT_esc, 1, function(x) all(x=="by",na.rm=TRUE)),"by",esc_p[[i]]$inc)
     #6. 
      hrj_by = NA
      esc_by = NA
     #7.
      if(i%%10==0) cat(round(i/x$nstocks*100,1), "% complete\n", sep="")
    } #next stock...
    #combine all the results
     tmp = do.call("rbind",hrj_p)
     tmp_esc = do.call("rbind",esc_p)
    #and add the 'p' method results to the HRJ object
     x$HRJ_P = tmp[, -match(paste("method_age", 2:6,sep=""),names(tmp))]
     x$ESC_P = tmp_esc[, -match(paste("method_age", 2:6,sep=""),names(tmp_esc))]
  } #end by data layout, access format
  return(x)
}

###########################################
# readHRJdir
#
# Description
# ------------------
# read all HRJ files in a directory. one noteworthy limitation of this function is that all HRJ's must be in the same format (#fisheries, strays)
#
# Argument(s)
# ------------------
#
#
# Output(s)
# ------------------
# an HRJ object
###########################################
readHRJdir <- function(userDir=choose.dir(), verbose=TRUE, zipped=FALSE, ...) {
#Get old dir
 odir = getwd()
#Set directory
 setwd(userDir)
#Find all HRJs in the user-specified directory
 #if NOT zipped (the default)
 if(!zipped) {
  myList <- list.files(".", pattern=".HRJ", full.names=TRUE)
  myListNoDir <- list.files("./", pattern=".HRJ")
 } else {
  zipFileList = list.files(".", pattern=".zip", full.names=TRUE)
  FilesInEachZip = lapply(zipFileList, readZIP_filelist)
  myList = unlist(lapply(FilesInEachZip, function(x) x[right(x,5)==".HRJ"]))
  myListNoDir = unlist(lapply(FilesInEachZip, function(x) right(x[right(x,5)==".HRJ"],9)))
 }
#Pull out the stock names and assign them numbers
 stkList = strtrim(myListNoDir,3)
 stkNum = sort(rep(1:length(unique(stkList)),2))
#Throw an exception if there's an odd number of HRJ's in the directory
 if((length(myList) %% 2) != 0) {
   cat("ERROR: an odd number of HRJ files was found in the directory. You're probably missing a C or B HRJ\n")
   print(myList)
   stop()
 }
#Initialize a few things
 hrjList <- list()
 ii = 1
#Read in the HRJ files by stock (and put them into the R HRJ data object format)
 for(i in 1:length(unique(stkList))) {
 #Throw an exception HRJ[1] and HRJ[2] are not the same stock
  if(substr(myListNoDir[ii],1,3) != substr(myListNoDir[ii+1],1,3)) {
    cat("ERROR: I don't know how this happened, but somehow the next two HRJ's are not the same stock\n")
    print(myList[ii])
    print(myList[ii+1])
    stop()
  }
  #Throw an exception HRJ[1] and HRJ[2] are not the same stock
   if(substr(myListNoDir[ii],4,4)!="B" || substr(myListNoDir[ii+1],4,4)!="C") {
     cat("ERROR: I don't know how this happened, but the B and C method HRJ's are out of order - they're read in as B, C\n")
     print(myList[ii])
     print(myList[ii+1])
     stop()
   }
 #Read in the the B, then C HRJ from the same stock
  #if NOT zipped (default)
   if(!zipped) { 
    if(verbose) cat("Reading HRJ File", ii, "of", length(myList), ":", myListNoDir[ii], "\n")
    hrj_tmp1 = readHRJ(myList[ii], ...)
    if(verbose) cat("Reading HRJ File", ii+1, "of", length(myList), ":", myListNoDir[ii+1], "\n")
    hrj_tmp2 = readHRJ(myList[ii+1], ...)
  #if zipped
   } else {
    if(verbose) cat("Reading HRJ File", ii, "of", length(myList), ":", myListNoDir[ii], "from zip file", zipFileList[i], "\n")
    hrj_tmp1 = readHRJ(c(zipFileList[i], myList[ii]), zipped=zipped, ...)
    if(verbose) cat("Reading HRJ File", ii+1, "of", length(myList), ":", myListNoDir[ii+1], "from zip file", zipFileList[i], "\n")
    hrj_tmp2 = readHRJ(c(zipFileList[i], myList[ii+1]), zipped=zipped, ...)
   }
 #Read in the data, which is organized by brood year 
  hrjList[[i]] = list(HRJ_BY=hrj_tmp1$HRJ, 
                      HRJ_CY=hrj_tmp2$HRJ,
                      ESC_BY=hrj_tmp1$ESC,
                      ESC_CY=hrj_tmp2$ESC,
                      stkAcronym=strtrim(myListNoDir[ii],3),
                      imMethod=c(substr(myListNoDir[ii],4,4),substr(myListNoDir[ii+1],4,4)) )
  #And increment
   ii=ii+2
  }
#Add stock names
 names(hrjList) = unique(stkList)
#Add the additional info for the R HRJ data object format
 hrjList$stknames = unique(stkList)
 #hrjList$fshnames = as.character(1:nrow(hrj_tmp1$HRJ)) rlp note: changed on 5/18/2020
 hrjList$fshnames = as.character(1:hrj_tmp1$nFisheries)
 hrjList$nstocks = length(unique(stkList))
 #hrjList$nfisheries = length(hrjList$fshnames) rlp note: changed on 5/18/2020
 hrjList$nfisheries = hrj_tmp1$nFisheries
 hrjList$HRJformat = "brood"
#Reset directory
 setwd(odir)
#Return HRJ object
 return(hrjList)
}

###########################################
# convertHRJ_RtoAccess
#
# Description
# ------------------
# Convert a R HRJ data object format into the access database format (later will be a function) by
# collapsing the by stock HRJ's B & C method (ESC & HRJ) data into individual dataframes
#
# Argument(s)
# ------------------
#
#
# Output(s)
# ------------------
# 
###########################################
convertHRJ_RtoAccess <- function(x, writeCSV=FALSE, userDir=NULL) {
 #Confirm that the first item in the vector is B because program assumes the order of B and C
  if(!(x[[1]]$imMethod[1]=="B" && x[[1]]$imMethod[2]=="C")) {
   cat("ERROR: B and C method HRJ methods are out of order. They should be in B/C Method\n")
   print(x[[1]]$imMethod)
   stop()
  }
 #Create the base BY method table
  hrj_by <- x[[1]]$HRJ_BY
  esc_by <- x[[1]]$ESC_BY
  hrj_by$stock <- 1
  esc_by$stock <- 1
 #Create the base CY method table
  hrj_cy <- x[[1]]$HRJ_CY
  esc_cy <- x[[1]]$ESC_CY
  hrj_cy$stock <- 1
  esc_cy$stock <- 1
 #Loop through and read the next
  if(x$nstocks>1) {
  for(i in 2:x$nstocks) {
   #BY
    hrj_tmp <- x[[i]]$HRJ_BY
    esc_tmp <- x[[i]]$ESC_BY
    hrj_tmp$stock <- i
    esc_tmp$stock <- i
    hrj_by <- rbind(hrj_by, hrj_tmp)
    esc_by <- rbind(esc_by, esc_tmp)
   #CY
    hrj_tmp <- x[[i]]$HRJ_CY
    esc_tmp <- x[[i]]$ESC_CY
    hrj_tmp$stock <- i
    esc_tmp$stock <- i
    hrj_cy <- rbind(hrj_cy, hrj_tmp)
    esc_cy <- rbind(esc_cy, esc_tmp)
  } #end for loop
  } #end if nstocks>1
  hrj_by <- hrj_by[,c(ncol(hrj_by),1:(ncol(hrj_by)-1))]
  esc_by <- esc_by[,c(ncol(esc_by),1:(ncol(esc_by)-1))]
  hrj_cy <- hrj_cy[,c(ncol(hrj_cy),1:(ncol(hrj_cy)-1))]
  esc_cy <- esc_cy[,c(ncol(esc_cy),1:(ncol(esc_cy)-1))]
  hrj=list(
    HRJ_BY=hrj_by,
    HRJ_CY=hrj_cy,
    ESC_BY=esc_by,
    ESC_CY=esc_cy,
    stknames = x$stknames,
    fshnames = x$fshnames,
    nstocks = x$nstocks,
    nfisheries = x$nfisheries,
    HRJformat = x$HRJformat
  )
 #if write CSV
  if(writeCSV) {
   #ask user to specify the directory iif userDir is equal to ask, otherwise write files to the default directory
    if(userDir=="ask") setwd(choose.dir()) 
   #write directories
   write.table(hrj_by, "hrj_by - by layout.csv")
   write.table(hrj_cy, "hrj_cy - by layout.csv")
   write.table(esc_by, "esc_by - by layout.csv")
   write.table(esc_cy, "esc_cy - by layout.csv") 
  }
 #Return output
  return(hrj)
}

###########################################
# convertHRJtoMRE
#
# Description
# ------------------
# 
# 
#
# Argument(s)
# ------------------
# x - matrix (output from a 
#
# Output(s)
# ------------------
# 
###########################################
convertHRJtoMRE <- function(x, datatype=c("fishery","escapement")) {
 if("cy" %in% names(x)) {
  if(datatype=="fishery") {
   myvec1 = c("stock","cy","fishery","oldestage","inc","AEQCat","AEQTot","NomCat","NomTot","Pop")
   myvec2 = c("stock","cy","fishery","oldestage","IMmethod","AEQCat","AEQTot","NomCat","NomTot","Pop","by")
  } else if (datatype=="escapement") {
   myvec1 = c("stock","cy","fishery","oldestage","inc","All_Esc","CA_Esc","US_Esc")
   myvec2 = c("stock","cy","fishery","oldestage","IMmethod","All_Esc","CA_Esc","US_Esc","by")
  }
  age2 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 2, sep=""))]
  age3 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 3, sep=""))]
  age4 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 4, sep=""))]
  age5 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 5, sep=""))]
  age6 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 6, sep=""))]
  age2$by = with(age2, cy-2)
  age3$by = with(age3, cy-3)
  age4$by = with(age4, cy-4)
  age5$by = with(age5, cy-5)
  age6$by = with(age6, cy-6)
  names(age2)=names(age3)=names(age4)=names(age5)=names(age6)=myvec2
  all_ages = rbind(age2,age3,age4,age5,age6)
  all_ages$age = with(all_ages, cy-by)
  if(datatype=="fishery") out = all_ages[is.na(all_ages$Pop)==FALSE,]
  if(datatype=="escapement") out = all_ages[is.na(all_ages$All_Esc)==FALSE,]
 } else if ("brood" %in% names(x)) { 
  if(datatype=="fishery") {
   myvec1 = c("stock","brood","fishery","oldestage","inc","AEQCat","AEQTot","NomCat","NomTot","Pop")
   myvec2 = c("stock","by","fishery","oldestage","IMmethod","AEQCat","AEQTot","NomCat","NomTot","Pop","cy")
  } else if (datatype=="escapement") {
   myvec1 = c("stock","brood","fishery","oldestage","inc","All_Esc","CA_Esc","US_Esc")
   myvec2 = c("stock","by","fishery","oldestage","IMmethod","All_Esc","CA_Esc","US_Esc","cy")
   if(!("inc" %in% names(x))) x$inc = NA
  }
  age2 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 2, sep=""))]
  age3 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 3, sep=""))]
  age4 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 4, sep=""))]
  age5 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 5, sep=""))]
  age6 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 6, sep=""))]
  age2$cy = with(age2, brood+2)
  age3$cy = with(age3, brood+3)
  age4$cy = with(age4, brood+4)
  age5$cy = with(age5, brood+5)
  age6$cy = with(age6, brood+6)
  names(age2)=names(age3)=names(age4)=names(age5)=names(age6)=myvec2
  all_ages = rbind(age2,age3,age4,age5,age6)
  all_ages$age = with(all_ages, cy-by)
 }
 if(datatype=="fishery") out = all_ages[is.na(all_ages$Pop)==FALSE,]
 if(datatype=="escapement") out = all_ages[is.na(all_ages$All_Esc)==FALSE,]
 return(out)
}

###########################################
# .convertAgeMatrixMRE
#
# Description
# ------------------
# Internal function
# 
#
# Argument(s)
# ------------------
# x - matrix (output from a 
#
# Output(s)
# ------------------
# 
###########################################
.convertAgeMatrixMRE <- function(x, type="by") {
 tmp = list()
 for(i in 1:ncol(x)) tmp[[i]] = cbind(as.numeric(rownames(x)), as.numeric(colnames(x)[i]),x[,i])
 tmp2 = do.call(rbind, tmp)
 rownames(tmp2) = 1:nrow(tmp2)
 tmp2 = as.data.frame(tmp2)
 if(type=="by") {
  names(tmp2) = c("by","age","value")
  tmp2$cy = tmp2$by+tmp2$age
 }
 if(type=="cy") {
  names(tmp2) = c("cy","age","value")
  tmp2$by = tmp2$cy-tmp2$age
 }
 return(tmp2)
}

###########################################
# roundUp (courtesy of Pete McHugh)
#
# Description
# ------------------
# defines  script-specific functions (this is a silly one, but it
# eliminates the need to install packages on end-user machines)
#
# Argument(s)
# ------------------
# x - matrix (output from a 
#
# Output(s)
# ------------------
# 
###########################################
roundUp <- function(x,to=10)
{
  to*(x%/%to + as.logical(x%%to))
}

###########################################
# plotGarciaAll (courtesy of Pete McHugh)
#
# Description
# ------------------
# Original garcia plotting script written by Pete McHugh, converted to a function for ease of use
# 
#
# Argument(s)
# ------------------
# garcia
# outdir
# outtype - either "pdf" or "tff"
# Output(s)
# ------------------
# 
###########################################
plotGarciaAll <- function(Garcia, outdir, outtype, pdffilename = NULL) {
#get original directory
 odir <- getwd()
#
 this_is_the_place<-outdir #set output directory for specs
#---------------------------------------------------
# set some general specs relevant to all plots ##
# Periods for coloring points
per1=as.character(levels(Garcia$Period)[1])
per2=as.character(levels(Garcia$Period)[2])
per3=as.character(levels(Garcia$Period)[3])
per4=as.character(levels(Garcia$Period)[4])
per5=as.character(levels(Garcia$Period)[5])
# The options for x-axis labeling (MRE ER vs. CY ER) and values to display
Xnames<-c("Mature-run equivalent exploitation rate (%)","Calendar year exploitation rate (%)")
xticks<-c("0%","20%","40%","60%","80%","100%")
#---------------------------------------------------
  if(is.null(pdffilename)) pdffilename = "Garcia Plot Figures.pdf"
  if(outtype == "pdf") pdf(file = paste(outdir,"\\",pdffilename,sep=""),height = 9.27, width = 12.76) # open graphics device
#---------------------------------------------------
# Now subset data, set specs, and make figures
for(Si in 1:max(Garcia$StockNum)){
  subGarc<-subset(Garcia,StockNum==Si) #get stock Si's data subset
  #recompute period range
   per1_sub = NA
   per2_sub = NA
   per3_sub = NA
   per4_sub = NA
   per5_sub = NA
  #
   hold_per1 = subset(subGarc, Period==per1&!is.na(Rate)&!is.na(Escapement))
   #if(nrow(hold_per1)!=0) per1_sub = paste(min(hold_per1$Year),"-",right(max(hold_per1$Year),2),sep="")
   if(nrow(hold_per1)!=0) per1_sub = "75-84"
   hold_per2 = subset(subGarc, Period==per2&!is.na(Rate)&!is.na(Escapement))
   #if(nrow(hold_per2)!=0) per2_sub = paste(min(hold_per2$Year),"-",right(max(hold_per2$Year),2),sep="")
   if(nrow(hold_per2)!=0) per2_sub = "85-98"
   hold_per3 = subset(subGarc, Period==per3&!is.na(Rate)&!is.na(Escapement))
   #if(nrow(hold_per3)!=0) per3_sub = paste(min(hold_per3$Year),"-",right(max(hold_per3$Year),2),sep="")
   if(nrow(hold_per3)!=0) per3_sub = "99-08"
   hold_per4 = subset(subGarc, Period==per4&!is.na(Rate)&!is.na(Escapement))
   #if(nrow(hold_per4)!=0) per4_sub = paste(min(hold_per4$Year),"-",right(max(hold_per4$Year),2),sep="")
   if(nrow(hold_per4)!=0) per4_sub = "09-18"
   hold_per5 = subset(subGarc, Period==per5&!is.na(Rate)&!is.na(Escapement))
   #if(nrow(hold_per5)!=0) per5_sub = paste(min(hold_per5$Year),"-",right(max(hold_per5$Year),2),sep="")
   if(nrow(hold_per5)!=0) per5_sub = "19-28"

  # Set stock(plot)-specific specs (i.e., the plotting 
  # range and y tick width ); also, do data manipulations
  # idea here is to make figure scale/display vary for
  # optimal display across a wide range of escapements
  m1<-max(subGarc$Escapement,na.rm=TRUE)/0.95
  yt1<-c("Spawning escapement (thousands)","Spawning escapement") # use different title, if /1K vs. not
  ytitle<-yt1[2] #raw values as default, /1K otherwise 
  ydat<-subGarc$Escapement #raw values as default, /1K otherwise
  sname<-as.character(subGarc$EISStock[1]) #stock (from old excel worksheet name) for fig naming
  fname<-paste(as.character(subGarc$Stock[1]),sep="")
  #Reference points
   LowerGoal<-subGarc$LowerGoal[1]
   SmsyRef<-subGarc$Smsy[1]
   S85Ref<-subGarc$S85[1]
   UmsyRef<-subGarc$Umsy[1]
  #If the escapement goal is a range (i.e. LowerGoal is specified then use it
   if(!is.na(LowerGoal)) {
    HorizRef1 = LowerGoal
    HorizRef2 = LowerGoal*.85
    HorizRefType = "LowerBound"
    HorizRefLabel1 = expression(Lower~Bound) #"Lower Bound"
    HorizRefLabel2 =  expression(S(0.85~Lower~Bound)) #"S(0.85 Lower Bound)"
   } else if (!is.na(SmsyRef)) {
    HorizRef1 = SmsyRef
    HorizRef2 = SmsyRef*.85
    HorizRefType = "Smsy"
    HorizRefLabel1 = expression(S[MSY])
    HorizRefLabel2 = expression(S(0.85~S[MSY]))
   } else {
    HorizRef1 = NA
    HorizRef2 = NA
    HorizRefType = NA
   }
  #scale
  if(m1>=250000){
    ymax<-roundUp(m1,50000)/1000
    ystep<-50000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    HorizRef1<-HorizRef1/1000
    HorizRef2<-HorizRef2/1000
  } else if (m1>=100000) {
    ymax<-roundUp(m1,25000)/1000
    ystep<-25000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    HorizRef1<-HorizRef1/1000
    HorizRef2<-HorizRef2/1000
  } else if (m1>=50000) {
    ymax<-roundUp(m1,10000)/1000
    ystep<-10000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    HorizRef1<-HorizRef1/1000
    HorizRef2<-HorizRef2/1000
  } else if (m1>=10000) {
    ymax<-roundUp(m1,5000)/1000
    ystep<-5000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    HorizRef1<-HorizRef1/1000
    HorizRef2<-HorizRef2/1000
  } else if(m1>=5000) {
    ymax<-roundUp(m1,1000)/1000
    ystep<-1000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    HorizRef1<-HorizRef1/1000
    HorizRef2<-HorizRef2/1000
  } else if(m1>=1000) {
    ymax<-roundUp(m1,500)/1000
    ystep<-1000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    HorizRef1<-HorizRef1/1000
    HorizRef2<-HorizRef2/1000
  } else {
    ymax<-roundUp(m1,100)
    ystep<-100
  }
  # Now actually plot them (write to file)
  #if(outtype == "tiff") tiff(file = paste(outdir,"\\",Si," ",sname,".tiff",sep=""),height = 9.27, width = 12.76,units='in',res=300) # open graphics device
  if(outtype == "tiff") tiff(file = paste(outdir,"\\",Si," ",sname,".tiff",sep=""),height = 9.27, width = 12.76,units='in',res=300,compression="zip") # open graphics device
  if(outtype == "png") png(file = paste(outdir,"\\",Si," ",sname,".png",sep=""),height = 9.27, width = 12.76,units='in',res=300) # open graphics device

    # plot data and points 
    par(mfrow=c(1,1), mar=c(4,5,0.5,1), oma=c(8,2,1,3),cex=1) #plot region specs
    plot(subGarc$Rate,ydat, #just an empty figure region initially
         pch="",xlab=Xnames[subGarc$RateType[1]],
         ylim=c(0,ymax),
         yaxs="i",xaxs="i",
         ylab=ytitle,xlim=c(0,1),cex.axis=1.4,cex.lab=1.8,
         font.lab=2,xaxt="n",yaxt="n")
    if(!is.na(HorizRefType)) abline(h=HorizRef1,lty=1,lwd=5) #Smsy or LG ref line
    if(!is.na(UmsyRef)) {
     abline(v=UmsyRef,lty=1,lwd=5,col="darkgray") #Umsy ref line
     dashedRefx<-seq(from=-.25,to=UmsyRef,by=0.01) #0.85*Smsy ref part1
    }
    if(!is.na(HorizRefType)) {
     dashedRefy<-rep(HorizRef2,length(dashedRefx)) #0.85*Smsy or 0.85*LG ref part2
     lines(dashedRefx,dashedRefy,lty="dotted",lwd=5,col="darkgray") #Add 0.85*Smsy or 0.85*LG line
    }
    axis(2,seq(0,ymax,ystep),cex.axis=1.4) #pretty y axis
    axis(1,seq(0,1,0.2),xticks,cex.axis=1.4) #pretty x axis
    #now add x,y points, color-type coded for each period
    points(subGarc$Rate[as.character(subGarc$Period)==per1],ydat[as.character(subGarc$Period)==per1],
           pch=21,bg="white",cex=3.2) #1975-84
    points(subGarc$Rate[as.character(subGarc$Period)==per2],ydat[as.character(subGarc$Period)==per2],
           pch=22,bg="chartreuse4",cex=3.2) #1985-1998
    points(subGarc$Rate[as.character(subGarc$Period)==per3],ydat[as.character(subGarc$Period)==per3],
           pch=24,bg="royalblue",cex=3) #1999-2008
    points(subGarc$Rate[as.character(subGarc$Period)==per4],ydat[as.character(subGarc$Period)==per4],
           pch=21,bg="orange",cex=3.2) #2009-2018
    points(subGarc$Rate[as.character(subGarc$Period)==per5],ydat[as.character(subGarc$Period)==per5],
           pch=24,bg="grey70",cex=3.2) #2019-present
    box(lwd=2) #add thick frame to plot region
    
    # make the legend (it's actually pretty complicated to get this right); the whole code chunk
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(4.0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    leg.lab<-c(per1_sub,per2_sub,per3_sub,per4_sub,per5_sub)
    leg.pch<-c(ifelse(is.na(per1_sub),NA,21),ifelse(is.na(per2_sub),NA,22),ifelse(is.na(per3_sub),NA,24),ifelse(is.na(per4_sub),NA,21),ifelse(is.na(per5_sub),NA,24))
    leg.cex<-c(3.2,3.2,3,3.2)
    leg.bg<-c(ifelse(is.na(per1_sub),NA,"white"),ifelse(is.na(per2_sub),NA,"chartreuse4"),ifelse(is.na(per3_sub),NA,"royalblue"),ifelse(is.na(per4_sub),NA,"orange"),ifelse(is.na(per5_sub),NA,"grey70"))
    legend("bottom",legend=leg.lab,pt.bg=leg.bg,pch=leg.pch,bty="n",cex=1.6,ncol=4)
    leg.lab<-c(ifelse(is.na(HorizRefType),NA, HorizRefLabel2),ifelse(is.na(UmsyRef),NA,expression(U[MSY])),ifelse(is.na(HorizRefType),NA,HorizRefLabel1))
    leg.lty<-c(ifelse(is.na(HorizRef2),NA,3),ifelse(is.na(UmsyRef),NA,1),ifelse(is.na(HorizRef1),NA,1))
    leg.col<-c(ifelse(is.na(HorizRef2),NA,"darkgray"),ifelse(is.na(UmsyRef),NA,"darkgray"),ifelse(is.na(HorizRef1),NA,"black"))  
    leg.cex<-c(rep(4,3))#3))

    if(outtype == "pdf") {     
     fig_text_line1 = paste("Figure ",Si,".-",Xnames[subGarc$RateType[1]],", spawning escapement, and threshold reference lines for exploitation rate and spawning escapement by CY", sep="")
     fig_text_line2 = paste("                for the ", subGarc$EISStock[1], " stock of Chinook salmon, 1979-", max(subGarc$Year,na.rm=TRUE),". Cumulative mature-run equivalent exploitation rate calculated from the ", as.character(subGarc$ERIS[1]), " CWT",sep="")
     fig_text_line3 = paste("                exploitation rate indicator stock.",sep="")
     mtext(fig_text_line1, side = 1, adj=0, line=1.1) #work in progress
     mtext(fig_text_line2, side = 1, adj=0, line=2.1) #work in progress
     mtext(fig_text_line3, side = 1, adj=0, line=3.1) #
    }

    if(!is.na(HorizRefType)) {
     #par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(2.3, 17, 0, 0), new = TRUE)
     par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(2.3, 27, 0, 0), new = TRUE)
     plot(2, 2, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
     legend("bottomleft",leg.lab[1],lty=leg.lty[1],col=leg.col[1],bty="n",xjust=0,lwd=leg.cex[1],cex=1.6,ncol=4)
    }
    if(!is.na(UmsyRef)) {
     #par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(2.3, 30, 0, 0), new = TRUE)
     par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(2.3, 45, 0, 0), new = TRUE)
     plot(2, 2, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
     legend(x="bottomleft",leg.lab[2],lty=leg.lty[2],col=leg.col[2],bty="n",xjust=0,lwd=leg.cex[2],cex=1.6,ncol=4)
    }
    if(!is.na(HorizRefType)) {
     #par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(2.3, 38, 0, 0), new = TRUE)
     par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(2.3, 14, 0, 0), new = TRUE)
     plot(2, 2, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
     legend(x="bottomleft",leg.lab[3],lty=leg.lty[3],col=leg.col[3],bty="n",xjust=0,lwd=leg.cex[3],cex=1.6,ncol=4)
    }

    if(outtype == "tiff" || outtype=="png") dev.off() #if tiff then close graphics device FOR EACH loop

} #end loop over all data (for Si in 1:...)
#----------------------------------------------------------
    if(outtype == "pdf") dev.off() #if pdf then close graphics device at the end of ALL loops
#reset to original directory
 setwd(odir)
}

###########################################
# calcMRE
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
calcMRE <- function(HRJ, ESC, fisheryinfotable, stknum, mre_startage="guess", eris_startage="guess", strays="guess") {
  #-------------------------------------------------#
  #Data (extract HRJ data via the Henry block method)
  #-------------------------------------------------#
   #Escapement (note, b/c transformed by BROOD YEAR b/c that's hwo it appears in the Henry block method; however, it could be sped up by doing it by CY...)
    if(strays=="guess") strays="omit" #which is the current default method per the logic of the Gayle-Henry-Larrie spreadsheet
    if(strays=="omit") Escapement = with(ESC, tapply(All_Esc, list(stock, by, age), sum))[stknum,,]
    if(strays=="include") Escapement = Escapement + with(ESC, tapply(CA_Esc, list(stock, by, age), sum))[stknum,,] + with(ESC, tapply(US_Esc, list(stock, by, age), sum))[stknum,,]
   #
    termfisheries=subset(fisheryinfotable, Cohort_Type=="Term")$FisheryNumber
    pretermfisheries=subset(fisheryinfotable, Cohort_Type=="Ocean")$FisheryNumber
    oceannetfisheries=subset(fisheryinfotable, Cohort_Type=="OceanNet")$FisheryNumber
   #Preterminal fishery catch
    PreterminalCat = with(subset(HRJ,fishery%in%pretermfisheries), tapply(NomTot, list(stock, by, age), sum))[stknum,,]
   #Ocean net fishery catch
    OcneannetCat = with(subset(HRJ,fishery%in%oceannetfisheries), tapply(NomTot, list(stock, by, age), sum))[stknum,,]
   #Terminal fishery catch
    TermCat = with(subset(HRJ,fishery%in%termfisheries), tapply(NomTot, list(stock, by, age), sum))[stknum,,]
   #Ocean cohort size (after natural mortality)
    OceanCohort = with(HRJ, tapply(Pop, list(stock, by, age), max))[stknum,,]

  #------------#
  #Computations
  #------------#
   #If eris age guess
    if(eris_startage=="guess") {
     #If not all of age 2 assume a 2-5 (or 2-6) age stock
      if(!all(is.na(Escapement[,as.numeric(colnames(Escapement))==2]))) eris_startage = 2
     #If all of age 2 are na, assume a 3-6 age stock
      if(all(is.na(Escapement[,as.numeric(colnames(Escapement))==2]))) eris_startage = 3
    }
   #If mre age guess
    if(mre_startage=="guess") mre_startage = eris_startage + 1
   #determine end age
    #if all of the age 6 data is NA, then eris startage should be 5
     if(all(is.na(Escapement[,as.numeric(colnames(Escapement))==6])))  eris_endage = 5
    #and if not all of the age 6 data is NA, then eris startage should be 5
     if(!all(is.na(Escapement[,as.numeric(colnames(Escapement))==6]))) eris_endage = 6
    #use eris_startage and eris_endage to compute the number of ages
     eris_ages = eris_startage:eris_endage
     eris_nages = length(eris_ages)
   #Differentiate terminal and non-terminal fish in the ocean net catch (i.e. if it's ERIS start age = 2, it implies that age 4 and older fish are mature fish in the ocean net fishery)
    OcneannetTerm = OcneannetCat
    OcneannetPreterm = OcneannetCat
    OcneannetTerm[,as.numeric(colnames(OcneannetTerm))<(eris_startage+2)] <- 0
    OcneannetPreterm[,as.numeric(colnames(OcneannetTerm))>=(eris_startage+2)] <- 0
    if(!(eris_startage==2 || eris_startage==3)) cat("WARNING: an ERIS start age of:", eris_startage, "is not generally recognized\n")
   #Survival from pre-terminal fisheries
    PreterminalHR = (PreterminalCat+OcneannetPreterm)/OceanCohort
    PreterminalSurv = 1-PreterminalHR
   #Mature terminal run
    MatureTermRun =  TermCat + OcneannetTerm + Escapement
  #Calculate cumulative pre-terminal survival
   #If a 2-5 (or 2-6) age stock
    if(eris_startage==2) CumSurvival = t(apply(PreterminalSurv, 1, cumprod))
   #If a 3-6 age stock
    if(eris_startage==3) CumSurvival = cbind(PreterminalSurv[,1],t(apply(PreterminalSurv[,2:ncol(PreterminalSurv)], 1, cumprod)))
  #Compute potential escapement
   PotentialEscapement = MatureTermRun/CumSurvival
  #Compute cumulative potential escapement
   hold = .convertAgeMatrixMRE(PotentialEscapement)
   CumPotentialEscapement = with(subset(hold, age>=mre_startage), tapply(value, list(cy), sum, na.rm=T))
  #Compute observed escapement
   hold = .convertAgeMatrixMRE(Escapement)
   CumObsEscapement = with(subset(hold, age>=mre_startage), tapply(value, list(cy), sum, na.rm=T))
   #note the following IF escapement is done by CY: CumObsEscapement = rowSums(Escapement[,as.numeric(colnames(Escapement))>=3], na.rm=TRUE)
  #valid mre calc? adds two variables. cy_nagesMRE & valid_mre. count the number of ages 
   validmre_mat = with(hold, tapply(value, list(cy, age), sum))
   cy_nagesMRE = rowSums(!apply(validmre_mat[,as.numeric(colnames(validmre_mat))%in%mre_startage:eris_endage],2,is.na))
   valid_mre = cy_nagesMRE==length(mre_startage:eris_endage)
  #### ADD INC AGE INFO LOGIC HERE #### 
  #Compute MRE
   MRE = 1 - (CumObsEscapement/CumPotentialEscapement)
  #number of ages present in a given brood year
   by_nages = rowSums(!apply(Escapement,2,is.na))
  #number of ages present in a given calednar year
   cy_nages = rowSums(!apply(with(hold, tapply(value, list(cy, age), sum)),2,is.na))
  #Subset cy_nages & valid_mre & cy_nagesMRE
   cy_nages = cy_nages[names(cy_nages)%in%names(MRE)]
   valid_mre = valid_mre[names(valid_mre)%in%names(MRE)]
   cy_nagesMRE = cy_nagesMRE[names(cy_nagesMRE)%in%names(MRE)]
  #Combine the intermediate calcs
   henry_block1 = cbind(PreterminalSurv, MatureTermRun, Escapement, CumSurvival, PotentialEscapement, by_nages)
   eris_ages= colnames(Escapement) #override eris_ages to what's actually present for auto-naming
   colnames(henry_block1) = c(paste("PretermSurv_Age",eris_ages,sep=""), paste("TermRun_Age",eris_ages,sep=""),paste("Escap_Age",eris_ages,sep=""),paste("CumPretermSurv_Age",eris_ages,sep=""),paste("PotEscap_Age",eris_ages,sep=""),"by_nages")
  #Combine the final calcs
   henry_block2 = cbind(CumPotentialEscapement, CumObsEscapement, MRE, cy_nagesMRE, valid_mre)
  #------#
  #Output
  #------#
  #add the output into a list object
   out=list()
   out$IntermediateCalcs = henry_block1
   out$FinalCalcs = henry_block2
   out$Inputs = list(StkNum = stknum, MRECalcStartAge=mre_startage, ERISCalcStartAge=eris_startage, Strays=strays)
  #return output
   return(out)
}

###########################################
# calcMREAll
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
calcMREAll <- function(HRJ, ESC, fisheryinfotable, mre_startage="guess", eris_startage="guess", strays="guess") {
 IntermediateCalcs = list()
 FinalCalcs = list()
 nstks = length(unique(HRJ$stock))
 stks = sort(unique(HRJ$stock))
 for(i in 1:nstks) {
  stkNum = stks[i]
  cat("calc",i,"of",nstks,"for hrj stock number",stkNum,"\n")
  #set defaults for mre and eris start age and what to do with escapement strays
   mre_startage2  = "guess"
   eris_startage2 = "guess"
   strays2 = "guess"
  #if mre_startage is not equal to guess, should be a vector...
   if(class(mre_startage)=="data.frame") {
    mre_startage2 = subset(mre_startage, stknum==stkNum)$MRE_StartAge
    #if length 0 (as in not specified)
    if(length(mre_startage2)==0) mre_startage2="guess"
    if(length(mre_startage2)>1) mre_startage2=mre_startage2[1]
   }
   if(class(eris_startage)=="data.frame") {
    eris_startage2 = subset(eris_startage, stknum==stkNum)$ERIS_StartAge
    #if length 0 (as in not specified)
    if(length(eris_startage2)==0) eris_startage2="guess"
    if(length(eris_startage2)>1) eris_startage2=eris_startage2[1]
   }
   if(class(strays)=="data.frame") {
     strays2 = subset(strays, stknum==stkNum)$Strays
     #if length 0 (as in not specified)
     if(length(strays2)==0) strays2="guess"
     if(length(strays2)>1) strays2=strays2[1]
   }
  #mre calcs
   tmp = calcMRE(HRJ, ESC, fisheryinfotable=fisheryinfotable, stknum=stkNum, mre_startage=mre_startage2, eris_startage=eris_startage2, strays=strays2)
  #manipulate data
   IntermediateCalcs[[i]] = cbind(stock=rep(stkNum, nrow(tmp$IntermediateCalcs)), by=as.numeric(rownames(tmp$IntermediateCalcs)), tmp$IntermediateCalcs)
   FinalCalcs[[i]] = cbind(stock=rep(stkNum, nrow(tmp$FinalCalcs)), cy=as.numeric(rownames(tmp$FinalCalcs)), mrecalcstartage=rep(tmp$Inputs$MRECalcStartAge, nrow(tmp$FinalCalcs)), tmp$FinalCalcs)
 }
 #collapse stock-specific results into a list
  out = list()
  out$IntermediateCalcs = do.call("rbind", IntermediateCalcs)
  out$FinalCalcs = do.call("rbind", FinalCalcs)
 #return output
  return(out)
}

###########################################
# right() & left()
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
#right 
right = function (string, char){
 substr(string,nchar(string)-(char-1),nchar(string))
}
#left 
left = function (string, char){
 substr(string,1,char)
}

###########################################
# pivotMatrix
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
pivotMatrix <- function(x, repeatcolumns, movecolumns) {
 out=list()
 for(i in 1:length(movecolumns)) out[[i]] = cbind(x[,repeatcolumns], ColName=names(x)[movecolumns[i]], Repeat=x[,movecolumns[i]])
 return(do.call("rbind",out))
}

###########################################
# externalHRadjustment
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
externalHRadjustment <- function(x, hrt, hrjstk, type=c("tm","lc"), newstkname=NULL) {
#determine which stock, by stock number of by name
 if(is.numeric(hrjstk)) select = hrjstk
 if(is.character(hrjstk)) select = match(hrjstk, x$stknames)
#if hrj data is not laid out by calendar year print an error message
 if(x$HRJformat!="calendar") cat("ERROR: HRJ data must be sorted by calendar year to apply the external HR adjustment to work correctly\n")
#if the newstkname already exists OR if newstkname isn't specified but and "adj" version already exists, print an error and stop
 if((!is.null(newstkname) && newstkname %in% x$stknames) || (is.null(newstkname) && paste(x$stknames[select],"adj",sep="") %in% x$stknames)) {
  cat("ERROR: newstkname already exists in the HRJ data object\n")
  stop("function externalHRadjustment will stop")
 }
#for each of the three files that need to be adjusted (note they may not all exist)
 unadj_hrj = subset(x$HRJ_P, stock==select)
 unadj_esc = subset(x$ESC_P, stock==select) #note that ESC_BY and ESC_CY are _usually_ the same
#create placeholder object to store the adjusted results
 adj_hrj = unadj_hrj
 adj_esc = unadj_esc
#determine hrt fisheries
 hrtfisheries = as.numeric(substr(names(hrt)[2:ncol(hrt)],2,4))
 nhrtfisheries = length(hrtfisheries)
#terminal area adjustment calculations
 #compute the sum total mortality for the HRT fisheries AND escapement
  #subset hrj data to be the first fishery in the hrt file
   adj_hrj_hrtfisheries = subset(adj_hrj, fishery==hrtfisheries[1])
  #determine the years in both hrt, hrj (fishery), and hrj (escapement) data
   years_in_both = c(1900:2100)[(1900:2100%in%adj_hrj_hrtfisheries$cy)+(1900:2100%in%unadj_esc$cy)+(1900:2100%in%hrt$CY)==3]
  #add first hrt fishery plus escapement
   ETtilda_AEQCat = (adj_hrj_hrtfisheries[adj_hrj_hrtfisheries$cy%in%years_in_both,paste("AEQCat",2:6,sep="")]+adj_esc[adj_esc$cy%in%years_in_both,paste("All_Esc",2:6,sep="")])
   ETtilda_AEQTot = (adj_hrj_hrtfisheries[adj_hrj_hrtfisheries$cy%in%years_in_both,paste("AEQTot",2:6,sep="")]+adj_esc[adj_esc$cy%in%years_in_both,paste("All_Esc",2:6,sep="")])
   ETtilda_NomCat = (adj_hrj_hrtfisheries[adj_hrj_hrtfisheries$cy%in%years_in_both,paste("NomCat",2:6,sep="")]+adj_esc[adj_esc$cy%in%years_in_both,paste("All_Esc",2:6,sep="")])
   ETtilda_NomTot = (adj_hrj_hrtfisheries[adj_hrj_hrtfisheries$cy%in%years_in_both,paste("NomTot",2:6,sep="")]+adj_esc[adj_esc$cy%in%years_in_both,paste("All_Esc",2:6,sep="")])
  #if 2 or more fisheries are present...
   if(nhrtfisheries>1) {
    for(i in 2:nhrtfisheries) {
     #subset hrj data to only the fishery in the hrt file
      adj_hrj_hrtfishery = subset(adj_hrj, fishery==hrtfisheries[i])
     #and add to the running total
      ETtilda_AEQCat = (adj_hrj_hrtfishery[adj_hrj_hrtfisheries$cy%in%years_in_both,paste("AEQCat",2:6,sep="")]+ETtilda_AEQCat)
      ETtilda_AEQTot = (adj_hrj_hrtfishery[adj_hrj_hrtfisheries$cy%in%years_in_both,paste("AEQTot",2:6,sep="")]+ETtilda_AEQTot)
      ETtilda_NomCat = (adj_hrj_hrtfishery[adj_hrj_hrtfisheries$cy%in%years_in_both,paste("NomCat",2:6,sep="")]+ETtilda_NomCat)
      ETtilda_NomTot = (adj_hrj_hrtfishery[adj_hrj_hrtfisheries$cy%in%years_in_both,paste("NomTot",2:6,sep="")]+ETtilda_NomTot) 
    }
   }
 #compute the adjusted escapement
  #noting you must make a decision on whether or not to do the computation in terms of total (or not)
   if(nhrtfisheries>1) { 
     if(type=="tm") Etilda = ETtilda_NomTot*(1-rowSums(hrt[hrt$CY%in%years_in_both,2:(nhrtfisheries+1)]))
     if(type=="lc") Etilda = ETtilda_NomCat*(1-rowSums(hrt[hrt$CY%in%years_in_both,2:(nhrtfisheries+1)]))
   } else {
     if(type=="tm") Etilda = ETtilda_NomTot*(1-hrt[hrt$CY%in%years_in_both,2:(nhrtfisheries+1)])
     if(type=="lc") Etilda = ETtilda_NomCat*(1-hrt[hrt$CY%in%years_in_both,2:(nhrtfisheries+1)])
   }
  #update escapement
   adj_esc[adj_esc$cy%in%years_in_both,paste("All_Esc",2:6,sep="")] = Etilda
 #compute the adjusted fishing mortality
  for(i in 1:nhrtfisheries) {
   #subset hrj data to only the fishery in the hrt file
    adj_hrj_hrtfishery = subset(adj_hrj, fishery==hrtfisheries[i])
   #compute adjusted terminal cat
    Ttilda_AEQCat = ETtilda_AEQCat*(hrt[hrt$CY%in%years_in_both,i+1])
    Ttilda_AEQTot = ETtilda_AEQTot*(hrt[hrt$CY%in%years_in_both,i+1])
    Ttilda_NomCat = ETtilda_NomCat*(hrt[hrt$CY%in%years_in_both,i+1])
    Ttilda_NomTot = ETtilda_NomTot*(hrt[hrt$CY%in%years_in_both,i+1])
   #update
    adj_hrj[adj_hrj$fishery==hrtfisheries[i]&adj_hrj$cy%in%years_in_both, paste("AEQCat",2:6,sep="")] = Ttilda_AEQCat
    adj_hrj[adj_hrj$fishery==hrtfisheries[i]&adj_hrj$cy%in%years_in_both, paste("AEQTot",2:6,sep="")] = Ttilda_AEQTot
    adj_hrj[adj_hrj$fishery==hrtfisheries[i]&adj_hrj$cy%in%years_in_both, paste("NomCat",2:6,sep="")] = Ttilda_NomCat
    adj_hrj[adj_hrj$fishery==hrtfisheries[i]&adj_hrj$cy%in%years_in_both, paste("NomTot",2:6,sep="")] = Ttilda_NomTot
  }
#drop years NOT adjusted (if there are any)
  #this code chunk was commented out in 2020 with the expectation that users will not try to adjust HRJ data with incomplete HRT data.
  #b/c this could be a poor assumption, an improvement to the code would be to flag years that were not adjusted
   #adj_esc = subset(adj_esc, cy%in%years_in_both)
   #adj_hrj = subset(adj_hrj, cy%in%years_in_both)
#assign the new stock a unique stock number
 adj_esc$stock = x$nstocks+1
 adj_hrj$stock = x$nstocks+1
#append results to the correct tables
 x$HRJ_P =  rbind(x$HRJ_P,adj_hrj) #<-NOTE results are ONLY added to the HRJ_P table
 x$ESC_P = rbind(x$ESC_P,adj_esc) #<-NOTE results are ONLY added to the ESC_P table
#update a few more things 
 if(is.null(newstkname)) newstkname = paste(x$stknames[select],"adj",sep="")
 x$nstocks = x$nstocks+1
 x$stknames = c(x$stknames, newstkname)
#return the updated HRJ data to user
 return(x)
}

###########################################
# msfSITadjustment
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
msfSITadjustment <- function(x, hrjstk, type=c("tm","lc"), newstkname=NULL) {

#***in progress***#

#if hrj data is not laid out by calendar year print an error message
 if(x$HRJformat!="calendar") cat("ERROR: HRJ data must be sorted by calendar year to apply the external HR adjustment to work correctly\n")
#determine which stock, by stock number of by name
 if(is.numeric(hrjstk)) select = hrjstk
 if(is.character(hrjstk)) select = match(hrjstk, x$stknames)
#for each of the three files that need to be adjusted (note they may not all exist)
 unadj_hrj = subset(x$HRJ_P, stock==select)
 unadj_esc = subset(x$ESC_P, stock==select) #note, generally the same as ESC_BY and ESC_CY
#create holders for the adjusted results
 adj_hrj = unadj_hrj
 adj_esc = unadj_esc
#determine hrt fisheries
 hrtfisheries = as.numeric(substr(names(hrt)[2:ncol(hrt)],2,4))
 nhrtfisheries = length(hrtfisheries)
#determine the years in both hrt and hrj files
 years_in_both = c(1900:2100)[(1900:2100%in%unadj_esc$cy)+(1900:2100%in%hrt$CY)==2]
#FOR EACH FISHERY IN THE HRT FILE
 for(i in 1:nhrtfisheries) {
  #subset hrj data to only the fishery in the hrt file
   adj_hrj_hrtfishery = subset(adj_hrj, fishery==hrtfisheries[i])
  #compute adjusted terminal cat
   Ttilda_AEQCat = (adj_hrj_hrtfishery[,paste("AEQCat",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(hrt[hrt$CY%in%years_in_both,i+1])
   Ttilda_AEQTot = (adj_hrj_hrtfishery[,paste("AEQTot",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(hrt[hrt$CY%in%years_in_both,i+1])
   Ttilda_NomCat = (adj_hrj_hrtfishery[,paste("NomCat",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(hrt[hrt$CY%in%years_in_both,i+1])
   Ttilda_NomTot = (adj_hrj_hrtfishery[,paste("NomTot",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(hrt[hrt$CY%in%years_in_both,i+1])
  #update
   adj_hrj[adj_hrj$fishery==hrtfisheries[i]&adj_hrj$cy%in%years_in_both, paste("AEQCat",2:6,sep="")] = Ttilda_AEQCat
   adj_hrj[adj_hrj$fishery==hrtfisheries[i]&adj_hrj$cy%in%years_in_both, paste("AEQTot",2:6,sep="")] = Ttilda_AEQTot
   adj_hrj[adj_hrj$fishery==hrtfisheries[i]&adj_hrj$cy%in%years_in_both, paste("NomCat",2:6,sep="")] = Ttilda_NomCat
   adj_hrj[adj_hrj$fishery==hrtfisheries[i]&adj_hrj$cy%in%years_in_both, paste("NomTot",2:6,sep="")] = Ttilda_NomTot
  #compute adjusted escapement, noting that you must make a decision here whether or not to move the IM over to escapement (or not)
   if(type=="tm") Etilda = (adj_hrj_hrtfishery[,paste("NomTot",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(1-hrt[hrt$CY%in%years_in_both,i+1])
   if(type=="lc") Etilda = (adj_hrj_hrtfishery[,paste("NomCat",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(1-hrt[hrt$CY%in%years_in_both,i+1])
  #update
   adj_esc[adj_esc$cy%in%years_in_both,paste("All_Esc",2:6,sep="")] = Etilda
  }
 #update the stock numbering with the new tables
  adj_esc$stock = x$nstocks+1
  adj_hrj$stock = x$nstocks+1
 #append the new reslts to the correc tables
  x$HRJ_P = rbind(x$HRJ_P,adj_hrj)
  x$ESC_P = rbind(x$ESC_P,adj_esc)
 #update a few more things 
  if(is.null(newstkname)) newstkname = paste(x$stknames[select],"adj",sep="")
  x$nstocks = x$nstocks+1
  x$stknames = c(x$stknames, newstkname)
 #return the updated HRJ data to users
  return(x)
}

###########################################
# mdt
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
mdt <- function(stkname, hrjobj, fmap, type=c("AEQCat","AEQTot","NomCat","NomTot"), strays=c("addtoesc","ignore","separate"), ages=2:6, criteria="mdt", shakermethod="P") {
#Stock number is defined by its location in stknames
 stkloc = match(stkname, hrjobj$stknames)
#which HRJ results should be referenced? 
 if(shakermethod=="b" || shakermethod=="B") {
   hrj=subset(hrjobj$HRJ_BY,stock==stkloc)
   esc=subset(hrjobj$ESC_BY,stock==stkloc)
 } else if (shakermethod=="c" || shakermethod=="C") {
   hrj=subset(hrjobj$HRJ_CY,stock==stkloc)
   esc=subset(hrjobj$ESC_CY,stock==stkloc)
 } else if (shakermethod=="p" || shakermethod=="P") {
   hrj=subset(hrjobj$HRJ_P,stock==stkloc)
   esc=subset(hrjobj$ESC_P,stock==stkloc)
 } else {
   cat("shaker method not recognized\n")
 }
#
 if(hrjobj$HRJformat=="calendar") yearformat="cy"
 if(hrjobj$HRJformat=="brood") {
  yearformat="brood"
  #within this function only, re-name the column 'brood' to 'cy'
  names(hrj)[2] = "cy"
  names(esc)[2] = "cy"
 }
#what to do with strays? note that the stray information appears twice (both in the hrj table and the esc table)..
#and so to prevent double counting, this function only references the escapement stray data in the esc. table, so make sure the ESC stray line is 'blanked' out
 if(nrow(fmap[left(fmap$FisheryName,1)=="X",])>0) fmap[left(fmap$FisheryName,1)=="X",]$CMB=NA
 if(is.factor(fmap$CMB)) fmap$CMB=droplevels(fmap$CMB)
 stray_pivot = NULL
#age check
 if(any(ages<2) | any(ages>6)) ages = ages[!c(ages<2 | ages>6)]
 if(length(ages)<1) {
  cat("ERROR: age vector not valid, program will default to using all ages\n\n")
  ages = 2:6
 }
#
 if(strays=="addtoesc") {
   esctype = c(paste("All_Esc",ages,sep=""),paste("CA_Esc",ages,sep=""),paste("US_Esc",ages,sep=""))
 } else if (strays=="ignore") { 
   esctype = paste("All_Esc",ages,sep="")
 } else if (strays=="separate") {
   esctype = paste("All_Esc",ages,sep="")
   straytype = c(paste("CA_Esc",ages,sep=""),paste("US_Esc",ages,sep=""))
   stray_pivot = pivotMatrix(esc, repeatcolumns=match(c("cy","fishery"),names(hrj)), movecolumns=match(straytype,names(esc)))
   names(stray_pivot)[3:4] = c("type","value")
 } else { cat("ERROR: user selected a strays option that is NOT RECOGNIZED\n")  }
#
 esc_pivot = pivotMatrix(esc, repeatcolumns=match(c("cy","fishery"),names(hrj)), movecolumns=match(esctype,names(esc)))
 names(esc_pivot)[3:4] = c("type","value")
#
 hrj_pivot = pivotMatrix(hrj, repeatcolumns=match(c("cy","fishery"),names(hrj)), movecolumns=match(c(paste(type,ages,sep="")),names(hrj)))
 names(hrj_pivot)[3:4] = c("type","value")
#
 hrj_pivot$age = as.numeric(right(as.character(hrj_pivot$type),1))
 esc_pivot$age = as.numeric(right(as.character(esc_pivot$type),1))
#
 hrj_pivot = merge(hrj_pivot, fmap[,c("FisheryNumber","CMB")], by.x="fishery", by.y="FisheryNumber")
#
 cat_byage = with(hrj_pivot, tapply(value, list(cy,age),sum))
 esc_byage = with(esc_pivot, tapply(value, list(cy,age),sum))
#determine which age classes are present
 agespresent = rep(NA, nrow(cat_byage))
 for(i in 1:nrow(cat_byage)) agespresent[i]=paste(colnames(cat_byage)[!is.na(cat_byage[i,])], collapse=",")
#if strays are present and separate accounting is specified
 if(!is.null(stray_pivot)) {
  #note that if strays are present in the hrj_pivot, then the hrj_pivot[!is.na(hrj_pivot$CMB),] needs to be further subsetted to remove said fishery
  numer = with(hrj_pivot[!is.na(hrj_pivot$CMB),], tapply(value, list(cy,CMB),sum,na.rm=TRUE))
  numer = cbind(numer,STRAY=with(stray_pivot[!is.na(stray_pivot$fishery),], tapply(value, list(cy),sum, na.rm=TRUE)),ESCAP=with(esc_pivot[!is.na(esc_pivot$fishery),], tapply(value, list(cy),sum, na.rm=TRUE))) #equivlanet to...numer$ESC = denom-rowSums(numer)
  denom = with(hrj_pivot[!is.na(hrj_pivot$CMB),], tapply(value, list(cy), sum,na.rm=TRUE))+with(esc_pivot[!is.na(esc_pivot$fishery),], tapply(value, list(cy),sum, na.rm=TRUE))+with(stray_pivot[!is.na(stray_pivot$fishery),], tapply(value, list(cy),sum, na.rm=TRUE))
#else
 } else {
  #
   hrj_pivot[hrj_pivot[,"fishery"]%in%fmap[left(fmap$FisheryName,1)=="X",]$FisheryNumber,]$value = 0
  #
  numer = with(hrj_pivot, tapply(value, list(cy,CMB), sum,na.rm=TRUE))
  numer = cbind(numer,Esc=with(esc_pivot, tapply(value, list(cy),sum, na.rm=TRUE))) #equivlanet to...numer$ESC = denom-rowSums(numer)
  denom = with(hrj_pivot, tapply(value, list(cy),sum,na.rm=TRUE))+with(esc_pivot, tapply(value, list(cy),sum, na.rm=TRUE))
 }
#
 morttab = matrix(NA,ncol=ncol(numer),nrow=nrow(numer))
 colnames(morttab) = colnames(numer)
 rownames(morttab) = rownames(numer)
 for(i in 1:ncol(numer)) morttab[,i] = numer[,i]/denom 
#
 criteriaout=rep(NA,length(as.numeric(row.names(morttab))))
 if(is.null(criteria)) {
  criteriaout = ifelse(nchar(agespresent)>0, "ok", "omit") #if no criteria is specified include all rows
 }
 if(!is.null(criteria)) {
  if(criteria=="mdt") {
    #Note: the criteria is always calculated using NomCat AND includes escapement strays!
    esctype_criteria = paste("All_Esc",ages,sep="")
    straytype_criteria  = c(paste("CA_Esc",ages,sep=""),paste("US_Esc",ages,sep=""))
    stray_pivot_criteria  = pivotMatrix(esc, repeatcolumns=match(c("cy","fishery"),names(hrj)), movecolumns=match(straytype_criteria ,names(esc)))
    names(stray_pivot_criteria )[3:4] = c("type","value")
    esc_pivot_criteria = pivotMatrix(esc, repeatcolumns=match(c("cy","fishery"),names(hrj)), movecolumns=match(esctype_criteria,names(esc)))
    names(esc_pivot_criteria)[3:4] = c("type","value")
    hrj_pivot_criteria = pivotMatrix(hrj, repeatcolumns=match(c("cy","fishery"),names(hrj)), movecolumns=match(c(paste("NomCat",ages,sep="")),names(hrj)))
    names(hrj_pivot_criteria)[3:4] = c("type","value")
    hrj_pivot_criteria$age = as.numeric(right(as.character(hrj_pivot_criteria$type),1))
    esc_pivot_criteria$age = as.numeric(right(as.character(esc_pivot_criteria$type),1))
    hrj_pivot_criteria = merge(hrj_pivot_criteria, fmap[,c("FisheryNumber","CMB")], by.x="fishery", by.y="FisheryNumber")
    denom_criteria = with(hrj_pivot_criteria[!is.na(hrj_pivot_criteria$CMB),], tapply(value, list(cy),sum,na.rm=TRUE))+with(esc_pivot_criteria[!is.na(esc_pivot_criteria$fishery),], tapply(value, list(cy),sum, na.rm=TRUE))+with(stray_pivot_criteria[!is.na(stray_pivot_criteria$fishery),], tapply(value, list(cy),sum, na.rm=TRUE))
   #apply criteria in the following sequence:
    #note the following use of nchar and definitions
     #-if nchar()=7, there are 4 ages present
     #-if nchar()=5, there are 3 ages present
     #-if nchar()=3, there are 2 ages present
     #-if nchar()=1, there is only 1 age present
    criteriaout[nchar(agespresent)==3] = "shade" #always shade when 2 age classes are present
    criteriaout[denom_criteria<=105] = "shade" #always shade if the number of recoveries is 105 or less (same as MDT program)
    criteriaout[nchar(agespresent)<=1] = "omit" #always print 'failed criteria' when only 1 age class is present
   #and only set criteria equal to ok if it meets the criteria: at least 3 ages and greater than 105 CWT recoveries
    criteriaout[denom_criteria>105 & nchar(agespresent)>3] = "ok"
  } #end criteria='mdt'
 } #end if criteria is not null
#
 out = list(StockName = stkname, Years = as.numeric(row.names(morttab)), Mortality = numer, PercentMortality = morttab, Recoveries = denom, AgesPresent = agespresent, Criteria = criteriaout, CriteriaType = criteria, YearFormat = yearformat, ShakerMethod="P Method")
 class(out) = "MDT"
 return(out)
}

###########################################
# print.MDT
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
print.MDT <- function(x, digits=1, type="percent", prop=FALSE, yearstoshow=1979:(thisyr-1)) {
 #select type of results - percent or number and format the data
  morttoshow = NA
  if(type=="percent") { 
   morttoshow = x$PercentMortality
   if(!prop) morttoshow = morttoshow * 100
  } else if (type=="number") {
   morttoshow = x$Mortality
  } else { cat("DNE\n") } 
  if(!is.null(digits)) morttoshow= round(morttoshow, digits)
  morttoshow=ifelse(is.nan(morttoshow), NA, morttoshow)
 #find the blank AgesPresent and replace those values with NA
  wherearetheblanks = x$AgesPresent==""
  morttoshow[wherearetheblanks,] = NA
  x$Recoveries[wherearetheblanks] = NA
 #
  tmp = data.frame(Year=x$Years, Recoveries=x$Recoveries, Ages=x$AgesPresent, morttoshow, criteria=x$Criteria)
 #if yearstoshow specified (i.e. not null), coerce data to that format
  if(!is.null(yearstoshow)) {
   #which years aren't present?
    yearsnotpresent = yearstoshow[(!yearstoshow %in% x$Years)]
   #create a temporary DF for the years not present in existing results
    tmp2 = as.data.frame(matrix(NA,nrow=length(yearsnotpresent),ncol=ncol(tmp)))
    colnames(tmp2) = colnames(tmp)
    #if all years are present then the dimension of the DF is ncol x 0 rows and it'll result in an error... ergo the following
    if(length(yearsnotpresent)>0) {
     tmp2$Year = yearsnotpresent
     tmp2$criteria = "omit"
    }
   #combine tmp and sort
    tmp = rbind(tmp, tmp2)
    tmp = tmp[order(tmp$Year),]
   #finally subset the data to only the years specified
    tmp = tmp[tmp$Year %in% yearstoshow,]
  }
 #rename column 1 for by vs cy layout
  names(tmp)[1] = ifelse(x$YearFormat=="cy","Catch Year","Brood Year")
 #print results
  print(tmp,row.names=FALSE)
}

###########################################
# summary.MDT
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
summary.MDT <- function(x, yearranges=list(1979:1984,1985:1995,1996:1998,1999:2008,2009:2018,2019:2028)) {
#
 yearlabs = rep(NA,length(yearranges))
 for(i in 1:length(yearranges)) yearlabs[i] = paste(right(min(yearranges[[i]]),2), right(max(yearranges[[i]]),2), sep="-")
#
 mort_summary = matrix(NA, nrow=length(yearranges), ncol=ncol(x$Mortality))
 colnames(mort_summary) = colnames(x$Mortality)
 perc_summary = matrix(NA, nrow=length(yearranges), ncol=ncol(x$PercentMortality))
 colnames(perc_summary) = colnames(x$PercentMortality)
 recov_summary = rep(NA,length(yearranges))
#
 for(i in 1:length(yearranges)) {
  whichyears = (x$Years %in% yearranges[[i]]) #select rows corresponding to which years
  whichyears = ifelse(x$Criteria=="ok", whichyears, FALSE) #remove selected row that don't match the criteria
  #the R function colMeans doesn't work with only 1 or zero rows, so..
   if(sum(whichyears)>1) {
    mort_summary[i,] = colMeans(x$Mortality[whichyears,],na.rm=TRUE)
    perc_summary[i,] = colMeans(x$PercentMortality[whichyears,],na.rm=TRUE)
    #recov_summary[i] = sum(x$Recoveries[whichyears],na.rm=TRUE)
    recov_summary[i] = round(mean(x$Recoveries[whichyears],na.rm=TRUE),0)
   } else if (sum(whichyears)==1) { 
    mort_summary[i,] = x$Mortality[whichyears,]
    perc_summary[i,] = x$PercentMortality[whichyears,]
    #recov_summary[i] = x$Recoveries[whichyears]
    recov_summary[i] = round(x$Recoveries[whichyears],0) 
   } else { 
    mort_summary[i,] = NA
    perc_summary[i,] = NA
    recov_summary[i] = NA
   }
  #end cases 
 }
#
 out <- list(StockName = x$StockName, YearLabs=yearlabs, AvgMortality=mort_summary, AvgPercentMortality=perc_summary, SumRecoveries=recov_summary)
 class(out) = "summary.MDT"
 return(out)
}

###########################################
# print.summary.MDT
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
print.summary.MDT <- function(x, digits=1, type="percent", prop=FALSE) {
 if(type=="percent") { 
  morttoshow = x$AvgPercentMortality
 if(!prop) morttoshow = morttoshow * 100
 } else if (type=="number") {
  morttoshow = x$AvgMortality
 } else { cat("DNE\n") } 
 if(!is.null(digits)) morttoshow= round(morttoshow, digits)
 morttoshow=ifelse(is.nan(morttoshow), NA, morttoshow)
 tmp = data.frame(Years=x$YearLabs, Recoveries=x$SumRecoveries, Ages=rep("",length(x$SumRecoveries)), morttoshow)
 print(tmp,row.names=FALSE)
}

###########################################
# MRE2Plot
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
MRE2Plot <- function(esc, mre, smap, stknames, mrecriteria, auxdata=NULL) {
 #
  Escap2 = pivotMatrix(esc, 1:2, 3:ncol(esc))
  names(Escap2)[3:4] = c("EISStock","Escapement")
 #Subset the escapement workbook to only the garcia stocks
  Escap3 = subset(Escap2, EISStock%in%unique(smap$EIS_StockName))
 #Append slookups to Escapement data frame
  Escap4 = merge(x=Escap3, y=smap, by.x="EISStock",by.y="EIS_StockName")
 #Append the mre calcs to the Escapement data frame
  mre_finalcalcs = as.data.frame(mre$FinalCalcs)
  hrjstknames = data.frame(HRJ_Number=1:length(stknames),ERIS_Name=stknames)
  mre_finalcalcs = merge(mre_finalcalcs, hrjstknames, by.x="stock", by.y="HRJ_Number")
  mre2 = mre_finalcalcs[,c("cy","MRE","ERIS_Name","valid_mre","cy_nagesMRE")]
 #IF NAN, replace with NA
  mre2$MRE=ifelse(is.nan(mre2$MRE),NA,mre2$MRE)
 #IF valid_mre is not equal to TRUE, replace with NA
  if(mrecriteria) {
  #criteria 1 - remove MRE calcs that have ANY missing data (usually 3 years of returns)
   #mre2$MRE=ifelse(mre2$valid_mre!=TRUE,NA,mre2$MRE)
  #criteria 2 - remove MRE calcs that have n ages worth of data (current criteria is set to remove calcs based on only a single age of return data)
   mre2$MRE=ifelse(mre2$cy_nagesMRE<1,NA,mre2$MRE)
  }

 #Append AUX DATA
  if(!is.null(auxdata)) mre2 = rbind(mre2,.convertAuxMREMatrix(auxdata))

 #SUBSET mre data to only the year's of interest (i.e. year's with escapement data!)
  mre2 = subset(mre2, cy%in%unique(Escap4$Year))
  mre3 = list()
  for(i in 1:nrow(smap)) {
   escaptemp=subset(Escap4, StockNum==i)
   mretemp  =subset(mre2, ERIS_Name==as.character(escaptemp$ERIS[1]))
   #re-order the columns in mretemp to match escaptemp
   mretemp  =mretemp[match(escaptemp$Year, mretemp$cy),]
   #occasionally there's less mre data than escapement data, so override the 'cy' column from the mre with the ESC 'Year' column
   mretemp$cy = escaptemp$Year
   #dunno if the following check is needed now given the line of code above: meh
   if(!all(escaptemp$Year==mretemp$cy)) cat("WARNING!: year mismatch between MRE and Escapement data for stock:", i, "\n")
   mre3[[i]] = cbind(escaptemp, Rate=mretemp$MRE)
  }
  mre4 = do.call("rbind", mre3)
#
 return(mre4)
}

###########################################
# MRE2Excel
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
MRE2Excel <- function(x, stknames, filename="mre calcs.xlsx") {
 require(openxlsx)
 colnames(MRE$IntermediateCalcs)
 colnames(MRE$FinalCalcs)
 nstks = length(stknames)
 wb <- createWorkbook("MRECalcs")
 #add the indiv stock sheets
 for(i in 1:nstks) {
  addWorksheet(wb=wb, sheetName=stknames[i])
  intercalcs = MRE$IntermediateCalcs[MRE$IntermediateCalcs[,1]==i,]
  finalcalcs = MRE$FinalCalcs[MRE$FinalCalcs[,1]==i,]
  out2Excel = list(Intermediate_Calculation=intercalcs, MRE_Calculation=finalcalcs)
  writeData(wb=wb, sheet=stknames[i], x=out2Excel[[1]], startCol=1 , borders="surrounding")
  writeData(wb=wb, sheet=stknames[i], x=out2Excel[[2]], startCol=30, borders="surrounding")
 }
 #add the all stock sheet
  addWorksheet(wb=wb, sheetName="all stocks")
  writeData(wb=wb, sheet="all stocks", x=MRE$IntermediateCalcs, startCol=1 , borders="surrounding")
  writeData(wb=wb, sheet="all stocks", x=MRE$FinalCalcs, startCol=30, borders="surrounding")
  writeData(wb=wb, sheet="all stocks", x=data.frame(stock=1:length(stknames),stknames=stknames), startCol=39, borders="surrounding")
 #save workbook
 saveWorkbook(wb, filename, overwrite = TRUE)
 #returns nothing
}

###########################################
# plotGarcia
#
# Description
# ------------------
# Adapted plot all stock code by Pete McHugh
# Function currently not maintained as plotGarciaAll is used; however, the long term goal is to switch to a 
# stock-to-stock plotting function instead of the 'loop' style
# 
#
# Argument(s)
# ------------------
# garcia
# outdir
# outtype - either "pdf" or "tff"
# Output(s)
# ------------------
# 
###########################################
plotGarcia <- function(Garcia) {

# set some general specs relevant to all plots ##
# Periods for coloring points
per1=as.character(levels(Garcia$Period)[1])
per2=as.character(levels(Garcia$Period)[2])
per3=as.character(levels(Garcia$Period)[3])
per4=as.character(levels(Garcia$Period)[4])
# The options for x-axis labeling (MRE ER vs. CY ER) and values to display
Xnames<-c("Mature-run equivalent exploitation rate (%)","Calendar year exploitation rate (%)")
xticks<-c("0%","20%","40%","60%","80%","100%")

# Now subset data, set specs, and make figures
  subGarc=Garcia

  # Set stock(plot)-specific specs (i.e., the plotting 
  # range and y tick width ); also, do data manipulations
  # idea here is to make figure scale/display vary for
  # optimal display across a wide range of escapements
  m1<-max(subGarc$Escapement,na.rm=TRUE)/0.95
  yt1<-c("Spawning escapement (thousands)","Spawning escapement") # use different title, if /1K vs. not
  ytitle<-yt1[2] #raw values as default, /1K otherwise 
  ydat<-subGarc$Escapement #raw values as default, /1K otherwise
  sname<-as.character(subGarc$EISStock[1]) #stock (from old excel worksheet name) for fig naming
  fname<-paste(as.character(subGarc$Stock[1]),".tif",sep="")
  SmsyRef<-subGarc$Smsy[1]
  S85Ref<-subGarc$S85[1]
  if(m1>=250000){
    ymax<-roundUp(m1,50000)/1000
    ystep<-50000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    SmsyRef<-subGarc$Smsy[1]/1000
    S85Ref<-subGarc$S85[1]/1000
  } else if (m1>=100000) {
    ymax<-roundUp(m1,25000)/1000
    ystep<-25000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    SmsyRef<-subGarc$Smsy[1]/1000
    S85Ref<-subGarc$S85[1]/1000
  } else if (m1>=50000) {
    ymax<-roundUp(m1,10000)/1000
    ystep<-10000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    SmsyRef<-subGarc$Smsy[1]/1000
    S85Ref<-subGarc$S85[1]/1000
  } else if (m1>=10000) {
    ymax<-roundUp(m1,5000)/1000
    ystep<-5000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    SmsyRef<-subGarc$Smsy[1]/1000
    S85Ref<-subGarc$S85[1]/1000
  } else if(m1>=5000) {
    ymax<-roundUp(m1,1000)
    ystep<-1000
  } else if(m1>=1000) {
    ymax<-roundUp(m1,500)
    ystep<-500
  } else {
    ymax<-roundUp(m1,100)
    ystep<-100
  }
  # Now actually plot them (write to file)
    # plot data and points 
    par(mfrow=c(1,1), mar=c(4,5,0.5,1), oma=c(8,2,1,3),cex=1) #plot region specs
    plot(subGarc$Rate,ydat, #just an empty figure region initially
         pch="",xlab=Xnames[subGarc$RateType[1]],
         ylim=c(0,ymax),
         yaxs="i",xaxs="i",
         ylab=ytitle,xlim=c(0,1),cex.axis=1.4,cex.lab=1.8,
         font.lab=2,xaxt="n",yaxt="n")
    if(!is.na(SmsyRef)) abline(h=SmsyRef,lty=1,lwd=5) #Smsy ref line
    if(!is.na(subGarc$Umsy[1])) { 
     abline(v=subGarc$Umsy[1],lty=1,lwd=5,col="darkgray") #Umsy ref line
     dashedRefx<-seq(from=-.25,to=subGarc$Umsy[1],by=0.01) #0.85*Smsy ref part1
    }
    if(!is.na(S85Ref)) { 
     dashedRefy<-rep(S85Ref,length(dashedRefx)) #0.85*Smsy ref part2
     lines(dashedRefx,dashedRefy,lty="dotted",lwd=5,col="darkgray") #Add 0.85*Smsy line
    }
    axis(2,seq(0,ymax,ystep),cex.axis=1.4) #pretty y axis
    axis(1,seq(0,1,0.2),xticks,cex.axis=1.4) #pretty x axis
    #now add x,y points, color-type coded for each period
    points(subGarc$Rate[as.character(subGarc$Period)==per1],ydat[as.character(subGarc$Period)==per1],
           pch=21,bg="white",cex=3.2) #1975-84
    points(subGarc$Rate[as.character(subGarc$Period)==per2],ydat[as.character(subGarc$Period)==per2],
           pch=22,bg="chartreuse4",cex=3.2) #1985-1998
    points(subGarc$Rate[as.character(subGarc$Period)==per3],ydat[as.character(subGarc$Period)==per3],
           pch=24,bg="royalblue",cex=3) #1999-2008
    points(subGarc$Rate[as.character(subGarc$Period)==per4],ydat[as.character(subGarc$Period)==per4],
           pch=21,bg="orange",cex=3.2) #2009-present
    box(lwd=2) #add thick frame to plot region
    
    # make the legend (it's actually pretty complicated to get this right); the whole code chunk
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(5, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    leg.lab<-c(per1,per2,per3,per4)
    leg.pch<-c(21,22,24,21)
    leg.cex<-c(3.2,3.2,3,3.2)
    leg.bg<-c("white","chartreuse4","royalblue","orange")
    legend(x="bottom",leg.lab,pt.bg=leg.bg,pch=leg.pch,bty="n",cex=1.4,ncol=4)
    leg.lab<-c("S (0.85 Smsy)","Umsy","Smsy")
    leg.lty<-c(3,1,1)
    leg.col<-c("darkgray","darkgray","black")
    leg.cex<-c(rep(4,3))#3))

    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(3.5, 17, 0, 0), new = TRUE)
    plot(2, 2, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    legend("bottom",leg.lab[1],lty=leg.lty[1],col=leg.col[1],bty="n",xjust=0,lwd=leg.cex[1],cex=1.25,ncol=4)
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(3.5, 22, 0, 0), new = TRUE)
    plot(2, 2, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    legend(x="bottom",leg.lab[2],lty=leg.lty[2],col=leg.col[2],bty="n",xjust=0,lwd=leg.cex[2],cex=1.25,ncol=4)
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(3.5, 37, 0, 0), new = TRUE)
    plot(2, 2, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    legend(x="bottom",leg.lab[3],lty=leg.lty[3],col=leg.col[3],bty="n",xjust=0,lwd=leg.cex[3],cex=1.25,ncol=4)

}

###########################################
# .convertAuxMREMatrix
#
# Description
# ------------------
# converts aux data into a format that can be appended to the mre final calc data frame
# 
#
# Argument(s)
# ------------------
# x - input matrix of auxiliary mre calcs in the layout of
#     * column 1 year, 
#     * column 2:n mre calcs by stock, 
#     * column name is numeric, corresponding to the model fishery number
#
# Output(s)
# ------------------
# out - a dataframe of the aux catch data in the layout used elsewhere
#
###########################################
.convertAuxMREMatrix <- function(x) {
	#note that the format of aux mre matrix is year, stock, and mre
	snames = names(x)[-1] #fishery names reformatted - R adds an "X" to numeric variables
	out = data.frame(cy = rep(matrix(as.matrix(x[,1])),ncol(x)-1),
	                 MRE = matrix(as.matrix(x[,2:ncol(x)])),
	                 ERIS_Name = matrix(sapply(snames, rep, nrow(x))))
      out$valid_mre   = NA
      out$cy_nagesMRE = NA
	return(out)
}

###########################################
# plotSynopticSynoptic
#
# Description
# ------------------
# plots the synoptic synoptic plot
# 
#
# Argument(s)
# ------------------
# garcia
# outdir
# outtype - either "pdf" or "tff" or null
# Output(s)
# ------------------
# 
###########################################
plotSynopticSynoptic <- function(garcia, year, outtype=NA, verbose=FALSE, filename=NA, addeistext=FALSE) {
#Subset garcia data
 subGarc = subset(garcia, Year==year)
#drop levels
# set some general specs relevant to all plots ##
# Periods for coloring points
 per1=as.character(levels(subGarc$EIS_Region)[1]) #ak
 per2=as.character(levels(subGarc$EIS_Region)[2]) #canada
 per3=as.character(levels(subGarc$EIS_Region)[3]) #col r
 per4=as.character(levels(subGarc$EIS_Region)[4]) #tbr
 per5=as.character(levels(subGarc$EIS_Region)[5]) #wa/or

#Compute the indices
 subGarc$EscIndex = with(subGarc, ifelse(is.na(LowerGoal),Escapement/Smsy, Escapement/LowerGoal))
 subGarc$MREIndex = with(subGarc, Rate/Umsy)
# Set stock(plot)-specific specs (i.e., the plotting 
# range and y tick width ); also, do data manipulations
# idea here is to make figure scale/display vary for
# optimal display across a wide range of escapements
  ymax<-max(subGarc$EscIndex,na.rm=TRUE)/0.95
  ytitle<-expression(Escapement~to~S[MSY]~Index)
  ydat<-subGarc$EscIndex #raw values as default, /1K otherwise
  ystep = 7
  SmsyRef<-1
  S85Ref<-.85
# The options for x-axis labeling (MRE ER vs. CY ER) and values to display
 xmax=max(1,max(subGarc$MREIndex,na.rm=TRUE)/.95)
 Xnames<-expression(ER~to~U[MSY]~Index)
 xticks<-paste(seq(0,xmax,by=.2)*100,"%",sep="")

 #Now actually plot them (write to file)
  if(is.na(filename)) filename=paste(year," synoptic summary",sep="")
  if(outtype == "pdf") pdf(file = paste(filename,".pdf",sep=""),height = 9.27, width = 12.76) # open graphics device
  if(outtype == "tiff") tiff(file = paste(filename,".tiff",sep=""),height = 9.27, width = 12.76,units='in',res=300,compression="zip") # open graphics device
 #Plot data and points 
    par(mfrow=c(1,1), mar=c(4,5,0.5,1), oma=c(8,2,1,3),cex=1) #plot region specs
    plot(subGarc$MREIndex,ydat, #just an empty figure region initially
         pch="",xlab=Xnames,
         ylim=c(0,ymax),
         yaxs="i",xaxs="i",
         ylab=ytitle,xlim=c(0,xmax),cex.axis=1.4,cex.lab=1.8,
         font.lab=2,xaxt="n",yaxt="n")
    abline(h=SmsyRef,lty=1,lwd=5) #Smsy ref line
    abline(v=1,lty=1,lwd=5,col="darkgray") #Umsy ref line
    dashedRefx<-seq(from=-.25,to=1,by=0.01) #0.85*Smsy ref part1
    dashedRefy<-rep(S85Ref,length(dashedRefx)) #0.85*Smsy ref part2
    lines(dashedRefx,dashedRefy,lty="dotted",lwd=5,col="darkgray") #Add 0.85*Smsy line
    axis(2,cex.axis=1.4) #pretty y axis
    axis(1,seq(0,xmax,0.2),xticks,cex.axis=1.4) #pretty x axis
    #now add x,y points, color-type coded for each period
    points(subGarc$MREIndex[as.character(subGarc$EIS_Region)==per1],ydat[as.character(subGarc$EIS_Region)==per1],
           pch=21,bg="white",cex=3.2) #Alaska
    points(subGarc$MREIndex[as.character(subGarc$EIS_Region)==per2],ydat[as.character(subGarc$EIS_Region)==per2],
           pch=22,bg="chartreuse4",cex=3.2) #Canada
    points(subGarc$MREIndex[as.character(subGarc$EIS_Region)==per3],ydat[as.character(subGarc$EIS_Region)==per3],
           pch=24,bg="royalblue",cex=3) #Columbia
    points(subGarc$MREIndex[as.character(subGarc$EIS_Region)==per4],ydat[as.character(subGarc$EIS_Region)==per4],
           pch=8,col="orange",cex=3.2,lwd=2) #TBR
    points(subGarc$MREIndex[as.character(subGarc$EIS_Region)==per5],ydat[as.character(subGarc$EIS_Region)==per5],
           pch=23,bg="red",cex=3.2) #2009-present
    box(lwd=2) #add thick frame to plot region
    #add text for each stock name if asked
    if(addeistext) text(subGarc$MREIndex, ydat, subGarc$EISStock)
    # make the legend (it's actually pretty complicated to get this right); the whole code chunk
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(4, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab="", ylab="")
    leg.lab<-c(per1,per2,per3,per4,per5)
    leg.pch<-c(21,22,24,8,23)
    leg.cex<-c(3.2,3.2,3,3.2,3.2)
    leg.bg<-c("white","chartreuse4","royalblue","orange","red")
    leg.col<-c("black","black","black","orange","black")
    leg.lwd=c(1,1,1,2,1)
    legend(x="bottom",leg.lab,pt.bg=leg.bg,pch=leg.pch,col=leg.col,bty="n",cex=1.5,horiz=TRUE)

   #
    leg.lab<-c(expression(S[MSY]),expression(S(0.85~S[MSY])),expression(U[MSY]))
    leg.lty<-c(1,3,1)
    leg.col<-c("black","darkgray","darkgray")
    leg.cex<-c(rep(4,3))#3))
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1.5, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    legend(x="bottom",leg.lab,lty=leg.lty,col=leg.col,lwd=leg.cex,xjust=0,bty="n",cex=1.5,horiz=TRUE, text.width=c(0.20,0.25,.5))
   #
    if(outtype=="pdf" || outtype=="tiff") dev.off() #if tiff or pdf then close graphics device FOR EACH loop
    if(verbose) return(subGarc)
}

###########################################
# MDT2Excel
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
MDT2Excel <- function(hrj, smap=NULL, fmap, filename="mdt tables.xlsx") {
 require(openxlsx)
 #if user specified stock names...
  if(is.null(smap)) {
   stknames = hrj$stknames
   stk_tablename = stknames
   stk_tam = rep("",length(stknames))
  } else {
   stknames = smap$Stock
   stk_tablename = smap$StockName
   stk_tam = smap$TAM
  }
 #number of stocks
  nstks = length(stknames)
 #else... use user specified stock list... code not implemented yet!
 wb <- createWorkbook("MDTCalcs")
 #add the indiv stock sheets
 ii=1
 for(i in 1:(nstks*2)) {
  wsname = paste(stknames[ii],ifelse(i%%2==0,"total mort", "reported catch"))
  if(stk_tam[ii] == "") {
    tablename = paste("Table C._. Percent distribution of", stk_tablename[ii], "AEQ", ifelse(i%%2==0,"total fishing mortalities", "reported catch"), "and escapement")
  } else {
    tablename = paste("Table C._. Percent distribution of", stk_tablename[ii], "AEQ", ifelse(i%%2==0,"total fishing mortalities", "reported catch"), "and escapement based on", stk_tam[ii], "CWT recoveries with terminal adjustments for basin-specific terminal fishery performance")
  }
  addWorksheet(wb=wb, sheetName=wsname)
  tempcalcs = mdt(stkname=stknames[ii], hrjobj=hrj, fmap=fmap, type=ifelse(i%%2==0,"AEQTot", "AEQCat"), strays="separate")
  out2Excel = list(Table_Name=tablename, Annual_Calcs=print.MDT(tempcalcs), Summary_Calcs=print.summary.MDT(summary.MDT(tempcalcs)))
  writeData(wb=wb, sheet=wsname, x=out2Excel[[1]], startRow=1)
  writeData(wb=wb, sheet=wsname, x=out2Excel[[2]], startRow=3, borders="surrounding")
  writeData(wb=wb, sheet=wsname, x=out2Excel[[3]], startRow=nrow(out2Excel[[2]]) + 1 + 3, borders="surrounding", colNames=FALSE)
  ii = ii + ifelse(i%%2==0,1,0)
 }
 #add the all stock sheet
  #addWorksheet(wb=wb, sheetName="all stocks")
  #writeData(wb=wb, sheet="all stocks", x=MRE$IntermediateCalcs, startCol=1 , borders="surrounding")
  #writeData(wb=wb, sheet="all stocks", x=MRE$FinalCalcs, startCol=30, borders="surrounding")
  #writeData(wb=wb, sheet="all stocks", x=data.frame(stock=1:length(stknames),stknames=stknames), startCol=39, borders="surrounding")
 #save workbook
 saveWorkbook(wb, filename, overwrite = TRUE)
 #returns nothing
}

###########################################
# MDT2CMZ
#
# Description
# ------------------
# MDT data in CMZ format, modified 9.27.22 to return a dataframe and only write to csv if 'filename' agrument is provided
#
# Argument(s)
# ------------------
# hrj
# fmap
# stknames
# filename -> this argument is optional, if provided will write to csv
# 
# Output(s)
# ------------------
# 
###########################################
MDT2CMZ <- function(hrj, fmap, stknames=NULL, filename="catchDistribution_CMZ.csv") {
 #list obj to store output
  out = list()
 #if user specified stock names...
  if(is.null(stknames)) {
   stknames = hrj$stknames
  }
 #number of stocks
  nstks = length(stknames)
 #For each stock...
  for(i in 1:length(stknames)) {
   #Calculate LC and TM
    LC = print.MDT(mdt(stkname=stknames[i], hrjobj=hrj, fmap=fmap, type="AEQCat", strays="separate"),digits=6)
    TM = print.MDT(mdt(stkname=stknames[i], hrjobj=hrj, fmap=fmap, type="AEQTot", strays="separate"),digits=6)
   #Add a few variables to these DF
    LC$stock = TM$stock = stknames[i]
    LC$MortType = "LC"
    TM$MortType = "TM"
   #Re-order and select columns
    whichcols = c((ncol(LC)-1):ncol(LC),1:(ncol(LC)-2)) #this assumes that columns 2 and 3 are Recoveries and Ages, which don't appear in the CMZ file
    LC = LC[,whichcols]
    TM = TM[,whichcols]
   #remove any data from rows equal "omit"
    LC[LC$criteria=="omit",c(4:(ncol(LC)-1))] = NA
    TM[TM$criteria=="omit",c(4:(ncol(TM)-1))] = NA
   #Store output
    out[[i]] = rbind(LC,TM)
  }
 #collapse the list object into a single DF
  OUT = do.call("rbind",out)
 #return dataframe and save to csv if 'filename' provided
  if(missing(filename) == TRUE) {
    return(OUT)
  } else {
    write.csv(OUT, file = filename, row.names=FALSE)
    return(OUT)
  }
}

# NUMERIC VERSION:
###########################################
MDT2CMZnum <- function(hrj, fmap, stknames=NULL, filename="catchDistribution_CMZ.csv") {
  #list obj to store output
  out = list()
  #if user specified stock names...
  if(is.null(stknames)) {
    stknames = hrj$stknames
  }
  #number of stocks
  nstks = length(stknames)
  #For each stock...
  for(i in 1:length(stknames)) {
    #Calculate LC and TM
    LC = print.MDT(mdt(stkname=stknames[i], hrjobj=hrj, fmap=fmap, type="AEQCat", strays="separate"),digits=6, type = 'number')
    TM = print.MDT(mdt(stkname=stknames[i], hrjobj=hrj, fmap=fmap, type="AEQTot", strays="separate"),digits=6, type = 'number')
    #Add a few variables to these DF
    LC$stock = TM$stock = stknames[i]
    LC$MortType = "LC"
    TM$MortType = "TM"
    #Re-order and select columns
    whichcols = c((ncol(LC)-1):ncol(LC),1,4:(ncol(LC)-2)) #this assumes that columns 2 and 3 are Recoveries and Ages, which don't appear in the CMZ file
    LC = LC[,whichcols]
    TM = TM[,whichcols]
    #remove any data from rows equal "omit"
    LC[LC$criteria=="omit",c(4:(ncol(LC)-1))] = NA
    TM[TM$criteria=="omit",c(4:(ncol(TM)-1))] = NA
    #Store output
    out[[i]] = rbind(LC,TM)
  }
  #collapse the list object into a single DF
  OUT = do.call("rbind",out)
  #return dataframe and save to csv if 'filename' provided
  if(missing(filename) == TRUE) {
    return(OUT)
  } else {
    write.csv(OUT, file = filename, row.names=FALSE)
    return(OUT)
  }
}

###########################################
# BYER2Excel
#
# Description
# ------------------
# Computes the CTC's BYER statistic (converted to AEQs) and saves it to an excel file 
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
BYER2Excel <- function(hrj, stknames=NULL, fmap, smap, filename="byer tables.xlsx") {
  require(openxlsx)
  if(is.null(stknames)) {
    stknames = hrj$stknames
  }
 #number of stocks
  nstks = length(stknames)
 #byer type definitions
  byer_defs = data.frame(BYER=c("Ocean","Term","Total"),wsname=c("OCN","TRM","TTL"),chart=c("Ocean Exploitation Rates","Terminal Exploitation Rates","Total Exploitation Rates"))
 #else... use user specified stock list... code not implemented yet!
  wb <- createWorkbook("BYERCalcs")
 #blank list obj to store results
  out2Excel = list()
 #add the indiv stock sheets
  for(i in 1:nstks) {
    #determine the stock-specific byer type from the smap file
     byer_type = as.character(subset(smap, Stock==stknames[i])$BYER)
     wsname = paste(stknames[i],subset(byer_defs,BYER==byer_type)$wsname)
     stock_name = as.character(subset(smap, Stock==stknames[i])$StockName)
     fig_subtitle = subset(byer_defs,BYER==byer_type)$chart
    #
     tablename = paste("Table D._. Brood year exploitation rates for ", stock_name, " (", stknames[i],"). Catch and incidental mortality are shown. Only completed brood years are included.", sep="")
     addWorksheet(wb=wb, sheetName=wsname)
    #P BYER results
    if(byer_type=="Terminal") {
      byer_p = byer(stkname=stknames[i], hrjobj=hrj, fmap=flookup, type="Terminal", strays="ignore", criteria="byer", shakermethod="P")
      p_estimates = data.frame(BY=byer_p$Years,
                               LandedCatch=byer_p$PercentMortality[,"Term_LC"],IncidentalMortality=byer_p$PercentMortality[,"Term_IM"],
                               criteria=byer_p$Criteria)
    } else if (byer_type=="Ocean") {
      byer_p = byer(stkname=stknames[i], hrjobj=hrj, fmap=flookup, type="Ocean", strays="ignore", criteria="byer", shakermethod="P")
      p_estimates = data.frame(BY=byer_p$Years,
                               LandedCatch=byer_p$PercentMortality[,"Ocean_LC"],IncidentalMortality=byer_p$PercentMortality[,c("Ocean_IM")],
                               criteria=byer_p$Criteria)
    } else if (byer_type=="Total") {
      byer_p = byer(stkname=stknames[i], hrjobj=hrj, fmap=flookup, type="Total", strays="ignore", criteria="byer", shakermethod="P")
      p_estimates = data.frame(BY=byer_p$Years,
                               LandedCatch=rowSums(byer_p$PercentMortality[,c("Ocean_LC","Term_LC")]),IncidentalMortality=rowSums(byer_p$PercentMortality[,c("Ocean_IM","Term_IM")]),
                               criteria=byer_p$Criteria)
    } else {
      cat("byer_type", as.character(byer_type), "not recognized. program will stop")
      break
    }
    out2Excel[[i]] = list(Table_Name=tablename)
    out2Excel[[i]]$BYER_Table = subset(p_estimates,criteria=="ok", select=-criteria)
    #out2Excel[[i]]$BYER_Table = merge(data.frame(BY=seq(min(out2Excel[[i]]$BYER_Table$BY), max(out2Excel[[i]]$BYER_Table$BY)), LandedCatch=NA, IncidentalMortality=NA), out2Excel[[i]]$BYER_Table, by.x="BY", by.y="BY", all.x=TRUE, suffixes=c(".x",""))
    out2Excel[[i]]$BYER_Table = merge(data.frame(BY=seq(1971, max(out2Excel[[i]]$BYER_Table$BY)), LandedCatch=NA, IncidentalMortality=NA), out2Excel[[i]]$BYER_Table, by.x="BY", by.y="BY", all.x=TRUE, suffixes=c(".x",""))
    out2Excel[[i]]$BYER_Table = subset(out2Excel[[i]]$BYER_Table, select=-c(LandedCatch.x,IncidentalMortality.x))
    out2Excel[[i]]$BYER_Table$Total = with(out2Excel[[i]]$BYER_Table, LandedCatch + IncidentalMortality)
    
    writeData(wb=wb, sheet=wsname, x=out2Excel[[i]]$Table_Name, startRow=1)
    writeData(wb=wb, sheet=wsname, x=out2Excel[[i]]$BYER_Table, startRow=3, borders="surrounding")
    
    par(mar=c(6.7,5.1,4.1,2.1))
    barplot(t(out2Excel[[i]]$BYER_Table[,c(-1,-4)]), ylim=c(0,1), ylab="Exploitation Rate", xlab="Brood Year", legend.text=c("LC","IM"), names.arg=right(out2Excel[[i]]$BYER_Table[,1],2), main=stock_name, args.legend=list(x="bottom",xpd=TRUE,horiz=TRUE,inset=c(0,-.65),cex=1),col=c("darkblue","orange"), axis.lty=1, space=1, las=1)
    mtext(fig_subtitle, side=3, line=0.5)
    box()
    insertPlot(wb=wb, sheet=wsname, startCol=6, startRow=5, width=6, height=4, fileType="png", units="in")

    out2Excel[[i]]$BYER_Table$Stock = stknames[i]
  }
  #add the all stock worksheet
   ALLSTOCKS = do.call("rbind", lapply(out2Excel, function(x) return(x$BYER_Table)))
   addWorksheet(wb=wb, sheetName="all stocks")
   writeData(wb=wb, sheet="all stocks", x=ALLSTOCKS, startRow=1 , borders="surrounding")
  #add the summary worksheet
   #filter data to find the last BY and extract the BYER for that year
    lastBY_BYER <- ALLSTOCKS %>% group_by(Stock) %>% slice(which.max(BY)) %>% select(Stock,Total)  %>% rename(lastBYER = Total)
   #create summary table
    SUMMARY <- ALLSTOCKS %>%
      group_by(Stock) %>%
      summarise(mean = mean(Total, na.rm=TRUE), min = min(Total, na.rm=TRUE), max = max(Total, na.rm=TRUE), nBY = sum(!is.na(Total)), lastBY = last(BY)) %>%
      left_join(lastBY_BYER, by = join_by(Stock == Stock))
   #add summary table to workbook
    addWorksheet(wb=wb, sheetName="summary")
    writeData(wb=wb, sheet="summary", x="Brood year exploitation rate summary statistics by stock", startRow=1)
    writeData(wb=wb, sheet="summary", x=SUMMARY, startRow=2, borders="surrounding")
  #save workbook
   saveWorkbook(wb, filename, overwrite = TRUE)
  #returns nothing
   return(SUMMARY)
}

###########################################
# plot.summary.MDT
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
plot.summary.MDT <- function(x, type="percent") {
 #
  require(ggplot2)
  require(reshape2)
 #show what?
  if(type=="percent") { 
    morttoshow = x$AvgPercentMortality
    myylab = "Total Mortality (%)" #technically this is almost always an average, but the label in the report is just TM
    mybreaks = c(0,0.20,0.40,0.6,0.8,.995) #setting the last break as .995 as opposed to 1 helps prevent the odd rounding issue that sometimes results in 100 not being displayed
    mylabels = c("0","20","40","60","80","100")
  } else if (type=="number") {
    morttoshow = x$AvgMortality
    myylab = "Total Mortality (#)" #technically this is almost always an average, but the label in the report is just TM
    mybreaks = waiver() #set mybreaks equal to waiver() to resort back to default scale_y_continuous break option
    mylabels = waiver() #set mylabels equal to waiver() to resort back to default scale_y_continuous break option
  } else { cat("DNE\n") }
 #coerce to a data.frame, DROP all year labs without data
  morttoshow=ifelse(is.nan(morttoshow), NA, morttoshow)
  tmp = morttoshow[!is.na(rowSums(morttoshow)),]
  tmp = data.frame(tmp)
  if(nrow(morttoshow)==1) tmp = data.frame(t(tmp))
  tmp$YearLabs = x$YearLabs[!is.na(rowSums(morttoshow))]
 #coerce dataframe into a format ggplot2 can use
  tmp2 <- melt(tmp, id="YearLabs", variable.name = "Fishery")
  tmp2$YearLabs = factor(tmp2$YearLabs, levels=x$YearLabs[!is.na(rowSums(morttoshow))])
 #the guts, ggplot2 is slightly unweildy
  p = ggplot(data=tmp2,aes(x=YearLabs, y=value, fill=Fishery)) + 
  geom_bar(width = 0.85,color="black",alpha=0.8,stat="identity") + #this is the bar portion
    ylab(myylab)+
    xlab("Year Range")+
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+ #removes the background grid
    theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+ #removes the background grid
    theme(panel.background = element_blank())+ #removes the gray filled back
    scale_y_continuous(expand=c(0,0), #this gets labels closer to axis
                       breaks=mybreaks, 
                       labels=mylabels)+
    scale_x_discrete(expand=c(0,0))+ #this gets labels closer to axis, gets rid of gap    
    theme(panel.background = element_rect(colour="black",size=1,fill="white")) #this adds a border
 #return plot
  return(p)
}

###########################################
# plot.summary.MDT
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
plot.summary.MDT.list <- function(x, type="percent") {
 #
  require(ggplot2)
  require(reshape2)
 #show what?
  if(type=="percent") { 
    morttoshow = lapply(x, function(x) x$AvgPercentMortality)
    myylab = "Total Mortality (%)" #technically this is almost always an average, but the label in the report is just TM
    mybreaks = c(0,0.20,0.40,0.6,0.8,.995) #setting the last break as .995 as opposed to 1 helps prevent the odd rounding issue that sometimes results in 100 not being displayed
    mylabels = c("0","20","40","60","80","100")
  } else if (type=="number") {
    morttoshow = lapply(x, function(x) x$AvgMortality)
    myylab = "Total Mortality (#)" #technically this is almost always an average, but the label in the report is just TM
    mybreaks = waiver() #set mybreaks equal to waiver() to resort back to default scale_y_continuous break option
    mylabels = waiver() #set mylabels equal to waiver() to resort back to default scale_y_continuous break option
  } else { cat("DNE\n") }
 #coerce to a data.frame, DROP all year labs without data
  tmp = do.call("rbind",morttoshow)
  tmp = data.frame(tmp)
  yearlabs = lapply(x, function(x) x$YearLabs)
  tmp$YearLabs = unlist(yearlabs)
  nlabs = lapply(yearlabs, length)
  stocknames = lapply(x, function(x) rep(x$StockName))
  tmp$StockName = rep(unlist(stocknames),unlist(nlabs))
 #coerce dataframe into a format ggplot2 can use
  tmp2 <- melt(tmp, id=c("YearLabs","StockName"), variable.name = "Fishery")
  tmp2$StockName = factor(tmp2$StockName, levels=stocknames)
  tmp2$YearLabs = factor(tmp2$YearLabs)
  if(nlevels(tmp2$YearLabs)==1) tmp2$StockYear = tmp2$StockName
  if(nlevels(tmp2$YearLabs)>1) tmp2$StockYear = paste(tmp2$StockName,tmp2$YearLabs,sep=":")
 #the guts, ggplot2 is slightly unweildy
  p = ggplot(data=tmp2,aes(x=StockYear,y=value, fill=Fishery)) + 
  geom_bar(width = 0.85,color="black",alpha=0.8,stat="identity") + #this is the bar portion
    ylab(myylab)+
    xlab("")+
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+ #removes the background grid
    theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+ #removes the background grid
    theme(panel.background = element_blank())+ #removes the gray filled back
    scale_y_continuous(expand=c(0,0), #this gets labels closer to axis
                       breaks=mybreaks,
                       labels=mylabels)+
    scale_x_discrete(expand=c(0,0))+ #this gets labels closer to axis, gets rid of gap    
    theme(panel.background = element_rect(colour="black",size=1,fill="white")) #this adds a border
 #return plot
  return(p)
}

###########################################
# readZIP_filelist
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
readZIP_filelist <- function(filename) {
    con <- file(filename)
    on.exit(close(con))
    txt <- as.character(unzip(filename, list = TRUE)$Name)
    return(txt)
}

###########################################
# readFILEinZIP
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
readFILEinZIP <- function(filename) {
    con <- unz(filename[1], filename[2])
    on.exit(close(con))
    txt <- readLines(con)
    return(txt)
}

###########################################
# readFILE
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
readFILE <- function(filepath) {
    con <- file(filepath)
    on.exit(close(con))
    txt <-readLines(con)
    txt
}

###########################################
# cyer
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
cyer <- function(eis, eris, fmap, smap, esc=NULL, hrjobj, period1=2009:2015, period2) {
 #Prelims
  out=list()
  out$period1 = period1 
  out$period2 = period2
  out$EIS = eis
  out$ERIS = eris
  out$MgmtObj = unique(subset(smap,EIS_StockName==eis)$CYER_Management_Objective)
 #Escapement
  if(!is.na(out$MgmtObj)) {
   stockloc = match(eis, names(esc))
   yearloc = match(period2, esc$Year)
   out$Escapement = data.frame(Year=period2, Escapement=esc[yearloc,stockloc])
   out$Escapement$MgmtObjMet = out$Escapement$Escapement>out$MgmtObj
  }
 #CYER
  #Compute MDT
   tmp_mdt = mdt(stkname=hrjobj$stknames[match(eris, hrjobj$stknames)], hrjobj=hrjobj, fmap=fmap, type="AEQTot", strays="separate", ages=2:6, criteria="mdt")
   tmp_summary.mdt = summary.MDT(tmp_mdt, yearranges=list(period1))
  #MDT during base period
   out$CYER_BasePeriodAnnual = tmp_mdt$PercentMortality[tmp_mdt$Years%in%period1,c("ISBM Canada","ISBM US")]
   #if the base period does not have data in the MDT, blank rows for those years
    if(sum(period1%in%tmp_mdt$Years)!=length(period1)) {
      years_not_in_bp = (period1)[!period1%in%tmp_mdt$Years]
      missing_years = matrix(NA,nrow=length(years_not_in_bp), ncol=2)
      colnames(missing_years) = colnames(out$CYER_BasePeriodAnnual)
      rownames(missing_years) = years_not_in_bp
      out$CYER_BasePeriodAnnual = rbind(out$CYER_BasePeriodAnnual,missing_years)
      out$CYER_BasePeriodAnnual = out$CYER_BasePeriodAnnual[sort(rownames(out$CYER_BasePeriodAnnual)),]
    }
  #Avg. MDT during base period
   out$CYER_BasePeriodAvg = tmp_summary.mdt$AvgPercentMortality[,c("ISBM Canada","ISBM US")] 
  #Observed MDTs during period 2
   out$CYER_RecentYear = tmp_mdt$PercentMortality[tmp_mdt$Years%in%period2,c("ISBM Canada","ISBM US")] 
  #CYER Objective
   out$CYER_Obj = unique(subset(smap,EIS_StockName==eis)[,c("CYER_ISBM_Canada","CYER_ISBM_US")])
  #CYER Limits
   out$CYER_Limits = tmp_summary.mdt$AvgPercentMortality[,c("ISBM Canada","ISBM US")] * unique(subset(smap,EIS_StockName==eis) [,c("CYER_ISBM_Canada","CYER_ISBM_US")])
   type_can <- unique(subset(smap,EIS_StockName==eis)[,"CYER_ISBM_Canada_type"])
   if(!is.na(type_can) & type_can == 2) {
     out$CYER_Limits$CYER_ISBM_Canada <- unique(subset(smap,EIS_StockName==eis) [,"CYER_ISBM_Canada"])
   }
   type_us <- unique(subset(smap,EIS_StockName==eis) [,"CYER_ISBM_US_type"])
   if(!is.na(type_us) & type_us == 2) {
     out$CYER_Limits$CYER_ISBM_US <- unique(subset(smap,EIS_StockName==eis) [,"CYER_ISBM_US"])
   }
 #Compliance?
  #Type of management objective
   out$HasMgmtObj = ifelse(!is.na(out$MgmtObj),TRUE,FALSE)
  #If the stock has a management objective
   # "For each stock with an agreed management objective set out in Attachment I, 
   #   the running three-year average shall include all years in which the management objective is not achieved, 
   #   and the years in which the management objective is achieved with a CYER that is less than or equal to the ISBM obligation identified in paragraph 5."
   if(out$HasMgmtObj) {
    #modify the MDT criteria type to denote it is the 'cyer' type (this isn't necessary per se, but it explains what's about to happen quite well)
     tmp_mdt$CriteriaType = "cyer"
    #extract the original criteria for period2
     OrigCriteria = tmp_mdt$Criteria[tmp_mdt$Years %in% period2]
    #compute the updated criteria for period2
     #-include values from all years when the management objective was not achieved
     #-and include all values for years when the management obj was achieved AND the CYER was less than the limit (note this criteria is now country specific...)
     NewCriteria_canada = ifelse(out$Escapement$MgmtObjMet==FALSE | (out$Escapement$MgmtObjMet==TRUE & (out$CYER_RecentYear[,"ISBM Canada"] < out$CYER_Limit[,"CYER_ISBM_Canada"])), "ok", "omit")
     NewCriteria_us = ifelse(out$Escapement$MgmtObjMet==FALSE | (out$Escapement$MgmtObjMet==TRUE & (out$CYER_RecentYear[,"ISBM US"] < out$CYER_Limit[,"CYER_ISBM_US"])), "ok", "omit")
    #
     NewCriteria_canada = ifelse(is.na(NewCriteria_canada),"omit",NewCriteria_canada)
     NewCriteria_us = ifelse(is.na(NewCriteria_us),"omit",NewCriteria_us)
    #update the original criteria...
     tmp_mdt.canada = tmp_mdt
     tmp_mdt.us = tmp_mdt
     tmp_mdt.canada$Criteria[tmp_mdt.canada$Years %in% period2] = ifelse(OrigCriteria!="ok", OrigCriteria, NewCriteria_canada)
     tmp_mdt.us$Criteria[tmp_mdt.us$Years %in% period2] = ifelse(OrigCriteria!="ok", OrigCriteria, NewCriteria_us)
    #and add the new criteria to the escapement table
     out$Escapement$CYERCriteriaCanada = NewCriteria_canada
     out$Escapement$CYERCriteriaUS = NewCriteria_us
     #...while noting that the above logic only supplants the original criteria if and only if it was originally marked as "ok".
      #i.e. if you don't have enough CWT recoveries (or ages) present in a calendar year,
      #     the cyer from that particular year would not be included in the running 3-year average regardless
    #and compute
     tmp_summary.mdt2.canada = summary.MDT(tmp_mdt.canada, yearranges=list(period2))     
     tmp_summary.mdt2.us     = summary.MDT(tmp_mdt.us, yearranges=list(period2))   
     out$CYER_Average = c(tmp_summary.mdt2.canada$AvgPercentMortality[,c("ISBM Canada")],tmp_summary.mdt2.us$AvgPercentMortality[,c("ISBM US")])
     names(out$CYER_Average) = c("ISBM Canada","ISBM US")
   }
  #If the stock does not have a management objective
   if(!out$HasMgmtObj) {
    out$Escapement = data.frame(Year=period2, Escapement=NA)
    out$Escapement$CYERCriteriaCanada = out$Escapement$CYERCriteriaUS = tmp_mdt$Criteria[tmp_mdt$Years %in% period2]
    tmp_summary.mdt2 = summary.MDT(tmp_mdt, yearranges=list(period2))
    out$CYER_Average = tmp_summary.mdt2$AvgPercentMortality[,c("ISBM Canada","ISBM US")]
   }
 #Compute years in period 1 (i.e. the base period)
   out$nCYER_BasePeriod = sum(ifelse(tmp_mdt$Criteria[tmp_mdt$Years %in% period1]=="ok",1,0))
 #Compute years in period 2 (e.g. the recent 3-year period)
   out$nCYER_RecentYear = sum(ifelse(tmp_mdt$Criteria[tmp_mdt$Years %in% period2]=="ok",1,0))
 #Lastly, append the raw MDT results to the output
  out$MDT = tmp_mdt
 #Set class, return object to user
  class(out) = "CYER"
  return(out)
}

###########################################
# print.CYER
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# x = a CYER object
#
# Output(s)
# ------------------
# 
###########################################
print.CYER <- function(x) {
 class(x) = "list"
 print(x)
}

###########################################
# summary.CYER
#
# Description
# ------------------
#
#
# Argument(s)
# ------------------
# x = a CYER object
# country = character, must equal either 'US' or 'Canada", which specifies what country's ISBM index to print the results from
#
# Output(s)
# ------------------
# 
###########################################
summary.CYER <- function(x, country=c("US","Canada")) {
 #
  if(country=="US") {
   tmp = data.frame(EIS=x$EIS, MgmtObj=x$MgmtObj, ERIS=x$ERIS, nBP=x$nCYER_BasePeriod, CYER_Limit=x$CYER_Limits[,"CYER_ISBM_US"], Year=x$period2, CYER=x$CYER_RecentYear[,"ISBM US"])
   tmp = reshape2::dcast(tmp, EIS + MgmtObj + ERIS + nBP + CYER_Limit ~ Year, value.var="CYER")
   tmp$AverageCYER=x$CYER_Average[2] #equals "ISBM US"
 #
  } else if (country=="Canada") {
   tmp = data.frame(EIS=x$EIS, MgmtObj=x$MgmtObj, ERIS=x$ERIS, nBP=x$nCYER_BasePeriod, CYER_Limit=x$CYER_Limits[,"CYER_ISBM_Canada"], Year=x$period2, CYER=x$CYER_RecentYear[,"ISBM Canada"])
   tmp = reshape2::dcast(tmp, EIS + MgmtObj + ERIS + nBP + CYER_Limit ~ Year, value.var="CYER")
   tmp$AverageCYER=x$CYER_Average[1] #equals "ISBM Canada"
 #
  } else {
      cat("country must be equal to either US or Canada\n") 
  }
 #to bold or not to bold...
  if(country=="Canada") tmp2 = data.frame(Bold=ifelse(x$Escapement$CYERCriteriaCanada=="ok","bold","not"), Year=x$period2)
  if(country=="US") tmp2 = data.frame(Bold=ifelse(x$Escapement$CYERCriteriaUS=="ok","bold","not"), Year=x$period2)
  tmp2 = reshape2::dcast(tmp2, 1 ~ Year, value.var="Bold")[-1]
 #what color should i be?
  if(x$HasMgmtObj) {
   tmp3 = data.frame(Color=ifelse(x$Escapement$MgmtObjMet==TRUE,"green","red"), Year=x$period2)
   tmp3 = reshape2::dcast(tmp3, 1 ~ Year, value.var="Color")[-1]
  } else {
   tmp3 = rep(NA,length(x$period2))
  }
 #replace the observed CYERs with NA if there is no CYER limit
  if(is.na(tmp$CYER_Limit)) tmp[,names(tmp)%in%x$period2] = NA
 #replace the avg CYER with NA if there is no CYER limit
  tmp$AverageCYER = ifelse(is.na(tmp$CYER_Limit), NA, tmp$AverageCYER)
 #
  out=list()
  out$results = tmp
  out$bold = tmp2
  out$color = tmp3
 #return results
  class(out) = "summary.CYER"
  return(out)
}

###########################################
# print.summary.CYER
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# x = a CYER object
# digits = numeric, how many digits should be printed with CYER results?
#
# Output(s)
# ------------------
# 
###########################################
print.summary.CYER <- function(x, digits=3) {
 #round digits
  x$results$CYER_Limit = round(x$results$CYER_Limit,digits)
  x$results$AverageCYER = round(x$results$AverageCYER,digits)
 #
  class(x$results) = "data.frame"
 #
  print(x$results)
}

###########################################
# CYER2Excel
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
CYER2Excel <- function(whichstocks, fmap, smap, esc, hrjobj, period1=2009:2015, period2=2019:thisyr, filename="cyer tables.xlsx") {
 #
  require(openxlsx)
 #number of stocks
  nstks = length(whichstocks)
 #
  wb <- createWorkbook("CYERCalcs")
 #
  table1 = table1.criteria1 = table1.criteria2 = table2 = table2.criteria1 = table2.criteria2 = app1 = app2 = list()
 #add the individual stock sheets
  for(i in 1:nstks) {
   #stock eis and eris name
    eis=as.character(smap$EIS_StockName[whichstocks[i]])
    eris=as.character(smap$ERIS[whichstocks[i]])
   #worksheet name equals eis name (which SHOULD be unique, but isn't so... worksheet name is eis-eris)
    wsname = paste(eis,"-",eris,sep="")
   #add worksheet
    addWorksheet(wb=wb, sheetName=wsname)
   #compute CYER
    tempcalcs = cyer(eis=eis, eris=eris, fmap=fmap, smap=smap, esc=esc, hrjobj=hrjobj, period1=period1, period2=period2)
   #Canada
    tmp = summary(tempcalcs, "Canada")
    table1[[i]] = tmp$results
    table1.criteria1[[i]] = tmp$bold
    table1.criteria2[[i]] = tmp$color
    app1[[i]] = data.frame(eis=tempcalcs$EIS, MgmtObj = tempcalcs$MgmtObj, reshape2::dcast(tempcalcs$Escapement, 1 ~ Year, value.var="Escapement")[-1], 
                            eris=tempcalcs$ERIS, CYERObj = tempcalcs$CYER_Obj["CYER_ISBM_Canada"], t(tempcalcs$CYER_BasePeriodAnnual[,"ISBM Canada"]), 
                            CYER_BPAvg=tempcalcs$CYER_BasePeriodAvg["ISBM Canada"], CYER_Limit=tempcalcs$CYER_Limits["CYER_ISBM_Canada"],
                            CYER_Recent=t(tempcalcs$CYER_RecentYear[,"ISBM Canada"]),CYER_3yrAvg=tempcalcs$CYER_Average["ISBM Canada"])
    tmp=NA
   #US
    tmp = summary(tempcalcs, "US")
    table2[[i]] = tmp$results
    table2.criteria1[[i]] = tmp$bold
    table2.criteria2[[i]] = tmp$color
    app2[[i]] = data.frame(eis=tempcalcs$EIS, MgmtObj = tempcalcs$MgmtObj, reshape2::dcast(tempcalcs$Escapement, 1 ~ Year, value.var="Escapement")[-1], 
                            eris=tempcalcs$ERIS, CYERObj = tempcalcs$CYER_Obj["CYER_ISBM_US"], t(tempcalcs$CYER_BasePeriodAnnual[,"ISBM US"]), 
                            CYER_BPAvg=tempcalcs$CYER_BasePeriodAvg["ISBM US"], CYER_Limit=tempcalcs$CYER_Limits["CYER_ISBM_US"],
                            CYER_Recent=t(tempcalcs$CYER_RecentYear[,"ISBM US"]),CYER_3yrAvg=tempcalcs$CYER_Average["ISBM US"])
    tmp=NA
   #
    tablename = paste("CYER calcs for EIS stock:", eis)
   #
    out2Excel = list(Table_Name=tablename, Canada_Calcs = app1[[i]], US_Calcs = app2[[i]])
   #
    writeData(wb=wb, sheet=wsname, x=out2Excel[[1]], startRow=1)
    writeData(wb=wb, sheet=wsname, x="Canada ISBM", startRow=3)
    writeData(wb=wb, sheet=wsname, x=out2Excel[[2]], startRow=4, borders="surrounding")
    writeData(wb=wb, sheet=wsname, x="US ISBM", startRow=7)
    writeData(wb=wb, sheet=wsname, x=out2Excel[[3]], startRow=8, borders="surrounding")
  }
 #add the all stock sheet
  addWorksheet(wb=wb, sheetName="canada fisheries - all stocks")
  writeData(wb=wb, sheet="canada fisheries - all stocks", x=do.call("rbind",table1), startRow=1 , borders="surrounding")
  writeData(wb=wb, sheet="canada fisheries - all stocks", x=do.call("rbind",app1), startRow=nstks+3, borders="surrounding")
  addWorksheet(wb=wb, sheetName="us fisheries - all stocks")
  writeData(wb=wb, sheet="us fisheries - all stocks", x=do.call("rbind",table2), startRow=1 , borders="surrounding")
  writeData(wb=wb, sheet="us fisheries - all stocks", x=do.call("rbind",app2), startRow=nstks+3, borders="surrounding")
 #save workbook
  saveWorkbook(wb, filename, overwrite = TRUE)
 #returns nothing
}

###########################################
# Appendix_F
#
# Description
# ------------------
# Function to generate Appendix F tables for ERA report.
#
# Argument(s)
# ------------------
# hrj -> hrj data
# fmap -> fishery lookup table
# hrt_file_path -> path to the folder where hrt.txt files are stored for EIS stocks
# AppF.stocks -> list that defines AppF table structures (ERIS stocks, EIS stocks, and relevant terminal fisheries)
# fishery.order -> vector of terminal fishery names for factoring
# 
# Output(s)
# ------------------
# object 'AppF', which is a list of the five Appendix F tables
# 
###########################################
Appendix_F <- function(hrj, fmap, hrt_file_path, AppF.stocks, fishery.order) {
  
  AppF <- list()
  
  for(i in 1:length(AppF.stocks)) {
    # get ERIS HRs
    fmap$CMB <- as.character(fmap$FisheryName)
    fmap$CMB <- ifelse(fmap$CMB %in% AppF.stocks[[i]][[3]], fmap$CMB, "Other")
    fmap$CMB <- factor(fmap$CMB, levels=fishery.order)
    eris.dat <- MDT2CMZ(hrj = hrj, fmap = fmap, stknames = AppF.stocks[[i]][[1]]) %>%
      filter(MortType == "TM") %>%
      select(-c(Recoveries, Ages)) %>%
      rename(ESC = ESCAP) %>%
      mutate(t.run = rowSums(across(all_of(gsub(" ", ".", AppF.stocks[[i]][[3]]))))) %>% # calculate the terminal run
      select(1,3,dim(.)[2],4:(length(AppF.stocks[[i]][[3]])+2)) %>% 
      pivot_longer(4:dim(.)[2], names_to = "id", values_to = "value") %>% 
      mutate(id = paste0(id, ".", AppF.stocks[[i]][[1]])) %>% # add ERIS stock acronym as suffix to column names
      mutate(value = value / t.run) %>% # convert terminal fishery ERs to terminal HRs
      # mutate(value = value / 100) %>% # leave as terminal fishery ERs but divide by 100
      pivot_wider(names_from = id, values_from = value) %>% 
      select(-1,-3)
    
    # get EIS HRs
    for(j in 1:length(AppF.stocks[[i]][[2]])) {
      eis.temp <- read.delim(paste0(hrt_file_path, AppF.stocks[[i]][[2]][[j]], ".txt")) %>% 
        pivot_longer(2:dim(.)[2], names_to = "id", values_to = "value") %>% 
        mutate(id = as.integer(sub('.', '', id))) %>% 
        left_join(., fmap[,c(1:2)], by = c("id" = "FisheryNumber")) %>% 
        select(CY, FisheryName, value) %>%
        rename(`Catch Year` = CY, id = FisheryName) %>%
        mutate(id = paste0(id, ".", AppF.stocks[[i]][[2]][[j]])) %>% 
        pivot_wider(names_from = id, values_from = value)
      
      if(j==1) {
        eis.dat <- eis.temp
      }
      if(j>1) {
        eis.dat <- merge(eis.dat, eis.temp)
      }
    }
    
    # merge ERIS and EIS files and convert to percent format
    AppF.x <- merge(eris.dat, eis.dat) %>% 
      pivot_longer(2:dim(.)[2], names_to = "id", values_to = "value") %>% 
      mutate(value = as.character(percent(value, digits = 0))) %>% 
      pivot_wider(names_from = id, values_from = value)
    
    AppF[[i]] <- AppF.x
  }
  
  return(AppF)
}


###########################################
# byer (a variation of mdt)
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
byer <- function(stkname, hrjobj, fmap, type=c("Ocean","Terminal","Total"), strays=c("addtoesc","ignore","separate"), ages=2:6, criteria="byer", shakermethod="P") {
 #Stock number is defined by its location in stknames
  stkloc = match(stkname, hrjobj$stknames)
 #which HRJ results should be referenced? 
  if(shakermethod=="b" || shakermethod=="B") {
    hrj=subset(hrjobj$HRJ_BY,stock==stkloc)
    esc=subset(hrjobj$ESC_BY,stock==stkloc)
  } else if (shakermethod=="c" || shakermethod=="C") {
    hrj=subset(hrjobj$HRJ_CY,stock==stkloc)
    esc=subset(hrjobj$ESC_CY,stock==stkloc)
  } else if (shakermethod=="p" || shakermethod=="P") {
    hrj=subset(hrjobj$HRJ_P,stock==stkloc)  
    esc=subset(hrjobj$ESC_P,stock==stkloc)
  } else {
    cat("shaker method not recognized\n")
  }
 #
  if(hrjobj$HRJformat=="brood") {
    yearformat="brood"
  }
 #determine min age, max age from HRJ data for this stock. this needs to be improved, as i assume a 4-age HRJ structure
  maxage = max(esc$oldestage, na.rm=TRUE)
  minage = ifelse(maxage==5,2,3)
 #what to do with strays? note that the stray information appears twice (both in the hrj table and the esc table)..
 #and so to prevent double counting, this function only references the escapement stray data in the esc. table, so make sure the ESC stray line is 'blanked' out
  fmap[left(fmap$FisheryName,1)=="X",]$Cohort_Type=NA
  if(is.factor(fmap$Cohort_Type)) fmap$Cohort_Type=droplevels(fmap$Cohort_Type)
  stray_pivot = NULL
  #age check
  if(any(ages<2) | any(ages>6)) ages = ages[!c(ages<2 | ages>6)]
  if(length(ages)<1) {
    cat("ERROR: age vector not valid, program will default to using all ages\n\n")
    ages = 2:6
  }
  if(strays=="addtoesc") {
    esctype = c(paste("All_Esc",ages,sep=""),paste("CA_Esc",ages,sep=""),paste("US_Esc",ages,sep=""))
  } else if (strays=="ignore") { 
    esctype = paste("All_Esc",ages,sep="")
  } else if (strays=="separate") {
    esctype = paste("All_Esc",ages,sep="")
    straytype = c(paste("CA_Esc",ages,sep=""),paste("US_Esc",ages,sep=""))
    stray_pivot = pivotMatrix(esc, repeatcolumns=match(c("brood","fishery"),names(esc)), movecolumns=match(straytype,names(esc)))
    names(stray_pivot)[3:4] = c("type","value")
  } else { cat("ERROR: user selected a strays option that is NOT RECOGNIZED\n")  }
  #
  esc_pivot = pivotMatrix(esc, repeatcolumns=match(c("brood","fishery"),names(esc)), movecolumns=match(esctype,names(esc)))
  names(esc_pivot)[3:4] = c("type","value")
  #always compute AEQ landed catch mortality
  hrj_pivot = pivotMatrix(hrj, repeatcolumns=match(c("brood","fishery"),names(hrj)), movecolumns=match(c(paste("AEQCat",ages,sep="")),names(hrj)))
  names(hrj_pivot)[3:4] = c("type","value")
  #always compute AEQ total mortality
  hrj_pivot2 = pivotMatrix(hrj, repeatcolumns=match(c("brood","fishery"),names(hrj)), movecolumns=match(c(paste("AEQTot",ages,sep="")),names(hrj)))
  names(hrj_pivot2)[3:4] = c("type","value")
  #
  hrj_pivot$age = as.numeric(right(as.character(hrj_pivot$type),1))
  hrj_pivot2$age = as.numeric(right(as.character(hrj_pivot2$type),1))
  esc_pivot$age = as.numeric(right(as.character(esc_pivot$type),1))
  #
  hrj_pivot = merge(hrj_pivot, fmap[,c("FisheryNumber","Cohort_Type")], by.x="fishery", by.y="FisheryNumber")
  hrj_pivot$Cohort_Type = as.character(hrj_pivot$Cohort_Type)
  hrj_pivot2 = merge(hrj_pivot2, fmap[,c("FisheryNumber","Cohort_Type")], by.x="fishery", by.y="FisheryNumber")
  hrj_pivot2$Cohort_Type = as.character(hrj_pivot2$Cohort_Type)
  #ocean net age switch
  hrj_pivot[hrj_pivot$Cohort_Type=="OceanNet" & !is.na(hrj_pivot$Cohort_Type),]$Cohort_Type = with(hrj_pivot[hrj_pivot$Cohort_Type=="OceanNet" & !is.na(hrj_pivot$Cohort_Type),],
                                                       ifelse(age >= (maxage-1), "Term", "Ocean"))
  
  hrj_pivot2[hrj_pivot2$Cohort_Type=="OceanNet" & !is.na(hrj_pivot2$Cohort_Type),]$Cohort_Type = with(hrj_pivot2[hrj_pivot2$Cohort_Type=="OceanNet" & !is.na(hrj_pivot2$Cohort_Type),],
                                                                                                   ifelse(age >= (maxage-1), "Term", "Ocean"))
  #
  cat_byage = with(hrj_pivot, tapply(value, list(brood,age), sum))
  esc_byage = with(esc_pivot, tapply(value, list(brood,age), sum))
  #determine which age classes are present
  agespresent = rep(NA, nrow(cat_byage))
  for(i in 1:nrow(cat_byage)) agespresent[i]=paste(colnames(cat_byage)[!is.na(cat_byage[i,])], collapse=",")
  #if strays are present and separate accounting is specified
  if(!is.null(stray_pivot)) {
    #note that if strays are present in the hrj_pivot, then the hrj_pivot[!is.na(hrj_pivot$Cohort_Type),] needs to be further subsetted to remove said fishery
    cat("not tested, program will stop\n")
    break
    numer = with(hrj_pivot[!is.na(hrj_pivot$Cohort_Type),], tapply(value, list(brood,Cohort_Type),sum,na.rm=TRUE))
    numer = cbind(numer,STRAY=with(stray_pivot[!is.na(stray_pivot$fishery),], tapply(value, list(brood),sum, na.rm=TRUE)),ESCAP=with(esc_pivot[!is.na(esc_pivot$fishery),], tapply(value, list(brood),sum, na.rm=TRUE))) #equivlanet to...numer$ESC = denom-rowSums(numer)
    denom = with(hrj_pivot2[!is.na(hrj_pivot2$Cohort_Type),], tapply(value, list(brood), sum,na.rm=TRUE))+with(esc_pivot[!is.na(esc_pivot$fishery),], tapply(value, list(brood),sum, na.rm=TRUE))+with(stray_pivot[!is.na(stray_pivot$fishery),], tapply(value, list(brood),sum, na.rm=TRUE))
    #else
  } else {
    #
    hrj_pivot[hrj_pivot[,"fishery"]%in%fmap[left(fmap$FisheryName,1)=="X",]$FisheryNumber,]$value = 0
    hrj_pivot2[hrj_pivot2[,"fishery"]%in%fmap[left(fmap$FisheryName,1)=="X",]$FisheryNumber,]$value = 0
    #
    if(type=="Terminal") {
      hrj_pivot = subset(hrj_pivot, Cohort_Type=="Term")
      hrj_pivot2 = subset(hrj_pivot2, Cohort_Type=="Term")
    }
    numer_lc = with(hrj_pivot, tapply(value, list(brood,Cohort_Type), sum,na.rm=TRUE))
    numer_tm = with(hrj_pivot2, tapply(value, list(brood,Cohort_Type), sum,na.rm=TRUE))
    numer_im = numer_tm - numer_lc
    colnames(numer_lc) = paste0(colnames(numer_lc),"_LC")
    colnames(numer_im) = paste0(colnames(numer_tm),"_IM")
    numer = cbind(numer_lc,numer_im)
    numer = cbind(numer,Esc=with(esc_pivot, tapply(value, list(brood),sum, na.rm=TRUE))) #equivalent to...numer$ESC = denom-rowSums(numer)
    denom = with(hrj_pivot2, tapply(value, list(brood),sum,na.rm=TRUE))+with(esc_pivot, tapply(value, list(brood),sum, na.rm=TRUE))
  }
  #
  morttab = matrix(NA,ncol=ncol(numer),nrow=nrow(numer))
  colnames(morttab) = colnames(numer)
  rownames(morttab) = rownames(numer)
  for(i in 1:ncol(numer)) morttab[,i] = numer[,i]/denom 
  #
  criteriaout=rep(NA,length(as.numeric(row.names(morttab))))
  if(is.null(criteria)) {
    criteriaout = ifelse(nchar(agespresent)>0, "ok", "omit") #if no criteria is specified include all rows
  }
  if(!is.null(criteria)) {
    if(criteria=="byer") {
      #Note: the criteria is always calculated using NomCat AND includes escapement strays! (though maybe it shouldn't include esc. strays? that's a question to the CTC)
      esctype_criteria = paste("All_Esc",ages,sep="")
      straytype_criteria  = c(paste("CA_Esc",ages,sep=""),paste("US_Esc",ages,sep=""))
      stray_pivot_criteria  = pivotMatrix(esc, repeatcolumns=match(c("brood","fishery"),names(esc)), movecolumns=match(straytype_criteria ,names(esc)))
      names(stray_pivot_criteria )[3:4] = c("type","value")
      esc_pivot_criteria = pivotMatrix(esc, repeatcolumns=match(c("brood","fishery"),names(esc)), movecolumns=match(esctype_criteria,names(esc)))
      names(esc_pivot_criteria)[3:4] = c("type","value")
      hrj_pivot_criteria = pivotMatrix(hrj, repeatcolumns=match(c("brood","fishery"),names(hrj)), movecolumns=match(c(paste("NomCat",ages,sep="")),names(hrj)))
      names(hrj_pivot_criteria)[3:4] = c("type","value")
      hrj_pivot_criteria$age = as.numeric(right(as.character(hrj_pivot_criteria$type),1))
      esc_pivot_criteria$age = as.numeric(right(as.character(esc_pivot_criteria$type),1))
      hrj_pivot_criteria = merge(hrj_pivot_criteria, fmap[,c("FisheryNumber","Cohort_Type")], by.x="fishery", by.y="FisheryNumber")
      denom_criteria = with(hrj_pivot_criteria[!is.na(hrj_pivot_criteria$Cohort_Type),], tapply(value, list(brood),sum,na.rm=TRUE))+with(esc_pivot_criteria[!is.na(esc_pivot_criteria$fishery),], tapply(value, list(brood),sum, na.rm=TRUE))+with(stray_pivot_criteria[!is.na(stray_pivot_criteria$fishery),], tapply(value, list(brood),sum, na.rm=TRUE))
      #apply criteria in the following sequence:
      #note the following use of nchar and definitions
      #-if nchar()=7, there are 4 ages present
      #-if nchar()=5, there are 3 ages present
      #-if nchar()=3, there are 2 ages present
      #-if nchar()=1, there is only 1 age present
      criteriaout[denom_criteria<=105] = "shade" #always shade when the number of nominal recoveries is 105 or less (same as MDT program)
      criteriaout[nchar(agespresent)<7] = "omit" #always omit when 3 or fewer age classes are present
      #and only set criteria equal to ok if it meets the criteria: at least 4 age classes and greater than 105 CWT recoveries
      criteriaout[denom_criteria>105 & nchar(agespresent)>=7] = "ok"
    } #end critera='byer'
  } #end if criteria is not null
  #
  out = list(StockName = stkname, Years = as.numeric(row.names(morttab)), Mortality = numer, PercentMortality = morttab, Recoveries = denom, AgesPresent = agespresent, Criteria = criteriaout, CriteriaType = criteria, YearFormat = yearformat, ShakerMethod="P Method")
  class(out) = "MDT"
  return(out)
}

###########################################
# convertHRJ_CYtoBY
#
# Description
# ------------------
# function to convert R Access version of the HRJ database from calendar year to brood year 
#
# Argument(s)
# ------------------
# x <- R Access version of the HRJ database, with data organized by calendar year
#
# Output(s)
# ------------------
# x <- R Access version of the HRJ database, with data organized by brood year
#
###########################################
convertHRJ_CYtoBY <- function(x) {
 #check to make sure data is brood format
  if(x$HRJformat=="brood") { 
    cat("ERROR: data is already in brood year format\n")
    break
  }
#for each "Access" style HRJ file...
  for(i in c("HRJ_BY","HRJ_CY","HRJ_P","ESC_BY","ESC_CY","ESC_P")) {
    if(!is.na(match(i, names(x)))) {
      #determine whether we're converting catch or escapement data, and use that to define a vector called variables names
      if(left(i,3)=="HRJ") variable_names = c(paste0("AEQCat",2:6),paste0("AEQTot",2:6),paste0("NomCat",2:6),paste0("NomTot",2:6),paste0("Pop",2:6))
      if(left(i,3)=="ESC") variable_names = c(paste0("All_Esc",2:6),paste0("CA_Esc",2:6),paste0("US_Esc",2:6))
      #convert wide to long
      x[[match(i, names(x))]] <- dplyr::select(x[[match(i, names(x))]], -inc) %>%
        tidyr::pivot_longer(cols = all_of(variable_names)) %>%
        dplyr::mutate(age = as.numeric(right(name,1))) %>%
        dplyr::mutate(brood = cy - age) %>%
        dplyr::select(stock,brood,fishery,oldestage,name,value) %>%
        tidyr::pivot_wider(names_from=name, values_from=value, values_fn = sum)
        #drop_na(all_of(variable_names))
      drop_na_step1 <- apply(x[[match(i, names(x))]][,variable_names], 2, function(y) is.na(y))
      drop_na_step2 <- apply(drop_na_step1, 1, all)
      x[[match(i, names(x))]] <- x[[match(i, names(x))]][!drop_na_step2,]
      x[[match(i, names(x))]] <- data.frame(x[[match(i, names(x))]]) #having the data formatted as a tibble was causing the function byer() to crash
    } #end if
  } #next suite of HRJ results
 #
  x$HRJformat="brood"
 #
  return(x)
}

###########################################
# readChinookModel
#
# Description
# ------------------
# reads in Chinook Model output and coerces it into an HRJ-object
#
# Argument(s)
# ------------------
# stock_ccc - stock CCC file (i.e., *_stk_CCC.csv)
# aabm_ccc - AABM CCC file (i.e., *_fish_AABM_CCC.csv)
# isbm_ccc - ISBM CCC file (i.e., *_fish_ISBM_CCC.csv)
# bse - BSE file (i.e., *.BSE)
# stk - STK file (i.e., *.STK)
# mataeq - MATAEQ file (i.e., MATAEQ_ETS_v5.dat)
# idl - IDL file (i.e., *.IDL)
# flookup - fishery code file (i.e., fisheryCodes.csv)
# slookup - stock code file (i.e., stockCodes.csv)
#
# Output(s)
# ------------------
# list object 
#
###########################################
readChinookModel <- function(stock_ccc_file, aabm_ccc_file, isbm_ccc_file, bse_file, stk_file, mataeq_file, idl_file, fishery_definition_file, stock_definition_file, lasty=2024) {

  # preliminaries ############################################
  firsty <-1979
  nyears <- lasty-firsty+1
  
  firsta <- 2
  lasta <- 5
  nages <- lasta-firsta+1
  
  nstocks <- 41
  nfisheries <- 48
  
  surv <- c(0.6,0.7,0.8,0.9) #natural mortality survival by age
  stock_names <- array(dim=nstocks)
  matrates <- array(dim=c(nstocks,nyears,nages))
  aeq <- array(dim=c(nstocks,nyears,nages),data=rep(0,nstocks*nyears*nages))
  esc <- array(dim=c(nstocks,nyears,nages),data=rep(0,nstocks*nyears*nages))
  coh <- array(dim=c(nstocks,nyears,nages),data=rep(0,nstocks*nyears*nages))
  trun <- array(dim=c(nstocks,nyears,nages),data=rep(0,nstocks*nyears*nages))
  idl <- array(dim=c(nstocks,nyears),data=rep(1,nstocks*nyears))
  aeqMort <- array(dim=c(nstocks,nfisheries,nyears,nages),data=rep(0,nstocks*nfisheries*nyears*nages))
  fishMort <- array(dim=c(nstocks,nfisheries,nyears,nages),data=rep(0,nstocks*nfisheries*nyears*nages))
  aeqCatMort <- array(dim=c(nstocks,nfisheries,nyears,nages),data=rep(0,nstocks*nfisheries*nyears*nages))
  fishCatMort <- array(dim=c(nstocks,nfisheries,nyears,nages),data=rep(0,nstocks*nfisheries*nyears*nages))
  termfisheryflag <- array(dim=c(nstocks,nfisheries),data=rep(0,nstocks*nfisheries))
  oceannetflag <- array(dim=c(nfisheries),data=rep(0,nfisheries))
  
  # read in aeq and escapements ##############################
  infile <- stock_ccc_file
  #junk <- read.csv(infile)
  #junk <- data.matrix(junk[,-11]) #this tripped me up b/c the row references change
  lines <- readLines(stock_ccc_file)
  junk <- lapply(lines[-1], function(x) unlist(strsplit(x[[1]], ","))[-11])
  junk <- lapply(junk, as.numeric)
  junk <- do.call("rbind",junk)
  
  jlen <- length(junk[,1])
  for(i in 1:jlen) {
    #browser()
    year <- junk[i,1]-firsty+1
    stock <- junk[i,2]  
    aeq[stock,year,] <- junk[i,c(3,5,7,9)]
    esc[stock,year,] <- junk[i,c(12,14,16,18)]
    coh[stock,year,] <- junk[i,c(4,6,8,10)] #cohort size BEFORE natural mortality
    trun[stock,year,] <- junk[i,c(11,13,15,17)]
  } # i in jlen
  
  # get fishery code lookup table ############################
  infile <- fishery_definition_file
  fishery_names <- read.csv("Data/2023/ChinookModel/fisheryCodes.csv")
  fcodes <- fishery_names[,2]
  
  # read in catch by fishery and aeq transform ###############
  infile <- aabm_ccc_file #AABM fisheries
  junk <- read.csv(infile)
  junk <- data.matrix(junk)
  
  # read in catch by fishery and aeq transform ###############
  jlen <- length(junk[,1])
  for(i in 1:jlen) {
    #browser()
    year <- junk[i,1]-firsty+1
    stock <- junk[i,2]  
    fishery <- junk[i,3]  
    age <- junk[i,4]-firsta+1
    fishMort[stock,fishery,year,age] <- sum(junk[i,5:20])
    fishCatMort[stock,fishery,year,age] <- sum(junk[i,5:6])
    # aeq mortality = aeq-adjusted ocean mortality including incidental mortality
    aeqMort[stock,fishery,year,age] <- sum(junk[i,seq(from=5,to=19,by=2)])*aeq[stock,year,age]+sum(junk[i,seq(from=6,to=20,by=2)])
    aeqCatMort[stock,fishery,year,age] <- sum(junk[i,5])*aeq[stock,year,age]+sum(junk[i,6])
  } # i in jlen
  
  infile <- isbm_ccc_file #ISBM fisheries are in a separate file
  junk <- read.csv(infile)
  junk <- data.matrix(junk)
  
  jlen <- length(junk[,1])
  for(i in 1:jlen) {
    #browser()
    year <- junk[i,1]-firsty+1
    stock <- junk[i,2]  
    fishery <- junk[i,3]  
    age <- junk[i,4]-firsta+1
    fishMort[stock,fishery,year,age] <- sum(junk[i,5:20])
    fishCatMort[stock,fishery,year,age] <- sum(junk[i,5:6])
    # aeq mortality = aeq-adjusted ocean mortality plus terminal mortality (for each stock and fishery combination only one type will apply)
    aeqMort[stock,fishery,year,age] <- sum(junk[i,seq(from=5,to=19,by=2)])*aeq[stock,year,age]+sum(junk[i,seq(from=6,to=20,by=2)])
    aeqCatMort[stock,fishery,year,age] <- sum(junk[i,5])*aeq[stock,year,age]+sum(junk[i,6])
  } # i in jlen
  
  # read in BSE file for ocean net and terminal definitions ##
  infile <- bse_file
  junk <- readLines(infile)[103] #ocean net fishery definitions
  oceannetflag = as.numeric(strsplit(junk[1], " ")[[1]][strsplit(junk[1], " ")[[1]]!=""])
  
  junk <- readLines(infile)[104:144] #stock-specific terminal fishery definitions
  for(s in 1:nstocks) {
    termfisheryflag[s,] = as.numeric(strsplit(junk[s], " ")[[1]][strsplit(junk[s], " ")[[1]]!=""])
  }
  
  # read in matrates by from STK and MATAEQ (calendar year) ##
  #get base period rates and file out matrates using them
  infile <- stk_file
  con <- file(infile) #lines are: stock abbreviation, initial cohort sizes, maturation rates, aeqs, and then BPERs by each of 48 fisheries 
  open(con)
  istock <- 1
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    stock_names[istock] <- as.character(unlist(strsplit(line, split="  "))) # get stock name
    readLines(con, n = 1, warn = FALSE) #initial cohort sizes
    line <- readLines(con, n = 1, warn = FALSE)
    xx <- as.numeric(unlist(strsplit(line, split="  ")))
    matrates[istock,,] <- rep(xx,each=nyears)
    readLines(con, n = 1, warn = FALSE) #aeqs
    for (i in 1:nfisheries) {
      readLines(con, n = 1, warn = FALSE) #BPERs by fishery
    } #i
    istock <- istock + 1
  } #while not EOF
  close(con)

  # overwrite matrates with updates in mataeq file
  infile <- mataeq_file
  con <- file(infile) #for each year, followed by lines with stock names, matrates, and aeqs 
  open(con)
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    if(substring(line,1,4)!="    ") {
      iyear <- as.integer(substring(line,1,4))-firsty+1 #find index of year for matrates array
    } else {
      line <- gsub(",","",line) #strip out commas
      line <- str_squish(line) #strip out double space
      jstock <- substring(line,1,3)
      istock <- match(jstock,stock_names)[1] #find index of stock for matrates array
      # extract matrates for this year
      line <- substring(line,first=5)
      matrates[istock,iyear,1:3] <- as.numeric(unlist(strsplit(line,split=" ")))[1:3]
    }
  } #while not EOF
  close(con)
  
  # get stock code lookup table ##############################
  infile <- stock_definition_file
  stock_names <- read.csv(infile)
  scodes <- stock_names[,2]
  
  # read in IDL values for certain stocks ####################
  infile <- idl_file
  junk <- readLines(infile) #ocean net fishery definitions
  index <- 2
  for(i in 1:as.numeric(junk[1])) {
    which_stock = subset(stock_names, stockShortName==junk[index])$stock
    start_year = as.numeric(junk[index+1])
    end_year = as.numeric(junk[index+2])
    if(lasty==2024) idl_vals = as.numeric(unlist(strsplit(junk[index+3], split="\t")))
    if(lasty==2025) idl_vals = as.numeric(unlist(strsplit(junk[index+3], split=",")))
    idl[which_stock,] = idl_vals[c(firsty-start_year):c(lasty-end_year+length(start_year:end_year)-1)]
    idl[which_stock,] = ifelse(is.na(idl[which_stock,]), mean(idl[which_stock,],na.rm=TRUE), idl[which_stock,]) #replace NA (most recent years only) with LTA
    index = index + 4
  }
  
  # coerce CM data into an HRJ-like object  #################
  myrow_fish = list()
  myrow_esc  = list()
  i=ii=1
  for(s in 1:stock) {
    for(y in 1:year) {
      for(f in 1:fishery) {
        #"HRJ" fishery data
        AEQTot = aeqMort[s,f,y,]  #AEQ TM
        names(AEQTot) = paste0("AEQTot",firsta:lasta)
        AEQCat = aeqCatMort[s,f,y,]  #AEQ LC
        names(AEQCat) = paste0("AEQCat",firsta:lasta)
        NomTot = fishMort[s,f,y,] #Nominal TM
        names(NomTot) = paste0("NomTot",firsta:lasta)
        NomCat = fishCatMort[s,f,y,] #Nominal LC
        names(NomCat) = paste0("NomCat",firsta:lasta)
        #the following if/else statements assign the type of cohort associated with a fishery
        #fisheries are defined as terminal, ocean net, or ocean per the BSE file
        #the logic statements are ordered and MUST be executed as such else oof, mistakes
        if(termfisheryflag[s,f]==1) {
          #cat("fishery", f, "is terminal for stock", s, "\n")
          Pop = trun[s,y,] #If terminal fishery, assign terminal run as the cohort size
        } else if (oceannetflag[f]==1) {
          #cat("fishery", f, "is ocean net for stock", s, "\n")
          Pop = c(coh[s,y,1:2],trun[s,y,3:4]) #If ocean net fishery, assign the mix of ocean / terminal cohort sizes
          Pop[1:2] = Pop[1:2] * surv[1:2] #Convert ocean cohort before natural to after natural mortality
        } else {
          #cat("fishery", f, "is ocean for stock", s, "\n")
          Pop = coh[s,y,] * surv #Else assign the ocean cohort size, and convert from ocean cohort before natural to ocean cohort after natural mortality
        }
        names(Pop) = paste0("Pop",firsta:lasta)
        myrow_fish[[ii]] = c(stock=s, cy=(firsty:lasty)[y], fishery=f, oldestage=5, AEQCat, AEQTot, NomCat, NomTot, Pop, inc=NA)
        ii=ii+1
      }
      #"HRJ" escapement data
      All_Esc = esc[s,y,]  #escapement
      names(All_Esc) = paste0("All_Esc",firsta:lasta)
      CA_Esc = US_Esc = rep(0,nages)
      names(CA_Esc) = paste0("CA_Esc",firsta:lasta)
      names(US_Esc) = paste0("US_Esc",firsta:lasta)    
      myrow_esc[[i]] = c(stock=s, cy=y, fishery=0, oldestage=5, All_Esc, CA_Esc, US_Esc, inc=NA)
      i = i+1
    }
  }
  tmpHRJ = data.frame(do.call("rbind", myrow_fish))
  tmpESC = data.frame(do.call("rbind", myrow_esc))
  tmpHRJ$AEQCat6 = tmpHRJ$AEQTot6 = tmpHRJ$NomCat6 = tmpHRJ$NomTot6 = tmpHRJ$Pop6 = NA
  tmpESC$All_Esc6 = tmpESC$CA_Esc6 = tmpESC$US_Esc6 = NA
  
  # format and return output to user
  out <- list()
  out$firsty <-firsty
  out$lasty <- lasty
  out$nyears <- nyears
  out$firsta <- firsta
  out$lasta <- lasta
  out$nages <- nages
  out$nstocks <- nstocks
  out$nfisheries <- nfisheries
  out$surv <- surv
  out$stock_names <- stock_names
  out$matrates <- matrates
  out$aeq <- aeq
  out$esc <- esc
  out$coh <- coh
  out$trun <- trun
  out$idl <- idl
  out$aeqMort <- aeqMort
  out$fishMort <- fishMort
  out$aeqCatMort <- aeqCatMort
  out$fishCatMort <- fishCatMort
  out$termfisheryflag <- termfisheryflag
  out$oceannetflag <- oceannetflag
  out$HRJformat <- "calendar"
  out$HRJ_P <- tmpHRJ
  out$ESC_P <- out$ESC_BY <- out$ESC_CY <- tmpESC
  out$stknames <- stock_names$stockShortName
  out$fshnames <- 1:nfisheries
  out$stock_names <- stock_names
  out$fishery_names <- fishery_names
  return(out)
}
