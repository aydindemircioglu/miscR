#!/usr/bin/Rscript  --vanilla 

library(BBmisc)
library(tools)
library(e1071)
library("SparseM")

source ("../software/helpers/lsdir.R")



# binary to call, this is has to be changed for OSX.
md5bin = 'md5sum'

# helper function
last <- function(x) { return( x[length(x)] ) }


checkFileUniqueness <- function (filename = '')
{
    # count the number of lines 
    messagef ("  Checking %s for unique data points.", filename)
    source ("../software/helpers/system3.R")
    s = system3('wc', filename)
    
    output = s$output
    pattern <- "\\s*(\\d+)\\s..*"
    nLines =  as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) 

    # now we sort the file, unique it and recount
    sortedFile = tempfile()
    uniqedFile = tempfile()
    s = system3('sort', c(filename, "|", "uniq", "|", "wc"))
    
    output = s$output
    pattern <- "\\s*(\\d+)\\s..*"
    uniqueLines =  as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) 

    print (nLines)
    print (uniqueLines)
    return (nLines - uniqueLines)
}



checkUniqueness <- function ()
{
    # parse through all .combined.scaled data
    path = "."
    folders = lsdir(path, all = FALSE, recursive = FALSE)

    rowForTable = list()
    for (folder in folders) {
        # throw out the wXa-hack.
        if  ( (grepl ("w\\da", folder) == TRUE) || (grepl ("a\\da", folder) == TRUE) )
        {
            messagef ("Skipping folder %s", folder)
            next
            continue
        }    
        files <- list.files (file.path (path, folder), pattern='*\\.combined.scaled')
        for (dataFile in files) {
        
            # read file
            currentDataFileName = file.path(path, folder, dataFile)
            messagef ("Processing %s", currentDataFileName)
            if (checkFileUniqueness (currentDataFileName) != 0) {
                messagef("File %s has non-unique data points!")
            }
        } 
    }
    return (-1)
}



countEntries <- function(sparseString, fac = TRUE, ncol = NULL)
{
    l <- strsplit(sparseString, "[ ]+")

    ## extract y-values, if any
    y <- if (is.na(l[[1]][1]) || length(grep(":",l[[1]][1])))
        NULL
    else
        sapply(l, function(x) x[1])

    ## x-values
    rja <- do.call("rbind",
                   lapply(l, function(x)
                          do.call("rbind",
                                  strsplit(if (is.null(y)) x else x[-1], ":")
                                  )
                          )
                   )
    ja <- as.integer(rja[,1])
    ia <- cumsum(c(1, sapply(l, length) - !is.null(y)))
    
    # there are sparse line with *everything* zero.
    if (length(ja) == 0)
        max.ja = -Inf
    else
        max.ja <- max(ja)

    # ia=#entries in line,  contains label, this we must substract
    return (c(ia[2]-1, max.ja))
}






removeSpaces <- function (filename) {
    if (filename != "") {
        # remove following things
        # -more than 1 spaces 
        # -spaces like in 1: .5 
        tmp = tempfile()
        tmp2 = tempfile()
        system3 ("sed", c("-e", "'s/  / /gi'", filename, ">", tmp))
        system3 ("sed", c("-e", "'s/  / /gi'", tmp, ">", tmp2))
        system3 ("sed", c("-e", "'s/: ./:./gi'", tmp2, ">", tmp))
        system3 ("cp", c(tmp, filename))
    }
}



checkForFileAndMD5Pre <- function (name, dir, md5)
{
    # does the file already exist?
    filename = paste(dir, "/", name, sep='')
    if (file.exists ( filename ))
    {
	# if so check md5 hash
	tmpmd5 = system ( paste (md5bin, filename, " | awk '{ print $1 }' "), intern = TRUE)
	if (tmpmd5 == md5)
	{
	    # file was downloaded already
	    messagef("  The dataset %s/%s has already been downloaded.\n", dir, name)
	    return (0)
	}
	else
	{
	    # file is broken.
	    stop("The dataset has a different md5 sum! Expected ", md5, ", but obtained ", tmpmd5)
	}
    }
    
    # file does not exist, so need to download
    return (-1)
}



checkForFileAndMD5Post <- function (name, dir, md5)
{
    # does the file already exist?
    filename = paste(dir, "/", name, sep='')
    if (file.exists ( filename ))
    {
	# if so check md5 hash
	tmpmd5 = system ( paste (md5bin, filename, " | awk '{ print $1 }' "), intern = TRUE)
	if (tmpmd5 == md5)
	{
	    # postcheck: file is ok, no warning
	    return (0)
	}
	else
	{
	    # file is broken.
	    stop("The dataset has a different md5 sum! Expected ", md5, ", but obtained ", tmpmd5)
	}
    }
    else
    {
	# postcheck: file should be downloaded, but is not.
	stop ("The file does not exist, maybe the download did not work.")
    }
}

 

# download from some repository 
# check also if the file already has been downloaded
# works only for binary datasets for now
downloadFromRepository <- function (name, dir, md5, repository = "libSVM", serverpath = "")
{
    if (checkForFileAndMD5Pre (name, dir, md5) != 0)
    {
        # download the data
        messagef ("Downloading %s..\n", name)
        
        # if we know it, we replace it, if not, we assume its a url anyway
        url = repository
        if (repository == "libSVM") 
            url = "http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/"
            
        if (repository == "UCI") 
            url = "http://archive.ics.uci.edu/ml/machine-learning-databases/"
            
        if (repository == "libSVMMultiClass") 
            url = "http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/multiclass/"

        url <- paste (url, serverpath, "/", name, sep='')
        
        messagef("  Loading from %s\n", url)
        wget_string <- paste("wget ", url, "-P ", dir)
        system (wget_string)
    }
    
    # need to recheck, if the download worked and the md5sum is correct
    checkForFileAndMD5Post (name, dir, md5)
    
    # then decompress if necessary and give back the decompressed filepath
    messagef ("  Trying to extract %s", name)
    extractedFilename = extract (name, dir, -1)
    
    messagef ("  Returning %s", extractedFilename)
    return (extractedFilename)
}
 


# main routine to get some data from a given repository name
retrieveFromRepositoryScaled <- function(dataFolder = '',
                                            fileList = c(), 
                                            repository = "libSVM", 
                                            serverpath = '', 
                                            dataProcessor = NULL,
                                            posLabels = c(0,1,2,3,4), 
                                            negLabels = c(5, 6,7,8,9),
                                            transformation = "VARIANCE") {

    unscaledFilenames = list()

    # download each file 
    for (currentFile in fileList) {
      fullFilename = downloadFromRepository (currentFile[1], dataFolder, currentFile[2], 
        repository = repository,
        serverpath = serverpath)
      
      messagef("  Downloaded %s", fullFilename)
      unscaledFilenames = c(unscaledFilenames, fullFilename)
    }
    
    # now we have to append all of them to one big file
    combinedFilename = file.path(dataFolder, paste(dataFolder, ".combined", sep = ""))
    if (!file.exists (combinedFilename)) {
      for (currentFile in unscaledFilenames) {
        messagef("  Combining %s into %s", currentFile[1], combinedFilename)
        file.append (combinedFilename, currentFile[1])
        messagef("     File has %d bytes", file.info(combinedFilename)$size)
      }
    } else {
      messagef("  Combined file %s exists.", combinedFilename)
    }

    if (is.null(dataProcessor) == FALSE) {
      messagef("\nCalling data proprocessor: %s", dataProcessor)
      do.call (dataProcessor, list(combinedFilename))
    }
    
    # if we have multiclasses, we first create binary combined before scaling
    if (repository == "libSVMMultiClass") {
      # make from 
      multiFilename = paste(combinedFilename, ".multi", sep = "")
      if (!file.exists (multiFilename)) {
        messagef("\nRelabeling %s into %s", combinedFilename, multiFilename)
        sData = relabelData (combinedFilename, posLabels = posLabels, negLabels = negLabels)
        messagef("  Writing relabeled data into %s", multiFilename)
        write.matrix.csr(sData$X, multiFilename, sData$y)
        } else {
        messagef("  Relabeled file %s exists.", multiFilename)
      }
      
      # make scaling work with relabled combined
      scaledCombinedFilename = paste(combinedFilename, ".scaled", sep = "")
      combinedFilename = multiFilename
    }
    else
    {
        # if we are in binary case, we can just take the combined filename as base
        scaledCombinedFilename = paste(combinedFilename, ".scaled", sep = "")
    }
    
    # now we need to scale that one file
    if (!file.exists (scaledCombinedFilename)) {
      messagef("  Scaling %s into %s", combinedFilename, scaledCombinedFilename)
      sData = scaleData (combinedFilename, transformation = transformation)
      messagef("  Writing scaled data into %s", scaledCombinedFilename)
      write.matrix.csr(sData$X, scaledCombinedFilename, sData$y)
    } else {
      messagef("  Scaled and combined file %s exists.", scaledCombinedFilename)
    }
}


retrieveFromRepositoryScaledUniquely <- function(...) {
    
    # sort non-unique
    retrieveFromRepositoryScaled(...)
    
    # get parameter
    eP = list (...)
    if (!is.null (eP[["dataFolder"]]))
        dataFolder = eP[["dataFolder"]]
    else
        stopf("Data folder parameter is missing?!")
    
    # put together filenames
    combinedFilename = file.path(dataFolder, paste(dataFolder, ".combined", sep = ""))
    scaledCombinedFilename = paste(combinedFilename, ".scaled", sep = "")
    messagef("  Removing duplicate lines and rewriting %s", scaledCombinedFilename )

    if (checkFileUniqueness (scaledCombinedFilename) != 0) {
        messagef ("  File %s has non-unique data points.", scaledCombinedFilename)
        messagef ("  Uniqueifying ", scaledCombinedFilename)
        uniFile = tempfile()
        system3 ('sort', c(scaledCombinedFilename, "|", 'uniq', "|", 'sort', '-R', '>', uniFile)) 
        system3 ('mv', c(uniFile, scaledCombinedFilename))
    } 
    else
        messagef ("  File %s is unique.", scaledCombinedFilename)
        
}



# main routine to get some data from a given repository name
retrieveFromRepository <- function(dataFolder, fileList, repository = "libSVM", serverpath = '') {
    unscaledFilenames = list()
    
    # download each file 
    for (currentFile in fileList) {
      fullFilename = downloadFromRepository (currentFile[1], dataFolder, currentFile[2], repository = repository, serverpath = serverpath)
      messagef("  Downloaded %s", fullFilename)
      unscaledFilenames = c(unscaledFilenames, fullFilename)
    }
    
    # now we have to append all of them to one big file
    combinedFilename = file.path(dataFolder, paste(dataFolder, ".combined", sep = ""))
    if (!file.exists (combinedFilename)) {
      for (currentFile in unscaledFilenames) {
        messagef("  Combining %s into %s", currentFile[1], combinedFilename)
        file.append (combinedFilename, currentFile[1])
        messagef("     File has %f bytes", file.info(combinedFilename)$size)
      }
    } else {
      messagef("  Combined file %s exists.", combinedFilename)
    }

    return (combinedFilename)
}



# main routine to get some data from a given repository name
retrieveFromBitbucket <- function(dir, fileList) {

    for (file in fileList) {
        filename = paste(dir, "/", file[1], sep='')
        md5 = file[2]
        if (file.exists ( filename ))
        {
            # if so check md5 hash
            tmpmd5 = system ( paste (md5bin, filename, " | awk '{ print $1 }' "), intern = TRUE)
            if (tmpmd5 == md5)
            {
                # file was downloaded already
                messagef("  The dataset %s has already been downloaded.\n", filename)
                return (0)
            }
                
            else
            {
                # file is broken.
                stop("The dataset has a different md5 sum! Expected ", md5, ", but obtained ", tmpmd5)
            }
        }    
        # no, so we need to load it.

        # first read pwd
        messagef ("  Reading passphrase from file 'pwd'")
        pwdFilename <- 'pwd'
        gitPwd = readChar(pwdFilename, file.info(pwdFilename)$size)

        # create temporary folder for our git checkout
        tmpDir = tempdir()
                
        # stupid but necessary as git will not checkout in non-empty dir
        unlink (tmpDir, recursive= TRUE, force = TRUE)
                
        messagef ("  Creating temporary directory at %s", tmpDir)
        #repoDir = "gitrepo"
        #dir.create(file.path(tmpDir, repoDir), showWarnings = FALSE)

        # call git with the temporary folder
        # remove stupid lineendings
        gitPwd = sub("\n", "", gitPwd[1])
        gitPwd = sub("\r", "", gitPwd)
        gitUrl = paste("https://largesvm:", gitPwd, "@bitbucket.org/aydin_demircioglu/", dir, ".git", sep ="") 
        messagef ("  Cloning data from git %s (may take a while).", gitUrl)
        system3 ("git", c("clone", gitUrl, tmpDir), stdout = TRUE)
        messagef ("  Finished.")
        
        # now use the key to decrypt the temporary file and copy it over to 
        # the correct place
        messagef ("  Decrypting using the key from file key (may take a while).")
        system3 ("openssl", c("enc", "-aes-256-cbc", "-d", "-kfile", "key", "-in", file.path(tmpDir, dir, paste(dir, ".enc", sep = "")), 
            "-out", filename), stdout = TRUE)


        # postcheck, please please refactor. later. 
        if (file.exists ( filename ))
        {
            # if so check md5 hash
            tmpmd5 = system ( paste (md5bin, filename, " | awk '{ print $1 }' "), intern = TRUE)
            if (tmpmd5 == md5)
            {
                # file was downloaded already
                messagef("  MD5 check went OK.\n")
                return (0)
            }
                
            else
            {
                # file is broken.
                stop("The dataset has a different md5 sum! Expected ", md5, ", but obtained ", tmpmd5)
            }
        }    
            
    }
}


 
extract <- function (file, dir, extractedfile, md5)
{
  # first test if the decompressed file does exist
  if (checkForFileAndMD5Pre (extractedfile, dir, md5) == 0) {
    # exists. go home.
    messagef("    Extracted file exists.")
    return (0)
  }
    
    if (mimeType == "application/x-gzip;")
    {
        cat ("Copying and Decompressing..\n")
        
        # copy file first
        cp_string <- paste("cp ", dir, "/", file, " ", dir, "/tmp.delme", sep='')
        system (cp_string)
        
        # extract
        gz_string <- paste("gzip -df ", dir, "/", file, sep='')
        system (gz_string)
        
        #copy back
        cp_string <- paste("mv ", dir, "/tmp.delme", " ", dir, "/", file, sep='')
        system (cp_string)
        
        # check now if the file exists and has the correct md5
        checkForFileAndMD5Post (extractedfile, dir, md5)
    }
}



checkForDecompressedFile <- function (dir, file) {
  extractedfile = basename(file_path_sans_ext(file))
  
  # first test if the decompressed file does exist
  if (file.exists(file.path(dir, extractedfile)) == TRUE)
  {
    # exists. go home.
    return (file.path(dir, extractedfile))
  }
  
  return (FALSE)
}


 
extract <- function (file, dir, md5) {
  # get mimetype. 
  # try the hard way, as a i am no admin on this maschine. there is a ; too much. who cares.
  mime_string <- paste("file ", dir, "/", file, " -i ", " -b   | awk '{ print $1 }' ", sep='')
  mimeType = system (mime_string, intern = TRUE)
  
  extractedFilename = file.path(dir, file)

  # try the hard way, as a i am no admin on this maschine. there is a ; too much. who cares.
    if (mimeType == "application/zip;")
    {
        cat ("Copying and Decompressing..\n")
        
        # copy file first
        cp_string <- paste("cp ", dir, "/", file, " ", dir, "/tmp.delme", sep='')
        system (cp_string)
        
        # extract
        gz_string <- paste("unzip -qo ", dir, "/", file, sep='')
        system (gz_string)
        
        #copy back
        cp_string <- paste("mv ", dir, "/tmp.delme", " ", dir, "/", file, sep='')
        system (cp_string)
        
        # check now if the file exists and has the correct md5
        extractedFilename = file.path(dir, basename(file_path_sans_ext(file)))
    }
    
  
  if (mimeType == "application/x-gzip;")
  {
    extractedFilename = checkForDecompressedFile(dir, file)
    if (extractedFilename == FALSE) {
      cat ("Copying and Decompressing..\n")
      
      # copy file first
      cp_string <- paste("cp ", dir, "/", file, " ", dir, "/tmp.delme", sep='')
      system (cp_string)
      
      # extract
      gz_string <- paste("gzip -df ", dir, "/", file, sep='')
      system (gz_string)
      
      #copy back
      cp_string <- paste("mv ", dir, "/tmp.delme", " ", dir, "/", file, sep='')
      system (cp_string)

      extractedFilename = file.path(dir, basename(file_path_sans_ext(file)))
      
      # check now if the file exists and has the correct md5
      #checkForFileAndMD5Post (extractedfile, dir, md5)
    }
  }
  
  
  if (mimeType == "application/x-bzip2;")
  {
    extractedFilename = checkForDecompressedFile(dir, file)
    if (extractedFilename == FALSE) {
      cat ("Copying and Decompressing..\n")
      
      # copy file first
      cp_string <- paste("cp ", dir, "/", file, " ", dir, "/tmp.delme", sep='')
      system (cp_string)
      
      # extract
      gz_string <- paste("bzip2 -d ", dir, "/", file, sep='')
      system (gz_string)
      
      #copy back
      cp_string <- paste("mv ", dir, "/tmp.delme", " ", dir, "/", file, sep='')
      system (cp_string)
      
      extractedFilename = file.path(dir, basename(file_path_sans_ext(file)))

      # check now if the file exists and has the correct md5
      #checkForFileAndMD5Post (extractedfile, dir, md5)
    }
  }
  
  return (extractedFilename)
}
 

 
createDir <- function (name) {
    cat ("Creating directory ", name, "\n")
    dir.create (name, showWarnings = FALSE)
}
 
 
# TODO refactor multiple downloadFromLIBSVM 


# from the mighty internet
normalize3 <- function(mat) { 
    apply(mat,2,function(x) {xmin <- min(x); 2*(x-xmin)/(max(x)-xmin)-1})
}
 

 
scaleData <- function(fileName, 
                                    type = "sparse", 
                                    transformation = "VARIANCE")
{
    messagef("  Reading data from file %s.", fileName)

    dataset <- matrix()
    if (type == "sparse") {
        dataset <- read.matrix.csr (fileName)
    }

    if (type == "csv") {
        dataset <- read.csv(fileName, sep=",")
    }

    messagef("  Scaling data.")

    if (transformation == "VARIANCE")
    {
        # declare result
        nX = as.matrix(dataset$x)
    
        # scale data, apply scaling columnwise
        for (t in seq(1:ncol(nX)))
        {
            # get column
            curCol = as.matrix(nX[, t])
            u = unique(curCol)
            
            # if its unary, then we do not do anything. this really happens.
            if (length(u) == 1)
            {
                curCol = 0*curCol
            }

            # binary, we scale to 0, 1
            if (length(u) == 2)
            {
                # rescale to 0, 1
                # count the numbers
                u = unique(curCol)
                countA = sum(curCol == u[1])
                countB = sum(curCol == u[2])
                
                if (countA > countB) {
                    tmp = u[2]
                    u[2] = u[1]
                    u[1] = tmp
                } 
                curCol = (curCol-u[2])/(u[1]-u[2])
            }
            
            # anything beyond binary, we scale by its variance
            if (length(u) > 2)
            {
                curCol = curCol/sd(curCol)
            }
            
            # re insert the column
            nX[, t] = curCol
        }
        
        # scale label. we check it is binary and rescale to -1, 1
        curCol = as.matrix(as.numeric(as.character(dataset$y)))
        u = unique(curCol)
        
        # if its binary, then 
        if (length(u) != 2)
        {
            stopf("Labels are not binary!")
        }

        curCol  = normalize3 (curCol)
        ny = curCol
    }
    
    if (transformation == "NORMALIZE")
    {
        nX = normalize3 (as.matrix(dataset$x))
        ny = normalize3 (as.matrix(as.numeric(as.character(dataset$y))))

        # now, we have seen that there are datasets where
        # a whole row is simply missing. this will make problems.
        # we try to remedy the situation by removing Nans
    }
    nX[is.na(nX)]<-0

    return (list("X" = nX, "y" = ny))
}
 

 
 
relabelData <- function(fileName, type = "sparse", posLabels = c(), negLabels = c()) {
  messagef("  Reading data from file %s.", fileName)
  print (paste ("  Positive labels: ", posLabels))
  print (paste ("  Negative labels: ", negLabels))
  
  dataset <- matrix()
  if (type == "sparse") {
    dataset <- read.matrix.csr (fileName)
  }
  
  if (type == "csv") {
    dataset <- read.csv(fileName, sep=",")
  }
  
  messagef("  Relabeling data.")

  # convert the ... first
  dataset$y = (as.numeric(as.character(dataset$y)))
  dataset$x = as.matrix(dataset$x)

  # there is for sure some fancy R thingy, but its ok to do it by hand
  for (i in seq(1, length(dataset$y))) 
  {
    newLabel = -424242424242
    if (dataset$y[i] %in% posLabels) {
        newLabel = 1
    }
    
    if (dataset$y[i] %in% negLabels) {
        newLabel = -1
    }
    
    # sanity check
    if (newLabel == -424242424242) {
        stop ("Converting labels failed. Not all classes are assigned to positive or negative!")
    }
    
    dataset$y[i] = newLabel    
  }  

  return (list("X" = dataset$x, "y" = dataset$y))
}
 

 
# @param path
#   path of the data file
# @param outDir
#   Directory for the output files. Must not contain files named testData.dat
#   and trainData.dat
# @param ratio1
#   Ratio for the size of the training data must sum up to 1
# @param ratio2
#   Ratio for the size of the valid, valid data. ratio1 + ratio2 < 1 must hold
#   and 1 - ratio1 - ratio2 is the ratio of the test data
# @param trainInds
#   Indizes of the training observations. if NULL, these are sample
# @return
#   list with pathes for traindata and testdata

makeTrainTestSplit <- function (path, outDir, ratio1, ratio2, trainInds = NULL) {
  # some tests, if files (not) exists
  if(!file.exists(path))
    stop ("  The combined and scaled file" , path, " does not exist!") 
  if(file.exists(paste(outDir, "testData.dat", sep = "/")))
    stop ("  There is already a testfile in ", outDir) 
  if(file.exists(paste(outDir, "trainData.dat", sep = "/")))
    stop ("  There is already a trainfile in ", outDir) 
  if(file.exists(paste(outDir, "validData.dat", sep = "/")))
    stop ("  There is already a trainfile in ", outDir) 
  
  dataset = read.matrix.csr (path)
  n = nrow(dataset$x)
  ratio = c(ratio1, ratio2, 1 - ratio1 - ratio2)
  if(is.null(trainInds)) {
    sizes = floor(ratio * n)
    inds = sample(unlist(lapply(1:3, function(i) rep(i, sizes[i]))))
  }
  
  createDir (outDir)
  write.matrix.csr(x = dataset$x[inds == 1,], y = dataset$y[inds == 1], 
                   file = paste(outDir, "trainData.dat", sep = "/"))
  write.matrix.csr(x = dataset$x[inds == 2,], y = dataset$y[inds == 2], 
                   file = paste(outDir, "validData.dat", sep = "/"))
  write.matrix.csr(x = dataset$x[inds == 3,], y = dataset$y[inds == 3], 
                   file = paste(outDir, "testData.dat", sep = "/"))
  
  return(list(train = paste(outDir, "trainData.dat", sep = "/"),
              valid  = paste(outDir, "validData.dat", sep = "/"),
              test  = paste(outDir, "testData.dat", sep = "/")
              ))
  
}

 
startRetrieving <- function( datasetName = datasetName )
{
    messagef ("\n\nRetrieving: %s", datasetName)
    createDir (datasetName)
}
 
    
a1aRetrieve <- function(){
    datasetName = 'a1a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('a1a', 'b62d93d768db2423dd06fadb24ea0c1f'),
      c('a1a.t', '4408b51b14837fb378b236462c2e4a5c'))
    retrieveFromRepositoryScaledUniquely (dataFolder = datasetName, rawData, repository = "libSVM")
    #makeTrainTestSplit (datasetName, file.path (paste (datasetName, '.combined.scaled', sep = '')))
} 



adultRetrieve <- function(){
    datasetName = 'adult'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('adult.data', '5d7c39d7b8804f071cdd1f2a7c460872'),
      c('adult.test', '35238206dfdf7f1fe215bbb874adecdc'))
    retrieveFromRepositoryScaledUniquely (dataFolder = datasetName, rawData, repository = "UCI")
    
    print ("Retrieving adult:")
    createDir ("adult")
} 
 


arthrosisRetrieve <- function(){
    datasetName = 'arthrosis'
    startRetrieving (datasetName = datasetName)

    retrieveFromBitbucket (dir = datasetName, 
        fileList = list( c('arthrosis.combined.scaled', '55218e6724ea0f7f9744263578e09822')))
} 

 

australianRetrieve <- function(){
    datasetName = 'australian'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('australian', '6692ebae6911e54f212479b091079456'))
    retrieveFromRepositoryScaledUniquely (dataFolder = datasetName, rawData, repository = "libSVM")
    #makeTrainTestSplit (datasetName, file.path (paste (datasetName, '.combined.scaled', sep = '')))   
} 

 
 
codrnaRetrieve <- function(){
    # load the binary covtype from libsvm (instead of the multiclass from UCI)
    datasetName = 'cod-rna'
    startRetrieving (datasetName = datasetName)
    
    #rawData <- list( c('covtype.data.gz', '99670d8d942f09d459c7d4486fca8af5'))
    #retrieveFromRepositoryScaled (datasetName, rawData, repository = "UCI", serverpath = 'covertype-mld' )
    
    rawData <- list( c('cod-rna', 'cd14eab76f391e288e6e8e592ae658dd'), 
        c('cod-rna.t', 'b876c22c86a33ef262342b12d2718655'),
        c('cod-rna.r', '65205a3cd47cfad08679aca12c209af7'))
    retrieveFromRepositoryScaledUniquely (dataFolder = datasetName, rawData, repository = "libSVM")
    
} 


 
covtypeRetrieve <- function(){
    # load the binary covtype from libsvm (instead of the multiclass from UCI)
    datasetName = 'covtype'
    startRetrieving (datasetName = datasetName)
        
    rawData <- list( c('covtype.libsvm.binary.bz2', '0d3439b314ce13e2f8b903b12bb3ea20'))
    retrieveFromRepositoryScaledUniquely (dataFolder = datasetName, rawData, repository = "libSVM")
} 



epsilonRetrieve <- function(){
    # load the binary covtype from libsvm (instead of the multiclass from UCI)
    datasetName = 'epsilon'
    startRetrieving (datasetName = datasetName)
        
        # DO NOT SCALE THIS BEAST!!
        
    rawData <- list( c('epsilon_normalized.bz2', 'aabcbb28ed850672d65247b41644bacc'),
        c('epsilon_normalized.t.bz2', '56745f5415d30bcc44f423c7c32477c9'))
    combinedFilename = retrieveFromRepository (datasetName, rawData, repository = "libSVM")
    
    # rename it to scaled, pretend its ok
    scaledCombinedFilename = paste(combinedFilename, ".scaled", sep = "")
    file.rename (combinedFilename, scaledCombinedFilename)
} 



heartRetrieve <- function(){
    datasetName = 'heart'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('heart', '9033ec466e09433636f5eee3e36610cd'))
    retrieveFromRepositoryScaledUniquely (dataFolder = datasetName, rawData, repository = "libSVM")
}



ionosphereRetrieve <- function(){
    datasetName = 'ionosphere'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('ionosphere_scale', '5dae06d1367d5b4fe7d96865a5b16bb7'))
    retrieveFromRepositoryScaledUniquely (dataFolder = datasetName, rawData, repository = "libSVM")
}



kddcup99Retrieve <- function(){
    # COMPLETELY BROKEN?
    print ("Retrieving kddcup99-mld:")
    createDir ("kddcup99-mld")
    downloadFromRepository ('kddcup.data_10_percent.gz', 'kddcup99-mld', 'c421989ff187d340c1265ac3080a3229', repository = "UCI")
    extract('kddcup.data_10_percent.gz', 'kddcup99-mld', 'kddcup.data_10_percent', 'eb43ac454e61166f88aad7943ef1e079')

    downloadFromRepository ('kddcup.data.gz', 'kddcup99-mld', '3745289f84bdd907c03baca24f9f81bc', repository = "UCI")
    extract('kddcup.data.gz', 'kddcup99-mld', 'kddcup.data', '13f75aac4e6fb218b8b2e685fb1c637c')

    downloadFromRepository ('kddcup.testdata.unlabeled_10_percent.gz', 'kddcup99-mld', '4927a02f9ad8c4de66bb573075c6babb', repository = "UCI")
    extract('kddcup.testdata.unlabeled_10_percent.gz', 'kddcup99-mld', 'kddcup.testdata.unlabeled_10_percent', '426758eedb2c9182f4f8f76fce5ff5fd')

    downloadFromRepository ('kddcup.testdata.unlabeled.gz', 'kddcup99-mld', '33767489af37228520007ea525bc4444', repository = "UCI")
    extract('kddcup.testdata.unlabeled.gz', 'kddcup99-mld', 'kddcup.testdata.unlabeled', 'e552ee9a6b3dc415daee1edbb97c779b')
} 



gisetteRetrieve <- function() {
    datasetName = 'gisette'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('gisette_scale.bz2', '8a8caa1628c98dafec8d5d7bfa67c20b'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")
}



ijcnn1Retrieve <- function() {
    datasetName = 'ijcnn1'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('ijcnn1.bz2', 'c3e9162f8e38e0b6c0e02d313a59de2b'),
#      c('ijcnn1.tr.bz2', '9889c2e9d957dca5304ed2d285f1be6d'),
      c('ijcnn1.t.bz2', '66433ab8089acee9e56dc61ac89a2fe2'))
    retrieveFromRepositoryScaledUniquely (dataFolder = datasetName, rawData, repository = "libSVM")
}



kddcup08Retrieve <- function () {
    datasetName = 'kddcup08'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('test.zip', '5951f5c728627dfacba72830ea919c3e'), 
         c('training.zip', 'afcd57fcdd8b38ae860aad73624de360'), 
         c('test_data.zip', '00498ae34ae388eaf4b4d23ce62b5b27'), 
         c('training_data.zip', '65aae7afe3eecad95768cc07a693c90f') )
    retrieveFromRepositoryScaled (datasetName, rawData, 
        repository = "http://idb.csie.ncku.edu.tw/tsengsm/course/DM/dataset/KDDCup08/")
}



mushroomsRetrieve <- function() {
    datasetName = 'mushrooms'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('mushrooms', '2a52251de564d72e772b25211a84f5f8'))
    retrieveFromRepositoryScaledUniquely (dataFolder = datasetName, rawData, repository = "libSVM")
}



mnistRetrieve <- function() {
    datasetName = 'mnist'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('mnist.bz2', '1eca9ecabee216ae06b3e2f811f4cfd7'), 
        c('mnist.t.bz2', 'a4aafe182113f147e3068d37760ece9d') )
    retrieveFromRepositoryScaledUniquely (dataFolder = datasetName, rawData, repository = "libSVMMultiClass",
        posLabels = c(0, 3, 6, 8, 9), negLabels = c(1, 2, 4, 5, 7)) 
}



pokerRetrieve <- function() {
    # WE DO NOT UNIQUEIFY THIS, as the duplication is only very mild (0.2%)
    datasetName = 'poker'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('poker.bz2', 'd21b1be3f70d52c1cead7ef6ea385cae'), 
        c('poker.t.bz2', 'fa949e243f568a35a100ffa836a05276') )
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVMMultiClass",
        posLabels = c(0), negLabels = c(1, 2, 3, 4, 5,6,7,8,9)) 
}



proteinRetrieve <- function() {
    datasetName = 'protein'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( #c('protein.bz2', '13c4c05fe7bde951c6b3badbf549f0bf'), 
        c('protein.t.bz2', 'bf94d04967234fc1f8ebf534e7678bb6'), 
        c('protein.tr.bz2', 'ff0b0904aa0954ff620e5429e96b4790'), 
        c('protein.val.bz2', '10b0c58a0ecf47ea045f53e560841740') )
    retrieveFromRepositoryScaledUniquely (dataFolder = datasetName, rawData, repository = "libSVMMultiClass", dataProcessor = "removeSpaces",
        posLabels = c(0), negLabels = c(1, 2)) 
}



shuttleRetrieve <- function() {
    datasetName = 'shuttle'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( #c('shuttle.scale', 'e409df1a6ba0808fc710799748a99b9b'), 
        c('shuttle.scale.t', '3074dd2a092ce65ecaa6e46560a72c3b'), 
        c('shuttle.scale.tr', 'bcb7de7c0ea92ee76995bd2ceb01487e'), 
        c('shuttle.scale.val', 'c2790238a3c9c95038e03b9fde19621a') )
    retrieveFromRepositoryScaledUniquely (dataFolder = datasetName, rawData, repository = "libSVMMultiClass",
        posLabels = c(1), negLabels = c(2,3,4,5,6,7))
}



spektrenRetrieve <- function(){
    datasetName = 'spektren'
    startRetrieving (datasetName = datasetName)

    retrieveFromBitbucket (dir = datasetName, fileList = 
        list( c('spektren.combined.scaled', '9961a9c546931fb138c6af7691e1411b')))
} 



vehicleRetrieve <- function() {
    datasetName = 'vehicle'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('combined.bz2', '5716b20b0a3817429fc52ecb356f2897'), 
        c('combined.t.bz2', '966abb2dfc7c82836fa3b55effd537f4') )
    retrieveFromRepositoryScaledUniquely (dataFolder = datasetName, rawData, repository = "libSVMMultiClass", serverpath = datasetName,
            posLabels = c(1,2), negLabels = c(3))

}



wXaRetrieve <- function() {
    datasetName = 'w1a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('w1a', '3d49d7d2ab90609d811d0af4c14642d9'),
      c('w1a.t', '45bc6b84b7f129c04b9d3ce9c1cd1a7f'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")

    
    datasetName = 'w2a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('w2a', '7c6d5479821194f754f8b8c7940cdcf3'),
      c('w2a.t', '9bafacce3b447bfdb855ab9905b070a2'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")

    
    datasetName = 'w3a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('w3a', '2ec1c9fab345a1bf340a78115ae2a1fc'),
      c('w3a.t', '020faf0091034ce5e7f4867718286d20'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")

    
    datasetName = 'w4a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('w4a', '7e9bfdd301df4ea8d6dde56268d9d184'),
      c('w4a.t', '5428da110a388172e6a77312cecebd3d'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")

    
    datasetName = 'w5a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('w5a', 'a10d17ef020da4172869a00529ef28e7'),
      c('w5a.t', 'f79162c9c7377067dc86d6452dd421fa'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")

    
    datasetName = 'w6a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('w6a', '984e89a34cd492cfa8c3495e4eaa8e5e'),
      c('w6a.t', '2f0ac7bc60616a27448799c0efe15eb9'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")

    
    datasetName = 'w7a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('w7a', 'be1685637ca070887c23eb598dcb26b5'),
      c('w7a.t', '99e242af7bc0d71abb2e934756a13be6'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")


    datasetName = 'w8a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('w8a', '05cdbfad8de0af3887c0e6f42fddc32b'),
      c('w8a.t', '938543adbdc198246247d5d2015f01e8'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")

    # todo: create own data repro add this there
    filename = file.path("wXa", "wXa.combined.scaled")
    if (!file.exists ( filename ))
    {
        messagef("\nCreating wXa at %s", filename)
        dir.create(file.path(".", "wXa"), showWarnings = FALSE)
        tmp = tempfile()
        system3 ("cat", c ("w1a/w1a.combined.scaled", "w2a/w2a.combined.scaled",
        "w3a/w3a.combined.scaled", "w4a/w4a.combined.scaled", "w5a/w5a.combined.scaled",
        "w6a/w6a.combined.scaled", "w7a/w7a.combined.scaled", "w8a/w8a.combined.scaled",
        "|", "sort", "|", "uniq", ">", tmp))
        system3 ("cat", c (tmp, "|", "sort", "|", "uniq", ">", "wXa/wXa.combined.scaled"))
    }
    else
    {
        messagef("\nwXa exists, not recreating it.")
    }
}



aXaRetrieve <- function() {
    datasetName = 'a1a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('a1a', 'b62d93d768db2423dd06fadb24ea0c1f'),
      c('a1a.t', '4408b51b14837fb378b236462c2e4a5c'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")

    
    datasetName = 'a2a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('a2a', '1328dfba5eee42f878af59e41fa414cf'),
      c('a2a.t', 'dddd345aa4bbcdea8493e941984d3767'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")

    datasetName = 'a3a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('a3a', '389aa51d762eac0fb2d0ebe9a8b56aef'),
      c('a3a.t', '94645189858233568c0c35beb4a0c681'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")

    
    datasetName = 'a4a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('a4a', 'ce53c338007a0ad8e7021186cc259b07'),
      c('a4a.t', '9603c250270fa627d9cb9b7adbdacad0'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")

    
    datasetName = 'a5a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('a5a', '556ff24aafeb30297763c0dcb0feeb4f'),
      c('a5a.t', 'dfd287c8f2b7142d224b46fd3edaff31'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")

    
    datasetName = 'a6a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('a6a', '02abf77023749b83eb5cfb1ad60969cc'),
      c('a6a.t', '1b71168068d9cf198408ac839d9f3b1a'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")

    datasetName = 'a7a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('a7a', '3285a272a2f5c5e343c74c9394cbac6c'),
      c('a7a.t', '469c6e0a99efaac458e59106dade2311'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")


    datasetName = 'a8a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('a8a', '247737ca98b22d3186c0c0db33d5f006'),
      c('a8a.t', '965fee39cd01a554231bcd86a4235369'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")

    
    datasetName = 'a9a'
    startRetrieving (datasetName = datasetName)
    
    rawData <- list( c('a9a', 'fb3596510d780aaf76a06dc721d49662'),
      c('a9a.t', 'f0bb16cf1ee3e80ff560e9d132ee647e'))
    retrieveFromRepositoryScaled (datasetName, rawData, repository = "libSVM")

    
    
    # todo: create own data repro add this there
    filename = file.path("aXa", "aXa.combined.scaled")
    if (!file.exists ( filename ))
    {
        messagef("\nCreating aXa at %s", filename)
        dir.create(file.path(".", "aXa"), showWarnings = FALSE)
        tmp = tempfile()
        system3 ("cat", c ("a1a/a1a.combined.scaled", "a2a/a2a.combined.scaled",
        "a3a/a3a.combined.scaled", "a4a/a4a.combined.scaled", "a5a/a5a.combined.scaled",
        "a6a/a6a.combined.scaled", "a7a/a7a.combined.scaled", "a8a/a8a.combined.scaled", "a9a/a9a.combined.scaled",
        "|", "sort", "|", "uniq", ">", tmp))
        system3 ("cat", c (tmp, "|", "sort", "|", "uniq", ">", "aXa/aXa.combined.scaled"))
    }
    else
    {
        messagef("\naXa exists, not recreating it.")
    }
}


smallRetrieve <- function() {
    print ("Retrieving small datasets:")
    australianRetrieve()
    ionosphereRetrieve() # 351x34
    mushroomsRetrieve()
}


allRetrieve <- function(){
    print ("Retrieving all datasets:")

    # libsvm binary
    aXaRetrieve()
    codrnaRetrieve()
    covtypeRetrieve()
 #   epsilonRetrieve()
    ijcnn1Retrieve()
    wXaRetrieve()
#    kddcup08Retrieve()
    
    # libsvm multi
    mnistRetrieve()
    pokerRetrieve()
    proteinRetrieve()
    shuttleRetrieve()
    vehicleRetrieve()
    
    # our own data
    arthrosisRetrieve()
    spektrenRetrieve()
} 


spektrenPrepare <- function () 
{
    # spektren has two files,

    # one containing labels the other the data 
    dataset <- read.csv2("spektren/Datensatz.txt", sep="\t", header=FALSE)
    labels <- read.csv2("spektren/DatensatzLabel.txt", sep="\t", header=FALSE)
    dataset = sapply(dataset, function(x) { as.numeric(as.character(x))})
    labels = sapply(labels, function(x) { as.numeric(as.character(x))})
    combinedFilename = tempfile()
    write.matrix.csr(dataset, combinedFilename, labels)
    
    # if we are in binary case, we can just take the combined filename as base
    scaledCombinedFilename = paste("spektren/spektren.combined.scaled", sep = "")
    
    # now we need to scale that one file
    if (!file.exists (scaledCombinedFilename)) 
    {
        messagef("  Scaling %s into %s", combinedFilename, scaledCombinedFilename)
        sData = scaleData (combinedFilename)
        messagef("  Writing scaled data into %s", scaledCombinedFilename)
        write.matrix.csr(sData$X, scaledCombinedFilename, sData$y)
    } 
    else {
        messagef("  Scaled and combined file %s exists.", scaledCombinedFilename)
    }
    
}



arthrosisPrepare <- function () 
{
    # spektren has two files,

    # one containing labels the other the data 
    dataset <- read.table("arthrosis/knee.csv", sep=",", header=FALSE)

    # need to do this stupidly because of memory problems
    
    dataset = sapply(dataset, function(x) { as.numeric(as.character(x))})
    
#    for (i in seq(1:nrow(dataset))) {
  #      for (j in seq(1:ncol(dataset))) {
    #        dataset[i,j] = as.numeric(as.character(dataset[i,j]))
   #     }
    #}
    
    combinedFilename = tempfile()
    write.matrix.csr(dataset[,1:ncol(dataset)-1], combinedFilename, dataset[,ncol(dataset)])
    
    # if we are in binary case, we can just take the combined filename as base
    scaledCombinedFilename = paste("arthrosis/arthrosis.combined.scaled", sep = "")
    
    # now we need to scale that one file
    if (!file.exists (scaledCombinedFilename)) 
    {
        messagef("  Scaling %s into %s", combinedFilename, scaledCombinedFilename)
        sData = scaleData (combinedFilename)
        messagef("  Writing scaled data into %s", scaledCombinedFilename)
        write.matrix.csr(sData$X, scaledCombinedFilename, sData$y)
    } 
    else {
        messagef("  Scaled and combined file %s exists.", scaledCombinedFilename)
    }
}


convertToArff <- function () {
    # parse through all .combined.scaled data
    path = "."
    folders = lsdir(path, all = FALSE, recursive = FALSE)

    rowForTable = list()
    for (folder in folders) {
        # throw out the a.a/w.a-datas which have been processed into aXa, wXa already
        if  ( (grepl ("w\\da", folder) == TRUE) || (grepl ("a\\da", folder) == TRUE) )
        {
            messagef ("Skipping folder %s", folder)
            next
            continue
        }    
        files <- list.files (file.path (path, folder), pattern='*\\.combined.scaled')
        for (dataFile in files) {
            # read file
            currentDataFileName = file.path(path, folder, dataFile)
            messagef ("Processing %s", currentDataFileName)

            # write as arff
            arffFileName = gsub ("combined.scaled", "arff", currentDataFileName )
            messagef ("  Writing as %s", arffFileName)
                        
            # now we would like to dump header first,
            # but for that we need to know how many features there are.
            # that is not 'trivial' for a sparse set.
            # we need to examine every line to know this.
            # as we read every line anyway the idea is first to
            # convert the data to sparse arff format. count the number
            # of feature meanwhile and after that write the corresponding 
            # header. then merge these two things to get the final file.
                        
            # write header  now
            tmpData = tempfile()            
            arffFileHandle <- file(tmpData, open = "w+")
            
            writeLines(paste ("", sep = ""), arffFileHandle)
            writeLines(paste ("@DATA", sep = ""), arffFileHandle)
            
            # read line by line
            con  <- file(currentDataFileName, open = "r")
            
            maxDim = 0
            while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
                # update number of features
                curDim = countEntries(oneLine)
                maxDim = max(maxDim, curDim[2])

                # convert line by removing 
                oneLine = gsub (" ", ", ", oneLine)
                oneLine = gsub (":", " ", oneLine)
                oneLine = gsub (", $", "", oneLine, perl = TRUE)
                
                # now we assume here (as later on) that the features start with 1!
                # so the 0th index is empty-- this is the ideal place to hide the label,
                # a line like -1 2:4 6:5 then becomes 0 -1, 2 4, 6 5-- and 0 is the label attribute. 
                oneLine = paste ("{0 ", oneLine, "}", sep = "")
                writeLines(oneLine, arffFileHandle)
            }

            # features are given by the maximum of the dimension
            nFeatures = maxDim

            # close the files
            close(arffFileHandle)
            close(con)
            
                                    
            # write header  now
            tmpHeader = tempfile()            
            arffFileHandle <- file(tmpHeader, open = "w+")
            writeLines(paste ("% Dataset ", folder,  sep = ""), arffFileHandle)
            writeLines(paste ("%", sep = ""), arffFileHandle)
            writeLines(paste ("% This data file was automatically converted from sparse data format.", sep = ""), arffFileHandle)
            writeLines(paste ("% It was used in the large scale SVM experiments (http://www.largescalesvm.de)", sep = ""), arffFileHandle)
            writeLines(paste ("% Original data has been normalized columnwise according to the following rules:", sep = ""), arffFileHandle)
            writeLines(paste ("% If a column only contains one value (constant feature), it will set to zero and thus removed by sparsity.", sep = ""), arffFileHandle)
            writeLines(paste ("% If a column contains two values (binary feature), the value occuring more often will be set to zero, the other to one.", sep = ""), arffFileHandle)
            writeLines(paste ("% If a column contains more than two values (multinary/real feature), the column is divided by its std deviation.", sep = ""), arffFileHandle)
            writeLines(paste ("%", sep = ""), arffFileHandle)
            writeLines(paste ("", sep = ""), arffFileHandle)
            writeLines(paste ("@RELATION ", folder, sep = ""), arffFileHandle)
            writeLines(paste ("", sep = ""), arffFileHandle)
            writeLines(paste ("@ATTRIBUTE Y {1, -1}", sep = ""), arffFileHandle)

            # i guess we assume here that we do not have features like 0:blabla. it starts with 1:
            for (i in 1:nFeatures) {
                writeLines(paste ("@ATTRIBUTE X", i, " numeric", sep = ""), arffFileHandle)
            }
            
            close(arffFileHandle)

            # finally do the join by bash command, should be faster
            system3 ("cat", c (tmpHeader, tmpData, ">", arffFileName) )          
        } 
    }
    return (1)
}




# needs all datasets to  be downloaded
# will convert all XX.combined.scaled to XX.arff
#
convertToArffBatch <- function () {
    # parse through all .combined.scaled data
    path = "."
    folders = lsdir(path, all = FALSE, recursive = FALSE)

    rowForTable = list()
    for (folder in folders) {
        # throw out the wXa-hack.
        if  ( (grepl ("w\\da", folder) == TRUE) || (grepl ("a\\da", folder) == TRUE) )
        {
            messagef ("Skipping folder %s", folder)
            next
            continue
        }    
        files <- list.files (file.path (path, folder), pattern='*\\.combined.scaled')
        for (dataFile in files) {
            # read file
            currentDataFileName = file.path(path, folder, dataFile)
            messagef ("Processing %s", currentDataFileName)
            dataset <- read.matrix.csr (currentDataFileName)
            
            # write as arff
            arffFileName = gsub ("combined.scaled", "arff", currentDataFileName )
            messagef ("  Writing as %s", arffFileName)

            # create 'custom' colnames
            colNames = sapply( (1:(dim(dataset$x)[2])), FUN = function(x) { paste("X",x, sep = "") })
            dataframe = as.data.frame(as.matrix(dataset$x))
            colnames(dataframe) <- colNames
            dataframe$Y = dataset$y
            
            write.arff (dataframe, arffFileName)
        } 
    }
    return (-1)
}








argumentParser <- function() {
    library(RWeka)

    options <- commandArgs(trailingOnly = TRUE)
    if (length(options) == 0L) {
        stop("No dataset to download is given.")
    }
    
    for (i in options) {
        # remove whites
        trim <- function (x) gsub("^\\s+|\\s+$", "", x)
        
        # check if it contrains prepare 
        t = (grep ("prepare", trim(i), ignore.case = TRUE))
        if (is.null(t))
        {
            do.call (trim(i), list())
        }
        else
        {
            # just stupidly try to call the function
            result = tryCatch({
                do.call (trim(i), list() )
                }, warning = function(w) {}
                , error = function(e) {
                    if (grepl ("could not find function", e) == TRUE)
                        do.call (paste(trim(i),sep = "", 'Retrieve'), list())
                    else print(e)
                }, finally = {}
            ) 
        }
    }
}



# stupid R check for pythons cool "name == __main__"
if (length(sys.frames()) == 0) 
{
    argumentParser()
}

