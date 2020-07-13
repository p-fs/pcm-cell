library(XML)
library("EBImage")
library("ggplot2")
library("ggtree")
library("ape")
mainDir <- "C:\\Users\\Pau Font\\OneDrive\\pau\\upc\\TFG\\project\\data"
pathname = "C:\\Users\\Pau Font\\OneDrive\\pau\\upc\\TFG\\project\\data\\090303-C2C12P15-FGF2,BMP2\\exp1_F0001 Data"
annotationfilename = "C:\\Users\\Pau Font\\OneDrive\\pau\\upc\\TFG\\project\\data\\090303-C2C12P15-FGF2,BMP2\\Annotation_Computer\\Computer exp1_F0001 Data.xml"
xdatatree <- xmlParse(annotationfilename)

createSubDirectory <- function(path, name) { #check if subdirectory exists #if it doesn't, it will create #one using x as the name
  if (file.exists(file.path(path, name))){
    print(paste("SubDirectory already exists: ", file.path(path, name))) } else { dir.create(file.path(path, name))
      print(paste("SubDirectory created: ", file.path(path, name))) } }


getLineageCentricInfo <- function(node) { #get cell info
  cellID <- xmlGetAttr(node, "id", NA) #get cell id
  parentcellID <- xmlGetAttr(xmlParent(xmlParent(node)), "id", NA) #get parent cell id, which is grandparent of node. #direct or immediate parent is an annotation structure or as
  child_ss <- node[c("ss")] 
  #get cell states 
  cellfirstframe <- xmlAttrs(child_ss[[1]][[1]]) 
  #get cell first frame attributes 
  celllastframe<- xmlAttrs(child_ss[[1]][[(xmlSize(child_ss[[1]]))]]) 
  #get cell last frame attributes 
  as_sslength <- xmlSize(node) 
  #determine size of xml node, this determines if there are daughter cells #(xmlSize == 2) or not (xmlSize == 1) 
  if (as_sslength == 1) {
    child_as <- NA #cell with no daughters, no need to get daughter ID 
    print("No daughter cells") 
    daughtercellID <- NA
  }
  
  else if (as_sslength == 2) {
    child_as <- node[c("as")]
    daughtercellnumber <- xmlSize(node[c("as")][[1]])
    print(paste(daughtercellnumber, " daughter cells"))
    daughtercellID <- vector("list", daughtercellnumber)
    for (i in 1:daughtercellnumber) {
      tempid <- xmlGetAttr(child_as[[1]][[i]], "id", NA)
      daughtercellID[[i]] <- tempid
    }
  }
  else if (as_sslength > 2) {
    child_as <- NA 
    print("Error: as_sslength > 2")
    daughtercellID <- "Error: as_sslength > 2"
  }
  
  mat <- list(cellID = cellID, parentcellID = parentcellID, cellfirstframe = cellfirstframe, celllastframe = celllastframe, child_ss = child_ss, daughtercellID = daughtercellID, as_sslength = as_sslength)
  names(mat) <- c("cellID", "parentcellID", "cellfirstframe", "celllastframe", "child_ss", "daughtercellID", "as_sslength")
  
  return(mat)
}

getFrameCentricInfo <- function(node) { #get frame info
  cellnode <- xmlParent(xmlParent(node)) #get cell node, which is grandparent of node. #direct or immediate parent is the cell state structure or ss
  cellID <- xmlGetAttr(cellnode, "id", NA) #get cell id #child_ss <- cellnode[c("ss")] #get cell states #cellfirstframe<- xmlAttrs(child_ss[[1]][[1]]) #get cell first frame attributes #celllastframe<- xmlAttrs(child_ss[[1]][[(xmlSize(child_ss[[1]]))]]) #get cell last frame attributes 
  xcoord<- as.numeric(xmlGetAttr(node, "x", NA)) #get cell x co-ordinate attributes 
  ycoord<- as.numeric(xmlGetAttr(node, "y", NA)) #get cell y co-ordinate attributes 
  cellstatus<- as.numeric( xmlGetAttr(node, "s", NA)) #get cell status attributes
  cellColour <- xmlGetAttr(cellnode, "brush", NA) #get cell colour 
  mat <- list(cellID = cellID, xcoord = xcoord, ycoord = ycoord, cellstatus = cellstatus, cellColour = cellColour, cellnode = cellnode) 
  names(mat) <- c("cellID", "xcoord", "ycoord", "cellstatus", "cellColour" , "cellnode") 
  return(mat)
}

generate_lineage_centric_info <- function(xtree) { #create lineage-centric #info
  lineage_centric_annotations <- xpathApply(xtree, ".//a",
                                            getLineageCentricInfo) #create list for lineage centric annotations
  names(lineage_centric_annotations) <- sapply(lineage_centric_annotations,
                                               get, x="cellID") #name lists in lineage centric annotations according to cell ID
  return(lineage_centric_annotations)
}
generate_frame_centric_info <- function(xtree, path) {#create frame-centric #info
  frame_centric_annotations <- vector("list", length(list.files(path,
                                                                pattern = "*.tif"))) #create empty list for lineage centric annotations
  for (i in 1:length(list.files(path, pattern = "*.tif"))){ #because start of timelapse (time 0) is frame 1
    print(paste("Processing frame number: ", (i-1)))
    frame_centric_annotations[[i]] <- xpathApply(xtree, paste(".//s[@i=",
                                                              (i-1),"]", sep=""), getFrameCentricInfo) #get more information but takes longer computation #frame_centric_annotations[[i]] <- xpathApply(xtree, path, # getFrameCentricInfoNodeOnly) #get less information but with (hopefully) shorter computation 
  } #fill list with lineage-centric data #setNames(frame_centric_annotations, #paste(0:(length(list.files(path, pattern = "*.tif"))-1))) #set the names by frame number but did not run as computationally #slow
  return(frame_centric_annotations)
}

separate_xtree_by_fannotation_nodes <- function(xtree) {
  f_annotation_documents <- xpathApply(xtree, "//f")
  print(paste("Number of f annotation documents: ", length(
    f_annotation_documents)))
  return(f_annotation_documents)
}

resizeTiffandReadasEBImage <- function(path, scalefactor) { #resize tiff and
  filename <- list.files(path, pattern = "*.tif")
  displayscalefactor <- scalefactor
  createSubDirectory(path, "tiff")
  setwd(file.path(path))
  for (i in 1:numberofframes) {
    tiff_var<-readImage(file.path(path, filename)[i])
    tiff_var <- resize(tiff_var, w = round((dim(tiff_var)[1] *
                                              displayscalefactor)), h = round((dim(tiff_var)[2] *
                                                                                 displayscalefactor)))
    writeImage(tiff_var, (file.path("tiff", filename)[i]),
               type = "tiff", 8, compression = "LZW")
    print(paste("Processing frame number: ", (i-1)))
  }
  mat<-readImage(file.path(path,"tiff",list.files(file.path(path,"tiff"),
                                                  pattern = "*.tif")))
  return(mat)
}

plotandSaveAllFrameAnnotationsPNG <- function(frames, path, scalefactor) {
  createSubDirectory(path, "png")
  setwd(file.path(path))
  filename <- list.files(path, pattern = "*.tif")
  xdim <- round(dim(readImage(file.path(path,
                                        filename[1])))*scalefactor)[1]
  ydim <- round(dim(readImage(file.path(path,
                                        filename[1])))*scalefactor)[2]
  filename <- gsub("tif", "png", filename)
  setwd(file.path(path, "png"))
  for (i in 1:length(frames)) {
    plotandSave1FrameAnnotationPNG(frames[[i]], fn = filename[i], xdim
                                   = xdim, ydim = ydim, scalefactor =
                                     scalefactor)
  }
}
plotandSave1FrameAnnotationPNG <- function(cells_in_1frame, fn, xdim, ydim,
                                           scalefactor) {
  png(filename = fn, bg = NA, width = xdim, height = ydim, units = "px",
      pointsize = 6, res = NA)
  par(bg = NA, xpd = NA, mar = c(0, 0, 0, 0), usr = c(0, 1, 0, 1), xaxs =
      "i", yaxs = "i")
  plot(x = (xdim / scalefactor), y = (ydim / scalefactor), type = "n", asp
       = 1, axes = FALSE, ann = FALSE, xlim = c(0, (xdim / scalefactor)),
       ylim = c((ydim / scalefactor), 0), bg = NA)
  lapply(cells_in_1frame, plot1CellAnnotation)
  print(fn)
  dev.off()
}

plot1CellAnnotation <- function(cell) { #read in and plot 
  text(x = round(cell[["xcoord"]]), y = round(cell[["ycoord"]]), label = cell[["cellID"]], adj = c(0,1), col = cell[["cellColour"]], cex = 1.0) 
  points(x = round(cell[["xcoord"]]), y = round(cell[["ycoord"]]), pch = 20, col = cell[["cellColour"]], cex = 1.0) 
}

plotTiffwithAnnotations <- function(frames, path) {
  createSubDirectory(path, "annotated")
  setwd(file.path(path))
  filename <- list.files(file.path(path), pattern = "*.tif")
  for (i in 1:length(frames)) {
    plotandSave1FrameAnnotationTIFF(frames[[i]], path = path, filename
                                    = filename[i])
  }
}
plotandSave1FrameAnnotationTIFF <- function(cells_in_1frame, path, filename)
{
  setwd(file.path(path))
  print(filename)
  tiff_var<-readImage(file.path(path, filename))
  display((tiff_var+ 0.005) * 15, method = "raster")
  lapply(cells_in_1frame, plot1CellAnnotation)
  setwd(file.path(path, "annotated"))
  dev.print(tiff, filename = filename, width = dim(tiff_var)[1], height = dim(tiff_var)[2])
  dev.off() 
  }
  

getRootCell <- function(cell) { #returns the root cell
  if (!is.na(cell[["parentcellID"]] == "NA")) {
  } else {
    return(cell) #return root cell
  }
}

getCellLifeTime <- function(cell) { #calculate cell life time for each cell
  cellID <- cell[["cellID"]]
  cellfirstframe <- as.numeric(cell[["cellfirstframe"]][["i"]])
  celllastframe <- as.numeric(cell[["celllastframe"]][["i"]])
  celllifetime <- (celllastframe - cellfirstframe)
  return(celllifetime)
}


getNewickTree <- function(cell, celllist, newickString) { #recursive function
  if (is.na(cell[["parentcellID"]])) {
    newickString <- paste((cell[["cellID"]]), ":", getCellLifeTime(cell),
                          ";", sep = "")
  } else {
  }
  if (!is.na(cell[["daughtercellID"]])) { #recursively finds daughter cells
    daughtercell <- cell[["daughtercellID"]]
    parentcell <- cell[["cellID"]]
    replacementString <- paste("( )", parentcell, ":",
                               getCellLifeTime(cell), sep = "")
    daughterString <- character(0)
    for (i in 1:length(daughtercell)) {
      currentdaughtercell <- celllist[[(daughtercell[[i]])]]
      daughterString <- paste(daughterString,
                              currentdaughtercell[["cellID"]], ":",
                              getCellLifeTime(currentdaughtercell), ","
                              , sep = "")
    }
    daughterString <- paste(sub(",$", "", daughterString))
    replacementString <- paste(sub(" ", daughterString,
                                   replacementString))
    findparentcell <- paste(parentcell, ":", getCellLifeTime(cell), sep =
                              "")
    newickString <- paste(sub(findparentcell, replacementString,
                              newickString))
    for (i in 1:length(daughtercell)) {
      newickString <- getNewickTree(celllist[[daughtercell[[i]]]],
                                    celllist, newickString)
    }
  } else {
  }
  return(newickString)
}

generate_root_cell_info <- function(lineage_centric_list) { #create list of
  rootCellsList <- vector("list", length(lineage_centric_list))
  for (i in 1:length(lineage_centric_list)) {
    annotationlist <- lineage_centric_list[[i]]
    rootCells <- lapply(annotationlist, getRootCell)
    rootCells[sapply(rootCells, is.null)] <- NULL
    rootCellsList[[i]] <- rootCells
  }
  names(rootCellsList) <- names(lineage_centric_list)
  return(rootCellsList)
}
generate_newick_info <- function(root_cell_list, lineage_centric_list) {
  newickStringList <- vector("list", length(root_cell_list))
  for (i in 1:length(lineage_centric_list)) {
    annotationlist <- lineage_centric_list[[i]]
    newickString <- character(0)
    newickString <- lapply(annotationlist, getNewickTree, annotationlist,
                           newickString)
    newickString <- Filter(length, newickString)
    names(newickStringList) <- names(lineage_centric_list)
  }
  return(newickStringList)
}

sep_xdatatree <- separate_xtree_by_fannotation_nodes(xdatatree)
names(sep_xdatatree) <- xpathApply(xdatatree, "//f", xmlGetAttr, "name")
lineage_centric_data <- lapply(sep_xdatatree, generate_lineage_centric_info)    
frame_centric_data <- lapply(sep_xdatatree, generate_frame_centric_info,
                             pathname)

imagesequence <- resizeTiffandReadasEBImage(pathname, 0.4)
display((imagesequence + 0.005) * 15, method = "browser")
plotandSaveAllFrameAnnotationsPNG(frame_centric_data[["Cells"]], pathname, 0.4)
plotTiffwithAnnotations(frame_centric_data[["Cells"]], pathname)


root_cell_data <- generate_root_cell_info(lineage_centric_data)
newicktree_data <- generate_newick_info(root_cell_data, lineage_centric_data)
    
x <- read.tree(text = newicktree_data[[1]][[1]])
ggtree(x, layout = "rectangular") + geom_nodepoint(color = "#b5e521", alpha =
                                                     0.9, size = 6) + geom_tippoint(color = "#FDAC4F", alpha = 0.9, size = 6) + geom_rootpoint(color = "#d3d3d3", alpha = 1.0, size = 8) + geom_nodelab(aes(x =
                                                                                                                                                                                                              branch, label = label), size = 3, color = "blue", vjust = -0.15) + geom_tiplab(aes(x = branch, label = label), size = 3, color = "blue", vjust = -0.15) + theme_tree2()
    
    
  
  
