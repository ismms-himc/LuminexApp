#' read lmx raw file to create a summarizedexperiment
#' >HOD is replaced by Inf, <LOD is replaced by -Inf;
#' retrieve STD3, and the lowest, the hightest STD's MFI, Conc.
#' (correlated to LLOD and HLOD and matching MFI)
#' refer to inhouse LuminexTool package 
read_lmx <- function(f, lot = "default", analyt_start_row){
  
  if(grepl("xls$", f)){
    fdata <- read_xls(f, sheet = 1, col_names = T)
  }
  else if(grepl("xlsx$", f)){
    fdata <- read.xlsx(f, sheetName = "Summary", header = F)
  }
  else{
    stop("Wrong input file type!")
  }
  
  fdata <- fdata[1 : (which(fdata[ , 1] == "Notes:")-1), ]
  
  coldata <- data.frame(Location = unlist(fdata[-c(1 : 4), 1]),
                        Sample = unlist(fdata[-c(1 : 4), 2]))%>%
    mutate(unique_id = paste(Location, Sample, sep = "_"),
           File = f,
           Lot = lot)%>%
    set_rownames(value = .$unique_id)
  
  analyt <- fdata[3, -c(1:2)]
  analyt <- analyt[!is.na(analyt)]
  rowdata <- data.frame(Analyt = make.names(analyt),
                        Unit = as.character(fdata[4, -c(1:2)][1: length(analyt)]))%>%
    set_rownames(value = .$Analyt)
  
  fdata <- data.frame(fdata[-c(1:4), -c(1:2)])
  fdata <- data.frame(fdata[, 1:length(analyt)])
  
  # get lod
  rowdata$LOD <- sapply(fdata, function(x){
    as.numeric(gsub("(↓|↑|<|>)", "", x[grep("<", x)][1]))
  })
  # get hod
  rowdata$HOD <- sapply(fdata, function(x){
    as.numeric(gsub("(↓|↑|<|>)", "", x[grep(">", x)][1]))
  })
  
  # replace  lod to -Inf Inf
  fdata_na <- apply(fdata, 2, function(x){
    case_when(
      grepl("(<|↓)", x) ~ -Inf,
      grepl("(>|↑)", x) ~ Inf,
      TRUE ~ as.numeric(x)
    )
  })
  
  # get missing lod pct
  rowdata$oor_pct <- sapply(data.frame(is.infinite(fdata_na)), function(x){
    round(sum(x) / length(x) * 100, 2)
  })
  
  
  # replace below lod to facevalue
  fdata <- apply(fdata, 2, function(x){
    as.numeric(gsub("(<|↓|>|↑)", "", x))
  })
  
  colnames(fdata_na) <- colnames(fdata) <-rowdata$Analyt
  rownames(fdata_na) <- rownames(fdata) <- coldata$unique_id
  
  #---flour intensity raw
  #sheet.list <- make.names(readxl::excel_sheets(f))
  sheet.list <- rowdata$Analyt
  
  rdata <- lapply(rowdata$Analyt, function(x){
    #temp <- read.xlsx(f, sheetIndex = which(sheet.list == x), header = T, startRow = analyt_start_row)
    if(grepl("xls$", f)){
      temp <- read_xls(f, sheet = (2 + which(sheet.list == x)), col_names = T, skip = (analyt_start_row - 1))
      temp <- temp[1: (grep("Note", temp$Location)-2), ]
    }
    if(grepl("xlsx$", f)){
      temp <- read.xlsx(f, sheetIndex = (2 + which(sheet.list == x)), header = T, startRow = analyt_start_row)
      temp <- temp[1: (grep("Note", temp$Location)-1), ]
    }
    
    temp <- temp%>%
      dplyr::select(Location, Sample, MFI, CV)%>%
      filter(!is.na(Sample))%>%
      set_colnames(value = c("Location", "Sample",
                             paste0(x, "_MFI"),
                             paste0(x, "_CV")))
    temp
  })
  
  
  rstd <- lapply(rowdata$Analyt, function(x){
    #temp <- read.xlsx(f, sheetIndex = which(sheet.list == x), header = T, startRow = analyt_start_row)
    if(grepl("xls$", f)){
      temp_std <- read_xls(f, sheet = (2 + which(sheet.list == x)), col_names = T, skip = 6)
      temp_std <- temp_std[1: (grep("Note", unlist(temp_std[ , 1]))[1]-2), ]
    }
    if(grepl("xlsx$", f)){
      temp_std <- read.xlsx(f, sheetIndex = (2 + which(sheet.list == x)), header = T, startRow = 7)
      temp_std <- temp_std[1: (grep("Note", unlist(temp_std[ , 1]))[1]-1), ]
    }
    
    idx_l <- which(!is.na(temp_std$MFI))[2]
    idx_h <- which(!is.na(temp_std$MFI))
    idx_h <- idx_h[length(idx_h)]
    c(temp_std[idx_l, "MFI"], temp_std[idx_l, 11], # lowest std gradient's mfi and conc.
      temp_std[idx_h, "MFI"], temp_std[idx_h, 11], # highest std gradient's mfi and conc.
      temp_std[temp_std$Location == "1E1", "MFI"], temp_std[temp_std$Location == "1E1", 11], temp_std[temp_std$Location == "1E1", 13],
      temp_std[temp_std$Location == "1G1", "MFI"], temp_std[temp_std$Location == "1G1", 11], temp_std[temp_std$Location == "1G1", 13])%>%
      as.numeric()# std3 conc.
  })%>%
    do.call(what = rbind)%>%
    set_colnames(value = c("low_MFI", "low_conc.", "high_MFI", "high_conc.", "std3_MFI", "std3_conc.", "std3_cv",
                           "std4_MFI", "std4_conc.", "std4_cv"))
  
  rowdata <- rowdata%>%
    cbind(data.frame(rstd))
  
  merge.rdata <- rdata[[1]]
  #merge.rdata$Sample[which(merge.rdata$Sample == "MM-0535-96-H_")] <- "MM-0535-96-H_2"
  if(length(rdata) > 1){
    for (i in 2 : length(rdata)) {
      merge.rdata <- merge(merge.rdata, rdata[[i]])
    }
  }
  rownames(merge.rdata) <- paste(merge.rdata$Location, merge.rdata$Sample, sep = "_")
  
  rdata <- data.frame(merge.rdata[match(rownames(fdata), rownames(merge.rdata)) , grep("MFI$", colnames(merge.rdata))])
  
  cvs <- data.frame(merge.rdata[match(rownames(fdata), rownames(merge.rdata)) , grep("CV$", colnames(merge.rdata))])
  cvs <- sapply(cvs, function(x){
    as.numeric(gsub("%", "", x))/100
  })
  colnames(rdata) <- colnames(cvs) <- colnames(fdata)
  rownames(rdata) <- rownames(cvs) <- rownames(fdata)
  
  re <- SummarizedExperiment(colData = coldata,
                             rowData = rowdata,
                             assays = list(data_default = t(fdata_na),
                                           data_imputed = t(fdata),
                                           mfi_default = t(rdata),
                                           cv = t(cvs)),
                             metadata = list("file_name" = toString(f)))
  return(re)
}


#' pull bridging sample from list of summarizeexperiment object by pattern string
#' get bridging sample only before normalization by plate
#'
pull_bdg <- function(f_list, pattern = "hd", fields = "Sample"){
  lapply(f_list, function(x){
    x[, grep(pattern, ignore.case = T, x[[fields]])]
  })
}


#' combind a list of luminex summarizedexperiment
#'
cmb_lmx_se <- function(se_list){
  
  rowdata_list <- lapply(se_list, function(x){
    temp <- data.frame(x@elementMetadata@listData)
    #Plate.ID <- gsub("(^.*/|\\.Detail.xls)",  "",x$File[1])
    colnames(temp) <- paste(colnames(temp), x$File[1], sep = "_")
    temp
  })
  
  row_data <- rowdata_list[[1]]
  #colnames(row_data)[1] <- "Analyt"
  for (i in 2 : length(rowdata_list)) {
    row_data <- cbind(row_data, rowdata_list[[i]][ ,-c(1,2)]) #Analyt Assay
  }
  
  colnames(row_data) <- make.names(colnames(row_data), unique = T)
  row_data$LOD <- rowMeans(row_data[ , grepl("LOD_.*$", colnames(row_data))], na.rm = T)
  row_data$HOD <- rowMeans(row_data[ , grepl("HOD_.*$", colnames(row_data))], na.rm = T)
  
  # remove LOD from combined se object does not make senes to keep it
  se_list <- lapply(se_list, function(x){
    x@elementMetadata@listData$LOD <- x@elementMetadata@listData$HOD <- NULL
    x@elementMetadata@listData <- data.frame()
    x
  })
  temp <- se_list[[1]]
  for (i in 2 : length(se_list)) {
    temp <- cbind(temp, se_list[[i]])
  }
  rowData(temp) <- row_data
  return(temp)
}

#' split long file name by deliminator, return minim unique parts
#' which identifies a given file   
#'
trim_string_bycommon <- function(strings, split = "_"){
  plid <- unique(strings)
  str_parts <- strsplit(plid, split = split)
  str_parts <- lapply(str_parts, function(x){
    temp <- x
    temp[1] <- paste0("^", temp[1])
    for (i in 2 : length(temp)) {
      temp[i] <- paste0(split, temp[i])
    }
    temp[length(temp)] <- paste0(temp[length(temp)], "$")
    temp
  })
  idf <- str_parts%>%
    unlist()%>%
    unique()
  
  idf_ct <- sapply(idf, function(x){
    sum(sapply(plid, function(y){
      grepl(x, y)
    }))
  })
  idf <- idf[which(idf_ct == length(plid))]
  idf <- paste(idf, collapse = "|")
  idf <- paste0("(", idf, ")")
  return(gsub(idf, "", strings))
}

#'
#'
bdg_norm_multi <- function(bridge.str, data.ls, between.plate.method = "median",
                           from_assay = "data_default", save_assay = "data_default_normed"){
  if(
    sapply(bridge.str, function(x){
      lapply(data.ls, function(y){
        sum(is.na(grepl(x, y$Sample)))
      })
    })%>%unlist()%>%sum() != 0
  ){
    stop("not all bridging samples exist in each plate!")
  }
  
  names(bridge.str) <- bridge.str
  adj.ls <- lapply(bridge.str, function(x){
    bridge <- pull_bdg(data.ls, pattern = x, fields = "Sample")%>%
      cmb_lmx_se()
    
    if(is.na(bridge@assays@data[[from_assay]]) %>% sum() != 0){
      stop("one or more analyte has NA values in bridging sample, can not perform normalization! Select other Bridging sample to try again.")
    }
    #  count
    bridge.plate.mean <- cbind.data.frame(File = bridge$File,
                                          t(bridge@assays@data[[from_assay]])%>%log10())%>%
      group_by(File)%>%
      summarize_all(.fun = mean, na.rm = T)
    
    
    bridge.median <- bridge.plate.mean%>%
      dplyr::select(-File)%>%
      summarize_all(.funs = between.plate.method, na.rm = T)
    
    
    # update adjust factor
    query <- bridge.plate.mean$File
    names(query) <- query
    
    # adjust factor count
    bridge.adj <- lapply(query, function(x){
      bridge.plate.mean[bridge.plate.mean$File == x ,-1] - unlist(bridge.median)
    })
    
    return(bridge.adj)
  })
  
  adj.mean <- adj.ls[[names(adj.ls)[1]]]
  if(length(adj.ls) > 1){
    for (i in names(adj.mean)) {
      for (j in 2 : length(adj.ls)) {
        adj.mean[[i]] <- adj.mean[[i]] + adj.ls[[j]][[i]]
      }
      adj.mean[[i]] <- adj.mean[[i]]/length(adj.ls)
    }
  }
  
  query <- names(adj.mean)
  names(query) <- query
  
  data.ls <- lapply(query, function(x){
    temp <- data.ls[[x]]
    temp@assays@data[[save_assay]] <- 10^(log10(temp@assays@data[[from_assay]]) - unlist(adj.mean[[x]]))%>%round(5)
    #temp@elementMetadata$LOD <- 10^(log10(as.numeric(unlist(temp@elementMetadata$LOD))) - unlist(adj.mean[[x]]))%>%round(5)
    #temp@elementMetadata$HOD <- 10^(log10(as.numeric(unlist(temp@elementMetadata$HOD))) - unlist(adj.mean[[x]]))%>%round(5)
    temp
  })
  
  data.ls
  
}
