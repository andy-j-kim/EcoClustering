library(janitor)
library(gt)
library(flextable)
library(weights)

preprocess_dhsfactors <- function(data){
  require(dplyr)
  require(stringr)
  require(sjlabelled)
  newdata <- data %>% 
    dplyr::mutate(water = sjlabelled::as_label(hv201) %>% 
                    str_replace_all('[Pp]iped into dwelling', 'pipe_dwel') %>% 
                    str_replace_all('[Pp]iped to yard/plot', 'pipe_yard') %>% 
                    str_replace_all('[Pp]iped to neighbor', 'pipe_neighbor') %>% 
                    str_replace_all('[Pp]ublic tap/standpipe', 'pipe_public') %>% 
                    str_replace_all('.* [Ww]ell.*', 'well') %>% 
                    str_replace_all('.* [Ss]pring.*|[Rr]ainwater|[Rr]iver.*', 'surface'),
                  water = str_detect(water, 'pipe.*|well|surface', negate = T) %>% 
                    ifelse('other', water)) %>% 
    dplyr::mutate(toilet = hv205 %>% 
                    str_replace_all('1[1-6]', 'flush') %>% 
                    str_replace_all('2[1-4]', 'pitlat') %>% 
                    str_replace_all('31', 'none') %>% 
                    str_replace_all('4[1-4]|96', 'other') %>% 
                    str_replace_all('99', NA_character_)) %>% 
    dplyr::mutate(floor = hv213 %>% 
                    str_replace_all('1[1-3]', 'natural') %>% 
                    str_replace_all('2[1-3]', 'rudimentary') %>% 
                    str_replace_all('3[1-6]', 'finished') %>% 
                    str_replace_all('96', 'other') %>% 
                    str_replace_all('9[79]', NA_character_)) %>% 
    dplyr::mutate(roof = hv215 %>% 
                    str_replace_all('1[1-3]', 'natural') %>% 
                    str_replace_all('2[1-6]', 'rudimentary') %>% 
                    str_replace_all('3[1-9]', 'finished') %>% 
                    str_replace_all('96', 'other') %>% 
                    str_replace_all('9[79]', NA_character_)) %>% 
    dplyr::mutate(cookfuel = sjlabelled::as_label(hv226) %>%
                    str_replace_all('9[79]', NA_character_))
  
  # Factorizing wall variable
  
  # Specific case for Gabon's wall variable
  if(grepl("GA", unique(data[["hv000"]]), fixed = TRUE)){
    newdata <- newdata %>% 
      mutate(wall = hv214 %>% 
               str_replace_all('31', 'natural') %>% 
               str_replace_all('1[1-3]|2[1-6]', 'rudimentary') %>% 
               str_replace_all('3[2-8]', 'finished') %>% 
               str_replace_all('96', 'other') %>% 
               str_replace_all('9[79]', NA_character_)) 
    
  } else if(grepl("MZ", unique(data[["hv000"]]), fixed = TRUE)){ # Specific case for Mozambique's Wall variable
    newdata <- newdata %>% 
      mutate(wall = hv214 %>% 
               str_replace_all('1[1-4]', 'natural') %>% 
               str_replace_all('2[1-6]', 'rudimentary') %>% 
               str_replace_all('27|3[1-8]|4[1-4]', 'finished') %>% 
               str_replace_all('96', 'other') %>% 
               str_replace_all('9[79]', NA_character_)) 
  } else {
    newdata <- newdata %>% 
      mutate(wall = hv214 %>% 
               str_replace_all('1[1-4]', 'natural') %>% 
               str_replace_all('2[1-7]', 'rudimentary') %>% 
               str_replace_all('3[1-8]|4[1-4]', 'finished') %>% 
               str_replace_all('96', 'other') %>% 
               str_replace_all('9[79]', NA_character_)) 
  }
  
  newdata <- newdata %>% select(-hv000, -hv201, -hv205, -hv213, -hv214, -hv215, -hv226)
  
  return(newdata)
}

calcASW <- function(data, num.in.cluster, ncores){
  
  ## Note: Nolan add error message for if num.in.cluster 
  ## is greater than the total number possible
  ASW<-vector()
  combi<-list()
  parD<-list()
  wcKMR<-list()
  
  if (class(data[,1])=="numeric"){
    wt<-data[,1]
    data<-data[,-1]
  } else {
    wt<-rep(1, nrow(data))
  }
  
  p <- ncol(data)
  col_cat<-seq(1,p,1)
  combn<-utils::combn(c(1:p),num.in.cluster)
  col_indx=matrix(c("NULL"), ncol(combn), p, byrow=FALSE)
  for (i in 1:ncol(combn)){
    col_indx[i,]<- t(is.element(col_cat, combn[,i]))
    col_indx[i,][col_indx[i,]==FALSE]<- NA
    colnames(col_indx)=colnames(data)
  }
  
  start.time<-Sys.time()
  ASW <- foreach::foreach (i=1:nrow(col_indx), .combine='c')  %dopar% {
    combi<-data[,!is.na(col_indx[i,])]
    combi_mat<-data.matrix(combi)
    parD<-parDist(combi_mat, method =  "hamming", threads = ncores) 
    wcKMR<-WeightedCluster::wcKMedRange(parD, kvals=(5), weights=wt)
    ASW[i]<-wcKMR$stats[,5]
    rm(combi)
    rm(parD)
    rm(wcKMR)
    return(ASW[i])
  }
  
  end.time<-Sys.time()
  total<-end.time-start.time
  
  asw_mat <- as.data.frame(cbind(ASW, col_indx))
  asw_mat$ASW <- as.numeric(asw_mat$ASW)
  for(i in 2:ncol(asw_mat)){
    asw_mat[,i] <- as.logical(asw_mat[,i])
  }
  asw_mat <- asw_mat[order(asw_mat$ASW, decreasing = TRUE),]
  
  return(list(ASW,asw_mat,total,parD))
}

clusNode <- function(data, num.in.cluster, ncores){
  
  ASW<-vector()
  combi<-list()
  parD<-list()
  wcKMR<-list()
  
  if (class(unlist(data[,1]))=="numeric"){
    wt<-unlist(data[,1])
    data<-data[,-1]
  } else {
    wt<-rep(1, nrow(data))
  }
  
  p <- ncol(data)
  col_cat<-seq(1,p,1)
  combn<-utils::combn(c(1:p),num.in.cluster)
  col_indx=matrix(c("NULL"), ncol(combn), p, byrow=FALSE)
  for (i in 1:ncol(combn)){
    col_indx[i,]<- t(is.element(col_cat, combn[,i]))
    col_indx[i,][col_indx[i,]==FALSE]<- NA
    colnames(col_indx)=colnames(data)
  }
  
  combi<-data[,!is.na(col_indx[i,])]
  combi_mat<-data.matrix(combi)
  parD<-parDist(combi_mat, method =  "hamming", threads = ncores) 
  wcKMR<-WeightedCluster::wcKMedRange(parD, kvals=(5), weights=wt)
  return(list(wcKMR,combi_mat))
}

preprocess_dhsfactors_adj <- function(data){
  require(dplyr)
  require(stringr)
  require(sjlabelled)
  newdata <- data %>% 
    dplyr::mutate(water = sjlabelled::as_label(hv201) %>% 
                    str_replace_all('[Pp]iped into dwelling', '0') %>% 
                    str_replace_all('[Pp]iped to yard/plot', '1') %>% 
                    str_replace_all('[Pp]iped to neighbor', '2') %>% 
                    str_replace_all('[Pp]ublic tap/standpipe', '3') %>% 
                    str_replace_all('.* [Ww]ell.*', '4') %>% 
                    str_replace_all('.* [Ss]pring.*|[Rr]ainwater|[Rr]iver.*', '5'), #%>% #
                  water = str_detect(water, '0|1|2|3|4|5', negate = T) %>% 
                    ifelse('6', water)) %>% 
    dplyr::mutate(toilet = hv205 %>% 
                    str_replace_all('1[1-6]', '0') %>% 
                    str_replace_all('2[1-4]', '1') %>% 
                    str_replace_all('31', '2') %>% 
                    str_replace_all('4[1-4]|96', '3') %>% 
                    str_replace_all('99', NA_character_)) %>% 
    dplyr::mutate(floor = hv213 %>% 
                    str_replace_all('1[1-3]', '0') %>% 
                    str_replace_all('2[1-3]', '1') %>% 
                    str_replace_all('3[1-6]', '2') %>% 
                    str_replace_all('96', '3') %>% 
                    str_replace_all('9[79]', NA_character_)) %>% 
    dplyr::mutate(roof = hv215 %>% 
                    str_replace_all('1[1-3]', '0') %>% 
                    str_replace_all('2[1-6]', '1') %>% 
                    str_replace_all('3[1-9]', '2') %>% 
                    str_replace_all('96', '3') %>% 
                    str_replace_all('9[79]', NA_character_)) %>% 
    dplyr::mutate(cookfuel = sjlabelled::as_label(hv226) %>%
                    str_replace_all('9[79]', NA_character_))
  
  # Factorizing wall variable
  
  # Specific case for Gabon's wall variable
  if(grepl("GA", unique(data[["hv000"]]), fixed = TRUE)){
    newdata <- newdata %>% 
      mutate(wall = hv214 %>% 
               str_replace_all('31', '0') %>% 
               str_replace_all('1[1-3]|2[1-6]', '1') %>% 
               str_replace_all('3[2-8]', '2') %>% 
               str_replace_all('96', '3') %>% 
               str_replace_all('9[79]', NA_character_)) 
    
  } else if(grepl("MZ", unique(data[["hv000"]]), fixed = TRUE)){ # Specific case for Mozambique's Wall variable
    newdata <- newdata %>% 
      mutate(wall = hv214 %>% 
               str_replace_all('1[1-4]', '0') %>% 
               str_replace_all('2[1-6]', '1') %>% 
               str_replace_all('27|3[1-8]|4[1-4]', '2') %>% 
               str_replace_all('96', '3') %>% 
               str_replace_all('9[79]', NA_character_)) 
  } else {
    newdata <- newdata %>% 
      mutate(wall = hv214 %>% 
               str_replace_all('1[1-4]', '0') %>% 
               str_replace_all('2[1-7]', '1') %>% 
               str_replace_all('3[1-8]|4[1-4]', '2') %>% 
               str_replace_all('96', '3') %>% 
               str_replace_all('9[79]', NA_character_)) 
  }
  
  newdata <- newdata %>% select(-hv000, -hv201, -hv205, -hv213, -hv214, -hv215, -hv226)
  
  return(newdata)
}

top_clus_table <- function(asw_mat, n){
  mat <- asw_mat %>% 
    arrange(desc(ASW)) %>%
    slice(1:n)
  
  mat2 <- select(mat,-c(ASW))
  
  #note: need to change 4 if diff number of variables used
  new_mat <- matrix(data = NA, nrow = n, ncol = 4)
  
  names <- colnames(mat2)
  
  for(i in 1:nrow(mat)){
    new_mat[i,] <- names[which(!is.na(mat2[i,]))]
  }
  
  mat3 <- as.data.frame(cbind(mat[,1],new_mat))
  
  return(mat3)
}

## Cluster Configurations


clusTable <- function(vars){
  
  data <- merged_data
  subset_data <- data %>% 
    select(wt, all_of(vars)) 
  
  out <- clusNode(subset_data, 4, ncores = 8)
  data$clusters <- out[[1]]$clustering
  
  data$id <- out[[1]]$clustering %>% 
    unlist() %>% 
    as.factor() %>% 
    as.numeric()
  
  dectab <- data %>%
    tabyl(id, de_cat, show_missing_levels = FALSE, show_na = FALSE)
  
  edutab <- data %>%
    tabyl(id, v149, show_missing_levels = FALSE, show_na = FALSE)
  
  pritab <- data %>%
    tabyl(id, health, show_missing_levels = FALSE, show_na = FALSE)
  
  dectab.f <- dectab %>%
    adorn_totals(where = "col") %>%
    adorn_percentages(denominator = "col") %>%
    adorn_pct_formatting() %>% 
    adorn_ns(position = "front") %>%
    adorn_title(
      row_name = "Cluster Number",
      col_name = "Children Deceased",
      placement = "combined") %>%
    flextable::flextable() %>%
    flextable::autofit() %>% 
    footnote(i = 1, j = 1,
             ref_symbols = "*",
             value = as_paragraph(paste("The chi-squared p-value is", 
                                        round(chisq.test(dectab)$p.val,4))))
  
  edutab.f <- edutab %>%
    adorn_totals(where = "col") %>%
    adorn_percentages(denominator = "col") %>%
    adorn_pct_formatting() %>% 
    adorn_ns(position = "front") %>%
    adorn_title(
      row_name = "Cluster Number",
      col_name = "Education Attained",
      placement = "combined") %>%
    flextable::flextable() %>%
    flextable::autofit() %>% 
    footnote(i = 1, j = 1,
             ref_symbols = "*",
             value = as_paragraph(paste("The chi-squared p-value is",
                                        round(wtd.chi.sq(data$id,data$v149,weight = data$v005)[3],4)))) %>%
    footnote(i = 2, j = 1,
             ref_symbols = "a",
             value = as_paragraph(paste("0=none,1=incomplete primary, 2=primary, 3=incomplete secondary, 4=secondary, 5=higher")))
  
  pritab.f <- pritab %>%
    adorn_totals(where = "col") %>%
    adorn_percentages(denominator = "col") %>%
    adorn_pct_formatting() %>% 
    adorn_ns(position = "front") %>%
    adorn_title(
      row_name = "Cluster Number",
      col_name = "Primary Healthcare Source",
      placement = "combined") %>%
    flextable::flextable() %>%
    flextable::autofit() %>% 
    footnote(i = 1, j = 1,
             ref_symbols = "a",
             value = as_paragraph(paste("The chi-squared p-value is",
                                        round(chisq.test(pritab)$p.val,4)))) %>%
    footnote(i = 2, j = 1,
             ref_symbols = "a",
             value = as_paragraph(paste("0=public/government, 1=private, 2=other")))
  
  ### ADD FOOTNOTE
  
  print(dectab.f)
  
  print(edutab.f)
  
  print(pritab.f)
  
  return(list(dectab.f, edutab.f, pritab.f))
  
}

detect_imbalance <- function(col, threshold){
  if(is.character(col)){
    return(FALSE)
  }else if(is.nan(mean(col, na.rm = T))){
    return(TRUE)
  }else{
    mean(col, na.rm = T) < threshold | mean(col, na.rm = T) > (1 - threshold)
  }
}
