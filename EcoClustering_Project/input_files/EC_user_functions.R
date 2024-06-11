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

# process multicat vars as numeric instead
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

print_top_varsets <- function(asw_mat, num_results, label_df){
  mat <- asw_mat %>% 
    arrange(desc(ASW)) %>%
    slice(1:num_results)
  
  mat2 <- select(mat,-c(ASW))
  
  num_vars <- sum(mat2[1,], na.rm = T) # sum no. of TRUE vals in 1st row
  new_mat <- matrix(data = NA, nrow = num_results, ncol = num_vars)
  
  names <- colnames(mat2)
  
  for(i in 1:nrow(mat)){
    new_mat[i,] <- names[which(!is.na(mat2[i,]))]
  }
  
  dhs_code_mat <- new_mat
  
  tab1_labs <- apply(dhs_code_mat, 2, function(x){
    plyr::mapvalues(x, from = label_df$dhs_code, to = label_df$dhs_label, warn_missing = F)
  })
  
  var_name_df <- as.data.frame(cbind(1:num_results, round(mat[,1], 4), tab1_labs)) %>% 
    `colnames<-`(c("Set", "ASW", paste0("Var.", seq(num_vars))))
  
  return(list(var_name_df, dhs_code_mat))
}

## Marginal Distributions
print_margdists <- function(data, code_mat){
  
  num_results <- nrow(code_mat)
  
  unq.vars <- unique(as.vector(code_mat))
  
  labels <- lapply(data, attr, "label")[unq.vars] %>% unlist()
  
  # In case one prints a different number of top variable sets (i.e. not 10)
  props_top_unsorted <- table(unlist(code_mat))*(100/num_results)
  props_top_sorted <- paste0(props_top_unsorted[match(unq.vars, names(props_top_unsorted))], "%")
  
  props <- subset(data, select = unq.vars) %>% 
    lapply(table) %>% 
    lapply(prop.table) %>% 
    lapply(print_proptable) %>% 
    as.data.frame() %>% 
    t()
  
  marg_dist_df <- cbind(unq.vars, labels, props_top_sorted, props) %>% 
    data.frame() %>% 
    `colnames<-`(c("Variable", "Description", "% in Top Sets", "Distribution")) %>% 
    `rownames<-`(NULL)
  
  return(marg_dist_df)
}

# clusTable <- function(vars){
#   
#   data <- merged_data
#   subset_data <- data %>% 
#     select(wt, all_of(vars)) 
#   
#   out <- clusNode(subset_data, 4, ncores = 8)
#   data$clusters <- out[[1]]$clustering
#   
#   data$id <- out[[1]]$clustering %>% 
#     unlist() %>% 
#     as.factor() %>% 
#     as.numeric()
#   
#   dectab <- data %>%
#     tabyl(id, de_cat, show_missing_levels = FALSE, show_na = FALSE)
#   
#   edutab <- data %>%
#     tabyl(id, v149, show_missing_levels = FALSE, show_na = FALSE)
#   
#   pritab <- data %>%
#     tabyl(id, health, show_missing_levels = FALSE, show_na = FALSE)
#   
#   dectab.f <- dectab %>%
#     adorn_totals(where = "col") %>%
#     adorn_percentages(denominator = "col") %>%
#     adorn_pct_formatting() %>% 
#     adorn_ns(position = "front") %>%
#     adorn_title(
#       row_name = "Cluster Number",
#       col_name = "Children Deceased",
#       placement = "combined") %>%
#     flextable::flextable() %>%
#     flextable::autofit() %>% 
#     footnote(i = 1, j = 1,
#              ref_symbols = "*",
#              value = as_paragraph(paste("The chi-squared p-value is", 
#                                         round(chisq.test(dectab)$p.val,4))))
#   
#   edutab.f <- edutab %>%
#     adorn_totals(where = "col") %>%
#     adorn_percentages(denominator = "col") %>%
#     adorn_pct_formatting() %>% 
#     adorn_ns(position = "front") %>%
#     adorn_title(
#       row_name = "Cluster Number",
#       col_name = "Education Attained",
#       placement = "combined") %>%
#     flextable::flextable() %>%
#     flextable::autofit() %>% 
#     footnote(i = 1, j = 1,
#              ref_symbols = "*",
#              value = as_paragraph(paste("The chi-squared p-value is",
#                                         round(wtd.chi.sq(data$id,data$v149,weight = data$v005)[3],4)))) %>%
#     footnote(i = 2, j = 1,
#              ref_symbols = "a",
#              value = as_paragraph(paste("0=none,1=incomplete primary, 2=primary, 3=incomplete secondary, 4=secondary, 5=higher")))
#   
#   pritab.f <- pritab %>%
#     adorn_totals(where = "col") %>%
#     adorn_percentages(denominator = "col") %>%
#     adorn_pct_formatting() %>% 
#     adorn_ns(position = "front") %>%
#     adorn_title(
#       row_name = "Cluster Number",
#       col_name = "Primary Healthcare Source",
#       placement = "combined") %>%
#     flextable::flextable() %>%
#     flextable::autofit() %>% 
#     footnote(i = 1, j = 1,
#              ref_symbols = "a",
#              value = as_paragraph(paste("The chi-squared p-value is",
#                                         round(chisq.test(pritab)$p.val,4)))) %>%
#     footnote(i = 2, j = 1,
#              ref_symbols = "a",
#              value = as_paragraph(paste("0=public/government, 1=private, 2=other")))
#   
#   ### ADD FOOTNOTE
#   
#   print(dectab.f)
#   
#   print(edutab.f)
#   
#   print(pritab.f)
#   
#   return(list(dectab.f, edutab.f, pritab.f))
#   
# }

detect_imbalance <- function(col, threshold){
  if(is.character(col)){
    return(FALSE)
  }else if(is.nan(mean(col, na.rm = T))){
    return(TRUE)
  }else{
    mean(col, na.rm = T) < threshold | mean(col, na.rm = T) > (1 - threshold)
  }
}

print_proptable <- function(proptable){
  if(length(proptable) == 2){
    labels <- names(proptable)
    values <- proptable
    
    value1 <- values[grepl("1", labels)] %>% 
      round(3) %>% 
      `*`(100)
    
    paste0("Binary, ", value1, "% (1/yes)")
  } else if(length(proptable) > 8){
    #labels <- names(proptable)
    values <- proptable
    
    paste0("Binary, Mode: ", names(which.max(values)))
  }
  else{
    labels <- names(proptable)
    values <- proptable %>% 
      round(3) %>% 
      `*`(100)
    
    l_v <- data.frame(values)
    f_labels <- paste0(0:(length(labels) - 1), " (", l_v[,1], ")") # attaching factors
    f_v_labels <- paste0(f_labels, " = ", values, "%", collapse = ", \n " )
    
    paste0("Categorical, ", f_v_labels)
  }
}

# OLD (DO NOT USE)
# clus_config <- function(data, i, tab, nCores){
#   vars <- as.vector(as.matrix(tab[i,2:5]))
#   labels <- plyr::mapvalues(vars,
#                             from = var_to_label_df$rowname,
#                             to = var_to_label_df$V1, 
#                             warn_missing = F)
#   
#   subset_data <- data %>% select("wt", vars) 
#   
#   out <- clusNode(subset_data,4,nCores)
#   
#   cluster_id <- subset_data$node <- out[[1]]$clustering %>% 
#     unlist() %>% 
#     as.character() %>% 
#     as.factor() %>% 
#     as.numeric()
#   
#   rel_prop <- as.matrix(table(subset_data$node)/nrow(subset_data)) %>% 
#     as.data.frame() %>% 
#     rownames_to_column() %>%
#     `colnames<-`(c("node", "prop"))
#   
#   grouped <- subset_data %>% 
#     unite(clus, c(vars), remove = F) %>% 
#     group_by(clus) %>%
#     summarise(node = unique(node),
#               var1 = first(!!as.name(vars[1])),
#               var2 = first(!!as.name(vars[2])),
#               var3 = first(!!as.name(vars[3])),
#               var4 = first(!!as.name(vars[4]))) %>% 
#     arrange(node) %>% 
#     select(!clus) %>% 
#     merge(rel_prop) %>% 
#     mutate(prop = round(prop*100, 2)) %>% 
#     mutate(prop = replace(prop, duplicated(prop), NA))
#   
#   colnames(grouped) <- c("Cluster Group", labels, "Proportion (%)")
#   
#   return(list(grouped, cluster_id))
# }

# updated clusNode fxn (much simpler)
find_cluster_id <- function(data, num_var, num_cluster, nCores){
  
  # num_var: the number of variables used to determine a set (default is 4)
  # num_cluster: the number of predetermined groups made using the variables (default is 5)
  
  # extract weights
  if (class(unlist(data[,1]))=="numeric"){
    wt <- unlist(data[,1])
    data <- data[,-1]
  } else {
    wt <- rep(1, nrow(data))
  }
  
  parD <- parDist(as.matrix(data), method = "hamming", threads = nCores) 
  wcKMR <- WeightedCluster::wcKMedRange(parD, kvals=num_cluster, weights=wt)
  cluster_id <- wcKMR$clustering %>% 
    unlist() %>% 
    as.character() %>% 
    as.factor() %>% 
    as.numeric()
  return(cluster_id)
}

# Updated clus_config fxn (calling "configurations" clustering "distributions" now)
# also ensures we don't have to rely on only using 4 variables in the future
print_clus_dist <- function(data = dhs, 
                            set = set, 
                            code_mat = top_dhscode_table, 
                            label_df = dhs_labels, 
                            nCores = nCores){
  
  nvar <- ncol(code_mat)
  set_vars <- as.vector(code_mat[set, 1:nvar])
  
  labels <- plyr::mapvalues(set_vars,
                            from = label_df$dhs_code,
                            to = label_df$dhs_label, 
                            warn_missing = F)
  
  # rerun clustering algorithm on variable set
  subset <- data %>% select("wt", all_of(set_vars))
  cluster_id <- find_cluster_id(data = subset, 
                                num_var = num_var, 
                                num_cluster = num_cluster, 
                                nCores = nCores)
  
  # create distribution table
  subset$clus_id <- cluster_id
  
  rel_prop <- as.matrix(table(subset$clus_id)/nrow(subset)) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>%
    `colnames<-`(c("clus_id", "prop"))
  
  grouped <- subset %>% 
    unite(clus, c(set_vars), remove = F) %>% 
    group_by(clus) %>%
    summarise(clus_id = unique(clus_id)) %>% 
    separate(col = clus, into = set_vars, sep = "_") %>% 
    arrange(clus_id) %>% 
    select(clus_id, everything()) %>% 
    merge(rel_prop) %>% 
    mutate(prop = round(prop*100, 2)) %>% 
    mutate(prop = replace(prop, duplicated(prop), NA))
  
  colnames(grouped) <- c("Cluster Group", labels, "Proportion (%)")
  
  return(list(grouped, cluster_id))
}

reassign_label <- function(clean_dhs_data){
  for(i in 1:length(Hmisc::label(clean_dhs_data))){
    if(Hmisc::label(clean_dhs_data)[i] == ""){
      Hmisc::label(clean_dhs_data[[i]]) <- names(clean_dhs_data)[i]
    }
  }
  return(clean_dhs_data)
}

create_label_df <- function(labelled_dhs_df){
  label_df <- data.frame(lapply(labelled_dhs_df, attr, "label")) %>% 
    t() %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    dplyr::rename(dhs_code = rowname,
                  dhs_label = V1)
  return(label_df)
}

### For generating validation tables

clean_individual_dhs <- function(ind_dhs){
  
  ind_vars <- c("v001", "v002", "v005")
  
  if(c("v149") %in% names(ind_dhs)){
    ind_vars <- c(ind_vars, "v149")
  }
  
  if(all(c("v201", "v206", "v207") %in% names(ind_dhs))){
    ind_dhs$deceas = ifelse(!is.na((ind_dhs$v206+ind_dhs$v207)/ind_dhs$v201), 
                            (ind_dhs$v206+ind_dhs$v207)/ind_dhs$v201, 0)
    ind_dhs$de_cat = cut(ind_dhs$deceas, 
                         breaks = c(-0.01,0.00001,0.3399,0.6699,1),
                         labels = c("0%", "1-33%", "34-66%", "67+%"))
    ind_vars <- c(ind_vars, "deceas", "de_cat")
  }
  
  if(all(c("v326") %in% names(ind_dhs))){
    ind_dhs$health = ind_dhs$v326 %>%
      str_replace_all('1[0-9]', '0') %>%
      str_replace_all('2[0-9]', '1') %>%
      str_replace_all('3[0-9]', '1') %>%
      str_replace_all('4[0-9]', '2') %>%
      str_replace_all('9[0-9]', '2')
    ind_vars <- c(ind_vars, "health")
  }
  
  ind_subset <- ind_dhs %>%
    dplyr::select(all_of(ind_vars))
  
  return(ind_subset)
}

create_validation_tables <- function(merged_data){
  
  list_of_tables <- list()
  
  if(all(c("de_cat") %in% names(merged_data))){
    dectab <- merged_data %>%
      tabyl(id, de_cat, show_missing_levels = FALSE, show_na = FALSE)
    
    ordered_decid <- cbind(dectab[,1],
                           prop.table(as.matrix(dectab[,-1]), 1)) %>% 
      as.data.frame() %>% 
      arrange(desc(`0%`)) %>% 
      select(V1) %>% 
      unlist()
    
    dectab <- dectab[match(ordered_decid, dectab$id),] %>% 
      as.data.frame()
    
    dectab2 <- cbind(dectab[,1:2], rowSums(dectab[,-c(1:2)])) %>% 
      `colnames<-`(c("id", "0%", ">0%"))
    
    ordered_decid2 <- cbind(dectab2[,1], prop.table(as.matrix(dectab2[,-1]), 1)) %>% 
      as.data.frame() %>% 
      arrange(desc(`0%`)) %>% 
      select(V1) %>% 
      unlist()
    
    dectab2 <- dectab2[match(ordered_decid2, dectab2$id),]
    
    list_of_tables <- c(list_of_tables, list(dectab = dectab), list(dectab2 = dectab2))
  }
  
  if(all(c("v149") %in% names(merged_data))){
    
    num_cluster <- length(unique(merged_data$id))
    
    edutab <- merged_data %>%
      tabyl(id, v149, show_missing_levels = FALSE, show_na = FALSE)
    
    # Sorted by weighted average across each row
    ordered_eduid <- cbind(c(1:num_cluster), 
                           prop.table(as.matrix(edutab[,-1]), 1) %*% diag(c(0:num_cluster)) %>% 
                             rowSums()) %>% 
      as.data.frame() %>% 
      arrange(desc(V2))
    
    edutab <- edutab[match(ordered_eduid$V1, edutab$id),] %>% 
      as.data.frame()
    
    list_of_tables <- c(list_of_tables, list(edutab = edutab), list(ordered_eduid = ordered_eduid))
  }
  
  if(all(c("health") %in% names(merged_data))){
    pritab <- merged_data %>%
      tabyl(id, health, show_missing_levels = FALSE, show_na = FALSE)
    
    ordered_priid <- cbind(pritab[,1],prop.table(as.matrix(pritab[,-1]), 1)) %>% 
      as.data.frame() %>% 
      arrange(`0`) %>% 
      select(V1) %>% 
      unlist()
    
    pritab <- pritab[match(ordered_priid, pritab$id),] %>% 
      as.data.frame()
    
    list_of_tables <- c(list_of_tables, list(pritab = pritab))
  }
  
  return(list_of_tables)
}


