library(shiny)
library(dplyr)
library(data.table)
library(DT)
library(shinythemes)
library(shinyTree)
library(tidyr)
library(data.tree)
library(collapsibleTree)
library(rgpt3)
library(httr)
library(caret)
library(qs)
library(randomForest)
library(stringr)
library(gridExtra)
library(ggplot2)
library(readr)
library(Hmisc)
library(ggrepel)
library(swfscMisc)
library(circular)
library(plotly)
library(ggforce)
library(skimr)
library(ggdark)
library(ggdist)
library(ggthemes)
library(chRoma)



setwd("/Users/hannahhapich/Documents/R_Scripts/TrashTaxonomy-master")

#Build cleaning functions
cleantext <- function(x) {
  x <- tolower(gsub("[[:space:]]", "", x))
  ifelse(x == "", NA, x)
}

removeslash <- function(x){
  gsub("/", " OR ", x)
}

cleanmaterials <- function(x) {
  x <- x[!names(x) %in% c("X1", "X2", "X3", "X4")]
  if(is.list(x)) lapply(x, cleanmaterials)
}

cleanitems <- function(x) {
  x <- x[!names(x) %in% c("X1", "X2", "X3", "X4", "X5", "X6")]
  if(is.list(x)) lapply(x, cleanitems)
}

#Build bootstrapping functions

BootMean <- function(data) {
  B <- 10000
  mean <- numeric(B)
  n = length(data)
  
  set.seed(34347)
  for (i in 1:B) {
    boot <- sample(1:n, size=n, replace = TRUE)
    mean[i] <- mean(data[boot], na.rm = T)
  }
  return(quantile(mean, c(0.025, 0.5, 0.975), na.rm = T))
}

AggregateTrees <- function(DF, Alias, Hierarchy){
  
  DF <- mutate_all(DF, cleantext) %>%
    mutate(Count = as.numeric(Count))
  
  colnames(DF) <- c("Alias", "Count")
  
  Hierarchy <- mutate_all(Hierarchy, cleantext)
  
  colnames(Hierarchy) <- c("from", "Key")
  
  Alias <- mutate_all(Alias, cleantext) 
  
  DF_v2 <- DF %>%
    left_join(Alias) %>%
    dplyr::select(Key, Count) %>%
    mutate(Key = ifelse(is.na(Key), "other", Key)) %>%
    right_join(Hierarchy) %>%
    select(from, Key, Count) %>%
    add_row(from = "trash", Key = "missing", Count = sum(DF$Count, na.rm = T) - sum(.$Count, na.rm = T))%>%
    mutate(Count = Count/sum(Count, na.rm = T))
  
  
  DF_network <- FromDataFrameNetwork(DF_v2)
  
  DF_network$Do(function(x) x$totalsum <- ifelse(is.null(x$Count), 0, x$Count) + sum(Get(x$children, "totalsum")), traversal = "post-order")
  
  Treedf <- ToDataFrameNetwork(DF_network, "totalsum") 
  
  Treedf %>%
    add_row(from = "trash", to = "", totalsum = sum(Treedf %>%
                                                      filter(from == "trash") %>% 
                                                      pull(totalsum)))
  
  
}

grouped_uncertainty <- function(DF_group, Group_Alias, Group_Hierarchy, type){
  
  groups <- DF_group
  
  df_join = data.frame(from = character(), 
                       to = character(), 
                       count = numeric())
  
  for(row in 1:nrow(groups)){
    df_subset <- DF_group %>%
      inner_join(groups[row,]) %>%
      select(Class, Count)
    
    df_join <- AggregateTrees(DF = df_subset, Alias = Group_Alias, Hierarchy = Group_Hierarchy) %>%
      mutate(from = ifelse(from == "trash", type, from)) %>%
      bind_rows(df_join)
  }
  
  df_join_boot <- df_join %>%
    #mutate(node_num = rep(1:27, times = nrow(groups))) %>%
    group_by(from, to) %>%
    summarise(mean_prop = mean(totalsum, na.rm = T), 
              min_prop = BootMean(totalsum)[1], 
              max_prop = BootMean(totalsum)[3])
  
}

sunburstplot <-function(df_join_boot){
  
  values <- paste(df_join_boot$to, 
                  "<br>", 
                  round(df_join_boot$mean_prop, 2) * 100, 
                  " (", 
                  round(df_join_boot$min_prop, 2) * 100, 
                  "-", 
                  round(df_join_boot$max_prop, 2) * 100, 
                  ")%", 
                  sep = "")
  
  values[df_join_boot$mean_prop < 0.1] <- NA
  
  plot_ly() %>%
    add_trace(
      labels = df_join_boot$to,
      parents = df_join_boot$from,
      type = "sunburst",
      maxdepth = 6,
      domain = list(column = 1), 
      branchvalues = 'total',
      texttemplate = values,
      values = df_join_boot$mean_prop) 
}

###create function to derive correction factor (CF) from Koelmans et al (equation 2)
CFfnx = function(a, #default alpha from Koelmans et al (2020)
                 x2D, #set detault values to convert ranges to (1-5,000 um) #5mm is upper defuault 
                 x1D, #1 um is lower default size
                 x2M, x1M){
  CF = (x2D^(1-a)-x1D^(1-a))/(x2M^(1-a)-x1M^(1-a)) 
  return(CF)}

#Files for tool
alius <- read.csv("data/PrimeMaterials.csv")
hierarchy <- read.csv("data/MaterialsHierarchyLower.csv")
aliusi <- read.csv("data/PrimeItems.csv")
hierarchyi <- read.csv("data/ITEMSHierarchyLower.csv")
microcolor <- read.csv("data/Microplastics_Color.csv")
micromathierarchy <- read.csv("data/Microplastics_Material_Hierarchy.csv")
aliasclean <- mutate_all(alius, cleantext)
aliascleani <- mutate_all(aliusi, cleantext)
hierarchyclean <- mutate_all(hierarchy, cleantext)
hierarchycleani <- mutate_all(hierarchyi, cleantext)
microcolorclean <- mutate_all(microcolor, cleantext)

#Creating materials hierarchy
Materials <- hierarchy
Materials[is.na(Materials)] <- ""
Materials <- mutate_all(Materials, removeslash)
Materials$pathString <- paste("Trash", Materials$X1, Materials$X2, Materials$X3, Materials$X4, sep = "/")
Materials <- as.Node(Materials[,])
Materials <- as.list(Materials)
Materials <- Materials[-1]
Materials <- cleanmaterials(Materials)

Materials_hierarchy <- hierarchy
Materials_hierarchy[is.na(Materials_hierarchy)] <- ""
Materials_hierarchy <- mutate_all(Materials_hierarchy, removeslash) %>%
  mutate(key = "trash") %>%
  relocate(key) %>%
  unite(pathString, sep = "/")
Materials_hierarchy <- as.Node(Materials_hierarchy, pathDelimiter = "/")
Materials_hierarchy <- as.list(Materials_hierarchy)
Materials_hierarchy <- Materials_hierarchy[-1]

#Creating items hierarchy
Items <- hierarchyi
Items[is.na(Items)] <- ""
Items <- mutate_all(Items, removeslash)
Items$pathString <- paste("Trash", Items$X1, Items$X2, Items$X3, Items$X4, Items$X5, Items$X6, sep = "/")
Items <- as.Node(Items)
Items <- as.list(Items)
Items <- Items[-1]
Items <- cleanitems(Items)

Items_hierarchy <- hierarchyi
Items_hierarchy[is.na(Items_hierarchy)] <- ""
Items_hierarchy <- mutate_all(Items_hierarchy, removeslash) %>%
  mutate(key = "trash") %>%
  relocate(key) %>%
  unite(pathString, sep = "/")
Items_hierarchy <- as.Node(Items_hierarchy, pathDelimiter = "/")
Items_hierarchy <- as.list(Items_hierarchy)
Items_hierarchy <- Items_hierarchy[-1]

#Files for display
Materials_Alias <- read.csv("data/PrimeMaterials.csv")
Materials_Hierarchy <- read.csv("data/MaterialsHierarchyLower.csv")
Items_Alias <- read.csv("data/PrimeItems.csv")
Items_Hierarchy <- read.csv("data/ITEMSHierarchyLower.csv")
Material_Item_Relation <- read.csv("data/MaterialItemRelationship.csv")
Brand_Manufacturer_Relation <- read.csv("data/BrandManufacturer.csv")
Brand_Item_Relation <- read.csv("data/BrandItem.csv")
NOAA <- read.csv("data/NOAA.csv")
PrimeUnclassifiable <- read.csv("data/PrimeUnclassifiable.csv")
Micro_Color_Display <-read.csv("data/Microplastics_Color.csv")

#Load up necessary data to generate embeddings
api_key <- readLines("data/openai.txt")
materials_alias_embeddings <- read.csv("data/Materials_Alias_.csv")
items_alias_embeddings <- read.csv("data/Items_Alias_.csv")
model <- 'text-embedding-ada-002'
material_embeddings <- read.csv("data/material_embeddings.csv")
material_dotprod <- data.table::transpose(material_embeddings, make.names = "name")
item_embeddings <- read.csv("data/item_embeddings.csv")
item_dotprod <- data.table::transpose(item_embeddings, make.names = "name")

#Read in item and material pathstrings for merging tool
pathstrings_items <- read.csv("data/Items_Pathstrings.csv")
pathstrings_materials <- read.csv("data/Materials_Pathstrings.csv")

#Start server

server <- function(input,output,session) {

###Find more and less specific items and materials
  df <- reactive({
    req(input$df)
    infile <- input$df
    df <- fread(infile$datapath)
    dataframe<- as.data.frame(df)%>%
      select(material, items)
    dataframe$material <- as.character(dataframe$material)
    dataframe$items <- as.character(dataframe$items)
    dataframeclean <- mutate_all(dataframe, cleantext) 
    
  #Material query tool cleaning
    for(row in 1:nrow(dataframeclean)) { 
      
      if(is.na(dataframeclean[row,"material"]) | dataframeclean[row,"material"] == "") {
        dataframe[row, "PrimeMaterial"] <- NA
        dataframe[row, "MoreSpecificMaterial"] <- NA
        next #Corrects for cases when there is no val. 
      }
      
      if(any(PrimeUnclassifiable == dataframeclean[row,"material"])) {
        dataframe[row, "MoreSpecificMaterial"] <- "Unclassifiable"
        dataframe[row, "PrimeMaterial"] <- NA
        #next #Corrects for cases when unclassifiable
      }
      
      #Identify Alias Row and Alias name in database
      Primename <- unique(aliasclean[unname(unlist(apply(aliasclean, 2, function(x) which(x == dataframeclean[row,"material"], arr.ind = T)))), "Material"])
      
      if(length(Primename) == 0 ){
          #Create new embedding
          embeddings_new <- lapply(dataframeclean[row,"material"], function(material){
            input = material
            
            parameter_list = list(input = input, model = model)
            
            request_base = httr::POST(url = "https://api.openai.com/v1/embeddings", 
                                      body = parameter_list, 
                                      httr::add_headers(Authorization = paste("Bearer", api_key)),
                                      encode = "json")
            
            output_base = httr::content(request_base)
            embedding_raw = to_numeric(unlist(output_base$data[[1]]$embedding))
            names(embedding_raw) = 1:1536
            data.table::as.data.table(as.list(embedding_raw)) %>%
              mutate(material = input)
          })
          
          #Bind new embeddings generated
          material_embeddings_new <- rbindlist(embeddings_new)
          
          #Make new dot prod
          material_dotprod_new <- data.table::transpose(material_embeddings_new, make.names = "material")
          
          #Take new cross prod
          cross_product <- crossprod(data.matrix(material_dotprod), material_dotprod_new[[1]])
          
          #Top match for alias given cross prod
          colnames(cross_product) <- c("match")
          cross_product_ <- data.frame(cross_product)
          cross_product_ <- cross_product_[order(-cross_product_$match),  ,drop=FALSE]
          cross_product_$Alias <- row.names(cross_product_)
          cross_product_ <- left_join(cross_product_, materials_alias_embeddings, by= "Alias")
          top_five <- head(unique(cross_product_$Material), n=5)
          match1 <- top_five[[1]]
          match2 <- top_five[[2]]
          match3 <- top_five[[3]]
          match4 <- top_five[[4]]
          match5 <- top_five[[5]]
          
          
          dataframe[row, "PrimeMaterial"] <- as.character(selectInput(paste("sel", row, sep = ""), "", choices = c(match1, match2, match3, match4, match5), width = "100px"))
          
          #top_five <- head(unique(cross_product_$Material), n=5)
          
          #dataframe[row, "PrimeMaterial"] <- sprintf(
           # '<input type="container" name="%s" value="%s"/>',
           # "str(top_five)", dataframe[row, "PrimeMaterial"]
          #)
          #Save new embeddings for future use
          colnames(material_embeddings_new) = colnames(material_embeddings)
          material_embeddings_new_ <- rbind(material_embeddings, material_embeddings_new)
          write.csv(material_embeddings_new_,'data/material_embeddings_new.csv')
      }
      
      else{
        dataframe[row, "PrimeMaterial"] <- Primename
      }
      
      #Identify Column Name in Hierarchy
      hierarchycolumnnum <- as.numeric(gsub("[[:alpha:]]", "", unique(names(unlist(apply(hierarchyclean, 1, function(x) which(x == dataframe[row, "PrimeMaterial"], arr.ind = T)))))))
      if(length(hierarchycolumnnum) == 0) {
        dataframe[row, "MoreSpecificMaterial"] <- "NO VAL IN DATABASE"
        next #Corrects for cases when there is no val. 
      }
      
      
      if(hierarchycolumnnum == ncol(hierarchyclean)) next #Corrects for cases when the value is already the most specific material.
      hierarchyrows <- unname(unlist(apply(hierarchyclean, 2, function(x) which(x == dataframe[row, "PrimeMaterial"], arr.ind = T))))
      
      #Get higher values
      vals <- tolower(unique(unname(unlist(hierarchy[hierarchyrows, (hierarchycolumnnum + 1):ncol(hierarchyclean)]))))
      
      #Push values into datatable
      dataframe[row, "MoreSpecificMaterial"] <- paste(vals[!is.na(vals)], collapse = " | ")
      
    }
    
    #Less Specific Materials
    for(row in 1:nrow(dataframeclean)) { 
      
      if(is.na(dataframeclean[row,"material"]) | dataframeclean[row,"material"] == "") {
        dataframe[row, "LessSpecificMaterial"] <- NA
        next #Corrects for cases when there is no val. 
      }
      
      if(any(PrimeUnclassifiable == dataframeclean[row,"material"])) {
        dataframe[row, "LessSpecificMaterial"] <- "Unclassifiable"
        next #Corrects for cases when unclassifiable
      }
      
      
      hierarchycolumnnum <- as.numeric(gsub("[[:alpha:]]", "", unique(names(unlist(apply(hierarchyclean, 1, function(x) which(x == dataframe[row, "PrimeMaterial"], arr.ind = T)))))))
      
      if(length(hierarchycolumnnum) == 0) {
        dataframe[row, "LessSpecificMaterial"] <- "NO VAL IN DATABASE"
        next #Corrects for cases when there is no value.
      }
      
      if(hierarchycolumnnum == 1) next #Corrects for cases when the value is already the least specific.
      hierarchyrows <- unname(unlist(apply(hierarchyclean, 2, function(x) which(x == dataframe[row, "PrimeMaterial"], arr.ind = T))))
      
      #Get higher values
      vals <- tolower(unique(unname(unlist(hierarchy[hierarchyrows, 1:(hierarchycolumnnum - 1)]))))
      
      #Push values into datatable
      dataframe[row, "LessSpecificMaterial"] <- paste(vals[!is.na(vals)], collapse = " | ")
      
    }
    
    #row = 1
    
    #find all more specific items
    for(row in 1:nrow(dataframeclean)) { 
      
      if(is.na(dataframeclean[row,"items"]) | dataframeclean[row,"items"] == "") {
        dataframe[row, "PrimeItem"] <- NA
        dataframe[row, "MoreSpecificItem"] <- NA
        next #Corrects for cases when there is no val. 
      }
      
      if(any(PrimeUnclassifiable == dataframeclean[row,"items"])) {
        dataframe[row, "MoreSpecificItem"] <- "Unclassifiable"
        dataframe[row, "PrimeItem"] <- NA
        next #Corrects for cases when unclassifiable
      }
      
      #Identify Alias Row and Alias name
      Primename <- unique(aliascleani[unname(unlist(apply(aliascleani, 2, function(x) which(x == dataframeclean[row,"items"], arr.ind = T)))), "Item"])
      
      if(length(Primename) == 0){
        #Create new embedding
        embeddings_newi <- lapply(dataframeclean[row,"items"], function(items){
          input = items
          
          parameter_list = list(input = input, model = model)
          
          request_base = httr::POST(url = "https://api.openai.com/v1/embeddings", 
                                    body = parameter_list, 
                                    httr::add_headers(Authorization = paste("Bearer", api_key)),
                                    encode = "json")
          
          output_base = httr::content(request_base)
          embedding_raw = to_numeric(unlist(output_base$data[[1]]$embedding))
          names(embedding_raw) = 1:1138
          data.table::as.data.table(as.list(embedding_raw)) %>%
            mutate(items = input)
        })
        
        #Bind new embeddings generated
        item_embeddings_new <- rbindlist(embeddings_newi)
        
        #Make new dot prod
        item_dotprod_new <- data.table::transpose(item_embeddings_new, make.names = "items")
        
        #Take new cross prod
        cross_product <- crossprod(data.matrix(item_dotprod), item_dotprod_new[[1]])
        
        #Top match for alias given cross prod
        Primename_ <- row.names(cross_product)[apply(cross_product, MARGIN = 2,  FUN = which.max)]
        
        #Second and third highest matches
        colnames(cross_product) <- c("match")
        cross_product_ <- data.frame(cross_product)
        cross_product_ <- cross_product_[order(-cross_product_$match),  ,drop=FALSE]
        second_alias = row.names(cross_product_)[2]
        third_alias = row.names(cross_product_)[3]
        fourth_alias = row.names(cross_product_)[4]
        fifth_alias = row.names(cross_product_)[5]
        
        #Top key alias match for given alias
        Primename <- items_alias_embeddings$Item[items_alias_embeddings$Alias == Primename_]
        second_Primename <- items_alias_embeddings$Item[items_alias_embeddings$Alias == second_alias]
        third_Primename <- items_alias_embeddings$Item[items_alias_embeddings$Alias == third_alias]
        
        #Input prime key into dataframe
        dataframe[row, "PrimeItem"] <- Primename
        
        #Save new embeddings for future use
        colnames(item_embeddings_new) = colnames(item_embeddings)
        item_embeddings_new_ <- rbind(item_embeddings, item_embeddings_new)
        write.csv(item_embeddings_new_,'data/item_embeddings_new.csv')
      }
      
      else{
        dataframe[row, "PrimeItem"] <- Primename
      }
      
      
      
      #Identify Column Name in Hierarchy
      hierarchycolumnnum <- as.numeric(gsub("[[:alpha:]]", "", unique(names(unlist(apply(hierarchycleani, 1, function(x) which(x == dataframe[row, "PrimeItem"], arr.ind = T)))))))
      if(length(hierarchycolumnnum) == 0) {
        dataframe[row, "MoreSpecificItem"] <- "NO VAL IN DATABASE"
        
        next #Corrects for cases when there is no value.
      }
      
      if(hierarchycolumnnum == ncol(hierarchycleani)) next #Corrects for cases when the value is already the most specific.
      
      hierarchyrows <- unname(unlist(apply(hierarchycleani, 2, function(x) which(x == dataframe[row, "PrimeItem"], arr.ind = T))))
      
      #Get higher values
      vals <- tolower(unique(unname(unlist(hierarchyi[hierarchyrows, (hierarchycolumnnum + 1):ncol(hierarchyclean)]))))
      
      #Push values into datatable
      dataframe[row, "MoreSpecificItem"] <- paste(vals[!is.na(vals)], collapse = " | ")
      
    }
    
    #find all less specific items
    for(row in 1:nrow(dataframe)) { 
      if(is.na(dataframeclean[row,"items"]) | dataframeclean[row,"items"] == "") {
        dataframe[row, "LessSpecificItem"] <- NA
        next #Corrects for cases when there is no val. 
      }
      
      if(any(PrimeUnclassifiable == dataframeclean[row,"items"])) {
        dataframe[row, "LessSpecificItem"] <- "Unclassifiable"
        next #Corrects for cases when unclassifiable
      }
      
      #Identify Alias Row and Alias name
      #Identify Column Name in Hierarchy
      hierarchycolumnnum <- as.numeric(gsub("[[:alpha:]]", "", unique(names(unlist(apply(hierarchycleani, 1, function(x) which(x == dataframe[row, "PrimeItem"], arr.ind = T)))))))
      if(length(hierarchycolumnnum) == 0) {
        dataframe[row, "LessSpecificItem"] <- "NO VAL IN DATABASE"
        next #Corrects for cases when there is no value.
      }
      
      if(hierarchycolumnnum == 1) next #Corrects for cases when the value is already the least specific.
      hierarchyrows <- unname(unlist(apply(hierarchycleani, 2, function(x) which(x == dataframe[row, "PrimeItem"], arr.ind = T))))
      
      #Get higher values
      vals <- tolower(unique(unname(unlist(hierarchyi[hierarchyrows, 1:(hierarchycolumnnum - 1)]))))
      
      #Push values into datatable
      dataframe[row, "LessSpecificItem"] <- paste(vals[!is.na(vals)], collapse = " | ")
      
    }
    
    
    return(dataframe)
  })
  
  columnnames <- reactive({
    return(input$variable)
  })
  
  ###START MERGING TOOL
  
  #Files for bootstrapping routine and sunburst plots
  ItemsHierarchy_sunburst <- read.csv("data/Items_Hierarchy_sunburstPlot.csv")
  MaterialsHierarchy_sunburst <- read.csv("data/Materials_Hierarchy_sunburstPlot.csv") 
  ItemsAlias_sunburst <- read.csv("data/PrimeItems.csv")%>%
    rename(Key = Item)
  MaterialsAlias_sunburst <- read.csv("data/PrimeMaterials.csv") %>%
    rename(Key = Material)
  
  df_ <- reactive({
    req(input$df_)
    req(input$d_f_)
    infile1 <- input$df_
    infile2 <- input$d_f_
    file1 <- fread(infile1$datapath)
    file2 <- fread(infile2$datapath)
    dataframe1 <- as.data.frame(file1) %>%
      select(material, items, count)
    dataframe2 <- as.data.frame(file2) %>%
      select(material, items, count)
    dataframe <- rbind(dataframe1, dataframe2)
    dataframe$material <- as.character(dataframe$material)
    dataframe$items <- as.character(dataframe$items)
    dataframe$count <- as.numeric(dataframe$count)
    dataframeclean <- mutate_all(dataframe, cleantext) 
    
    #Material query tool cleaning
    #Matching material to prime material
    for(row in 1:nrow(dataframeclean)) { 
      
      #Identify Alias Row and Alias name in database
      Primename <- unique(aliasclean[unname(unlist(apply(aliasclean, 2, function(x) which(x == dataframeclean[row,"material"], arr.ind = T)))), "Material"])
    
      if(is.na(length(Primename))){
        #Create new embedding
        embeddings_new <- lapply(dataframeclean[row,"material"], function(material){
          input = material
          
          parameter_list = list(input = input, model = model)
          
          request_base = httr::POST(url = "https://api.openai.com/v1/embeddings", 
                                    body = parameter_list, 
                                    httr::add_headers(Authorization = paste("Bearer", api_key)),
                                    encode = "json")
          
          output_base = httr::content(request_base)
          embedding_raw = to_numeric(unlist(output_base$data[[1]]$embedding))
          names(embedding_raw) = 1:1536
          data.table::as.data.table(as.list(embedding_raw)) %>%
            mutate(material = input)
        })
        
        #Bind new embeddings generated
        material_embeddings_new <- rbindlist(embeddings_new)
        
        colnames(material_embeddings_new) = colnames(material_embeddings)
        
        material_embeddings_new_ <- rbind(material_embeddings, material_embeddings_new)
        
        #Make new dot prod
        material_dotprod_new <- data.table::transpose(material_embeddings_new, make.names = "material")
        
        #Take new cross prod
        cross_product <- crossprod(data.matrix(material_dotprod), material_dotprod_new[[1]])
        
        #Top match for alias given cross prod
        Primename_ <- row.names(cross_product)[apply(cross_product, MARGIN = 2,  FUN = which.max)]
        
        #Top key alias match for given alias
        Primename <- materials_alias_embeddings$Material[materials_alias_embeddings$Alias == Primename_]
        
        #Input prime key into dataframe
        dataframe[row, "PrimeMaterial"] <- Primename
        
        #Save new embeddings for future use
        write.csv(material_embeddings_new_,'data/material_embeddings_new.csv')
      }
      
      else{
        dataframe[row, "PrimeMaterial"] <- Primename
      }
      
    }
    
    #Merge all materials to most specific common denominator
    
    #Find all unique materials
    unique_materials <- as.data.frame(unique(dataframe[,"PrimeMaterial"]))
    colnames(unique_materials) <- c('materials')
    unique_materials <- mutate_all(unique_materials, cleantext)
    unique_materials <- left_join(unique_materials, pathstrings_materials, by="materials")
    unique_materials <- cbind(unique_materials, merged_material=NA)
    
    #If parent term exists, replace pathname with parent pathname
    for(x in 1:nrow(unique_materials)) {
      for(y in 1:nrow(unique_materials)){
        z = grepl(unique_materials$pathString[[y]], unique_materials$pathString[[x]], ignore.case=TRUE)
        length_x <- str_length(unique_materials$pathString[[x]])
        length_y <- str_length(unique_materials$pathString[[y]])
        if(z== TRUE && length_x > length_y) {
          unique_materials$merged_material[[x]] <- paste(unique_materials$pathString[[y]])
        }
      }
    }
    
    #If no parent term exists, new pathname remains the same
    for(x in 1:nrow(unique_materials)) {
      if(is.na(unique_materials$merged_material[[x]])){
        unique_materials$merged_material[[x]] <- paste(unique_materials$pathString[[x]])
      }
    }
    
    #Link new pathname to new material
    unique_materials <- unique_materials %>% rename("pathString_unmerged"="pathString",
                                                    "pathString"="merged_material",
                                                    "material"="materials")
    unique_materials <- unique_materials %>% left_join(pathstrings_materials, by="pathString") %>%
      rename("merged_material"="materials") %>%
      subset(select= -c(pathString_unmerged, pathString))
    
    #Replace old material with merged material
    dataframe <- dataframe %>% left_join(unique_materials, by="material") %>%
      subset(select= -c(material)) %>%
      rename("material"="merged_material")
    
    

    #Clean and match items to prime alias
    for(row in 1:nrow(dataframe)) { 

      Primename <- unique(aliascleani[unname(unlist(apply(aliascleani, 2, function(x) which(x == dataframeclean[row,"items"], arr.ind = T)))), "Item"])
      
      if(length(Primename) == 0){
        #Create new embedding
        embeddings_newi <- lapply(dataframeclean[row,"items"], function(items){
          input = items
          
          parameter_list = list(input = input, model = model)
          
          request_base = httr::POST(url = "https://api.openai.com/v1/embeddings", 
                                    body = parameter_list, 
                                    httr::add_headers(Authorization = paste("Bearer", api_key)),
                                    encode = "json")
          
          output_base = httr::content(request_base)
          embedding_raw = to_numeric(unlist(output_base$data[[1]]$embedding))
          names(embedding_raw) = 1:1138
          data.table::as.data.table(as.list(embedding_raw)) %>%
            mutate(items = input)
        })
        
        #Bind new embeddings generated
        item_embeddings_new <- rbindlist(embeddings_newi)
        
        colnames(item_embeddings_new) = colnames(item_embeddings)
        
        item_embeddings_new_ <- rbind(item_embeddings, item_embeddings_new)
        
        #Make new dot prod
        item_dotprod_new <- data.table::transpose(item_embeddings_new, make.names = "items")
        
        #Take new cross prod
        cross_product <- crossprod(data.matrix(item_dotprod), item_dotprod_new[[1]])
        
        #Top match for alias given cross prod
        Primename_ <- row.names(cross_product)[apply(cross_product, MARGIN = 2,  FUN = which.max)]
        
        #Top key alias match for given alias
        Primename <- items_alias_embeddings$Item[items_alias_embeddings$Alias == Primename_]
        
        #Input prime key into dataframe
        dataframe[row, "PrimeItem"] <- Primename
        
        #Save new embeddings for future use
        write.csv(item_embeddings_new_,'data/item_embeddings_new.csv')
      }
      
      else{
        dataframe[row, "PrimeItem"] <- Primename
      }
      
    }
    
    #Merge all items to most specific common denominator
    
    #Find all unique items
    unique_items <- as.data.frame(unique(dataframe[,"PrimeItem"]))
    colnames(unique_items) <- c('items')
    unique_items <- mutate_all(unique_items, cleantext)
    unique_items <- left_join(unique_items, pathstrings_items, by="items")
    unique_items <- cbind(unique_items, merged_items=NA)
    
    #If parent term exists, replace pathname with parent pathname
    for(x in 1:nrow(unique_items)) {
      for(y in 1:nrow(unique_items)){
        z = grepl(unique_items$pathString[[y]], unique_items$pathString[[x]], ignore.case=TRUE)
        length_x <- str_length(unique_items$pathString[[x]])
        length_y <- str_length(unique_items$pathString[[y]])
        if(z== TRUE && length_x > length_y) {
          unique_items$merged_items[[x]] <- paste(unique_items$pathString[[y]])
        }
      }
    }
    
    #If no parent term exists, new pathname remains the same
    for(x in 1:nrow(unique_items)) {
      if(is.na(unique_items$merged_items[[x]])){
        unique_items$merged_items[[x]] <- paste(unique_items$pathString[[x]])
      }
    }
    
    #Link new pathname to new item
    unique_items <- unique_items %>% rename("pathString_unmerged"="pathString",
                                            "pathString"="merged_items",
                                            "unmerged_items"="items")
    unique_items <- unique_items %>% left_join(pathstrings_items, by="pathString") %>%
      rename("merged_items"="items",
             "items"="unmerged_items") %>%
      subset(select= -c(pathString_unmerged, pathString))
    
    #Replace old items with merged items
    dataframe <- dataframe %>% left_join(unique_items, by="items") %>%
      subset(select= -c(items)) %>%
      rename("items"="merged_items")
    
    #Combine any new identical terms
    dataframe <- dataframe %>%
      group_by(material, items) %>%
      summarise(across(count, sum))
    
    #Save input data
    #legacy_data <- read.csv("data/legacy_count_data.csv")
    #legacy_data <- rbind(legacy_data, dataframe)
    #legacy_data <- legacy_data %>%
    #  group_by(material, items) %>%
    #  summarise(across(count, sum))
    #write.csv(legacy_data,"data/legacy_count_data.csv")
    return(dataframe)
    
    
  })
  
  #Plot new merged data as sunburst plots
  ##Material Sunburst Plot ----
  
  output$plot1 <- renderPlotly({
    req(input$df_)
    req(input$d_f_)
    
    dataframe <- as.data.frame(df_()[, c("material", "items", "count")])
    
    Material_DF <- dataframe %>%
      rename(Count = count) %>%
      group_by(material) %>%
      summarise(Count = n()) %>%
      ungroup()
    
    Material_DF_group <- dataframe %>%
      rename(Count = count) %>%
      group_by(material) %>%
      summarise(Count = n()) %>%
      ungroup() %>%
      rename(Class = material)
    
    MaterialTreeDF <- AggregateTrees(DF = Material_DF, Alias = MaterialsAlias_sunburst, Hierarchy = MaterialsHierarchy_sunburst) %>%
      mutate(from = ifelse(from == "trash", "material", from))
    
    material_grouped <- grouped_uncertainty(DF_group = Material_DF_group, Group_Alias = MaterialsAlias_sunburst, Group_Hierarchy = MaterialsHierarchy_sunburst, type = "material")
    
    Materials_Plot <- sunburstplot(df_join_boot = material_grouped)
    
    return(Materials_Plot)
  })
  
  
  
  ##Item Sunburst Plot ----
  output$plot2 <- renderPlotly({
    req(input$df_)
    req(input$d_f_)
    
    dataframe <- as.data.frame(df_()[, c("material", "items", "count")])
    
    Item_DF <- dataframe %>%
      rename(Count = count) %>%
      group_by(items) %>%
      summarise(Count = n()) %>%
      ungroup() 
    
    
    Item_DF_group <- dataframe %>%
      rename(Count = count) %>%
      group_by(items) %>%
      summarise(Count = n()) %>%
      ungroup() %>%
      rename(Class = items)
    
    ItemTreeDF <- AggregateTrees(DF = Item_DF, Alias = ItemsAlias_sunburst, Hierarchy = ItemsHierarchy_sunburst) %>%
      mutate(from = ifelse(from == "trash", "items", from))
    
    #Item prop uncertainty
    item_grouped <- grouped_uncertainty(DF_group = Item_DF_group, Group_Alias = ItemsAlias_sunburst, Group_Hierarchy = ItemsHierarchy_sunburst, type = "items")
    
    Items_Plot <- sunburstplot(df_join_boot = item_grouped)
    
    return(Items_Plot)
  })
  
  
  
  
  
  ###END MERGING TOOL
  
  #Output correct survey sheet
  MicroOnly <- read.csv("data/PremadeSurveys/Most_Specific_Microplastics.csv")
  AllMore <- read.csv("data/PremadeSurveys/Most_Specific_All.csv")
  AllLess <- read.csv("data/PremadeSurveys/Least_Specific_All.csv")
  
  selectSurvey <- reactive({
    data = data.frame()
    if(input$sizeRange == "Micro"){data = MicroOnly
    data = as.data.frame(data)
    survey_columns <- c("material","items","color","size")
    colnames(data) = c("material","items","color","size")
    return(data)}
    if(input$specificity == "More Specific"){data = AllMore
    data = as.data.frame(data)
    survey_columns <- c("use", "material","items","count")
    colnames(data) = c("use", "material","items","count")}
    if(input$specificity == "Less Specific"){data = AllLess
    data = as.data.frame(data)
    survey_columns <- c("use", "material","items","count")
    colnames(data) = c("use", "material","items","count")}
    
    if(input$media == "Surface Water"){
      req(input$specificity)
      data <- data %>% filter(!use == "large",
                              !use == "vehicledebris")
      if(input$specificity == "More Specific"){
        data <- data %>% filter(!items == "bricks, cinderblocks, chunks of cement",
                                !items == "piping",
                                !items == "traffic cones",
                                !items == "appliances",
                                !items == "anchor")
      }
    }
    
    if(input$sizeRange == "Macro"){
      req(input$specificity)
      data <- data %>% filter(!use == "microplastics")
    }
    
    if(input$environments == "Marine/Estuarine"){
      req(input$specificity)
      data <- data %>% filter(!use == "gardening&farmingrelated",
                              !use == "officesupplies",
                              !use == "safetyrelated",
                              !use == "constructionmaterials")
    }
    
    if(input$environments == "Riverine"){
      req(input$specificity)
      data <- data %>% filter(!use == "ocean/waterwayactivities",
                              !use == "scuba&snorkelgear,masks,snorkels,fins")
    }
    
    if(input$environments == "Terrestrial"){
      req(input$specificity)
      data <- data %>% filter(!use == "fishinggear",
                              !use == "scuba&snorkelgear,masks,snorkels,fins",
                              !use == "shorelineandrecreationalactivites",
                              !use == "ocean/waterwayactivities")
    }
    
    

    
    return(data)
  })
  
  convertedParticles <- reactive({
    req(input$particleData)
    infile <- input$particleData
    file <- fread(infile$datapath)
    dataframe <- as.data.frame(file) %>%
      select(length_um, morphology, polymer)
    dataframe$length_um <- as.numeric(dataframe$length_um)
    dataframe$morphology <- as.character(dataframe$morphology)
    dataframe$polymer <- as.character(dataframe$polymer)
    dataframeclean <- mutate_all(dataframe, cleantext) 
    
    #convert morphologies in TT to morphologies with defined dimensions
    morphology <- c("fiber", "nurdle", "foam", "sphere", "line", "bead", "sheet", "film", "fragment", "rubberyfragment", "fiberbundle")
    morph_dimension <- c("fiber", "sphere", "foam", "sphere", "fiber", "sphere", "film", "film", "fragment", "fragment", "film")
    morph_conversion <- data.frame(morphology = morphology,
                                   morph_dimension = morph_dimension)
    dataframeclean <- left_join(dataframeclean, morph_conversion, by = "morphology", copy = FALSE)
    dataframeclean <- dataframeclean %>%
      select(-morphology)
    dataframeclean <- dataframeclean %>%
      rename(morphology = morph_dimension)
    
    #Make polymer-density dataframe
    polymer_db <- read.csv("data/all_polymer_densities.csv")
    polymer_db <- data.frame(polymer_db)
    polymer_db$polymer <- cleantext(polymer_db$polymer)
    polymer_db$density <- as.numeric(polymer_db$density)
    density_mg_um_3 <- polymer_db$density * 1e-9
    polymer_db <- polymer_db %>%
      mutate(density_mg_um_3 = density_mg_um_3)
    polymer_density <- polymer_db %>%
      select(polymer, density_mg_um_3)
    
    #Make CSF-morphology dataframe
    morphology <- c("fragment","sphere","fiber","film","foam")
    L <- c(1,1,1,1,1)
    W_min <- c(0.1,0.60,0.001,0.1,0.1)
    W_max <- c(1,1,0.5,1,1)
    H_min <- c(0.01,0.36,0.001,0.001,0.01)
    H_max <- c(1,1,0.5,0.1,1)
    morphology_shape <- data.frame(morphology=morphology,
                                   L=L,
                                   W_min=W_min,
                                   W_max=W_max,
                                   H_min=H_min,
                                   H_max=H_max
    )
    
    dataframeclean <- left_join(dataframeclean, morphology_shape, by = "morphology", copy = F)
    dataframeclean <- left_join(dataframeclean, polymer_density, by = "polymer", copy = F)
    dataframeclean <- data.frame(dataframeclean) %>%
      mutate(L = as.numeric(L) * as.numeric(length_um),
             W_min = as.numeric(W_min) * as.numeric(length_um),
             W_mean = (as.numeric(W_min) + as.numeric(W_max))/2 ,
             W_max = as.numeric(W_max) * as.numeric(length_um),
             H_min = as.numeric(H_min) * as.numeric(length_um),
             H_mean = (as.numeric(H_min) + as.numeric(H_max))/2 ,
             H_max = as.numeric(H_max) * as.numeric(length_um))
    
    dataframeclean <- data.frame(dataframeclean) %>%
      mutate(volume_min_um_3 = L * W_min* H_min,
             volume_mean_um_3 = L * W_mean* H_mean,
             volume_max_um_3 = L * W_max * H_max) 
    dataframeclean_particles <- data.frame(dataframeclean) %>%
      mutate(min_mass_mg = dataframeclean$density_mg_um_3 * dataframeclean$volume_min_um,
             mean_mass_mg = dataframeclean$density_mg_um_3 * dataframeclean$volume_mean_um,
             max_mass_mg = dataframeclean$density_mg_um_3 * dataframeclean$volume_max_um)
    return(dataframeclean_particles)
    })
  
  
  correctionFactor <- reactive({
    req(input$calculate_distribution)
    req(input$concentrationData)
    req(input$concentration_type)
    req(input$corrected_min)
    req(input$corrected_max)
    
    #clean incoming data
    infile <- input$concentrationData
    file <- fread(infile$datapath)
    dataframe <- as.data.frame(file) %>%
      select(study_media, concentration, size_min, size_max, concentration_units)
    dataframe$concentration <- as.numeric(dataframe$concentration)
    dataframe$size_min <- as.numeric(dataframe$size_min)
    dataframe$size_max <- as.numeric(dataframe$size_max)
    dataframe$study_media <- as.character(dataframe$study_media)
    dataframe$concentration_units <- as.character(dataframe$concentration_units)
    dataframeclean <- mutate_all(dataframe, cleantext) 
    
    #Make df for alpha values
    study_media <- c("marinesurface","freshwatersurface","marinesediment","freshwatersediment","effluent", "biota")
    length <- c(2.07, 2.64, 2.57, 3.25, 2.54, 2.59)
    mass <- c(1.32, 1.65, 1.50, 1.56, 1.40, 1.41)
    volume <- c(1.48, 1.68, 1.50, 1.53, 1.45, 1.40)
    surface_area <- c(1.50, 2.00, 1.75, 1.89, 1.73, 1.69)
    specific_surface_area <- c(1.98, 2.71, 2.54, 2.82, 2.58, 2.46)
    
    alpha_vals <- data.frame(study_media=study_media,
                             length=length,
                             mass=mass,
                             volume=volume,
                             surface_area=surface_area,
                             specific_surface_area=specific_surface_area
    )
    
    if(input$concentration_type == "length (um)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "length")], by = "study_media", all.x=TRUE)
                                                    dataframeclean <- dataframeclean %>%
                                                      rename("alpha" = "length")}
    if(input$concentration_type == "mass (ug)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "mass")], by = "study_media", all.x=TRUE)
                                                  dataframeclean <- dataframeclean %>%
                                                    rename("alpha" = "mass")}
    if(input$concentration_type == "volume (um3)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "volume")], by = "study_media", all.x=TRUE)
                                                    dataframeclean <- dataframeclean %>%
                                                      rename("alpha" = "volume")}
    if(input$concentration_type == "surface area (um2)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "surface_area")], by = "study_media", all.x=TRUE)
                                                          dataframeclean <- dataframeclean %>%
                                                            rename("alpha" = "surface_area")}
    if(input$concentration_type == "specific surface area (g/m2)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "specific_surface_area")], by = "study_media", all.x=TRUE)
                                                                    dataframeclean <- dataframeclean %>%
                                                                      rename("alpha" = "specific_surface_area")}
    
    dataframeclean <- dataframeclean %>%
      add_column(correction_factor = NA,
                 corrected_concentration = NA)
    
    
    #Extrapolated parameters
    x1D_set = as.numeric(input$corrected_min) #lower limit default extrapolated range is 1 um
    x2D_set = as.numeric(input$corrected_max) #upper limit default extrapolated range is 5 mm
    
    for(x in 1:nrow(dataframeclean)) {
      x1M_set = as.numeric(dataframeclean$size_min[[x]])
      x2M_set = as.numeric(dataframeclean$size_max[[x]])
      alpha = as.numeric(dataframeclean$alpha[[x]])
      
       CF <- CFfnx(x1M = x1M_set,#lower measured length
                   x2M = x2M_set, #upper measured length
                   x1D = x1D_set, #default lower size range
                   x2D = x2D_set,  #default upper size range
                   a = alpha #alpha for count 
                                                     
      )
       
       CF <- as.numeric(CF)
       
       CF <- format(round(CF, 2), nsmall = 2)
         
      
      dataframeclean$correction_factor[[x]] <- CF
      
      dataframeclean$corrected_concentration[[x]] <- as.numeric(dataframeclean$correction_factor[[x]]) * as.numeric(dataframeclean$concentration[[x]])
      
    }
    
    return(dataframeclean)
  })
  
  output$contents <- renderDataTable(server = F,
                                     datatable({
                                       df()[, c("material","items",  input$variable)]
                                     }, 
                                     extensions = 'Buttons',
                                     options = list(
                                       paging = TRUE,
                                       searching = TRUE,
                                       fixedColumns = TRUE,
                                       autoWidth = TRUE,
                                       ordering = TRUE,
                                       dom = 'Bfrtip',
                                       buttons = c('copy', 'csv', 'excel', 'pdf')
                                     ),
                                     class = "display",
                                     style="bootstrap"))
  
  output$contents1 <- DT :: renderDataTable(
                                      datatable({df()[, c("material","PrimeMaterial")] %>% distinct()},
                                                extensions = 'Buttons',
                                                class = "display",
                                                style="bootstrap",
                                                escape = FALSE,
                                                options = list(server = FALSE, dom="Bfrtip", paging=TRUE, ordering=TRUE, buttons=c('copy', 'csv', 'excel', 'pdf')),
                                                callback = JS("table.rows().every(function(row, tab, row) {
                                              var $this = $(this.node());
                                              $this.attr('id', this.data()[0]);
                                              $this.addClass('shiny-input-container');
                                            });
                                            Shiny.unbindAll(table.table().node());
                                            Shiny.bindAll(table.table().node());"))
                                      
                                      )
                                      
  
  output$contents2 <- renderDataTable(server = F, 
                                      datatable({df()[, c("items","PrimeItem")] %>% distinct()},
                                                extensions = 'Buttons',
                                                options = list(
                                                  paging = TRUE,
                                                  searching = TRUE,
                                                  fixedColumns = TRUE,
                                                  autoWidth = TRUE,
                                                  ordering = TRUE,
                                                  dom = 'Bfrtip',
                                                  buttons = c('copy', 'csv', 'excel', 'pdf')
                                                ),
                                                class = "display",
                                                style="bootstrap"))
  
  output$contents3 <- renderDataTable(server = F, 
                                      datatable({
                                        df_()[, c("material", "items", "count")]
                                      }, 
                                      extensions = 'Buttons',
                                      options = list(
                                        paging = TRUE,
                                        searching = TRUE,
                                        fixedColumns = TRUE,
                                        autoWidth = TRUE,
                                        ordering = TRUE,
                                        dom = 'Bfrtip',
                                        buttons = c('copy', 'csv', 'excel', 'pdf')
                                      ),
                                      class = "display",
                                      style="bootstrap"))
  
  output$contents4 <- renderDataTable(server = F, 
                                      datatable({
                                        selectSurvey()
                                      }, 
                                      extensions = 'Buttons',
                                      options = list(
                                        paging = TRUE,
                                        searching = TRUE,
                                        fixedColumns = TRUE,
                                        autoWidth = TRUE,
                                        ordering = TRUE,
                                        dom = 'Bfrtip',
                                        buttons = c('copy', 'csv', 'excel', 'pdf')
                                      ),
                                      class = "display",
                                      style="bootstrap"))
  
  output$contents5 <- renderDataTable(server = F, 
                                      datatable({
                                        convertedParticles()[, c("length_um", "morphology", "polymer", "L", "W_mean", "H_mean", "volume_mean_um_3", "mean_mass_mg")]
                                      }, 
                                      extensions = 'Buttons',
                                      options = list(
                                        paging = TRUE,
                                        searching = TRUE,
                                        fixedColumns = TRUE,
                                        autoWidth = TRUE,
                                        ordering = TRUE,
                                        dom = 'Bfrtip',
                                        buttons = c('copy', 'csv', 'excel', 'pdf')
                                      ),
                                      class = "display",
                                      style="bootstrap"))
  
  output$contents6 <- renderDataTable(server = F, 
                                      datatable({
                                        correctionFactor()[, c("study_media", "concentration", "concentration_units", "size_min", "size_max",  "alpha", "correction_factor", "corrected_concentration")]
                                      }, 
                                      extensions = 'Buttons',
                                      options = list(
                                        paging = TRUE,
                                        searching = TRUE,
                                        fixedColumns = TRUE,
                                        autoWidth = TRUE,
                                        ordering = TRUE,
                                        dom = 'Bfrtip',
                                        buttons = c('copy', 'csv', 'excel', 'pdf')
                                      ),
                                      class = "display",
                                      style="bootstrap"))
  
  output$plot3 <- renderPlot({
    req(convertedParticles())
    ggplot(convertedParticles(), aes(x = morphology, y = volume_mean_um_3, fill = factor(morphology))) +
      geom_flat_violin(
        position = position_nudge(x = 0.1),
        alpha = 0.5,
        scale = "width",
        trim = FALSE,
        width = 0.8,
        lwd = 1,
      ) +
      geom_boxplot(
        width = 0.12,
        outlier.shape = 8,
        outlier.color = "navy",
        alpha = 1
      ) +
      stat_dots(
        position = position_jitterdodge(jitter.width = 1, dodge.width = 0.4, jitter.height = 10),
        dotsize = 15,
        side = "left",
        justification = 1.1,
        binwidth = 0.08,
        alpha = 1.0
      ) +
      scale_fill_brewer(palette = "Spectral") +
      labs(
        title = "Particle Volume by Morphology Type",
        x = "Morphology",
        y = "Volume (um3)",
        fill = "Morphology"
      ) +
      coord_flip() +
      dark_theme_gray() +
      theme(
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18)
      )
  })

  output$plot4 <- renderPlot({
    req(convertedParticles())
    ggplot(convertedParticles(), aes(x = polymer, y = mean_mass_mg, fill = factor(polymer))) +
      geom_flat_violin(
        position = position_nudge(x = 0.1),
        alpha = 0.5,
        scale = "width",
        trim = FALSE,
        width = 0.8,
        lwd = 1,
      ) +
      geom_boxplot(
        width = 0.12,
        outlier.shape = 8,
        outlier.color = "navy",
        alpha = 1
      ) +
      stat_dots(
        position = position_jitterdodge(jitter.width = 1, dodge.width = 0.4, jitter.height = 10),
        dotsize = 15,
        side = "left",
        justification = 1.1,
        binwidth = 0.08,
        alpha = 1.0
      ) +
      scale_fill_brewer(palette = "Spectral") +
      labs(
        title = "Particle Mass by Polymer Type",
        x = "Polymer",
        y = "Mass (mg)",
        fill = "Polymer"
      ) +
      coord_flip() +
      dark_theme_gray() +
      theme(
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18)
      )
  })
  
  
  
  output$downloadPlot3 <- downloadHandler(
    filename = function() { "particle_volume_plot.pdf" },
    content = function(file) {
      pdf(file, paper = "default")
      plot(plot3())
      dev.off()
    }
  )
  
  output$downloadPlot4 <- downloadHandler(
    filename = function() { "particle_mass_plot.pdf" },
    content = function(file) {
      pdf(file, paper = "default")
      plot(plot4())
      dev.off()
    }
  )
  
  output$downloadData1 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Materials_Alias)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Materials_Alias, file, row.names=FALSE)
    }
  )
  
  output$downloadData2 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Materials_Hierarchy)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Materials_Hierarchy, file, row.names=FALSE)
    }
  )
  
  output$materialhierarchy <- renderTree({
    Materials_hierarchy
  }
  )
  
  output$itemshierarchy <- renderTree({
    Items_hierarchy
  }
  )
  
  output$downloadData3 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Items_Alias)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Items_Alias, file, row.names=FALSE)
    }
  )
  
  output$downloadData4 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Items_Hierarchy)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Items_Hierarchy, file, row.names=FALSE)
    }
  )
  
  
  output$downloadData5 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Material_Item_Relation)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Material_Item_Relation, file, row.names=FALSE)
    }
  )
  
  output$downloadData6 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Brand_Manufacturer_Relation)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Brand_Manufacturer_Relation, file, row.names=FALSE)
    }
  )
  
  output$downloadData7 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Brand_Item_Relation)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Brand_Item_Relation, file, row.names=FALSE)
    }
  )
  
  output$downloadData8 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(PrimeUnclassifiable)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(PrimeUnclassifiable, file, row.names=FALSE)
    }
  )
  
  output$downloadtest <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(NOAA)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(NOAA, file, row.names=FALSE)
    }
  )
  
  output$table1 = DT::renderDataTable({
    Materials_Alias
  }, style="bootstrap")
  
  output$table2 = DT::renderDataTable({
    Materials_Hierarchy
  }, style="bootstrap")
  
  output$table3 = DT::renderDataTable({
    Items_Alias
  }, style="bootstrap")
  
  output$table4 = DT::renderDataTable({
    Items_Hierarchy
  }, style="bootstrap")
  
  output$table5 = DT::renderDataTable({
    Material_Item_Relation
  }, style="bootstrap")
  
  output$table6 = DT::renderDataTable({
    Brand_Manufacturer_Relation
  }, style="bootstrap")
  
  output$table7 = DT::renderDataTable({
    Brand_Item_Relation
  }, style="bootstrap")
  
  output$table8 = DT::renderDataTable({
    PrimeUnclassifiable
  }, style="bootstrap")
  
  
}




