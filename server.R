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


#Files for tool
alius <- read.csv("data/PrimeMaterials.csv")
hierarchy <- read.csv("data/MaterialsHierarchyLower.csv")
aliusi <- read.csv("data/PrimeItems.csv")
hierarchyi <- read.csv("data/ITEMSHierarchyLower.csv")
micromat <- read.csv("data/Microplastics_Material.csv")
micromorph <- read.csv("data/Microplastics_Morphology.csv")
microcolor <- read.csv("data/Microplastics_Color.csv")
micromathierarchy <- read.csv("data/Microplastics_Material_Hierarchy.csv")
aliasclean <- mutate_all(alius, cleantext)
aliascleani <- mutate_all(aliusi, cleantext)
hierarchyclean <- mutate_all(hierarchy, cleantext)
hierarchycleani <- mutate_all(hierarchyi, cleantext)
micromatclean <- mutate_all(micromat, cleantext)
micromorphclean <- mutate_all(micromorph, cleantext)
microcolorclean <- mutate_all(microcolor, cleantext)
micromathierarchyclean <- mutate_all(micromathierarchy, cleantext)

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

#Creating microplastic taxonomy hierarchy
MicroMaterials <- micromathierarchy
MicroMaterials[is.na(MicroMaterials)] <- ""
MicroMaterials <- mutate_all(MicroMaterials, removeslash)
MicroMaterials$pathString <- paste("Trash", MicroMaterials$X1, sep = "/")
MicroMaterials <- as.Node(MicroMaterials[,])
MicroMaterials <- as.list(MicroMaterials)
MicroMaterials <- MicroMaterials[-1]
MicroMaterials <- cleanmaterials(MicroMaterials)

MicroMaterials_hierarchy <- micromathierarchy
MicroMaterials_hierarchy[is.na(MicroMaterials_hierarchy)] <- ""
MicroMaterials_hierarchy <- mutate_all(MicroMaterials_hierarchy, removeslash) %>%
  mutate(key = "trash") %>%
  relocate(key) %>%
  unite(pathString, sep = "/")
MicroMaterials_hierarchy <- as.Node(MicroMaterials_hierarchy, pathDelimiter = "/")
MicroMaterials_hierarchy <- as.list(MicroMaterials_hierarchy)
MicroMaterials_hierarchy <- MicroMaterials_hierarchy[-1]

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
Micro_Mat_Display <- read.csv("data/Microplastics_Morphology.csv")
MicroMaterials_Hierarchy <-read.csv("data/Microplastics_Material_Hierarchy.csv")
Micro_Morph_Display <-read.csv("data/Microplastics_Morphology.csv")
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
    legacy_data <- read.csv("data/legacy_count_data.csv")
    legacy_data <- rbind(legacy_data, dataframe)
    legacy_data <- legacy_data %>%
      group_by(material, items) %>%
      summarise(across(count, sum))
    write.csv(legacy_data,"data/legacy_count_data.csv")
    
    return(dataframe)
  })
  
  ###END MERGING TOOL
  
  #Output correct survey sheet
  MicroOnly <- read.csv("data/PremadeSurveys/Most_Specific_Microplastics.csv")
  MacroMarineMore <- read.csv("data/PremadeSurveys/Most_Specific_Marine.csv")
  MacroMarineLess <- read.csv("data/PremadeSurveys/Least_Specific_Marine.csv")
  MarineMore <- read.csv("data/PremadeSurveys/Most_Specific_Marine_All.csv")
  MarineLess <- read.csv("data/PremadeSurveys/Least_Specific_Marine_All.csv")
  MacroRiverineMore <- read.csv("data/PremadeSurveys/Most_Specific_Riverine.csv")
  MacroRiverineLess <- read.csv("data/PremadeSurveys/Least_Specific_Riverine.csv")
  RiverineMore <- read.csv("data/PremadeSurveys/Most_Specific_Riverine_All.csv")
  RiverineLess <- read.csv("data/PremadeSurveys/Least_Specific_Riverine_All.csv")
  MacroEstuarineMore <- read.csv("data/PremadeSurveys/Most_Specific_Estuarine.csv")
  MacroEstuarineLess <- read.csv("data/PremadeSurveys/Least_Specific_Estuarine.csv")
  EstuarineMore <- read.csv("data/PremadeSurveys/Most_Specific_Estuarine_All.csv")
  EstuarineLess <- read.csv("data/PremadeSurveys/Least_Specific_Estuarine_All.csv")
  MacroTerrestrialMore <- read.csv("data/PremadeSurveys/Most_Specific_Terrestrial.csv")
  MacroTerrestrialLess <- read.csv("data/PremadeSurveys/Least_Specific_Terrestrial.csv")
  TerrestrialMore <- read.csv("data/PremadeSurveys/Most_Specific_Terrestrial_All.csv")
  TerrestrialLess <- read.csv("data/PremadeSurveys/Least_Specific_Terrestrial_All.csv")
  MacroAllMore <- read.csv("data/PremadeSurveys/Most_Specific_Macro.csv")
  MacroAllLess <- read.csv("data/PremadeSurveys/Least_Specific_Macro.csv")
  AllMore <- read.csv("data/PremadeSurveys/Most_Specific_All.csv")
  AllLess <- read.csv("data/PremadeSurveys/Least_Specific_All.csv")
  
  selectSurvey <- reactive({
    if(input$sizeRange == "Micro"){return (MicroOnly)}
    if(input$sizeRange == "Macro" && input$environments == "Marine" && input$specificity == "More Specific"){data = MacroMarineMore}
    if(input$sizeRange == "Macro" && input$environments == "Marine" && input$specificity == "Less Specific"){data = MacroMarineLess}
    if(input$sizeRange == "All" && input$environments == "Marine" && input$specificity == "More Specific"){data = MarineMore}
    if(input$sizeRange == "All" && input$environments == "Marine" && input$specificity == "Less Specific"){data = MarineLess}
    if(input$sizeRange == "Macro" && input$environments == "Riverine" && input$specificity == "More Specific"){data = MacroRiverineMore}
    if(input$sizeRange == "Macro" && input$environments == "Riverine" && input$specificity == "Less Specific"){data = MacroRiverineLess}
    if(input$sizeRange == "All" && input$environments == "Riverine" && input$specificity == "More Specific"){data = RiverineMore}
    if(input$sizeRange == "All" && input$environments == "Riverine" && input$specificity == "Less Specific"){data = RiverineLess}
    if(input$sizeRange == "Macro" && input$environments == "Estuarine" && input$specificity == "More Specific"){data = MacroEstuarineMore}
    if(input$sizeRange == "Macro" && input$environments == "Estuarine" && input$specificity == "Less Specific"){data = MacroEstuarineLess}
    if(input$sizeRange == "All" && input$environments == "Estuarine" && input$specificity == "More Specific"){data = EstuarineMore}
    if(input$sizeRange == "All" && input$environments == "Estuarine" && input$specificity == "Less Specific"){data = EstuarineLess}
    if(input$sizeRange == "Macro" && input$environments == "Terrestrial" && input$specificity == "More Specific"){data = MacroTerrestrialMore}
    if(input$sizeRange == "Macro" && input$environments == "Terrestrial" && input$specificity == "Less Specific"){data = MacroTerrestrialLess}
    if(input$sizeRange == "All" && input$environments == "Terrestrial" && input$specificity == "More Specific"){data = TerrestrialMore}
    if(input$sizeRange == "All" && input$environments == "Terrestrial" && input$specificity == "Less Specific"){data = TerrestrialLess}
    if(input$sizeRange == "Macro" && input$environments == "All" && input$specificity == "More Specific"){data = MacroAllMore}
    if(input$sizeRange == "Macro" && input$environments == "All" && input$specificity == "Less Specific"){data = MacroAllLess}
    if(input$sizeRange == "All" && input$environments == "All" && input$specificity == "More Specific"){data = AllMore}
    if(input$sizeRange == "All" && input$environments == "All" && input$specificity == "Less Specific"){data = AllLess}
    if(input$sizeRange == "" && input$environments == "" && input$specificity == ""){return(NULL)}

    data = as.data.frame(data)
    colnames(data) = c("material","items","count")
    return(data)
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
                                        selectSurvey()[, c("material", "items", "count")]
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
  
  #output$material_tree <- renderCollapsibleTree(collapsibleTree(Materials_Hierarchy,
  #                                                              root = "Materials Hierarchy",
  #                                                              hierarchy = names(Materials_Hierarchy), 
  #                                                              #width = 800,
  #                                                              fontSize = 14))
  
  output$materialhierarchy <- renderTree({
    #shiny::validate(shiny::need(input$file1, "Please upload a dataset to get started")) 
    Materials_hierarchy
  }
  )
  
  output$itemshierarchy <- renderTree({
    #shiny::validate(shiny::need(input$file1, "Please upload a dataset to get started")) 
    Items_hierarchy
  }
  )
  output$micromaterialhierarchy <- renderTree({
    #shiny::validate(shiny::need(input$file1, "Please upload a dataset to get started")) 
    MicroMaterials_hierarchy
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
  
  #MICROTAX ADD ON
  output$download9 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Micro_Mat_Display)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Micro_Mat_Display, file, row.names=FALSE)
    }
  )
  
  output$download10 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(MicroMaterials_Hierarchy)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(MicroMaterials_hierarchy, file, row.names=FALSE)
    }
  )
  
  output$download11 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Micro_Morph_Display)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Micro_Morph_Display, file, row.names=FALSE)
    }
  )
  
  output$download12 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Micro_Color_Display)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Micro_Color_Display, file, row.names=FALSE)
    }
  )
  #END MICROTAX ADD ON
  
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
  
  #MICRO TAX ADD ON
  output$table9 = DT::renderDataTable({
    Micro_Mat_Display
  }, style="bootstrap")
  
  output$table10 = DT::renderDataTable({
    MicroMaterials_Hierarchy
  }, style="bootstrap")
  
  output$table11 = DT::renderDataTable({
    Micro_Morph_Display
  }, style="bootstrap")
  
  output$table12 = DT::renderDataTable({
    Micro_Color_Display
  }, style="bootstrap")
  #END MICRO TAX ADD ON
  
}




