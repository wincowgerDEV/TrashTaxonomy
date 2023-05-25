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
          Primename_ <- row.names(cross_product)[apply(cross_product, MARGIN = 2,  FUN = which.max)]
          
          #Top key alias match for given alias
          Primename <- materials_alias_embeddings$Material[materials_alias_embeddings$Alias == Primename_]
          
          #Input prime key into dataframe
          dataframe[row, "PrimeMaterial"] <- Primename
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
    for(row in 1:nrow(dataframe)) { 
      
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
        
        #Top key alias match for given alias
        Primename <- items_alias_embeddings$Item[items_alias_embeddings$Alias == Primename_]
        
        #Input prime key into dataframe
        dataframe[row, "PrimeItem"] <- Primename
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
  
  output$contents1 <- renderDataTable(server = F,
                                      datatable({df()[, c("material","PrimeMaterial")] %>% distinct()},
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




