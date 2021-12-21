#Packages ----
library(collapsibleTree)
library(dplyr)
library(tidyr)
library(data.tree)
#library(collapsibleTree)

##Functions ----
cleantext <- function(x) {
    tolower(gsub("[[:space:]]", "", x))
}

DF <- SMC
Hierarchy <- ItemsHierarchy
Alias <- ItemsAlias
ColNum <- 2

not_all_na <- function(x) any(!is.na(x))

#requires alias to have Alias, RowID and Key columns, requires DF to have a count column and some column to match
jointohierarchy <- function(DF, Hierarchy, Alias, ColNum) {
    DF <- mutate_all(DF, cleantext) %>%
      mutate(Count = as.numeric(Count))
    Hierarchy <- mutate_all(Hierarchy, cleantext)
    Alias <- mutate_all(Alias, cleantext) %>%
      mutate(RowID = as.integer(RowID))
    
    DF$RowID <- unlist(apply(DF, 1, function(x) which(Alias == as.character(x[ColNum]), arr.ind = TRUE)[1]))
    
    DF <- DF %>%
        left_join(dplyr::select(Alias, RowID, Key)) %>%
        dplyr::select(Key, Count) %>%
        mutate(Key = ifelse(is.na(Key), "other", Key))
    
    list <- apply(DF, 1, function(x) which(Hierarchy == as.character(x[1]), arr.ind = TRUE))
    
    for(n in 1:length(list)){
        if(length(list[[n]]) == 0) next
        DF[n, "Row"] <- unname(list[[n]][1,1])
        DF[n, "Column"] <- unname(list[[n]][1,2])
    }
    
    RowColSum <- DF %>%
        group_by(Row, Column) %>%
        summarise(sum = sum(Count)) %>%
        filter(!is.na(Row)) #This one of the culprits, the rows aren't being matched so they are being dropped and we need some backup mechanism. 
    
    HierarcyFinalForm <- Hierarchy[0,]
    HierarcyFinalForm$sum <- numeric()
    
    row = 25
    for(row in 1:nrow(RowColSum)){
        filter1 <- Hierarchy %>%
            filter(.[[1]] == Hierarchy[unlist(RowColSum[row, "Row"]),1])
        filter2 <- filter1 %>%
            filter_all(any_vars(. == Hierarchy[unlist(RowColSum[row, "Row"]),unlist(RowColSum[row, "Column"])])) %>%
            dplyr::select(where(not_all_na)) %>%
            mutate(sum = unlist(RowColSum[row, "sum"]))
             
        if(unlist(RowColSum[row, "Column"]) < ncol(filter2)-1) {
            filter2[,(unlist(RowColSum[row, "Column"])+1):(ncol(filter2)-1)] <- NA
        }
        filter2 <- filter2 %>%
            distinct()
        
        #print(sum(filter2$sum) == sum(RowColSum[row, "sum"]))
    
        HierarcyFinalForm <- bind_rows(HierarcyFinalForm, filter2)
    }
    
    test <-  HierarcyFinalForm %>%
        mutate_if(is.character, as.factor) %>%
        mutate(sum = as.numeric(unname(sum))) %>%
        dplyr::group_by(across(c(-sum))) %>%
        summarise(sum = sum(sum)) %>%
        ungroup() %>%
        mutate_if(is.factor, as.character) 
    
   }

removeslash <- function(x){
  gsub("/", " OR ", x)
}

converttotree <- function(x){
  #x[is.na(x)] <- ""
  x <- mutate_all(x, removeslash) %>%
    mutate(key = "trash") %>%
    mutate(sum = as.numeric(sum)) %>%
    dplyr::relocate(key) %>%
    dplyr::group_by(across(c(-sum))) %>%
    dplyr::summarise(sum = sum(sum)) %>%
    unite(pathString, sep = "/", na.rm = T, -sum) ##Seems like we may be losing some of the sums here, would expect original values to be equal to the summed.
  FromDataFrameTable(x)
}

#check out this: https://stackoverflow.com/questions/45225671/aggregating-values-on-a-data-tree-with-r
myApply <- function(node) {
  node$totalsum <- 
    sum(c(node$sum, purrr::map_dbl(node$children, myApply)), na.rm = TRUE)
}

DF1 <- NOAA
DF2 <- SMC
Hierarchy <- ItemsHierarchy
Alias <- ItemsAlias
ColNum <- 2


AggregateTrees <- function(DF1, DF2, Alias, Hierarchy, ColNum){
  
   DFA <- jointohierarchy(DF = DF1, Hierarchy = Hierarchy, Alias = Alias, ColNum = ColNum)
  
   DFB <- jointohierarchy(DF = DF2, Hierarchy = Hierarchy, Alias = Alias, ColNum = ColNum)
 
   binded <- bind_rows(DFA, DFB) %>%
    add_row(Level.1 = "missing", sum = sum(DF1$Count) + sum(DF2$Count) - sum(DFA$sum) - sum(DFB$sum))
   
   bindedtree <- converttotree(binded)
   
   myApply(bindedtree)
   print(bindedtree, "sum", "totalsum")
}

##Working Directory ----
setwd("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/TrashTaxonomy/WinTrashTaxonomy/Data/Raw Data")


#Data sets ----
ItemsHierarchy <- read.csv("ITEMSHierarchyLower.csv")

MaterialsHierarchy <- read.csv("MaterialsHierarchyLower.csv") 

ItemsAlias <- read.csv("PrimeItems.csv")%>%
  mutate(RowID = 1:nrow(.)) %>%
  rename(Key = Item)

MaterialsAlias <- read.csv("PrimeMaterials.csv") %>%
  mutate(RowID = 1:nrow(.)) %>%
  rename(Key = Material)

SMC <- read.csv("RawMaterialItem.csv") %>%
    filter(Organization == "SMC")

NOAA <- read.csv("RawMaterialItem.csv") %>%
    filter(Organization == "NOAA")


#Data Processing ----

set.seed(128)
SMC$Count <- sample(1:10, nrow(SMC), replace = T)
set.seed(128)
NOAA$Count <- sample(1:10, nrow(NOAA), replace = T)

#runing the aggregate trees function ----
#These will output the aggregated trees for the example in the paper. 
#Output for lumping analysis ----
AggregateTrees(SMC, NOAA,  Alias = ItemsAlias, Hierarchy = ItemsHierarchy, ColNum = 2)

#DFA <- jointohierarchy(DF = SMC, Hierarchy = ItemsHierarchy, Alias = ItemsAlias, ColNum = 2)


AggregateTrees(SMC, NOAA,  Alias = MaterialsAlias, Hierarchy = MaterialsHierarchy, ColNum = 1)

#Figures of the hierarchy trees ----
collapsibleTree(
  MaterialsHierarchy, hierarchy = c(names(MaterialsHierarchy)), collapsed = F, fontSize = 30, zoomable = T
)

collapsibleTree(
  ItemsHierarchy, hierarchy = c(names(ItemsHierarchy)), fontSize = 20, zoomable = T, width = 3000, height = 1000
)
