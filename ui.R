library(shiny)
library(dplyr)
library(data.table)
library(crosstalk)
library(shinyjs)
library(shinythemes)
library(DT)
library(shinyhelper)
library(shinyTree)
library(igraph)
library(listviewer)
library(treemap)
library(data.tree)

#setwd("/Users/hannahhapich/desktop/Trash_Taxonomy/TrashTaxonomy")

ui <- fluidPage(
  theme=shinytheme("cyborg"),
  titlePanel("Trash Taxonomy"),
  tabsetPanel(
    tabPanel("About",
             fluidRow(
               column(3),
               column(6,
                      
                      shiny::HTML("<br><br><center> <h1>Overview</h1> </center><br>"),
                      shiny::HTML("<h5>Trash Taxonomy website is a portal for relational tables that relate trash survey nomenclature. It includes 7 relational tables and a tool that can be used to query the relational tables with trash survey sheets.
                                  This tool was developed by collating and comparing categories and terms used from over 50 commonly used in trash survey sheets.</h5>"),
                      shiny::HTML("<h5>We are grateful for the funding for this project provided by the National Marine Sanctuary Foundation, the National Oceanic and Atmospheric Administration Marine Debris Program, and the Benioff Ocean Initiative</h5>")
                      
                      ),
               column(3)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>About the Relational Tables</h1> </center><br>"),
                      align = "center",
                      img(src="RelationalStructure.png", width = "100%"),
                      shiny::HTML("<h5>These relational tables describe alias relationships (words that mean the same thing) and hierarchical relationships (words that are nested groups within one another). You can view or download these tables using the relational table tab above!</h5>")
               ),
               column(3)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             # HOW
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>About the Query Tool</h1> </center><br>"),
                      shiny::HTML("<h5>This tool queries the relational tables with an uploaded trash survey list. To use the tool, upload a csv file to the upload file tab. 
                                  The file needs to be a csv with one column named -material- and another named -items-. 
                                  The material should correspond to the item names in the same row.</h5>")
                      ),
               column(3)
                      ),
             tags$hr(),
             
             
             fluidRow(
               column(3),
               column(6,
                      
                      shiny::HTML("<br><br><center> <h1>How To Use</h1> </center><br>"),
                      shiny::HTML("<h5>In order to assist your navigation through both the relational tables and query tool functions of this app, please refer to the video tutorial below.</h5>"),
                      shiny::actionButton(inputId='ab1', label="Tutorial", 
                                          icon = icon("th"), 
                                          onclick ="window.open('https://youtu.be/BXy8oFJZAnMx', '_blank')")
                      
               ),
               column(3)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      shiny::HTML("<br><br><center> <h1>Additional Hierarchy Tool</h1> </center><br>"),
                      shiny::HTML("<h5>With help visualizing and rearranging the hierarchy sheets available for download on this site, along with any you may create, use the link below.</h5>"),
                      shiny::actionButton(inputId='ab1', label="Reorganize Hierarchies", 
                                          icon = icon("th"), 
                                          onclick ="window.open('https://wincowger.shinyapps.io/RearrangeHierarchies/', '_blank')")
                      
               ),
               column(3)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Funded By</h1> </center><br>"),
                      align = "center",
                      img(src="NOAA.png", width = "50%"),
                      img(src="NMSF.png", width = "50%"),
                      img(src="boi.png", width = "50%")
                      
                      #downloadButton(NOAA, label="Download")
                      #tags$div(align = "center",
                      #        tags$a("Sample Data",
                      #              onclick = "window.open('https://drive.google.com/file/d/1YKyEDf4VbZaeSlV6yxgh0XVqstvel6LQ/view', '_blank')",
                      #             class="btn btn-primary btn-lg")
                      #     )
                      
               ),
               column(3)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             # INSTRUCTIONAL SECTION
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Citation</h1> </center>
                                  <br>"),
                      shiny::HTML("<h5> H. Hapich, W. Cowger, A. Gray, Jambeck Research Group. 2020. Trash Taxonomy. https://trashtaxonomy.shinyapps.io/trashtaxonomy/</h5>")
                      ),
               column(3)
             )
             
             
             #end of about panel
             ),
    
    
    tabPanel("Relational Tables",
             titlePanel(tags$h4("View and Download Relational Tables")),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3
               ),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Materials Alias Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table describes the aliases that can be used to describe material types and links them to a key term. Each row represents a unique material and each column is an alias for that material.</h5>"),
                      checkboxInput("show1", "Show Table", width = '100%')
                      
               ),
               column(3
               )
             ),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      tags$div(align = "center", 
                               downloadButton('downloadData1', 'Download')
                      )
                      
               ),
               column(3)),
             
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show1 == true",
                                       DT::dataTableOutput('table1')
                      )
               ), 
               column(1)
             ),
             
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Materials Hierarchy Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table describes how the unique material types relate to one another in a hierarchical structure (ex: foam and rubber are a subset of plastic).</h5>"),
                      shinyTree(outputId = "materialhierarchy"),
                      checkboxInput("show2", "Show Table", width = '50%')
                      
               ),
               column(3)
             ),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      tags$div(align = "center", 
                               downloadButton('downloadData2', 'Download')
                      )
                      
               ),
               column(3)
             ),
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show2 == true",
                                       DT::dataTableOutput('table2')
                      )
               ), 
               column(1)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Items Alias Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table describes the aliases that can be used to describe item types and links them to a key term. Each row represents a unique item and each column is an alias for that item.</h5>"),
                      checkboxInput("show3", "Show Table", width = '50%')
               ),
               column(3)
             ),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      tags$div(align = "center", 
                               downloadButton('downloadData3', 'Download')
                      )
                      
               ),
               column(3)
             ),
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show3 == true",
                                       DT::dataTableOutput('table3')
                      )
               ), 
               column(1)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Items Hierarchy Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table describes how the unique items relate to one another in a hierarchical structure (ex: forks, knives, and spoons all fall under utensils).</h5>"),
                      shinyTree(outputId = "itemhierarchy"),
                      checkboxInput("show4", "Show Table", width = '50%')
                      
               ),
               column(3)
             ),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      tags$div(align = "center", 
                               downloadButton('downloadData4', 'Download')
                      )
                      
               ),
               column(3)
             ),
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show4 == true",
                                       DT::dataTableOutput('table4')
                      )
               ), 
               column(1)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Material-Item Relational Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table relates the items, materials, and survey sheets used to make the other relational tables.</h5>"),
                      checkboxInput("show5", "Show Table", width = '50%')
                      
               ),
               column(3)
             ),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      tags$div(align = "center", 
                               downloadButton('downloadData5', 'Download')
                      )
                      
               ),
               column(3)
             ),
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show5 == true",
                                       DT::dataTableOutput('table5')
                      )
               ), 
               column(1)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Manufacturer Brand Relational Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table relates brand types to their respective manufacturer.</h5>"),
                      checkboxInput("show6", "Show Table", width = '50%')
                      
               ),
               column(3)
             ),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      tags$div(align = "center", 
                               downloadButton('downloadData6', 'Download')
                      )
                      
               ),
               column(3)
             ),
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show6 == true",
                                       DT::dataTableOutput('table6')
                      )
               ), 
               column(1)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Item-Brand Relational Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table relates brands to items.</h5>"),
                      checkboxInput("show7", "Show Table", width = '50%')
                      
               ),
               column(3)
             ),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      tags$div(align = "center", 
                               downloadButton('downloadData7', 'Download')
                      )
                      
               ),
               column(3)
             ),
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show7 == true",
                                       DT::dataTableOutput('table7')
                      )
               ), 
               column(1)
             ),
             fluidRow(
               
               style = "height:50px;"),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Misaligned Categories Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table displays all categories that did not fit our item-material framework.</h5>"),
                      checkboxInput("show8", "Show Table", width = '50%')
                      
               ),
               column(3)
             ),
             
             # PAGE BREAK
             tags$hr(),
             
             fluidRow(
               column(3),
               column(6,
                      
                      tags$div(align = "center", 
                               downloadButton('downloadData8', 'Download')
                      )
                      
               ),
               column(3)
             ),
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show8 == true",
                                       DT::dataTableOutput('table8')
                      )
               ), 
               column(1)
             ),
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Citation</h1> </center>
                                  <br>"),
                      shiny::HTML("<h5> H. Hapich, W. Cowger, A. Gray, Jambeck Research Group. 2020. Trash Taxonomy. https://trashtaxonomy.shinyapps.io/trashtaxonomy/</h5>")
                      ),
               column(3)
             )
             
             ),
    
    
    tabPanel("Query Tool",
             titlePanel(tags$h4("Query the Relational Tables with Trash Survey Sheets")),
             fluidRow(
               column(2, 
                      fileInput('df', "Choose CSV File", multiple = FALSE, accept = c(".csv"))%>%
                        helper(type = "inline",
                               title = "Upload Help",
                               content = c("To use the tool, upload a csv file to the upload file tab. The file needs to be a csv with one column named -material- and another named -items-. The material should correspond to the item names in the same row."),
                               size = "m"),
                      
                      downloadButton('downloadtest', 'Download Test Data'),
                      
                      checkboxGroupInput('variable', "Functions:",
                                         c("More Specific Materials"="MoreSpecificMaterial",
                                           "Less Specific Materials"="LessSpecificMaterial",
                                           "More Specific Items"="MoreSpecificItem",
                                           "Less Specific Items"="LessSpecificItem"))),
               column(10, 
                      dataTableOutput('contents')
               )
               
             ),
             hr(),
             fluidRow(
               column(3), 
               column(6,shiny::HTML("<br><br><center> <h4>View Key Alias Matches</h4> </center><br>")
               ),
               column(3)
               
             ),
             fluidRow(
               column(1),
               column(5, 
                      dataTableOutput('contents1')
               ),
               column(5, 
                      dataTableOutput('contents2')
               ), 
               column(1)
             ),
             fluidRow(
               align="center",
               hr(),
               tags$p("Citation: H. Hapich, W. Cowger, A. Gray, Jambeck Research Group. 2020. Trash Taxonomy. https://trashtaxonomy.shinyapps.io/trashtaxonomy/")
             )
    )
    )
             )
