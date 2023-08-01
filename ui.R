library(shiny)
library(dplyr)
library(data.table)
#library(crosstalk)
library(shinyjs)
library(shinythemes)
library(DT)
library(shinyhelper)
library(shinyTree)
#library(igraph)
#library(listviewer)
#library(treemap)
library(data.tree)
library(collapsibleTree)
library(plotly)

ui <- fluidPage(
  theme=shinytheme("cyborg"),
  titlePanel("Microplastics and Trash Taxonomy"),
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      body {
        background-color: black;
        color: white;
      }
                    "))
  ),
  tags$head(tags$style(HTML("
  .form-group.shiny-input-container select {
    background-color: #000000;  /* Black background color */
    color: #ffffff;             /* White font color */
    margin: 0;                  /* Remove margins */
    padding: 0;                 /* Remove padding */
    display: inline-block;      /* Add this to allow vertical-align to take effect */
    vertical-align: middle;     /* Align the dropdown vertically in the middle */
    width: 100%;                /* Set width to 100% of containing element */
  }
  table.dataTable tbody td { 
    vertical-align: middle;     /* Align the content of the cells vertically in the middle */
    text-align: center;
  }
"))),
  
  #About ----
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
                      img(width = "100%", src = "db_diagram.JPG"),
                      #HTML('<iframe width="560" height="315" src='https://dbdiagram.io/embed/5f3d9342cf48a141ff557dfe'> </iframe>'),
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
                      shiny::HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/sqeLaJKyol8" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                      
               ),
               column(3)
             ),
             
             fluidRow(
               
               style = "height:50px;"),
             
             tags$hr(),
             
             
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
    
    #Relational Tables ----
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
               column(1),
               column(10,
                      shiny::HTML("<br><br><center> <h1>Materials Hierarchy Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table describes how the unique material types relate to one another in a hierarchical structure (ex: foam and rubber are a subset of plastic).</h5>"),
                      #shinyTree(outputId = "materialhierarchy"),
                      
                        #collapsibleTreeOutput(outputId = "material_tree", width = "100%", height = "500px")
                      checkboxInput("show2", "Show Table", width = '50%'),
                      shinyTree::shinyTree(outputId = "materialhierarchy", dragAndDrop=F, sort = F, wholerow = T, theme = "default-dark", themeIcons = F, search = F)
                        
               ),
               column(1)
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
               column(1),
               column(10,
                      shiny::HTML("<br><br><center> <h1>Items Hierarchy Table</h1> </center><br>"),
                      shiny::HTML("<h5>This table describes how the unique items relate to one another in a hierarchical structure (ex: forks, knives, and spoons all fall under utensils).</h5>"),
                      #shinyTree(outputId = "itemhierarchy"),
                      #div(style = "background-color: white;",
                      #    collapsibleTreeOutput(outputId = "item_tree", width = "100%", height = "500px")
                      #),
                      checkboxInput("show4", "Show Table", width = '50%'),
                      shinyTree::shinyTree(outputId = "itemshierarchy", dragAndDrop=F, sort = F, wholerow = T, theme = "default-dark", themeIcons = F, search = F)
                      
                      
               ),
               column(1)
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
             
             tags$hr(),
             
             fluidRow(
               column(3
               ),
               column(6,
                      shiny::HTML("<br><br><center> <h1>Microplastic Color Alias</h1> </center><br>"),
                      shiny::HTML("<h5>This table describes the aliases that can be used to describe microplastic colors. Each row represents a unique color and each column is an alias for that color.</h5>"),
                      checkboxInput("show12", "Show Table", width = '100%')
                      
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
                               downloadButton('download12', 'Download')
                      )
                      
               ),
               column(3)),
             
             
             fluidRow(
               column(1),
               column(10,
                      
                      conditionalPanel(condition = "input.show12 == true",
                                       DT::dataTableOutput('table12')
                      )
               ), 
               column(1)
             ),
             
             
             fluidRow(
               
               style = "height:50px;"),
             
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
    
    #Tool ----
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
    ),
    #Survey Merging Tool ----
    tabPanel("Survey Merging and Visualization",
             titlePanel(tags$h4("Merge two or more exisitng survey sheets into one dataset")),
             
             fluidRow(
               column(2, 
                      fileInput('df_', "Choose Survey 1 CSV File", multiple = FALSE, accept = c(".csv"))%>%
                        helper(type = "inline",
                               title = "Upload Help",
                               content = c("To use the tool, upload a csv file to the upload file tab. This file need to be a csv with one column named -material- one named -items- and another named -count-. The material should correspond to the item names in the same row."),
                               size = "m"),
                      fileInput('d_f_', "Choose Survey 2 CSV File", multiple = FALSE, accept = c(".csv"))%>%
                        helper(type = "inline",
                               title = "Upload Help",
                               content = c("To use the tool, upload a csv file to the upload file tab. This file need to be a csv with one column named -material- one named -items- and another named -count-. The material should correspond to the item names in the same row."),
                               size = "m"),



             ),
             
             column(10, 
                    dataTableOutput('contents3')
             )
               
             ),
             
             hr(),
             fluidRow(
               column(3), 
               column(6,shiny::HTML("<br><br><center> <h4>Hierarchical Data Visualization</h4> </center><br>")
               ),
               column(3)
               
             ),
             fluidRow(
               column(1),
               column(5, 
                      plotlyOutput('plot1')
               ),
               column(5, 
                      plotlyOutput('plot2')
               ), 
               column(1)
             ),

             fluidRow(
               align="center",
               hr(),
               tags$p("Citation: H. Hapich, W. Cowger, A. Gray, Jambeck Research Group. 2020. Trash Taxonomy. https://trashtaxonomy.shinyapps.io/trashtaxonomy/")
             )
    ),
    
    #Concentration Conversion Tool ----
    tabPanel("Particle Mass Calculator",
             titlePanel(tags$h4("Convert between count, mass, and volume of particle level microplastic data")),
             
             fluidRow(
               column(2, 
                      fileInput('particleData', "Choose CSV File", multiple = FALSE, accept = c(".csv"))%>%
                        helper(type = "inline",
                               title = "Upload Help",
                               content = c("To use the tool, upload a csv file to the upload file tab. This file need to be a csv with one column named -length_um- one named -morphology- and another named -polymer-. Data should be reported at the particle level."),
                               size = "m"),
                      
                      
               ),
               
               column(10, 
                      dataTableOutput('contents5')
               ),
               
               fluidRow(
                 column(1),
                 column(5, 
                        plotOutput('plot3', width = "500px", height = "500px"),
                        
                        downloadButton('downloadPlot3', 'Download Plot')
                 ),
                 column(5, 
                        plotOutput('plot4', width = "500px", height = "500px"),
                        
                        downloadButton('downloadPlot4', 'Download Plot')
                 ), 
                 column(1)
               ),
               
             ),
             
             fluidRow(
               align="center",
               hr(),
               tags$p("Citation: H. Hapich, W. Cowger, A. Gray, Jambeck Research Group. 2020. Trash Taxonomy. https://trashtaxonomy.shinyapps.io/trashtaxonomy/")
             )
    ),
    
    #Data Correction Tool ----
    tabPanel("Microplastic Concentration Allignment",
             titlePanel(tags$h4("Correct your microplastic concentration data to fit the full distribution of microplastic sizes")),
             
             fluidRow(
               column(2, 
                      
                      fileInput('concentrationData', "Choose CSV File", multiple = FALSE, accept = c(".csv"))%>%
                        helper(type = "inline",
                               title = "Upload Help",
                               content = c("To use the tool, upload a csv file to the upload file tab. This file need to be a csv with columna named -study_media-, -concentration-, -concentration_units-, -size_min- and -size_max-. Columns -size_min- and -size_max- should be the actual size range studied for that sample."),
                               size = "m"),
                      
                      selectInput('concentration_type', "Known Particle Characteristic", c("", "length (um)","mass (ug)","volume (um3)","surface area (um2)","specific surface area (g/m2)")) %>%
                        helper(type = "inline",
                               title = "Selection Help",
                               content = c("Select the measured characteristic of your particles over which to normalize"),
                               size = "m"),
                      
                      numericInput('corrected_min', "Corrected Particle Range Minimum", 1, min = 1),
                      
                      
                      numericInput('corrected_max', "Corrected Particle Range Maximum", 5000, min = 1),
                      
                      
                      actionButton("calculate_distribution", "Calculate")
                      
               ),
               
               column(10, 
                      dataTableOutput('contents6'),
             ),
             
             ),
             
             
             fluidRow(
               align="center",
               hr(),
               tags$p("Citation: H. Hapich, W. Cowger, A. Gray, Jambeck Research Group. 2020. Trash Taxonomy. https://trashtaxonomy.shinyapps.io/trashtaxonomy/")
             )
    ),
    
    #Surveys for Download ----
    tabPanel("Surveys for Download",
             titlePanel(tags$h4("View and download suggested trash surveys to fit your study needs")),
             
             fluidRow(
               column(2, 
                      selectInput('sizeRange', "Choose size range", c("", "Micro","Macro","All")) %>%
                        helper(type = "inline",
                               title = "Selection Help",
                               content = c("Select if your study will include microplastics, macro-debris, or both."),
                               size = "m"),
                      selectInput('environments', "Choose environment", c("", "Marine/Estuarine", "Riverine", "Terrestrial", "All")) %>%
                        helper(type = "inline",
                               title = "Selection Help",
                               content = c("Select the environment your study will be conducted in, or include all."),
                               size = "m"),
                      selectInput('media', "Choose media", c("", "Surface Water","Sediment")) %>%
                        helper(type = "inline",
                               title = "Selection Help",
                               content = c("Select the media your study will be conducted in."),
                               size = "m"),
                      selectInput('specificity', "Choose specificity", c("", "More Specific","Less Specific")) %>%
                        helper(type = "inline",
                               title = "Selection Help",
                               content = c("Select how specific descriptor terms will be. More specific terms reccomended for scientific studies to increase comparability; less specific terms reccomended for volunteer groups to increase speed of surveying."),
                               size = "m"),
                      
               ),
               
               column(10, 
                      dataTableOutput('contents4')
               )
               
             ),
             
             fluidRow(
               align="center",
               hr(),
               tags$p("Citation: H. Hapich, W. Cowger, A. Gray, Jambeck Research Group. 2020. Trash Taxonomy. https://trashtaxonomy.shinyapps.io/trashtaxonomy/")
             )
    ),
             
             

    )
    #)
             )
