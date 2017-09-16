install.packages(shiny)
install.packages(visNetwork)
install.packages(reshape)
install.packages(igraph)
install.packages(magrittr)

library(shiny)
library(visNetwork)
library(reshape)
library(igraph)
library(magrittr)


data <- read.csv("C:/Users/phani/Desktop/Desktop/Ms Courses/Advanced Business Analytics with R/Part 1 project/COCAINE_DEALING.csv")
data1 <- read.csv("C:/Users/phani/Desktop/Desktop/Ms Courses/Advanced Business Analytics with R/Part 1 project/COCAINE_DEALING.csv",row.names = 1)
mdata <- melt(data, id = 1)
Submdata <- subset(mdata, mdata$value!=0)
sub<- data[,c(2:29)]
nodes <- data.frame(id = data[,1],label= data[,1])
edges <- data.frame(from = Submdata[,1], 
                    to = Submdata[,2])
my_matrix <- as.matrix(data1)
g <- graph_from_adjacency_matrix(my_matrix, mode = "undirected", diag = FALSE, weighted = TRUE)
g1 <- graph_from_adjacency_matrix(my_matrix, mode = "directed", diag = FALSE, weighted = TRUE)
dgre <- igraph::degree(g,mode="all")
bet <- betweenness(g)
Indegree <- igraph::degree(g1,mode="in")
Outdegree <- igraph::degree(g1,mode="out")
a <- cbind(dgre,Indegree,Outdegree,bet)
colnames(a) <- c("Degree", "InDegree","OutDgree","Betweenness")
c<- paste(data[,1],dgre,bet)
Total = sum(mdata$value)
MostCalls = which.max(mdata$value)
mcr <- aggregate(value ~ variable, mdata, sum)
mcr
Index = which( mcr$value == max(mcr$value) )
mcrr<-mcr$variable[Index]
mc <- aggregate(value ~ mdata$ï.., mdata, sum)
mcindex = which( mc$value == max(mc$value) )
mcc<- mc$`mdata$ï..`[mcindex]

b <- data.frame(mcrr)

ui <- fluidPage(
  titlePanel("PhaniTejaBalagam_SNA"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file"), # fileinput() function is used to get the file upload contorl option
      helpText("Default max. file size is 5MB"),
      tags$hr(),
      h5(helpText("Select the read.table parameters below")),
      checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
      br(),
      radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
    ),
    mainPanel(
      uiOutput("tb")
      # use below code if you want the tabset programming in the main panel. If so, then tabset will appear when the app loads for the first time.
      #       tabsetPanel(tabPanel("Summary", verbatimTextOutput("sum")),
      #                   tabPanel("Data", tableOutput("table")))
    )
    
  )
)

# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
# options(shiny.maxRequestSize = 9*1024^2)

server <- function(input,output){
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.table(file=file1$datapath, sep=input$sep, header = input$header)
    
  })

  # this reactive output contains the summary of the dataset and display the summary in table format
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$betweennesss <- renderVisNetwork({
    if(is.null(data())){return ()}  
    nodes$shape <- "dot"  
    nodes$shadow <- TRUE # Nodes will drop shadow
    nodes$title <- bet # Text on click
    nodes$size <- bet/2 # Node size
    nodes$borderWidth <- 2 # Node border width
    
    
    #nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$`in-degree centrality`]
    nodes$color.border <- "black"
    nodes$color.highlight.background <- "orange"
    nodes$color.highlight.border <- "darkred"
    
    visNetwork(nodes, edges,height = "500px", width = "100%")%>%
      visEdges(color = list(color = "black", highlight = "blue")) %>% visEdges(color = list(hover = "red"))%>% 
      visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", 
                                         degree = list(from = 0, to = 2))) %>%
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
                                         background: #f8f8f8;
                                         color: darkblue;
                                         border:none;
                                         outline:none;'))
  })
  
  output$Degree <- renderVisNetwork({
    nodes$shape <- "dot"  
    nodes$shadow <- TRUE # Nodes will drop shadow
    nodes$title <- dgre # Text on click
    nodes$borderWidth <- 2 # Node border width
    
    
    #nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$`in-degree centrality`]
    nodes$color.border <- "black"
    nodes$color.highlight.background <- "orange"
    nodes$color.highlight.border <- "darkred"
    nodes$size <- dgre*2 # Node size
    
    visNetwork(nodes, edges,height = "500px", width = "100%")%>%
      visEdges(color = list(color = "red", highlight = "blue")) %>% visEdges(color = list(hover = "red"))%>% 
      visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", 
                                         degree = list(from = 0, to = 2))) %>%
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
                                         background: #f8f8f8;
                                         color: darkblue;
                                         border:none;
                                         outline:none;'))})
  
  output$In <- renderVisNetwork({
    if(is.null(data())){return ()}  
    nodes$shape <- "dot"  
    nodes$shadow <- TRUE # Nodes will drop shadow
    nodes$title <- Indegree # Text on click
    nodes$size <- Indegree*10 # Node size
    nodes$borderWidth <- 2 # Node border width
    
    
    #nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$`in-degree centrality`]
    nodes$color.border <- "black"
    nodes$color.highlight.background <- "orange"
    nodes$color.highlight.border <- "darkred"
    
    visNetwork(nodes, edges,height = "500px", width = "100%")%>%
      visEdges(color = list(color = "black", highlight = "blue")) %>% visEdges(color = list(hover = "red"))%>% 
      visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", 
                                         degree = list(from = 0, to = 2))) %>%
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
                                         background: #f8f8f8;
                                         color: darkblue;
                                         border:none;
                                         outline:none;'))
  })
  
  output$Out <- renderVisNetwork({
    if(is.null(data())){return ()}  
    nodes$shape <- "dot"  
    nodes$shadow <- TRUE # Nodes will draop shadow
    nodes$title <- Outdegree # Text on click
    nodes$size <- Outdegree*5 # Node size
    nodes$borderWidth <- 2 # Node border width
    
    
    #nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$`in-degree centrality`]
    nodes$color.border <- "black"
    nodes$color.highlight.background <- "orange"
    nodes$color.highlight.border <- "darkred"
    
    visNetwork(nodes, edges,height = "500px", width = "100%")%>%
      visEdges(color = list(color = "black", highlight = "blue")) %>% visEdges(color = list(hover = "red"))%>% 
      visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", 
                                         degree = list(from = 0, to = 2))) %>%
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
                                         background: #f8f8f8;
                                         color: darkblue;
                                         border:none;
                                         outline:none;'))
  })
  
  output$Inbound <- renderVisNetwork({
    if(is.null(data())){return ()}  
    nodes$shape <- "dot"  
    nodes$shadow <- TRUE # Nodes will draop shadow
    nodes$borderWidth <- 2 # Node border width
    nodes$title <- c # Text on click
    
    #nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$`in-degree centrality`]
    nodes$color.border <- "black"
    nodes$color.highlight.background <- "orange"
    nodes$color.highlight.border <- "darkred"
    
    edges <- data.frame(from = Submdata[,1], 
                        to = Submdata[,2], width = Submdata$value)
    visNetwork(nodes, edges,height = "500px", width = "100%")%>% visEdges(arrows = 'to') %>%
      visEdges(color = list(color = "green", highlight = "blue")) %>% visEdges(color = list(hover = "red"))%>% 
      visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", 
                                         degree = list(from = 0, to = 2))) %>%
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
                                         background: #f8f8f8;
                                         color: darkblue;
                                         border:none;
                                         outline:none;'))
  })
  
  output$OutBound <- renderVisNetwork({
    if(is.null(data())){return ()}  
    nodes$shape <- "dot"  
    nodes$shadow <- TRUE # Nodes will draop shadow
    nodes$borderWidth <- 2 # Node border width
    nodes$title <- c 
    
    #nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$`in-degree centrality`]
    nodes$color.border <- "black"
    nodes$color.highlight.background <- "orange"
    nodes$color.highlight.border <- "darkred"
    
    edges <- data.frame(from = Submdata[,1], 
                        to = Submdata[,2], width = Submdata$value)
    visNetwork(nodes, edges,height = "500px", width = "100%")%>% visEdges(arrows = 'from') %>%
      visEdges(color = list(color = "green", highlight = "blue")) %>% visEdges(color = list(hover = "red"))%>% 
      visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", 
                                         degree = list(from = 0, to = 2))) %>%
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
                                         background: #f8f8f8;
                                         color: darkblue;
                                         border:none;
                                         outline:none;'))
  })
  
  output$all <- renderVisNetwork({
    if(is.null(data())){return ()}  
    nodes$shape <- "dot"  
    nodes$shadow <- TRUE # Nodes will draop shadow
    nodes$borderWidth <- 2 # Node border width
    nodes$title <- c 
    
    #nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$`in-degree centrality`]
    nodes$color.border <- "black"
    nodes$color.highlight.background <- "orange"
    nodes$color.highlight.border <- "darkred"
    
    edges <- data.frame(from = Submdata[,1], 
                        to = Submdata[,2], width = Submdata$value)
    visNetwork(nodes, edges,height = "500px", width = "100%")%>%
      visEdges(color = list(color = "green", highlight = "blue")) %>% visEdges(color = list(hover = "red"))%>% 
      visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", 
                                         degree = list(from = 0, to = 2))) %>%
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
                                         background: #f8f8f8;
                                         color: darkblue;
                                         border:none;
                                         outline:none;'))
  })
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  
  output$transformedtable <- renderTable({
    if(is.null(data())){return ()}
    melt(data(), id = 1)
  })
  
  output$Summary <- renderTable(a,rownames = TRUE)
  
  output$text <- renderUI({ 
  str1 <- paste("Total Calls Placed", Total)
  str2 <- paste("Most Calls were received by",b[1,],"and",b[2,])
  str3 <- paste("Most Calls were Placed by:",mcc)
  HTML(paste(str1, str2, str3, sep = '<br/>'))
  })
  
  output$Comparison <- renderUI({ 
    str1 <- paste("* From graphs its clear that kay is operating the whole drug trafficking")
    str2 <- paste("* Menna,Tommy,Steve, Blacky seems to be assistants of Kay. These people are working for kay to assigning the tasks to asscoiated workers working under each on eof them")
    str3 <- paste("* All the others in the Network are workers under Menna, Tommy,Steve, Blacky or Kay")
    str4 <- paste("* Kay calls Tommy regulary to check the status of their task or to assign the work. 
                  Once Tommy gets some  task he inturn assigns it to his associated workers by calling on them(Donald, Marizo, David). Also, Tommy some times has conversation with Menna who is also a Important person in this trafficking ")
    
    
    HTML(paste(str1, str2, str3,str4, sep = '<br/>'))
  })
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      h1("Social Network Analysis",heigth=200, width=200)
    else
      tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data", tableOutput("table")),tabPanel("Transformedtable", tableOutput("transformedtable")),tabPanel("Centrality Score", tableOutput("Summary")),tabPanel("Summary", uiOutput("text")),
                  tabPanel("Network Graphs", tabsetPanel( tabPanel("Degree", visNetworkOutput("Degree")),tabPanel("betweenness", visNetworkOutput("betweennesss")),tabPanel("Indegree", visNetworkOutput("In")), tabPanel("Outdegree", visNetworkOutput("Out")),
                                                          tabPanel("All Calls", visNetworkOutput("all")),tabPanel("Inbound", visNetworkOutput("Inbound")),tabPanel("OutBound", visNetworkOutput("OutBound"))
                          )), tabPanel("Comparison", uiOutput("Comparison")))
  })
  
  
}

shinyApp(ui = ui, server = server)

###https://datastorm-open.github.io/visNetwork/shiny.html