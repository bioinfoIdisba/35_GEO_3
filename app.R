library(shiny)
  source("llibreries_Informes.R")
  source("llibreries.R")
  load(file = "gse_abstract")
  #s
  ui <- (fluidPage(
    # add_busy_bar(color = "lightblue", height = "18px"),
    add_busy_spinner(spin = "flower",position = "top-right",color = "blue",
                     margins = c(400, 400),
                     height = "200px",
                     width = "200px"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css.css")
    ),
    titlePanel(title = span(img(src = "actv_1.jpg", height = 75), "Housekeeping")),
    # titlePanel( div(column(width = 6, h2("Housekeeping")),column(width = 6, tags$img(src = "actv_1.jpg"))),
    #             windowTitle="MyPage"
    # ),
    # titlePanel("miRNA Housekeeping"),
    sidebarLayout(
      sidebarPanel(
        
        textInput("geoID", "ID GEO", value = ""),
        numericInput("cv_max", "CV max", min = 0.001, max = 10,value = 0.05),
        textInput("species_ID", "Species Scientific Name", value = "Homo sapiens"),
        textInput("type", "Sequence Type", value = "miRNA"),
        h5(
          "Gene Expression Omnibus
      GEO is a public functional genomics data repository supporting MIAME-compliant data submissions. Array- and sequence-based data are accepted. Tools are provided to help users query and download experiments and curated gene expression profiles.
      The table show all series of miRNA 4.0 available, with ID, title and summary of experiment."
          
        ),
        hr(),
        h5("geNorm is a popular algorithm to determine the most stable reference (housekeeping) genes from a set of tested candidate reference genes in a given sample panel. From this, a gene expression normalization factor can be calculated for each sample based on the geometric mean of a user-defined number of reference genes. The algorithm also determines the optimal number of reference genes needed for accurate normalization."),
        hr(),
        h5("Important. Genes are filtered by type, organism and CV (coefficient of variation) with the aim of reducing the number of candidates."),
        h5("Default: \n
         Type= miRNA.
         Organism: Homo sapiens.
         CV= 0.05")
        
        
      ),
      mainPanel(
        tabsetPanel(
          
          tabPanel("GEO Arrays", 
                   
                   div(
                     
                     DTOutput("gseTable")
                   )),
          tabPanel("Phenotype Data", 
                   DTOutput("phenoTable")),
          tabPanel("Feature Data", 
                   DTOutput("featureTable")),
          tabPanel("NormFinder Results", 
                   h5("Number of candidare genes is:"),
                   textOutput("num_house"),
                   h5("Fore more than 100 candidates it's not recomended execute geneNorm, will take lot of time "),
                   actionButton("genenorm", "GeneNorm"),
                   DTOutput("normFinderTable")),
          tabPanel("miRNA Target",
                   h5("Get the mRNA targets associated with selected miRNA"),
                   textInput("mirna_id", "Introduce your miRNA", value = "hsa-miR-3677-5p"),
                   actionButton("mirna", "Target finder"),
                   DTOutput("targetTable")
          )
          
          
          
          
        )
      )
    )
  ))
  
  server <- function(input, output, session) {
    # Load GPL21572 data
    # gpl97 <- getGEO('GPL21572')
    
    # series_id <- data.frame(ID = gpl97@header$series_id)
    output$gseTable <- renderDT({
      gse_abstract<-data.frame(gse_abstract)
      # datatable_Mod(gse_abstract,to_search = T,to_filter = "top",to_colvis=T,to_download_current = T)
      datatable_jm(data.frame(gse_abstract))
    })
    
    # Load GEO data based on user input
    gset <- reactive({
      geoID <- "GSE81767"
      geoID <- input$geoID
      if(length(geoID)>0){
        gset <- getGEO(geoID, GSEMatrix = TRUE, AnnotGPL = FALSE)
        gset <- gset[[1]]
        return(gset)}
    })
    # colnames(gset@featureData@data)
    # Display Phenotype Data
    output$phenoTable <- renderDT({
      gset<-gset()
      # datatable_Mod(gset@phenoData@data,to_search = T,to_filter = "top",to_colvis=T,to_download_current = T)
      
      datatable_jm(gset@phenoData@data)
    })
    
    # Display Feature Data
    output$featureTable <- renderDT({
      gset<-gset()
      colnames(gset@featureData@data)
      # datatable_Mod(gset@featureData@data,to_search = T,to_filter = "top",to_colvis=T,to_download_current = T)
      datatable_jm(gset@featureData@data,column = c("Alignments",
                                                    "Genome Context",
                                                    "Clustered miRNAs within 10kb",
                                                    "Target Genes","Sequence Source","miRNA_ID","SPOT_ID"))
    })
    
    # Run NormFinder and display results
    
    gset2 <- reactive({
      gset2<-gset()
      gset2<-gset2[gset2@featureData@data$`Species Scientific Name` == input$species_ID, ]
      gset2 <- gset2[gset2@featureData@data$`Sequence Type` == input$type, ]
      return(gset2)
    })
    
    gset_bol <- reactive({genefilter(gset2(), cv(0, input$cv_max))})
    output$num_house<-renderText((sum(gset_bol())))
    genrom_res <- eventReactive(input$genenorm, {
      gset_bol<-gset_bol()
      print(sum(gset_bol()))
      
      gset2 <- gset2()[gset_bol, ]
      
      data_to_geNorm <- exprs(gset2)
      data_to_geNorm <- t(data_to_geNorm)
      colnames(data_to_geNorm) <- make.names(gset2@featureData@data$`Transcript ID(Array Design)`, unique = TRUE)
      as <- geNorm2(data_to_geNorm, genes = data.frame(Genes = character(0), Avg.M = numeric(0)), ctVal = FALSE)
      as$Genes <- gsub("[.]", "-", as$Genes)
      return(as)
      
      
    })
    
    output$normFinderTable <- renderDT({
      # datatable_Mod(genrom_res(),to_search = T,to_filter = "top",to_colvis=T,to_download_current = T)
      genrom_res<-genrom_res()
      genrom_res[,2][is.na(genrom_res[,2])]<-0
      datatable_jm(genrom_res)
    })
    
    targets<-eventReactive(input$mirna, {
      
      
      validated <- get_multimir(mirna = input$mirna_id, summary = TRUE,table = "validated")
      validated@data
    })
    
    output$targetTable<-renderDT({
      datatable_jm(targets())
      
    })
  }
  shinyApp(ui, server)

