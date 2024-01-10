library(rvest)

webpage <- read_html("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GPL21572")

tbls <- html_nodes(webpage, "table")

tbls_ls <- webpage %>%
  html_nodes("table") %>%
  .[1:5] %>%
  html_table(fill = TRUE)


tbls2_ls <- list()

# scrape Table 2. Nonfarm employment...
# tbls2_ls$Table1 <- 
as<-  
  webpage %>%
  html_nodes("table")

data.frame(as)
library(XML)
tbls_xml <- readHTMLTable(webpage)


gpl97 <- getGEO('GPL21572')

gpl97@header$series_id


gset1 <- getGEO(gpl97@header$series_id[10], GSEMatrix =TRUE, AnnotGPL=FALSE)
gset1<-gset1[[1]]

abstract<-lapply(gpl97@header$series_id[-4], function (x){ gset1 <- getGEO(x, GSEMatrix =TRUE, AnnotGPL=FALSE); 
gset1 <-gset1[[1]];
gset1@experimentData@abstract })


title<-lapply(gpl97@header$series_id[-4], function (x){ gset1 <- getGEO(x, GSEMatrix =TRUE, AnnotGPL=FALSE); 
gset1 <-gset1[[1]];
gset1@experimentData@title })

gset1@experimentData@abstract

x<-"GSE81867"
gpl97@header$series_id!=x
gset1 <- getGEO(x, GSEMatrix =TRUE, AnnotGPL=FALSE); 
gset1@experimentData

length(gpl97@header$series_id[-4])
length(abstract)

gse_abstract<-cbind(gpl97@header$series_id[-4],title,abstract)



save(gse_abstract,file = "gse_abstract")


library(ctrlGene)
library(genefilter)


gset1 <- getGEO(gpl97@header$series_id[1], GSEMatrix =TRUE, AnnotGPL=FALSE)
gset1<-gset1[[1]]
gset1<-gset1[gset1@featureData@data$`Species Scientific Name`=="Homo sapiens",]
gset1<-gset1[gset1@featureData@data$`Sequence Type`=="miRNA",]
## Take lot of time. Filter with gene filter before?


library(genefilter)
cvfun <- cv(1,1)
cvfun <- cv(0,0.05)


gset_bol<-genefilter(gset1,cvfun)
gset2<-gset1[gset_bol,]
gset2@featureData@data$`Transcript ID(Array Design)`

dim(gset2)

# expression:  a matrix of expression levels. Each row corresponds to a sample and each column to a gene.
# genes:  a data frame to output the result of the function
# ctVal: a logical value indicating data type. If ct-values are input, ctVal=TRUE, otherwise, ctVal=FALSE.
dim(t(exprs(gset2)))
data_to_geNorm<-t(exprs(gset2))
colnames(data_to_geNorm)<-make.names(gset2@featureData@data$`Transcript ID(Array Design)`,unique = T)
as<-geNorm2(data_to_geNorm, genes = data.frame(Genes = character(0), Avg.M =numeric(0)), 
            ctVal = F)
as$Genes<-gsub("[.]","-",as$Genes)


# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4619470/