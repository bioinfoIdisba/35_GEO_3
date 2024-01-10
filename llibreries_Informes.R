#Version 1 de la funcion DT:datatable(no usar con shiny)
datatable_mod<-function(df){DT::datatable(df,
                                          filter="top",
                                          extensions = 'Buttons',
                                          escape = FALSE,
                                          rownames = T,
                                          options = list(dom = 'Blfrtip',
                                                         pageLength = 10,info = TRUE,
                                                         lengthMenu = list(c(5,10,20,-1), c(5,10,20,"All rows")),
                                                         buttons =list('colvis', list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),
                                                                                      text = 'Download',filename= 'test')),
                                                         scrollX = TRUE,order=list(list(1,'desc'))))}
# Version 2 de la funccion datatable_mod para poder descargar solamente las columnas visibles con mas funcionalidades
datatable_mod2<-function(df){DT::renderDataTable(server=TRUE,{
  DT::datatable(
    df,editable = TRUE,
    extensions = "Buttons",
    filter = "top",
    selection = "none", #this is to avoid select rows if you click on the rows
    
    options = list(
      
      scrollX = TRUE,
      autoWidth = FALSE,
      dom = 'Blrtip', # the important thing is that there is the l to allow for the lengthMenu 
      # https://stackoverflow.com/questions/52645959/r-datatables-do-not-display-buttons-and-length-menu-simultaneously
      
      buttons = list(
        
        # insert buttons with copy and print
        # colvis includes the button to select and view only certain columns in the output table
        # from https://rstudio.github.io/DT/extensions.html 
        I('colvis'), 'copy', 'print',
        
        # code for the first dropdown download button
        # this will download only the current page only (depends on the number of rows selected in the lengthMenu)
        # using modifier = list(page = "current")
        # only the columns visible will be downloaded using the columns:":visible" option from:
        # https://stackoverflow.com/questions/72317260/how-to-download-only-the-selected-columns-in-a-dataframe-using-colvis-from-dt-in/72317607#72317607
        list(
          extend = 'collection',
          buttons = list(
            list(extend = "csv", filename = "page",exportOptions = list(
              columns = ":visible",modifier = list(page = "current"))
            ,server=TRUE),
            list(extend = 'excel', filename = "page", title = NULL, 
                 exportOptions = list(columns = ":visible",modifier = list(page = "current"))),server=TRUE,
                 list(extend = 'pdf', filename = "page", title = NULL, 
                  exportOptions = list(columns = ":visible",modifier = list(page = "current"))),server=TRUE),
          
          text = 'Download current page'),
        
        # code for the  second dropdown download button
        # this will download the entire dataset using modifier = list(page = "all")
        list(
          extend = 'collection',
          buttons = list(
            list(extend = "csv", filename = "data",exportOptions = list(
              columns = ":visible",modifier = list(page = "all"))
            ,server=FALSE),
            list(extend = 'excel', filename = "data", title = NULL, 
                 exportOptions = list(columns = ":visible",modifier = list(page = "all")),server=FALSE),
            list(extend = 'pdf', filename = "data", title = NULL, 
                exportOptions = list(columns = ":visible",modifier = list(page = "all")))),
          text = 'Download all data')
      ),
      # add the option to display more rows as a length menu
      lengthMenu = list(c(10, 30, 50, -1),
                        c('10', '30', '50', 'All'))
    ),
    class = "display"
  )

})}

# Version 2 de la funccion datatable_mod para poder descargar solamente las columnas visibles con mas funcionalidades(si que funciona con shiny)

datatable_mod3<-function(df,file_name=NULL){
  datatable(
  df,editable = FALSE,
  extensions = "Buttons",
  filter = "top",
  selection = "none", #this is to avoid select rows if you click on the rows
  
  options = list(
    scrollX = TRUE,
    autoWidth = FALSE,
    dom = 'Blrtip', # the important thing is that there is the l to allow for the lengthMenu 
    # https://stackoverflow.com/questions/52645959/r-datatables-do-not-display-buttons-and-length-menu-simultaneously
    
    buttons = list(
      
      # insert buttons with copy and print
      # colvis includes the button to select and view only certain columns in the output table
      # from https://rstudio.github.io/DT/extensions.html 
      I('colvis'), 'copy', 'print',
      
      # code for the first dropdown download button
      # this will download only the current page only (depends on the number of rows selected in the lengthMenu)
      # using modifier = list(page = "current")
      # only the columns visible will be downloaded using the columns:":visible" option from:
      # https://stackoverflow.com/questions/72317260/how-to-download-only-the-selected-columns-in-a-dataframe-using-colvis-from-dt-in/72317607#72317607
      list(
        extend = 'collection',
        buttons = list(
          list(extend = "csv", filename =file_name,exportOptions = list(
            columns = ":visible",modifier = list(page = "current"))
            ),
          list(extend = 'excel', filename = file_name, title = NULL, 
               exportOptions = list(columns = ":visible",modifier = list(page = "current"))),
          list(extend = 'pdf', filename = file_name, title = NULL, 
               exportOptions = list(columns = ":visible",modifier = list(page = "current")))),
        
        text = 'Download current page'),
      
      # code for the  second dropdown download button
      # this will download the entire dataset using modifier = list(page = "all")
      list(
        extend = 'collection',
        buttons = list(
          list(extend = "csv", filename = file_name,exportOptions = list(
            columns = ":visible",modifier = list(page = "all"))
            ),
          list(extend = 'excel', filename = file_name, title = NULL, 
               exportOptions = list(columns = ":visible",modifier = list(page = "all"))),
          list(extend = 'pdf', filename = file_name, title = NULL, 
               exportOptions = list(columns = ":visible",modifier = list(page = "all")))),
        text = 'Download all data')
    ),
    # add the option to display more rows as a length menu
    lengthMenu = list(c(10, 30, 50, -1),
                      c('10', '30', '50', 'All')),
  ),
  class = "display"
)
  
  
}


datatable_mod4<-function(df,file_name=NULL,col_inv=NULL,num_rows=10,order=T,rowname=T,filter="top"){
  col_inv <- which(names(df) %in% c(col_inv))
  
  
  datatable(df,
            editable = FALSE,
            
            extensions = c("Buttons"),
            filter = filter,
            rownames = rowname,
            options = list(pageLength=num_rows,
                           
                           columnDefs = list(list(visible=TRUE,
                                                  targets=c(col_inv))),scrollX = FALSE,  autoWidth = TRUE,scrollY = FALSE,
                           dom = 'Bflrtip',
                           ordering=order,
                           buttons = list(I('colvis'), 'copy',
                           list(extend = 'collection',buttons = list(list(extend = "csv",filename =file_name,exportOptions = list(columns = ":visible",
                                                                                        modifier = list(page = "current"))),
                                                              list(extend = 'excel', filename = file_name, title = NULL,
                                                                   exportOptions = list(columns = ":visible",modifier = list(page = "current"))),
                                                              list(extend = 'pdf', filename = file_name, title = NULL,
                                                                   exportOptions = list(columns = ":visible",modifier = list(page = "current")))),
                                               text = 'Download current page')),
                           lengthMenu = list(c(5,10, 30, 50, -1),
                                             c('5','10', '30', '50', 'All')))
            #class = c('compact cell-border stripe hover')
            )}


datatable_mod5<-function(df,file_name=NULL,col_inv=NULL,sel_name=NULL,sel_fold=NULL){
  col_inv <- which(names(df) %in% c(col_inv))
  if(length(grep("gen",colnames(as),ignore.case = T))>0){
    sel_name<-grep("gen",colnames(as),ignore.case = T)
  }
  
  if(length(grep("FC",colnames(as),ignore.case = T))>0){
    sel_fold<-grep("FC",colnames(as),ignore.case = T)
  }
  
  datatable(
    df,editable = FALSE, 
    extensions = c("Buttons",'Select'),
    selection = list(mode='multiple',target = "column",selected = c(sel_name,sel_fold)),  
    filter = "top", options = list(autoWidth = TRUE, bAutoWidth = T),
    # selection = "multiple", #this is to avoid select rows if you click on the rows
    options = list(columnDefs = list(list(visible=FALSE, targets=c(col_inv))),
                   scrollX = TRUE,
                   autoWidth = FALSE,
                   dom = 'Bflrtip', # the important thing is that there is the l to allow for the lengthMenu 
                   # https://stackoverflow.com/questions/52645959/r-datatables-do-not-display-buttons-and-length-menu-simultaneously
                   buttons = list(I('colvis'), 'copy', 'print',
                                  
                                  
                                  list(
                                    extend = 'collection',
                                    buttons = list(
                                      list(extend = "csv", filename =file_name,exportOptions = list(
                                        columns = ":visible",modifier = list(page = "current"))
                                      ),
                                      list(extend = 'excel', filename = file_name, title = NULL, 
                                           exportOptions = list(columns = ":visible",modifier = list(page = "current"))),
                                      list(extend = 'pdf', filename = file_name, title = NULL, 
                                           exportOptions = list(columns = ":visible",modifier = list(page = "current")))),
                                    
                                    text = 'Download current page'),
                                  
                                  # code for the  second dropdown download button
                                  # this will download the entire dataset using modifier = list(page = "all")
                                  list(
                                    extend = 'collection',
                                    buttons = list(
                                      list(extend = "csv", filename = file_name,exportOptions = list(
                                        columns = ":visible",modifier = list(page = "all"))
                                      ),
                                      list(extend = 'excel', filename = file_name, title = NULL, 
                                           exportOptions = list(columns = ":visible",modifier = list(page = "all"))),
                                      list(extend = 'pdf', filename = file_name, title = NULL, 
                                           exportOptions = list(columns = ":visible",modifier = list(page = "all")))),
                                    text = 'Download all data')
                   ),
                   # add the option to display more rows as a length menu
                   lengthMenu = list(c(10, 30, 50, -1),
                                     c('10', '30', '50', 'All'))
    ),
    class = "display"
  )
  
  
}

# Per fer diferents busquedes a se vegada

separarTargets <- function(targets){
  target2 <- strsplit(targets, ',')
  targets2 <-target2[[1]][1]
  for(i in 2:length(target2[[1]])){
    targets2 <- c(targets2, target2[[1]][i])
  }
  return(targets2)
}




datatable_jm<-function(x,column=NULL){
  datatable(
    x,
    extensions = 'Buttons',
    filter = list(position = 'top', clear = T),
    options = list(dom = 'Blfrtip',
                   scrollX = TRUE,
                   scrollY = T, 
                    scrollCollapse = TRUE,
                   
                   # buttons = list(list(extend = 'colvis')),
                   buttons = list(list(extend = 'colvis'),'copy', 'print',
                                  list(extend = 'collection',
                                       buttons = c('csv', 'excel', 'pdf'),
                                       text = 'Download')),
                   columnDefs = list(list(visible=FALSE, targets=column))))}



datatable_Mod <- function(
    df,
    to_print = FALSE,
    to_copy = FALSE,
    to_search = TRUE,
    to_entries= TRUE,
    to_download_all = TRUE,
    to_download_current = FALSE,
    to_download_format=c('csv', 'excel', 'pdf'),
    to_filter = 'none', # c('top', 'bottom', 'none')
    to_edit = FALSE,
    to_colvis =FALSE # column visibility
) {
  
  if(to_search + to_entries == 2 ){to_s_e <- 'Blfrtip'}else{ # to_s_e = search and entries
    if(to_search){to_s_e <- 'Bfrtip'}
    if(to_entries){to_s_e <- 'Blrtip'}
    if(to_search + to_entries == 0){to_s_e <- 'Brtip'}
  }
  
  #Funcio modificada datatable
  datatable(
    df,
    editable = to_edit,
    filter = to_filter,
    
    selection = "none", # ni idea de que fa
    escape = FALSE, # ni idea de que fa
    
    extensions = "Buttons",
    options = list(
      scrollX = TRUE,
      autoWidth = FALSE,
      dom = to_s_e,
      buttons = list(
        #I('colvis'),
        ifelse(to_colvis , I('colvis'),  ''),
        ifelse(to_search , 'search',  ''),
        ifelse(to_copy , 'copy',  ''),
        ifelse(to_print, 'print', ''),
        if(to_download_current){
          list(
            extend = 'collection',
            buttons =
              list(
                if('csv' %in% to_download_format){
                  list(
                    extend = "csv",
                    filename = "page",
                    title = 'page',
                    exportOptions = list(columns = ":visible", modifier = list(page = "current"))
                  )
                }else{''},
                
                if('excel' %in% to_download_format){
                  list(
                    extend = "excel",
                    filename = "page",
                    title = 'page',
                    exportOptions = list(columns = ":visible", modifier = list(page = "current"))
                  )
                }else{''},
                
                if('pdf' %in% to_download_format){
                  list(
                    extend = "pdf",
                    filename = "page",
                    title = 'page',
                    exportOptions = list(columns = ":visible", modifier = list(page = "current"))
                  )
                }else{''}
              ),
            text = 'Download current page'
          )
        }else{''},
        if(to_download_all){
          list(
            extend = 'collection',
            buttons =
              list(
                if('csv' %in% to_download_format){
                  list(
                    extend = "csv",
                    filename = "data",
                    title = 'data',
                    exportOptions = list(columns = ":visible", modifier = list(page = "all"))
                  )
                }else{''},
                
                if('excel' %in% to_download_format){
                  list(
                    extend = "excel",
                    filename = "data",
                    title = 'data',
                    exportOptions = list(columns = ":visible", modifier = list(page = "all"))
                  )
                }else{''},
                
                if('pdf' %in% to_download_format){
                  list(
                    extend = "pdf",
                    filename = "data",
                    title = 'data',
                    exportOptions = list(columns = ":visible", modifier = list(page = "all"))
                  )
                }else{''}
              ),
            text = 'Download all data'
            
          )
        }else {''}
        
      ),
      lengthMenu = list(c(10, 30, 50, -1), c('10', '30', '50', 'All'))
    ),
    class = "display"
  )
}

