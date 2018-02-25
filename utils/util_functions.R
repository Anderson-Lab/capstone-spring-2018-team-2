meps_scrape = function(){
  library('rvest')
  
  #Specifying the url for desired website to be scrapped
  urls = c("utils/files/fyc_codes.html", "utils/files/prp_codes.html")
  #Reading the HTML code from the website
  dfs = list()
  
  for (i in 1:2){
    webpage = read_html(urls[i])
    
    #Using CSS selectors to scrap the rankings section
    table_data_html = html_nodes(webpage,'.codes')
    
    #Converting the table data to text
    table_data = html_text(table_data_html)
    
    # convert html table to a dataframe
    codes_df = html_table(table_data_html, header = F, trim = T)[[1]]
    
    rownames(codes_df) = codes_df$X1
    
    drops <- c("X2","X3")
    
    dfs[[i]] = codes_df[ 2:nrow(codes_df), !(names(codes_df) %in% drops)]
    
  }
    
    full_df = rbind(dfs[[1]], dfs[[2]])
    # map the variable description in column 'X4' to the codes in rd.p
    
    meta_named_char = setNames(full_df$X4, full_df$X1)
  
    save(meta_named_char, file = "meta.rda")
}
meps_scrape()
