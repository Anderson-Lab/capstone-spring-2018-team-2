meps_scrape = function(){
  library('rvest')
  
  load("data.rda")
  
  #Specifying the url for desired website to be scrapped
  url = "utils/files/meps_codes.html"
  
  #Reading the HTML code from the website
  webpage = read_html(url)
  
  #Using CSS selectors to scrap the rankings section
  table_data_html = html_nodes(webpage,'.codes')
  
  #Converting the table data to text
  table_data = html_text(table_data_html)
  
  # convert html table to a dataframe
  codes_df = html_table(table_data_html, header = F, trim = T)[[1]]
  
  rownames(codes_df) = codes_df$X1
  
  codes_df$X1 = NULL
  
  # map the variable description in column 'X4' to the codes in rd.p

  meta_named_char = sapply(colnames(rd.p), function(x) codes_df[x, 'X4'] )
  
  meta = data.frame(as.list(descriptions))
  
  save(meta, file = "meta.rda")
  
}
