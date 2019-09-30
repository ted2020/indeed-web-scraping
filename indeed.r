# indeed jobs analysis

library(rvest)
library(dplyr)
library(tidyverse)
library(xml2)
library(purrr)
library(stringi)

job_title <- "Data+Scientist"
location <- "New+York%2C+NY"
radius <- 100
limit <- 50

page_result_start <- 0 # starting page 
page_result_end <- 1000 # last page results
page_results <- seq(from = page_result_start, to = page_result_end, by = 50)
page <- paste0("https://www.indeed.com/jobs?q=",job_title,"&l=",location,"&radius=",radius,"&limit=",limit,"&start=",page_results)


jobtitles <- lapply(page,
                function(url){
                    url %>% read_html() %>% 
                        html_nodes(xpath='//*[@data-tn-element = "jobTitle"]') %>%
                        html_attr("title")
                })
jobtitles <- data.frame(jobtitles=unlist(jobtitles))
head(jobtitles,1)

joblinks <- lapply(page,
                function(url){
                    url %>% read_html() %>% 
                        html_nodes(xpath='//*[@data-tn-element = "jobTitle"]') %>%
                        html_attr("href")
                })
joblinks <- data.frame(joblinks=unlist(joblinks))
head(joblinks,1)

for (i in 1:length(joblinks[,])){
joblinks2 <- paste0("www.indeed.com",joblinks[i,])
    #print((joblinks2))
    i <- i+1
    }
print(i)

joblinks2 <- data.frame(lapply(joblinks, as.character), stringsAsFactors=FALSE)
joblinks2 <- lapply(joblinks2, function(x) paste("https://www.indeed.com", x, sep=""))
joblinks2 <- (as.data.frame(joblinks2))
head(joblinks2)

i <- 1
read_html(as.character(joblinks2[i,1]))%>% html_nodes(xpath='//*[@class="jobsearch-jobDescriptionText"]')%>% html_text()

i = 1
desc = c()

len <- (nrow(joblinks2[])) * (1/100)
for (i in 1:len) {
    desc[i] = read_html(as.character(joblinks2[i, 1])) %>% html_nodes(xpath = "//*[@class=\"jobsearch-jobDescriptionText\"]") %>% 
        html_text()
}
desc <- data.frame(desc[])
desc






















listings <- data.frame(title=character(),
                 company=character(), 
                 location=character(), 
                 summary=character(), 
                 link=character(), 
                 description = character(),
                 stringsAsFactors=FALSE) 
for (i in seq(0, 30, 10)){
  url_ds <- paste0('https://www.indeed.com/jobs?q=data+scientist&l=all&start=',i)
  var <- read_html(url_ds)
  
  #job title
  title <-  var %>% 
    html_nodes('#resultsCol .jobtitle') %>%
    html_text() %>%
    str_extract("(\\w+.+)+") 
  
  #company
  company <- var %>% 
    html_nodes('#resultsCol .company') %>%
    html_text() %>%
    str_extract("(\\w+).+") 
  
  #location
  location <- var %>%
    html_nodes('#resultsCol .location') %>%
    html_text() %>%
    str_extract("(\\w+.)+,.[A-Z]{2}")   
  #summary
  summary <- var %>%
    html_nodes('#resultsCol .summary') %>%
    html_text() %>%
    str_extract(".+")
  
  #link
  link <- var %>%
    html_nodes('#resultsCol .jobtitle .turnstileLink, #resultsCol a.jobtitle') %>%
    html_attr('href') 
  link <- paste0("https://www.indeed.com",link)
    
  listings <- rbind(listings, as.data.frame(cbind(title,
                                                  company,
                                                  location,
                                                  summary,
                                                  link)))
}

#obtain full description for all job postings
for (i in (1:length(listings$link))){
  desciption <- tryCatch(
     html_text(html_node(read_html(as.character(listings$link[i])),'.jobsearch-JobComponent-description')),
     error=function(e){NA}
  )
  if (is.null(desciption)){
    desc <- NA
  }
  listings$description[i] <- desciption
}













