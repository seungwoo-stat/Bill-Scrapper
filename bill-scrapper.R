library(httr2)
library(rvest)
library(foreach)
library(doParallel)

pagestart <- 1
pageend <- 2517

## register clusters
cl <- parallel::makeCluster(detectCores()-1, outfile = "")
registerDoParallel(cl)

## progress bar
pb <- txtProgressBar(min = pagestart, max = pageend, style = 3)

data <- foreach(page=pagestart:pageend, .packages=c('httr2','rvest'), .combine = rbind) %dopar% {
  setTxtProgressBar(pb, page) 
    
  ## request POST to the simple search page
  read.page <- request('https://likms.assembly.go.kr/bill/BillSearchResult.do') |> 
    req_headers(
      Host="likms.assembly.go.kr",
      Origin="https://likms.assembly.go.kr",
      Referer="https://likms.assembly.go.kr/bill/BillSearchResult.do",
      `User-Agent`="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36"
    ) |>
    req_body_form(
      ageFrom=21, ######## 21대 국회부터
      ageTo=21, ######## 21대 국회까지 의안 정보 검색
      billKind="전체",
      proposerKind= "전체",
      proposeGubn="전체",
      strPage=page,  ######## 페이지번호
      tabMenuType= "billSimpleSearch",
    ) |>
    req_perform()
  
  ## read response 
  html.page <- read.page$body |> 
    read_html()
  
  ### (1) extract urls of each bill
  urls <- html.page |>
    html_nodes(xpath='/html/body/div/div[2]/div[2]/div/div[2]/table/tbody/tr[*]/td[2]/div[2]/a') |>
    html_attr('href')
  url.quotes.where <- sapply(gregexpr(pattern ="'",urls),\(l)l[1:2])
  bill.urls <- substr(urls,url.quotes.where[1,]+1,url.quotes.where[2,]-1)
  
  n <- length(bill.urls)
  bill.name = character(n)
  bill.num = numeric(n)
  propose.date = character(n)
  propose.who = character(n)
  proposer.short = character(n)
  propose.session = character(n)
  bill.url = character(n)
  proposer.full = character(n)
  decision.date = character(n)
  decision.result = character(n)
  progress.status = character(n)
  
  ### (2) extract infos from each bill
  propose.who <- html.page |>
    html_nodes(xpath='/html/body/div/div[2]/div[2]/div/div[2]/table/tbody/tr[*]/td[3]') |>
    html_text(trim=TRUE)
  
  decision.date <- html.page |>
    html_nodes(xpath='/html/body/div/div[2]/div[2]/div/div[2]/table/tbody/tr[*]/td[5]') |>
    html_text(trim=TRUE)
  
  decision.result <- html.page |>
    html_nodes(xpath='/html/body/div/div[2]/div[2]/div/div[2]/table/tbody/tr[*]/td[6]') |>
    html_text(trim=TRUE)
  
  progress.status <- html.page |>
    html_nodes(xpath='/html/body/div/div[2]/div[2]/div/div[2]/table/tbody/tr[*]/td[8]') |>
    html_text(trim=TRUE)
  
  for(i.bill in seq_along(bill.urls)){
    bill.url[i.bill] <- paste0("https://likms.assembly.go.kr/bill/billDetail.do?billId=",bill.urls[i.bill])
    html.bill <- read_html(bill.url[i.bill])
    bill.name[i.bill] <- html.bill |>
      html_nodes(xpath='/html/body/div/div[2]/div[2]/h3/text()') |> 
      html_text(trim=TRUE)
    info <- html.bill |>
      html_nodes(xpath='/html/body/div/div[2]/div[2]/div/div[3]/div[1]/table/tbody/tr/td') |>
      html_text(trim=TRUE)
    bill.num[i.bill] <- info[1]
    propose.date[i.bill] <- info[2]
    proposer.short[i.bill] <- info[3]
    propose.session[i.bill] <- info[5]
    proposer.full[i.bill] <- read_html(paste0("https://likms.assembly.go.kr/bill/coactorListPopup.do?billId=",bill.urls[i.bill])) |>
      html_nodes(xpath='//*[@id="periodDiv"]/div[2]/div/a') |>
      html_text(trim=TRUE) |> 
      paste0(collapse=' ')
  }
  ### (3) return
  cbind(
    bill.name,
    bill.num,
    propose.date,
    propose.who,
    proposer.short,
    propose.session,
    bill.url,
    proposer.full,
    decision.date,
    decision.result,
    progress.status
  )
}
  
## close parallel clusters and progress bar
close(pb)
stopCluster(cl)

## change column names
data <- data.frame(data)
names(data) <- c('의안명','의안번호','제안일자','제안구분','제안자(short)',
                 '제안회기','URL','제안자(full)','의결일자','의결결과','심사진행상태')

View(data)

## save
write.csv(data,"~/Desktop/congress_data.csv")
