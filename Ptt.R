rm(list=ls(all=TRUE));
library(RCurl);
library(XML);
library(Rcpp);
dir.create(".\\data");
host <- "https://www.ptt.cc";
allWeb <- readLines(con="Pttweb.txt");

for( i in 1:length(allWeb))
{
  web <- allWeb[i];
  
  board <- htmlParse(getURL(paste0(host,web)));
  board <- xpathSApply(board,"//a[@class='btn wide']",xmlGetAttr,"href")[2];
  NewestPage <- gregexpr("[0-9]+.html",board)[[1]];
  NewestPage <- substr(board,NewestPage,NewestPage+attr(NewestPage,"match.length")-6);
  NewestPage <- as.integer(NewestPage)+1;
  
  web <- sub(".html","",web);
  webPage <- NULL;
  for( j in 1:NewestPage)
  {
    tmpPage <- paste0(host,web,j,".html");
    webPage <- c(webPage,tmpPage);
  }
  
  webName <- gregexpr("/bbs/[^/]+",web);
  webName <- substr(web,webName[[1]][1]+5,webName[[1]][1]+attr(webName[[1]],"match.length")-1);
  tmpwebPage <- NULL;
  tmp <- NULL;
  
  n <- as.integer(NewestPage/50);
  file <- paste0(".\\data\\",webName,".txt");
  file <- file(file,open='w');
  des <- "//div[@class='title']/a";
  for( j in 0:n)
  {
    tmpwebPage <- htmlParse(getURL(webPage[(j*50+1):(j*50+50)]));
    tmpwebPage <- xpathSApply(tmpwebPage,des,xmlGetAttr,"href");
    tmp <- write(x=tmpwebPage,append = T,file);
  }
  if(NewestPage %% 50 !=0 && n > 0)
  {
    tmpwebPage <- htmlParse(getURL(webPage[((n+1)*50+1):NewestPage]));
    tmpwebPage <- xpathSApply(tmpwebPage,"//div[@class='title']/a",xmlGetAttr,"href");
    tmp <- write(x=tmpwebPage,append = T,file = paste0(".\\data\\",webName,".txt"));
  }
  close(file);
}
