rm(list=ls(all=TRUE));
library(RCurl);
library(XML);
library(rjson);

host <- "https://www.ptt.cc";

Parser <- function(url)
{
  urlx <- paste0(host,url);
  web <- htmlParse(getURL(urlx));
  webHref <- xpathSApply(web,"//div[@id='prodlist']/dl/dd/p/a", xmlGetAttr,"href");
  return (webHref);
}


web <- NULL;
IndexH <- Parser("/bbs/index.html");
X <- Parser(IndexH);
web <- X[-grep("[0-9]+.html",X)];
X <- X[grep("[0-9]+.html",X)];
X <- X[-grep("4881",X)];
for( i in 1:length(X))
{
  x <- X[i];
  while(length(x) > 0)
  {
    t <- Parser(x);
    if(any(grepl("bbs/1.html",t)==T))
      t <- t[-grep("bbs/1.html",t)];
    num <- grep("[0-9]+.html",t);
    if( length(num) <= 0 )
    {
      web <- c(web,t);
      t <- NULL;
      x <- NULL;
      break;
    } else {
      web <- c(web,t[-num]);
      x <- t[num];
    }  
  }
}
write(web,"PTTweb.txt");

