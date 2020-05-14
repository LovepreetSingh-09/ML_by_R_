library(Rook)
library(randomForest)

load('thRS500.Rdata')
str(buzztrain)
summary(buzztrain)
varslist

numericPositions = sapply(buzztrain[varslist], is.numeric)

model_fn <- function(env){
  errors <- c()
  warnings <- c()
  val <- c()
  row <- c()
  tryCatch(
    {
     arg <- Multipart$parse(env)
     row <- as.list(arg[varslist])
     names(row) <- varslist
     row[numericPositions] <- as.numeric(row[numericPositions])
     frame <- data.frame(row)
     val <- predict(fmodel,newdata = row)
    },
    warning =function(w){message(w)
      warnings <- c(warnings, as.character(w))},
    error = function(e){ message(e)
      errors <- c(errors, as.character(e))}
  )
  body <- paste(
    'val=',val,'\n',
    'nerrors=',length(errors),'\n',
    'nwarnings=',length(warnings),'\n',
    'query=',env$QUERY_STRING,'\n',
    'errors=',paste(errors,collapse=' '),'\n',
    'warnings=',paste(warnings,collapse=' '),'\n',
    'data row','\n',
    paste(capture.output(print(row)),collapse='\n'),'\n',
    sep='')
  list(
    status=ifelse(length(errors)<=0,200L,400L),
    headers=list('Content-Type' = 'text/text'),
    body=body
    )
}

s <- Rhttpd$new()
s$add(name='modelFn', app=modelFn)
s$start()
print(s)

rowAsForm <- function(url,row) {
  s <- paste('<HTML><HEAD></HEAD><BODY><FORM action="',url,
             '" enctype="multipart/form-data" method="POST">\n',sep='')
  s <- paste(s,'<input type="submit" value="Send"/>',sep='\n')
  qpaste <- function(a,b) {
    paste('<p> ',a,' <input type="text" name="',a,
          '" value="',b,'"/> </p>',sep='') }
  assignments <- mapply('qpaste',varslist,as.list(row)[varslist])
  s <- paste(s,paste(assignments,collapse='\n'),sep='\n')
  s <- paste(s,'</FORM></BODY></HTML>',sep='\n')
  s
}

url <- 'http://127.0.0.1:20714/custom/modelFn'

cat(rowAsForm(url,buzztest[7,]),file='buzztest7.html')

extractTrees <- function(rfModel) {
  ei <- function(i) {
    ti <- getTree(rfModel,k=i,labelVar=T)
    ti$nodeid <- 1:dim(ti)[[1]]
    ti$treeid <- i
    ti
  }
  nTrees <- rfModel$ntree
  do.call('rbind',sapply(1:nTrees,ei,simplify=F))
}

write.table(extractTrees(fmodel),
            file='rfmodel.tsv',row.names=F,sep='\t',quote=F)
