rm(list=ls()) 
library(SocialMediaLab)
library(igraph)
library(syuzhet)

setwd("...")

apikey<-"..."
key<-AuthenticateWithYoutubeAPI(apikey)

# (PLatzi) video<-c('poeKLD-dZp0&t=421s','wNug81vId6Y','B5gHQ0glwIw&t=1s','Vru1iXZRHes','Lju2-YcsdO0','n0iKLfEZOI4')

video<-c("YvtCLceNf30","tN9Xl1AcSv8","tN9Xl1AcSv8","sxQaBpKfDRk","kn83BA7cRNM","R4yfNi_8Kqw")
ytData<-CollectDataYoutube(video,key,writeToFile = FALSE,maxComments=5000)

View(ytData)
#Structure of the data
str(ytData)
write.csv(ytData,file = 'yt.csv',row.names = FALSE)
data<-read.csv('yt.csv',header = T)
str(data)

data<-data[data$ReplyToAnotherUser !=FALSE,]
View(data)

y<-data.frame(data$User,data$ReplyToAnotherUser)
View(y)

#USER NETWORK

net<-graph.data.frame(y,directed = T)
net<-simplify(net)

#Vertex
V(net)
E(net)

V(net)$label<-V(net)$name
V(net)$degree<-degree(net)


hist(V(net)$degree,col='blue',main = 'Grados de los Nodos', 
     ylab = 'Frecuencia',xlab = 'Grados de los vertices')


#Network diagram
#plot(net,vertex.size=0.3*V(net)$degree,edge.arrow.size=0.1,vertex.label.cex=0.1*V(net)$degree)
plot(net,vertex.size=1*V(net)$degree,edge.arrow.size=0.1,vertex.label.cex=0.8)



#Mineria de opinion
data<-read.csv('yt.csv',header = T)

comments<-iconv(data$Comment,to='utf-8',sub = "byte")

s<-get_nrc_sentiment(comments)
s

s$neutral<-ifelse(s$negative+s$positive==0,1,0)
head(s)

barplot(100*colSums(s)/sum(s),las=2,col = rainbow(10),ylab = "Porcentaje %",main = "Resultados analisis Youtube CrashCourse")



################### Nube de palabras ###################### 

dfTJ<-do.call("rbind",lapply(data$Comment,as.data.frame))
head(dfTJ$text)
colnames(dfTJ)<-c("text")

dfTJ$text<-sapply(dfTJ$text,function(row) iconv(row,"UTF-8","latin1",sub=""))
head(dfTJ$text)
dfTJ$text<-gsub("(f|ht)tp(s?)://(.*)[.][a-z]+","",dfTJ$text) #Elimina las URL
head(dfTJ$text)
dfTJtx<-dfTJ$text

txt_corpus<-Corpus(VectorSource(dfTJtx))
View(txt_corpus)

txt_corpus<-tm_map(txt_corpus,tolower)
txt_corpus<-tm_map(txt_corpus,removePunctuation)
txt_corpus<-tm_map(txt_corpus,removeNumbers)
txt_corpus<-tm_map(txt_corpus,stripWhitespace)

#Eliminar las palabras de conexion. 
txt_corpus<-tm_map(txt_corpus,removeWords,stopwords("en"))

head(txt_corpus$content)
View(txt_corpus)

dtm<-DocumentTermMatrix(txt_corpus)
dtm<-as.matrix(dtm)
dtm<-t(dtm)
View(dtm)

#Suma el numero de apariciones
number_ocurrences=rowSums(dtm)
number_ocurrences=sort(number_ocurrences,decreasing = TRUE)
head(number_ocurrences)

wordcloud(head(names(number_ocurrences),35),head(number_ocurrences,35),scale = c(3,2))


