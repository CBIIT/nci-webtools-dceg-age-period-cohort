library('RJSONIO');
library('stringr');
library('ggplot2');
library('lattice');
library('directlabels');


tmpDirectory <- "./tmp/";
#inputfile="/media/sf_shared/CrosTalk_test/China_test.csv"

#Graphs the rates tab. Input is a csv file
graph_TemporalTrends<-function(inputfile){
  information=parseCSV(inputfile)
  table=information[[3]]
  rows=nrow(table)
  cols=ncol(table)
 
   years <- numeric(0)
  year_ranges<-table[1,2:cols]
  number_of_years=length(year_ranges)
  for(i in 1:number_of_years) {
    year<-as.numeric(strsplit(year_ranges[[i]], "-")[[1]][1])
    print (year_ranges[[i]])
    years[i]=year
  }
  
  ages<-table[2:rows,1]
  rates<-apply(table[2:cols,2:rows], 1,as.numeric) 
  data <- numeric(0)
  for (i in 2:ncol(rates)){
      color=paste(sample(0:255,size=3,replace=TRUE),collapse=" ")
      line <- data.frame(Calender_Periods=years, Rate_Per_100000=rates[i,1:ncol(rates)], age=table[i],shapes=3) # linear x=x
     # print (line)
      data <- rbind(data,line)
  }
  p <- ggplot(data, aes(x=Calender_Periods, y=Rate_Per_100000, group=age,colour=age))+geom_line()+geom_point(aes(shape=age))
  ggsave(file = paste0(unique(p$SIC), "CrossTalk.png"))
    

}
#Parses a csv file and returns a vecor of the matrix, title and description
parseCSV<-function(inputfile){
  
  contents = read.csv(inputfile, stringsAsFactors = T, header = F, skip = 2)
  headers = read.csv(inputfile, stringsAsFactors = F, header = F)[[1]][1:2]
  
  Title=as.character(headers[[1]]);
  Description=as.character(headers[[2]]);
  
  data=as.matrix(contents);
  content_list =list(Title, Description, data);
  
  return (content_list)

}


#graph_TemporalTrends(inputfile)
