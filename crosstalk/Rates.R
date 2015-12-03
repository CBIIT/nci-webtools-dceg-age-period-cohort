library('RJSONIO');
library('stringr');
library('ggplot2');
library('lattice');
library('directlabels');


tmpDirectory <- "./tmp/";
inputfile="/media/sf_shared/CrosTalk_test/China_test.csv"

#Graphs the rates tab. Input is a csv file
#Rows and Columns much be the same
graph_TemporalTrends<-function(inputfile,uniqeID){
  information=parseCSV(inputfile) #retrieving a vector containg the contents, title and descrition of the csv file
  table=information[[3]] #storing the contents
  rows=nrow(table) #counting how many rows (age) are in the table
  cols=ncol(table) #counting how many columns (years) are in the table
 
   years <- numeric(0) 
  year_ranges<-table[1,2:cols] #stores the first row which contains the year ranges (ex/ 1991 - 1992)
  number_of_years=length(year_ranges)
  for(i in 1:number_of_years) {
    year<-as.numeric(strsplit(year_ranges[[i]], "-")[[1]][1]) #parses each elemnet and retrieves the first year of that element (ex: 1991 - 1992 become 1991) and stores as an int
    years[i]=year
  }
  
  ages<-table[2:rows,1] #stores the first row which contains the age ranges (ex/ 31-32)
  rates<-apply(table[2:rows,2:cols], 1,as.numeric)  #Stores part of the table that contains the rates
  data <- numeric(0)
  for (i in 1:nrow(rates)){
    line <- data.frame(Calender_Periods=years, Rate_Per_100000=rates[1:nrow(rates),i], age=table[i+1],shapes=i) # x= years, y=rates, label=ages
      print(line)
      data <- rbind(data,line) #adding each line to an array
  }
  p <- ggplot(data, aes(x=Calender_Periods, y=Rate_Per_100000, group=age,colour=age))+geom_line()+geom_point(aes(shape=age)) #graphing the vector
  ggsave(file = paste0(unique(p$SIC), as.character(uniqeID),"Rates.png"),width=10,height=10,limitsize=FALSE,path=tmpDirectory) #saving the image
    return(paste0(tmpDirectory,as.character(uniqeID),"Rates.png"))

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


graph_TemporalTrends(inputfile,1234)
