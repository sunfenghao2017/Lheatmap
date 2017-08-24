#' for your own heatmap at anywhere
#' @param table,data matrix
#' @param coordinate,the image's site to plot
#' @param rowname,an boolean to mean if table has the row names
#' @export
Xheatmap=function(table,coordinate,rowname=TRUE,picnum){
  my_tble<-read.table(table,sep="\t",header=T)
  if(rowname==TRUE){
    row.names(my_tble)=my_tble[,1]
    my_table<-my_tble[,-1]
    cname<-colnames(my_tble)
    rname<-row.names(my_tble)
  }else if(rowname==FALSE){
    my_table<-my_table
  cname<-colnames(my_tble)
  rname<-my_tble[,1]
  }
  collen<-length(cname)
  rowlen<-nrow(my_tble)
  wids<-(1/rowlen)/1.3
  heis<-(1/collen)/(2+(picnum-1)*1.5)
  fixler<-collen-1
  for(j in 1:fixler){
    for(i in 1:rowlen+1){
      if(j==1){
      my_group<-colors[as.numeric(my_tble[,j+1])]
      my_group1=my_group
      my_col1<-as.data.frame(cbind(my_group1,as.character(my_tble[,j+1])))
      my_col1<-unique(my_col1[!(duplicated(my_col1[,1])&duplicated(my_col1[,2]))])
      my_name1<-levels(my_tble[,j+1])
      
      }else if(j==2){
        my_group<-colors[as.numeric(my_tble[,j+1])+2]
        my_group2=my_group
        my_col2<-as.data.frame(cbind(my_group2,as.character(my_tble[,j+1])))
        my_col2<-unique(my_col2)
        my_name2<-levels(my_tble[,j+1])
      }else if(j==3){
        colorss<-c("#00FFFF","skyblue","blue","purple","grey","pink")
        my_group<-colorss[as.numeric(my_tble[,j+1])]
        my_group3=my_group
        my_col3<-as.data.frame(cbind(my_group3,as.character(my_tble[,j+1])))
        my_col3<-unique(my_col3)
        my_name3<-levels(my_tble[,j+1])
      }else if(j==4){
      my_group<-c(rep("white",times=c(rowlen)))
      }else{
      my_group<-colors[as.numeric(my_tble[,j+1])+9]
      if("突变型" %in% my_tble[,j+1]){}else{my_group<-c(rep("white",times=c(rowlen)))}
      }
     
      grid.rect(x = unit(coordinate[1]-wids*(i), "npc"), y = unit(coordinate[2]-heis*(j+1), "npc"),
            width = unit(wids, "npc"), height = unit(heis, "npc"),gp=gpar(fill =my_group[i-1]))
    }
  }
 
  for(j in 1:collen)
    grid.text(cname[j],x=c(coordinate[1]-wids*(rowlen+2)+wids/2,coordinate[1]-wids*(rowlen+2)+wids/2),y=c(coordinate[2]-heis*(j),coordinate[2]-heis*(j)),just = "right", gp=gpar(col="black", fontsize=8))

  for(i in 1:rowlen+1)
    grid.text(rname[i-1],x=c(coordinate[1]-wids*(i),coordinate[1]-wids*(i)),y=c(coordinate[2]-heis,coordinate[2]-heis), gp=gpar(col="black", fontsize=8))
  for(j in 1:fixler){
    for(i in 2:rowlen){
  for(i in 1:length(my_col1[,1])){
    grid.rect(x = unit(coordinate[1]+0.01, "npc"), y = unit(coordinate[2]-0.02*i, "npc"),
              width = unit(0.02, "npc"), height = unit(0.02, "npc"),gp=gpar(fill =as.character(my_col1[i,1])))
    grid.text(my_col1[i,2],x=c(coordinate[1]+0.04,coordinate[1]+0.04),y=c(coordinate[2]-0.02*i,coordinate[2]-0.02*i), gp=gpar(col="black", fontsize=9))
   
  }
      grid.text(cname[2],x=c(coordinate[1]+0.02,coordinate[1]+0.02),y=c(coordinate[2]+0.02,coordinate[2]+0.02), gp=gpar(col="black", fontsize=10,family = 'SimSun'))
      grid.text(cname[3],x=c(coordinate[1]+0.02,coordinate[1]+0.02),y=c(coordinate[2]-0.02*(length(my_name1)+1)-0.01,coordinate[2]-0.02*(length(my_name1)+1)-0.01), gp=gpar(col="black", fontsize=10,family = 'SimSun'))
      t3=coordinate[2]-0.02*length(my_name2)-0.02*(length(my_name1)+1)-0.05
      grid.text(cname[4],x=c(coordinate[1]+0.02,coordinate[1]+0.02),y=c(t3,t3), gp=gpar(col="black", fontsize=10,family = 'SimSun'))
      for(i in 1:length(my_col2[,1])){
        grid.rect(x = unit(coordinate[1]+0.01, "npc"), y = unit(coordinate[2]-0.02*i-0.02*(length(my_name1)+1)-0.02, "npc"),
                  width = unit(0.02, "npc"), height = unit(0.02, "npc"),gp=gpar(fill =as.character(my_col2[i,1])))
        grid.text(my_col2[i,2],x=c(coordinate[1]+0.03,coordinate[1]+0.03),y=c(coordinate[2]-0.02*i-0.02*(length(my_name1)+1)-0.02,coordinate[2]-0.02*i-0.02*(length(my_name1)+1)-0.02), gp=gpar(col="black", fontsize=9))
        
      }
     
      for(i in 1:length(my_col3[,1])){
        
        grid.rect(x = unit(coordinate[1]+0.01, "npc"), y = unit(t3-0.02*i-0.01, "npc"),
                  width = unit(0.02, "npc"), height = unit(0.02, "npc"),gp=gpar(fill =as.character(my_col3[i,1])))
        grid.text(my_col3[i,2],x=c(coordinate[1]+0.04,coordinate[1]+0.04),y=c(t3-0.02*i-0.01,t3-0.02*i-0.01), gp=gpar(col="black", fontsize=9))
        
      }
    }
  }
  return(my_col2[,2])
}