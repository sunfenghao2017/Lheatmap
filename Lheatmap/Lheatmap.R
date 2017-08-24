#' for your own heatmap at anywhere
#' @param table,data matrix
#' @param coordinate,the image's site to plot
#' @param rowname,an boolean to mean if table has the row names
#' @export
Xheatmap=function(table,coordinate,rowname=TRUE){
    my_table<-read.table(table,sep="\t",header=T)
	if(rowname==TRUE){
	  rows.name(my_table)=my_table[,1]
	  my_table<-my_table[,-1]
	}elseif(rowname==FALSE){
	 my_table<-my_table
	}
	return(my_table)
}