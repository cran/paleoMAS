filter.p <-
function(x,presen=1,persist=0.05)
{
{
	filter<-matrix(nrow=ncol(x),ncol=3,
		dimnames=list(colnames(x),
		c("n","n over minimum presence","quality")))
	ifelse(x>0,1,0)->pres.abs
	apply(pres.abs,2,sum)->filter[,1]
	ifelse(x>presen,1,0)->presence
	apply(presence,2,sum)->persistence
	persistence->filter[,2]
	a<-as.integer(persist*nrow(x))
	ifelse(filter[,2]>=a,1,0)->filter[,3]
	which(as.matrix(persistence)>=a)->filter1
	x[,filter1]->filtered
	matrix(nrow=3,ncol=1,dimnames=list(c("%","minimum n",
		"# taxa"),"value"))->result
	result[1,]<-presen
	result[2,]<-a
	result[3,]<-ncol(filtered)
	filtered<-round(filtered,2)
}
results<-list(filtered,filter,result)
names(results)<-c("filtered","filter","result")
return(results)
}

