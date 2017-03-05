walk <- function(currdir,f,arg,firstcall=TRUE){
	print("Top of walk")
	print(currdir)

	setwd(currdir)


	dirlist<-list.files()
	flist<-character()
	dlist<-character()
	#print(filelist)
	i <- 1
	j <- 1
	print("Iterating through dirlist")
	 for (object in dirlist) {
    	if (!file.info(object)$isdir) {
    			#print(object)
    			#print("Found file")
    			flist[[i]]<-object 
    			i <- i+1

	 	}
	 	else{
	 		#print("Found directory")
	 		dlist[[j]]<-object
	 		j <- j+1
	 		
	 	}
	 }
	 print(flist)

	 print(dlist)
	 
	 if(length(dlist) == 0)
	 {
	 	print("Empty directory list")
	 	return (1);
	 }

	 else
	 {
		 print("Entering dlist")
		 h <- 1
		 for (dir in dlist)
		 {

		 	drname <- dlist[h]
		 	print(drname)
		 	drname <- paste(getwd(), drname, sep="/")
		 	walk(drname,f,arg,firstcall=FALSE)
		 	print("Made it out of walk")
		 	setwd("..")
		 	print(getwd())
		 	i<-i+1
		 }

	 }


	# dirlist<-list.dirs(path = ".", full.names = TRUE, recursive = TRUE)
	# print("Directory list: ",dirlist)
}