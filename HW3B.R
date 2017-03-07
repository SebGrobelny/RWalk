rmemptydirs <- function(drname,filelist,arg){
	if(length(filelist) == 0)
		print(drname)





}




##Calculate the number of bytes present all files in directory
nbytes <- function(drname,filelist,arg){
	#arg takes in number of bytes

	for(obj in filelist)
	{
		arg <- arg + file.size(obj)
	}

	# if(arg == NA)
	# {
	# 	arg = 0
	# }

	return(arg);


}




##walk function
walk <- function(currdir,f,arg,firstcall=TRUE){


	# print("Top of walk")
	# print(currdir)

	setwd(currdir)


	dirlist<-list.files()
	flist<-character()
	dlist<-character()
	#print(filelist)
	i <- 1
	j <- 1
	# print("Iterating through dirlist")
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
	 # print(flist)

	 # print(dlist)
	 
	 if(length(dlist) == 0)
	 {
	 	# no subdirectories in current directory so need to go back up a level
	 	# print("No directories here")
	 	# print(firstcall)
	 	f <- match.fun(f)
		arg <- f(drname,flist,arg)
		setwd("..")
		# print(arg)
	 	return (arg);
	 }

	 else
	 {
		 h <- 1
		 for (dir in dlist)
		 {

		 	drname <- dlist[h]
		 	drname <- paste(getwd(), drname, sep="/")
		 	arg<-walk(drname,f,arg,firstcall=FALSE)

		 	#since our argument from our current subdirectory gets updated we must recalulate the next arg to pass into our walk 
		 	#for the next subdirectory 
		 	f <- match.fun(f)
		 	arg <- f(drname,flist,arg)
		 	# print("Made it out of walk")
		 	# print(arg)
		 	
		 	
		 	h<-h+1
		 }

		 # here we run out of directories to traverse so need to return to go back up a level
		 

		 #check if we have iterated through all of the directories under our root
			 if( firstcall == TRUE)
			 {
			 	#print("Back at the root")

			 	setwd("..")
			 	# print(getwd())
			 	return (arg);
			 }

		#print("Traversed through directories exiting ")

		f <- match.fun(f)
		arg <- f(drname,flist,arg)
		setwd("..")
		# print(arg)

		 return (arg);

	 }

}