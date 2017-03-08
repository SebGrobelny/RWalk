

#remove empty dirs
rmemptydirs <- function(drname,filelist,arg){
	arg <- 0
	if(length(filelist) == 0)
	{
		#directory has no files/ directories then remove it
		unlink(drname,recursive=TRUE)
	}

	return(arg)




}




##Calculate the number of bytes present all files in directory
nbytes <- function(drname,filelist,arg){
	#arg takes in number of bytes

	for(obj in filelist)
	{
		arg <- arg + file.size(obj)
	}


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

	 	#store files in one list
    	if (!file.info(object)$isdir) {
  
    			flist[[i]]<-object 
    			i <- i+1




	 	}
	 	#store directories in the other
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

	 	f <- match.fun(f)
	 	drname <- currdir
		arg <- f(drname,flist,arg)
		setwd("..")
	 	return (arg);
	 }

	 else
	 {
		 h <- 1
		 #traverse through list of directories and call walk on each
		 for (dir in dlist)
		 {

		 	drname <- dlist[h]
		 	drname <- paste(getwd(), drname, sep="/")
		 	print(drname)
		 	arg<-walk(drname,f,arg,firstcall=FALSE)

		 	#since our argument from our current subdirectory gets updated we must recalulate the next arg to pass into our walk 
		 	#for the next subdirectory 
		 	f <- match.fun(f)
		 	arg <- f(drname,flist,arg)
 	
		 	
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


		f <- match.fun(f)
		arg <- f(drname,flist,arg)
		setwd("..")

		 return (arg);

	 }

}