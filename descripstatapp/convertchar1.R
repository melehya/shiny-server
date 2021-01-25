##### Convert character to numeric string, can handle ordinal varialbes 
#### will convert them to independent binary response variables, which is 
#### appropriate for descriptive analysis, but for other analyses variables 
###  should be kept in their ordinal form. Also determines if any columns contain 
### dates and removes them. Otherwise would turn each individual date into a separate 
### categorical variables 
#install.packages("shinycssloaders")

### Removes specified column from dataframe or list, empty if 
### neither dataframe or list 

charlist_create <- function(dataframe_column)
{
  charlist = list()
  for(j in dataframe_column)
  {
    if((is.null(dataframe_column)==TRUE)| j=='')
    {
      next
    }
    ifelse((is_list_element(charlist,j)==FALSE),
           {
             charlist <- append(charlist,j)
           },
           {
             next
           })
  }
  return(charlist)
}

remove_var <- function(dataframe, var)
{
  ifelse((class(dataframe)=="data.frame"),
         {
           new.frame=data.frame(matrix(nrow=1,ncol=1))
           newcolnames<- list()
           for (i in colnames(dataframe))
           {
             ifelse((var!=i),
                    {
                      new.frame <- data.frame(new.frame, dataframe[[i]])
                      newcolnames <- append(newcolnames, i) 
                      next
                    },
                    {
                      next
                    })
           }
           new.frame <- new.frame[-1]
           colnames(new.frame) <- newcolnames
         },
         {
           ifelse((class(dataframe)=="list"),
                  {
                    new.frame=list()
                    for (i in dataframe)
                    {
                      ifelse((var!=i),
                             {
                               new.frame = append(i, new.frame)
                               next
                             },
                             {
                               next
                             })
                    }
                  },
                  {
                    new.frame = list()
                  })
         })
  return(new.frame)
}
#### need to deal with ordinal variables 

ConvertChar <- function(dataframe, group_name)
{
  new_frame <- data.frame(matrix(nrow=1,ncol=1))
  colname_new <- list()
  frame_list <- list()
  for (i in colnames(dataframe))
  {
    if(datedetect(dataframe[[i]])==TRUE)
    {
      next
    }
    e = 1
    ifelse((class(dataframe[[i]])=="character"),
           {
             #colnamelist = colnames(dataframe)
             newvarlist = list()
             charlist = list()
             numlist = list()
             start=1
             count=0
             for(j in dataframe[[i]])
             {
               if((is.null(j)==TRUE)| j=='')
               {
                 next
               }
               ifelse((is_list_element(charlist,j)==FALSE),
                      {
                        charlist <- append(charlist,j)
                      },
                      {
                        next
                      })
             }
             for(x in charlist)
             {
               numlist <- append(numlist,count)
               count=count+1
             }
             ifelse(((length(charlist)>2)&(is_list_element(charlist, group_name)==FALSE)),
                    {
                      #colnamelist <- colnames(dataframe)
                      d = 1
                      m = 1
                      while(m <= length(charlist))
                      { 
                        newvarlist = list()
                        blanklist = list()
                        for (z in dataframe[[i]])
                        {
                          ifelse((z==charlist[[m]]),
                                 {
                                   newvarlist = append(newvarlist, 1)
                                 },
                                 {
                                   newvarlist = append(newvarlist, 0)
                                 })
                          if(e==length(charlist))
                          {
                            blanklist = append(blanklist,'')
                          }
                        }
                          #colnamelist <- append(paste(charlist[[m]]),colnamelist)
                        colname_new <-append(paste(charlist[[m]]),colname_new)
                          #dataframe <- data.frame(unlist(newvarlist),dataframe)
                        new_frame <- data.frame(unlist(newvarlist),new_frame)
                          colnames(new_frame) <- colname_new
                          
                        m = m + 1
                        e = e + 1
                      }
                      dataframe <- remove_var(dataframe, i)
                       #colnamelist <- colnames(dataframe)
                     # dataframe <- data.frame(unlist(blanklist),dataframe)
                      new_frame <-data.frame(unlist(blanklist),new_frame)
                      #colnamelist <- append(paste(i), colnamelist)
                      colname_new <- append(paste(i),colname_new)
                      colnames(new_frame)<-colname_new
                    },
                    {
                      while(start <= length(dataframe[[i]]))
                      {
                        charlistnum=1
                        while(charlistnum<=length(charlist))
                        {
                          ifelse((dataframe[[i]][[start]]==charlist[[charlistnum]]),
                                 {
                                   dataframe[[i]][[start]] <- numlist[[charlistnum]]
                                   start = start+1
                                   break
                                 },
                                 {
                                   charlistnum = charlistnum + 1
                                 })
                        }
                        next
                      }
                      next
                    })
             next
           },
           {
             next
           })
  }
  dataframe <- as.data.frame(apply(dataframe, 2, as.numeric))
  new_frame <- remove_var(new_frame, "NA")
  new_frame <- as.data.frame(apply(new_frame, 2, as.numeric))
  frame_list <- list(dataframe,new_frame)
  return(frame_list)
}