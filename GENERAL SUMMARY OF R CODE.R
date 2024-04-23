################################ General tips and short cuts  #########################

%>% : cmd+shift+m
~ : alt+n
| : Shift+alt+L = pipe

Collapse — Alt+Cmd+L.
Expand — Shift+Alt+Cmd+L.
Collapse All — Alt+Cmd+O.
Expand All — Shift+Alt+Cmd+O.

Markdown :
  Insert Chunk : Cmd+Alt+i

get() : #allow to search an object by its name !!!!!!!!!!

setwd(.../"+tab") if you place the mouse after the "/", press "TAB", it will OPEN A DIALAOG BOX with the forlders !!! 
  
############################### R Basis - UDEMY ##############################


#Creation list
1:10 #creat a list of "n" between the 2 numbers

#Generation of sequence seq()
x <- seq(from=1, to=20 , by=2)
seq(1,20,2) #mean exactly the same as up
seq(from=10, to=30, length=3) #"lenght" automatically calculate the number of terms to add in the seq
seq(from=10, to=30, by=10) # by = we specify the step increment. (le pas)

1:length(x) #tric that combine the 2 functions, from 1 to y (y beeing the length of x)
seq_along(x) # is doing the same !

#generate a random number list
z <- sample(100, size=10)
#means generate a vector z containing a sample size of 10 natural number randomly picked between 1 and 100.
#can be written also 
z <- sample(1:100, size=10)

#Get information on the object :
summary(iris) #give summary of object especially the basic stat distribution of variables
head(iris) #give the begining of the object
str(iris) #give a detailed structure of the object including exemple and Headers
glimpse(iris) #dplyr version fo str

table(iris) # CALCULATE the proportion of each value

%>% 
  rename(data, new = old) #rename a label, var in data / Using dplyr


#--------------------35 SET UP  working space --------------------------

#The "working Directory" is the folder were we work/save/export...
 current working directory
getwd() 
#Set the working directory
Setwd("pathway of the folder/.../")
#You can drag and drop a folder in a "Terminal" to recover the path  
setwd("/Users/tapponnieryann/Desktop/2021\ 02\ 23\ Les\ base\ de\ la\ programmation\ en\ R")

# OR go in the 4th window / Files and choose the working directory and assign it in "more"

#--------------------36 install and get a package --------------------------
# CRAN is the principal source, but nowday "github" is also nice
install.packages("nom_du_paquet")
install.packages(c("nom_du_paquet1","nom_du_paquet2", etc))
#you need also to install all dependencies / on Rstudio, you can "tick" it for automatic installations

#Then you need to call the package / Or "tick it" in RStudio
Library("nom_du_paquet")

#uncall a package / Or "un-tick it" in RStudio
detach('nom_du_paquet')

#uninstall package
remove.packages("nom_du_paquet")

#--------------------38 Obtaining information on our data --------------------------

ls() #allow to print all object generated during the session.
search() #give the list of all package called during the session
args(command_name) #give you indication on the arguments of the command (like the 4th window in RStudio)
data() #visualisation of all datasets already charged on R
summary(Object_name) #give you all info / basic Stats on the variable of the object
str(Object_name) #give more info on the dataset
head(Object_name,n) #plot the "n" number of first case of a dataset
tail(Object_name,n) #plot the "n" number of last case of a dataset
names(Object_name) #plot the names of the objects
objects.size(Object_name) #plot the size of variables
dataset_names$variable_name # plot 1 variable of the dataset
range(Object_name) /range(Object_name$variable)   #give you the hishest and lowest value of the object/variable

Fix(Object_name) #open window to visualise the dataset
edit(Object_name) #open window to visualise AND edit the dataset
attributes(Object_name) #give the object attributes


#--------------------39 Usufull fonctions on data and folder organisation --------------------------
dir.create() #pour créer un nouveau dossier
file.create() #pour créer un nouveau fichier
file.exist() #vérifier si le fichier existe dans le dossier de travail
file.info() #vérifie les informations du fichier
file.rename(“vieux_nom”, “nouveau_nom”) #pour renommer un fichier
file.copy(“nom_fichier”, “nom_copie”) #pour créer une copie

####### DATA PASTA - Copy pasting creating a vector using short cuts ##### 
xfilter (row.names(CC14_nc_new_rank_mean)https://cran.r-project.org/web/packages/datapasta/vignettes/how-to-datapasta.html
https://learn.r-journalism.com/en/importing_exporting/copying_pasting/copying-pasting/

Now from a list of Names, I copy (cmd+c) then paste as a horizontal vector (cmd+shft+v)

  

#--------------------opérations standar--------------------------




  Instruction
+ - / * 
  mean() median() quantile() sqrt() abs() #Absolut value 
pi()  

x^y   log(x, base = n)
sin() cos()  tan()  
min()  max()  
cor() #correlation
cumsum() #cumulative sum
cumprod() #cumulative product
colMeans() 
rowMeans() 
colSums() 
rowSums()


ceiling() # round up
floot() # round down
round (x, digit =n) # round the number x at n digits
  
  
#--------------------interrogate the nature of the vector ####
class(z) 
mode(z)
typeof(z)

# (y) vector is numerical, to change it definitely:
y2 <- as.character(y)
y3 <- as.logical(y)
as.numeric()
as.list()
as.matrix()
as.data.frame()
as.factor()
Blabla <- factor(some_vector, ordered = TRUE, levels = c("lev1", "lev2", ...))
get() : #allow to search an object by its name !!!!!!!!!!
  
#-------------------- MATRIX ####
#création matrice à partir de vecteur
matrice.client <- matrix(c(client1,client2,client3), nrow = 3, byrow=T)

#assign label to column then row
colnames(matrice.client) <- c("achat1","achat2")
row.names(matrice.client) <- c("client1","client2","client3")

    # changing the name of a specifiv col or row
  colnames(df)[x] <- "name"
  
  # AND NOT :because df[,x] is not anymore a df, it is a vector !!!!!!!!!
  colnames(df[,x])# does not work 
  colnames(df)[,x] # the colnames have no columns !!!
  
  
  #using dinmaes (dimensional names)
dimnames(matrixnames) <- list (c("row1","row2","etc"),c("column1","column2","etc"))

dimnames(matrice.client) <- list (c("client1","client2","client3"),c("Achat1","achat2"))
#--------------------30 Logical operatiors--------------------------
<  inferieur
>  supérieur
== equal
<= inferieur ou égale
>= superieur ou egale
!= differnet de 
& et
| ou (alt+maj+L)
! non
&& et (seulement pour le premier élément d'un vecteur)
|| ou (seulement pour le premier élément d'un vecteur)
~ associer des variable dans des equation


#--------------------31 Iterative structures--------------------------

if,else #to test a condition
  for #repeat a function for n times
while #met en execution une fonction tant qu'une condition est rempli
repeat ##repeat a function for n times
  break #stop execution
next #jump to an other element
return #terminate the execution of the function


#example
x <- 4
if (x==5){
  print("x est égale à 5")
} else {
  print("x n'est pas égale à 5")
}
############################--2 Sorting (edX-Data Science) --############################
#The function order() returns the index vector needed to sort the vector. 
#This implies that sort(x) and x[order(x)] give the same result.

sort() __ sort(data$var) # sort the data in increasing order
order() __ order(vec or data$var) # generate a index vector after ordering the var (containing the row numbers).
rank(x)    # returns ranks (smallest to largest) of the original var
rank(-x) #returns from largest to smallest

# We can apply the index given by order/which.--- to another column/var of the table.
vec[order(vec)] # reorganise the vec by the index vector generated by order()
which.max(var) - which.min(var) #which.max/min will give me the index position of the max value

pops[1] #I get access to the 1st entry on vec "pops" // as pops is sorted, it is the smallest
##############################--Indexing function (which-match-%in%) --##############################
# logical operator (< > = & |) give answer as logical vector/matrix....
which() __ which(data < n) #gives us the INDEX of entries when a logical vector that are true.
match() __ match(searched_entry, data) #looks for entries in a vector and returns the INDEX needed to access them.
%in% #(LOGICAL) if we want to know whether or not each element of a first vector is in a second vector. (answer as logical vector)
  I can create a function to generate a negative %in% = which in NOT into...
  '%!in%' = Negate('%in%')
  library(Hmisc) # have a negative %in% : %nin%
distinct () #dplyr function to keep only the distinct rows !!
setdiff(a , b) # give the answer of what is the difference between 2 vector
#Old fashion 
  unique(b[! b %in% a]) # give the answer of what is the difference between 2 vector
  
  #Examples :
  time_displ <- c(17, 16, 20, 24, 22, 15, 21, 15, 17, 22)
  # to get the indices
  which(time_displ > 20)
  temp1 <- which(time_displ > 20)
  #  [1]  4  5  7 10
  
  # to get the values
  time_displ[time_displ>20]
  time_displ[temp1]
  time_displ[ which(time_displ > 20)]
  # [1] 24 22 21 22
  
#--------------------56 Data treatment using dplyr --------------------------
#evolution of plyr
install.packages("dplyr")
library(dplyr)
  
# UNIGE SUPER COOOOOOOL EXPLANATION OF ALL TINY FUNCTIONS !!!
  https://edutechwiki.unige.ch/fr/Manipuler_des_donn%C3%A9es_avec_dplyr
  filter(str_starts(colnames, "BLABALABLA"))
  str_detect(colnames, "BLABALABLA")
#out source explaination
https://www.sharpsightlabs.com/blog/dplyr-quick-introduction/
  
select() #Retrieve variable/columns by names (To subset the data by selecting specific columns,)
filter() #Retrieve obs/row based on value (select a subset of obs/rows)
distinct () #dplyr function to keep only the distinct rows !! // .keep.all = T allow to keep all variables
arrange() #Organize the case (ascending / descending)
mutate() #Add new variable/columns (containing result of function done on other)
summarize() #Compute a summar statistic (apply function and returns a df with subset of the total df (especially after %>% or group_by))
#SUMMARIZE CAN ONLY RETURN A SINGLE VALUE (like the mean of a group...)
mutate(nom_dataframe, name_new_var = calculation_you_want_to_make)

eg. select(var_name) / (c(var1,var2)) /(c(var1/var4)) / (-c(var1,var4))
eg. filter() # rlist is a list of 10 data frame
Blabla_up2 <- filter(rlist[[1]][,1] >=0) %>% row.names() # this DOES NOT work because it has nothing to export !!!
# by adding ; rlist[[1]] %>% ;  I am IN THE DATAFRAME 
Blabla_up <- rlist[[1]] %>% filter( rlist[[1]][,1] >=0) %>% row.names() # I do not NEEED the select() or only the first columns. I just need to specify on wich col I want to filter !!!

%>% (cmd+shift+M) # We can perform a series of operations by sending the results of one function to another function using the pipe operator, %>%.

  group_by() #to group by set of....then apply summarise(), so summarise will be apply by group 
#= (if calculate the "mean", it is not anymore the mean of the Variable of total df, it will be the mean in/of each groupe !!!)
##### if you filter by one factor (gender == "male") you need to repeat the same for the other levels of the factor (female)
##### using group_by(gender) then summarise() you can say, I want the mean() of that var and it will give it for all levels of the factor (here for both gender)
 
   rowwise() # a bit like group_by it is treating the df by row.
    #However you need to specify which column to take in account in your calulation.
    center_reduce2 <-  CC14_nc_mean %>%  
      rowwise () %>%  # treat by row (a bit like group by treat by group)
      mutate (average = mean(c_across(P3C11:P3E3z)), SD = var (c_across(P3C11:P3E3z))) 
 
   # Old fashion to add a new col 
    to add a new column : df$New_col_name <- c(1, 2 ,3 ,4, 5 etc)
  
  
?rename() #to rename label/var_name


#######  Dot placeholder ".$"
us_murder_rate %>% .$rate # is equivalent to .
us_murder_rate$rate

it is "... .$" concider the "..." in ".$var_name" as the data_frame_before_pipe
The aim is to return a SINGLE VECTOR or number instead of DATA.FRAME like summarise() do

#The dot operator allows you to access values stored in data that is being piped in using the %>% character. 
#The dot is a placeholder for the data being passed in through the pipe.
#The dot operator allows dplyr functions to return single vectors or numbers instead of only data frames.


pull() # is an equivalent way to extract a single column using the pipe 
us_murder_rate %>% pull(rate)

:
summarize : output is data.frame
.$var : output is numerical value (extract the column as single or vector)
pull(var) : vector (numerical)



##############################--Basic Plots 3-2 --##############################
plot() #We can create a simple scatterplot using the function plot().
hist() #Histograms are graphical summaries that give you a general overview of the types of values you have.  
boxplots() #provide a more compact summary of a distribution than a histogram and are more useful for comparing distributions. 

plot(x,y)
boxplot (y ~ x , data) #plot y by x strats/levels/factor 

#----------------------62 Basic Graph (Udemy)
#basic R graphs
table() #plot the data in a form of table
plot() #synthetise 1 or 2 var
hist() #histogram of 1var
bareplot()
pie()
mosaicplot()

plot(x=..., y=..., main,)
#main = Title  
#sub=sub-title  
#col= color 
#pch=change the type of dot$
#xlab=  change x label
#ylab= change y label
#rug = add graduation on x axis
#abline
#lty = add a line to the graph
#lwd 
#legend = add legend + specify the location: top right/bottom left


#----------------------64 Correlation ---------------------------- 
#how to calculate the correlation between 2 var

cor()
cor.test()

#calculation coefecient of correlation
cor(var1,var2, method = "pearson")
cor(data$var1, data$var2)
cor.test

# cor compute the correlation of x and y if these are vectors. 
# If x and y are matrices then it comptute  the covariances (or correlations) between the columns of x and the columns of y

# Pearson or Spearman : 
  https://towardsdatascience.com/clearly-explained-pearson-v-s-spearman-correlation-coefficient-ada2f473b8
  


#----------------------65 Test d'hypothese ---------------------------- 
#give you test on proportion
prop.test (positive, number_repetition)
prop.test(52,100)

chisq.test() # number of occurence
fisher.test() #
CrossTable() #
t.test() #continus var
##############################--Conditionals 4-2 --##############################
if - else # most commun conditional, form "if [condition], perform [expression], else perform [alternative expression]".
  ifelse() #The  function works similarly to an if-else statement, but it is particularly useful since it works on vectors by examining each element of the vector and returning a corresponding answer accordingly.
any() # takes a vector of logicals and returns true if any of the entries are true.
all() # takes a vector of logicals and returns true if all of the entries are true.
##############################--Other Functions - Apply familly 4-' --##############################
#generally we don't use for loops in R because of other more powerfull functions :
apply() #can be only apply on Matrix : so containing the same data type
lapply() #liste apply = work on list :::::: output = list
sapply() #simplifyed lapply = work on list :::::: output = vector
vapply() #allow to choose the output format
mapply() #allow the application to each element of a list
tapply() #allow to apply a function to a subset of a vector of the matrix
rapply()

apply(matix_name, n, fonction)

apply(matrice, 1, mean) #calculate the mean of each row of the matrix
apply(matrice, 2, mean) #calculate the mean of each column of the matrix
apply(matrice, 1:2, FUN) #calculate the FUNCTION on each cell of the matrix


complex examle :

cor_yt <- function(x, met) {
  cor(x,vexp_ATP11C, method = met )
}

cor_mat_ATP11C <- apply(df, 1 , cor_yt (met = "spearman")) # does not work because it miss x
cor_mat_ATP11C <- apply(df, 1 , cor_yt, met = "spearman") # that way it knows that the x is the x of apply !!!
# that way it knows that the x is the x of apply !!! but the "met =" is for the cor_yet



split()
cut()
quantile()
reduce()
identical()
unique()
setdiff(a , b) # give the answer of what is the difference between 2 vector


#--------------------57 Restructuration and re-agregation of the data (melt-dcast) --------------------------

install.packages(reshape2)
library(reshape2)

melt() #reorganize vertically the data by sorting "id" and copy pasting measure - 
# It is like copy pasting multiple colon on the top of each other and add a new col containing the name of the "copied colon" next to each value  --> So you have 1 col with the data and 1 col with the parameter you can call for "grouping" 
dcast() #undo the melt by placing horizontally (side by side) the var

#multiply id.vars and place other "measure.vars verticaly on the top of each other for each id.var combination
melt(dataset_name, id.vars= c("var_name1","var_name2" ))
melt(dataset_name, id.vars= c("var_name1","var_name2" ), measure.vars = c("var_name3","var_name4", etc))
dcast(dataset_name, id.vars_name1+id.vars_name2  ~ variable)
#--------------------57 tidyr (Udemy 1) --------------------------
#--> easy Restructuration and re-agregation of the data

install.packages("tidyr")
library(tidyr)

gather()
spread()
unite()
separate()


pivot_longer() # Correspond to the melt function of reshape2
pivot_wider()  # correspond to the spread function of reshape2

# I don't know how to deal with row.names (like genes)
# so I extract them creating a column and as all columns not call are keepts and repeated it is fine.
# example in Mfuzz CC36
lg_df <- df %>%
  pivot_longer(
    cols = 1:16, #colums to pivot to a longer format // All columns not call are keepts and repeated
    names_to = "CC36_clones", # what to do with the col.names
    values_to = "norm_counts") # what to do with the value

I have placed on the top of each other the expression of my 16 clones of CC36.
copying their name in the new  "CC36_clones" column
the value are placed in the new "norm_counts" column
and gene names (17th col) is keept and copy 16times.


#gather() = melt to organise verticaly the data
#spread() = casted
#unite() = combine 2 column
#separate() = split a column

gather(dataset_name, new_key-var_name, new_value_name, var_to_gather, na.rm=FALSE, convert = FALSE )  #na.rm = FALSE eliminate missing value #convert = TRUE convert the data in the good format
#Key name is  the new header of the column and original var name will become the value of Key column (vertically)
#value name is  the new headerof the column and original value will become the value of value column (stack vertically)


gat1 <- gather(patients1, traitements, score, traitement1:traitement4, na.rm = FALSE, convert = FALSE)
gat2 <- gather(patients1, traitements, score, traitement1, na.rm = FALSE, convert = FALSE)


Spread (data, key, value, fill=NA, convert = F)
#the key name will desapear and the key value will become the name of var
#the value name will desapear and the value value will be spread in each new column created

spread1 <- spread(gat1, traitements, score, fill=NA, convert=F )
spread2 <- spread(gat2, traitements, score, fill=NA, convert=F )


unite(data, col, ...., sep=" ", remove = T)
#unite/Fuse 2 columns
#col = new_var_name
#.... = which_var you want to unite
#sep, what type of separator you want in between 2 the values
#remove = T means that you remove the previous columns


separate(data, col, into = c(= , ), sep=" ", remove = T, convert = F, "x")
#separate 1 column into 2
#col = current name of the column
#into = new var name
#sep = current separator,
#remove = T sup the separate columns
#convert = T convert the data in appropriate format
#x = if no separator cut after x caracter.
######################## General Statistics #################
The 68-95-99.7 Rule 
#1SD / 2SD / 3SD

pnorm(a, avg, s)  # gives the value of the cumulative distribution function  𝐹(𝑎)  for the normal distribution defined by average avg and standard deviation s.
qnorm(p, mu, sigma) # gives the theoretical value of a quantile with probability p of observing a value <= than that quantile value given a normal distribution with mean mu and standard deviation sigma:
quantile(data, p) #p could be a vector (a seq with the step you want ex : seq(0.01, 0.99, 0,01))

# make QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)



########################-- VISUALISATION (edX-Data Science 2) -- --########################

# ALL EXPLANATION IN DETAIL FOR GGPLOT FUNCTIONS !!!!!!!!!!!
https://rdrr.io/cran/ggforce/man/facet_wrap_paginate.html


GGPLOT2 WORK ONLY ON DATA TABLES !!! row = Observation  columns = var


##### Graph Component
#3 main components:
# Data: The dataset being summarized
# Geometry: The type of plot (scatterplot, boxplot, barplot, histogram, qqplot, smooth density, etc.
# Aesthetic MAPPING: Variables mapped to visual cues, such as x-axis and y-axis values and color
#how we define aes() DEPENDS of the GEOM

#AESTHETIC MAPPING : is a MAPPING so it add things depending of a variable (size/color etc...)
ex: the size of the dot is dependant of the value
the color is dependant on a category 

If you define the size/color outside the AES(), it will be apply to all dot/bar/etc the same !!!
  
  The AES() define in GGPLOT(aes()) are applied to all the reste au the aes() of each GEOM + COMPONENT
  BUT the local AES() (define in a GEOM for example) OVERIDE the global aes().




  #There are ADDITIONNAL COMPONENTS:
  #Scale
  #Labels, Title, Legend
  #Theme/Style
  
  # to know what aes to define, check what is required, represented in BOLD in help of the GEOM.
  # ONLY AES() RECOGNIZE THE LABEL OF VARIABLES DIRECTLY !!!

  

                                      
####details

#composed by 7 elements that can be used alone or seperated
aes #form mapping size color by a variable
geoms #geometric element : line / bar dot /dot cloud
facets #elements used is conditionnal formating
stats #for statistical functions : quantil / coordonates
scales # allow distinction of subset of data  : by color for ex

library(ggrepel)
library(ggthemes)
p + theme_economist()    # style of the Economist magazine
p + theme_fivethirtyeight()    # style of the FiveThirtyEight website

library(gridExtra) #allow to associate multiple plot generated separatly in 1 page  next to each other !!!
grid.arrange(p1, p2, p3, ncol = 3)




                                     
# Aesthetic mappings describe how properties of the data connect with features of the graph (axis position, color, size, etc.)
aes() # uses directly the variable names from the object component (ex : total rather than murders$total).
DIRECT VAR NAMES are only understood by aes()    
# ONLY aes() understand the direct var_names, (geom_point(total)) does NOT work !!

classical interesting aes : 
  ggplot( aes(group = "var", represent the data by groups
              color = "var", represent the data by groups and color it
              fill = "var", represent the data by groups and fill it
              weight = "var",)wieght the proportion of data of groups by weight ex : in density plot 
          
          
    # ALL EXPLANATION IN DETAIL FOR GGPLOT FUNCTIONS !!!!!!!!!!!
    https://rdrr.io/cran/ggforce/man/facet_wrap_paginate.html         
          
geom_point() # creates a scatterplot and requires x and y aesthetic mappings. 
geom_text() and geom_label() # add text to a scatterplot and require x, y, and label aesthetic mappings.


scale_x_continuous(
    trans = "log2", or "log10")  # change scaling
    limit = c(10,100)# set the limit of the axis values
    breaks = c(1,8,64,1024) # argument allows us to set the location of the axis labels and tick marks.
    logistic or logit transformation # convert fold change into constant increase. 
    #is defined as 𝑓(p)=log(p/(1−p), or the log of odds. This scale is useful for highlighting differences near 0 or near 1.
    # 90% = 90/10 < 10times smaller than 99/1 < 10times smaller than 99,9/0,1. --> it enlarge tiny diff when talking about small odds.(near 0 or 1)

scale_x_log10() # change scaling
xlab("name of x Axis") +
ggtitle("title of the graph")
geom_text_repel()
grid.arrange()

pull(var) or .$var # is an equivalent way to extract a single column using the pipe 

summarize() #  computes summary statistics from the data frame.  
#It returns a DATA FRAME whose column names are defined within the function call.
# summarize() can compute any summary function that operates on vectors and returns a single value, but it cannot operate on functions that return multiple values.

group_by() #function from dplyr  converts a data frame to a grouped data frame, 
#creating groups using one or more variables. (it is like have multiple table in one object (each table beeing a group))
#summarize() and some other dplyr functions will behave differently on grouped data frames.
#Using summarize() on a grouped data frame computes the summary statistics for each of the separate groups.



######### Faceting ###-###
# ALL EXPLANATION IN DETAIL FOR GGPLOT FUNCTIONS !!!!!!!!!!!
https://rdrr.io/cran/ggforce/man/facet_wrap_paginate.html

faceting() #makes multiple side-by-side plots stratified by some variable.
facet_grid() # allows faceting by up to two variables, with rows faceted by one,  columns by the other.
#To facet by only one variable, use the dot operator as the other variable.
facet_wrap() #facets by one variable and automatically wraps the series of plots so they have readable dimensions.

# Faceting keeps the axes fixed across all plots, (easing comparisons between plots)
    facet_grid(continent ~ year)
    facet_grid(. ~ year)
    facet_wrap(~year, nrow = 2)
    facet_wrap_paginate(page = i) # indicate the number of page you want to plot // OR create a loop to plot them all 1 after the others on multiple pdf pages !!
    
    
####### FACTOR : FUSING 2 plot using FACTOR ###-###
    p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))
    # using fill or color = factor() force ggplot to FUSE the 2 graphs and it color differentially automatically the different level of a factor
    # here the factor(year) : year was numerical and become a factor !!!


####### Sorting Data Tables
sort() __ sort(data$var) # sort the data in increasing order
order() __ order(vec or data$var) # generate a index vector after ordering the var (containing the row numbers).
rank(x)    # returns ranks (smallest to largest) of the original var
rank(-x) #returns from largest to smallest

arrange() # The arrange() function from dplyr sorts a data frame by a given column.
arrange(desc(var)) #By default, arrange() sorts in ascending order. use desc() :  arrange(desc(var)) .
# You can arrange() by multiple levels: within equivalent values of the first level, observations are sorted by the second level, and so on.
slice_min(var, n= x)/slice_max(var, n= x)#NOTE: The top_n() function has been superseded in favour of slice_min()/slice_max() 
slice()
slice_head (n=) # take the n first rows
sliece_tail (n=) # take the n last rows
#before slice, we used :
top_n() # The top_n() function shows the top results ranked by a given variable, but the results are not ordered. 
arrange() %>% top_n() #You can combine top_n() with arrange() to return the top results in order.


ds_theme_set()    # set plot theme


reorder() # changes the order of factor levels based on a related numeric vector.
reorder(vec1, vec2, FUN = function) #it reorders vec1 by the function FUN apply to vec2
# Class : vec1 = Factor, vec2 = numerical, 


intersect()  # used to find the overlap between two vectors.

fill=factor() or color=factor() #use to force ggplot to FUSE the 2 graphs


geom_density(position = "stack") #to stack the curve on top of each other

Problem here is that if we plot density plot of the 2 group "WEST" VS "developing" they have not the same number of country BUT the Y-axis will be the % of each group.
To correct it we need to weight the y-axis by the relative size of the group : using ..count.. 
..var_name.. #surounding a var_name by .. is calling it in axis
..count..  #Change the y-axis of density plots to variable counts using ..count.. as the y argument.

case_when() # Use to creats groups : function defines a factor whose levels are defined by a variety of logical operations to group data.
breaks # argument allows us to set the location of the axis labels and tick marks.
logistic or logit transformation # convert fold change into constant increase. is defined as 𝑓(p)=log(p/(1−p), or the log of odds. This scale is useful for highlighting differences near 0 or near 1.
The ecological fallacy  #is assuming that conclusions made from the average of a group apply to all members of that group.


      #ggplot color example
      http://www.sthda.com/french/wiki/ggplot2-couleurs-changer-les-couleurs-automatiquement-et-manuellement-logiciel-r-et-visualisation-de-donnees
      scale_color_gradient(low="blue", high="red")
      
      # Gradients divergents
      mid<-mean(mtcars$qsec)
      sp2+scale_color_gradient2(midpoint=mid,  low="blue", mid="white", high="red"  space = "Lab")


## The mutating joins add columns from y to x, matching rows based on the keys:
  
inner_join()# includes all rows in x and y.

left_join()# includes all rows in x.

right_join()# includes all rows in y.

full_join()# includes all rows in x or y.

If a row in x matches multiple rows in y, all the rows in y will be returned once for each matching row in x.

# dplyr v3.0 allows you to join on different variables:
  left_join(x, y, by = c("a" = "b") will match x.a to y.b
                  by = c("a", "b") will match x$a to y$a and x$b to y$b.
                  by = c("a" = "b", "c" = "d") will match x$a to y$b and x$c to y$d.

            # either
left_join(test_data, kantrowitz, by.x = "first_name", by.y = "name")
# or
left_join(test_data, kantrowitz, by = c("first_name", "name"))


# Alternative : 
  merge(test_data, kantrowitz, by.x = "first_names", by.y = "name", all.x = TRUE)



######### Density plot - ..count..- case_when #########
Problem here is that if we plot density plot of the 2 group "WEST" VS "developing" they have not the same number of country BUT the Y-axis will be the % of each group.
To correct it we need to weight the y-axis by the relative size of the group : using ..count.. 

..var_name.. #surounding a var_name by .. is calling it in axis
..count..  #Change the y-axis of density plots to variable counts using ..count.. as the y argument.
case_when() # Use to creats groups : function defines a factor whose levels are defined by a variety of logical operations to group data.
#Plot stacked density plots using position="stack".
#Define a weight aesthetic mapping to change the relative weights of density plots 
#- for example, this allows weighting of plots by population rather than number of countries.
mutate(group = case_when(
  .$var1 %in% vec_already_defined ~ "new_group_name"
  .$var1 %in% "case_name1" ~ "new_group_name1",
  .$var1 %in% c("case_name2", "case_name3") ~ "new_group_name2",
  .$continent == "BLA" & .$region != "Case_name4" ~ "new_groupe3", TRUE ~ "new_gorup4"))


gapminder <- gapminder %>%
    mutate(group = factor(group, levels = c("Others", "bla_1", "bla_2", "bla_3", "bla_4")))

factor(some_vector, ordered = TRUE, levels = c("lev1", "lev2", ...))
  
  geom_density(bw = 0.4)  #is the bindwidth for difining thesmoothness of the plot
  geom_density(position = "stack") can be influenced by aes(weight='var_weight')
  


  
  
  
#### Example
library(tidyverse)
library(dslabs)
  
library(ggthemes)
  p + theme_economist()    # style of the Economist magazine
  p + theme_fivethirtyeight()    # style of the FiveThirtyEight website

  # rotate names on x-axis
  p + geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) #hjust = 1 justify the text near the axis
  
  
  
library(gridExtra) #allow to associate multiple plot generated separatly in 1 page  next to each other !!!
  grid.arrange(p1, p2, p3, ncol = 3)
  
  
data(gapminder)
  
  data(gapminder)
  
  # add column creating additional groups define by conditions
  gapminder <- gapminder %>%
    mutate(group = case_when(
      .$region %in% west ~ "The West",
      .$region %in% "Northern Africa" ~ "Northern Africa",
      .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
      .$region == "Southern Asia" ~ "Southern Asia",
      .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
      .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
      .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands",
      ))
  
######################## GGPLOT2 Udemy ##############
df <- mpg
ggplot(data, aes(x = var1, y = var2)) #this is just the frame of the graph #THERE IS NO DATA !!!
ggplot(data, aes(x = var1, y = var2)) + geom_point() #geom_point specify how you want the data to be ploted




aes()
x = var1
y = var2
col = #color
  size = #Varx represented by the size of the dots
  alpha = # ratio of transparency (between 0 and 1)
  shape = n

geom_...
geom_point() #numage de point
shape = n
size = n or var
alpha = #transparency
  col = 'name' or n  #colors

geom_smooth # line of tendency
linetype= #type of line
  se = TRUE =# ombrage (confidence interval)
  se = FALSE = # whitout CI
  
  geom_jitter() #add little variation in each point to allow better representation of overlaping data
geom_point (position = jitter) #normally write like this, the first nomenclature = short cut
shape = n
size = n or var
alpha = between 0 and 1 #transparency ratio
col = 'name' or n  #colors

geom_text()
  nudge_x = n #move aside the text (to not cover the dots for example)
  label = 'var'

facet_grid() #Create new subgraph representing data for 1 variable
(. ~ var1) #graph (will be a column) distributed in 1 row
(var1 ~ .) #graph (will be a row) distributed in 1 column       

aes(label = var) + geom_text() #Place a text/value of a VAR specified previously in aes(label = var)
#for ex instead of representing the number of cylindre of the car by size, you can plot this number instead of the dot

aes(x = var1, fill = var2)) #fill = VAR depending on the color of remplissage of the barplot
+ geom_bar(
  position = 
    "stack" # addition of value on the top of each other (bars of different size)
  "fill" # represent each bar in a % or ration (all bars at 100%, it fill the graph)
  "dodge" # bar side by side for each X (witdh is addapted each time to the number of bar represented of each x)
  
  
  geom_freqpoly(   #frequency graph with curve
    binwidth = n #is the interval of X you include for each point
############## NEW tips and trick the I discover #####


#How to make a incremental list of names going : gene_1, gene_2, gene_3 etc.....
rownames(mymat) <- paste("gene", 1:dim(mymat)[1], sep="_")
#incremental names / incremental gene etc...
paste(sep="_", rep("P3C11",3),seq(1:3))


# how to use paste + single name df to run the code
paste("heatmap",dfname, sep="_") # allow to change the name automatically

dfname <- "CC36_nc_mean2"
dfname <- "GOI_50var"
dfname <- "GOI_75var"

setwd(dir = "/Volumes/UNIGE/Experiments/Bioinformatic - Seq/RNAseq Data/2022 02 Ramping Clone Analysis CC36/R data")
write.table(GOI_75var , file = paste(dfname, sep=".","txt" ), sep= "\t", dec = ",", row.names = T, col.names = T)

table_name <- paste(dfname, sep = "." , "txt")
df <- read.table( file = table_name, sep = "\t", dec=",", row.names = 1)

Then use df to do what ever you want !!!!

  
##  VECTOR NAMING :
names() # allow to rename even vector entry (like adding col names of a table of 1 row)
  x <- c(a=2, b=3, c=5 , d=10)
EQUIVALENT TO : 
y <- c(2,3,5,10)
names(y) <-  c("a","b","c","d")
y
# x and y are numeric vector with names as attributes

# To rename only the column x
colnames(df)[x] <- 'name'

# Old fashion to add a new col 
to add a new column : df$New_col_name <- c(1, 2 ,3 ,4, 5 etc)

####### Color - palette - and Ramping palette ######
#ggplot color example
http://www.sthda.com/french/wiki/ggplot2-couleurs-changer-les-couleurs-automatiquement-et-manuellement-logiciel-r-et-visualisation-de-donnees

library("grDevices")
https://bookdown.org/rdpeng/exdata/plotting-and-color-in-r.html


colorRamp() #Take a palette of colors and return a function that takes valeus between 0 and 1, indicating the extremes of the color palette (e.g. see the gray() function)
colorRampPalette() #Take a palette of colors and return a function that takes integer arguments and returns a vector of colors interpolating the palette (like heat.colors() or topo.colors())

Example
> pal <- colorRamp(c("red", "blue"))
##Red
> pal(0)
[,1] [,2] [,3]
[1,]  255    0    0
> ## blue
  > pal(1)
[,1] [,2] [,3]
[1,]    0    0  255

#you can make a sequence in between 0 and 1 to have a ramp of color
pal(seq(0, 1, len = 10))


pal <- colorRampPalette(c("red", "yellow"))
## Return 10 colors in between red and yellow
> pal(10)
[1] "#FF0000" "#FF1C00" "#FF3800" "#FF5500" "#FF7100" "#FF8D00" "#FFAA00"
[8] "#FFC600" "#FFE200" "#FFFF00"

library("RColorBrewer")
 # 3 types :
Sequential # for numerical data that are ordered
Diverging #for numerical data that can be positive or negative, often representing deviations from some norm or baseline
Qualitative #for qualitative unordered data

display.brewer.all() # allow to visuallize all palletter existing  

cols <- brewer.pal(5, "BuPu") #(number you want, "palette you want) Here it is a sequential palet so the color are ramping
cols # "#E5F5F9" "#99D8C9" "#2CA25F"


## COMBINATION OF THE FUNCTIONS !!!!
pal <- colorRampPalette(cols) #Those three colors make up my initial palette. Then I can pass them to colorRampPalette() to create my interpolating function.

# volcano is a matrix of continus value, it is a good example to test the colors
data(volcano)
class(volcano) # matrix

?image() ## Create a colored of grey-scale rectangles with colors corresponding to value !!!!!
image(volcano)
image(volcano, col = pal(20))


mGOI_75var <- as.matrix(GOI_75var)
mLog <- apply(mGOI_75var, 1:2,log10)
image(mGOI_75var)

#ggplot color example
http://www.sthda.com/french/wiki/ggplot2-couleurs-changer-les-couleurs-automatiquement-et-manuellement-logiciel-r-et-visualisation-de-donnees
scale_color_gradient(low="blue", high="red")

  # Gradients divergents
    mid<-mean(mtcars$qsec)
    sp2+scale_color_gradient2(midpoint=mid,  low="blue", mid="white", high="red"  space = "Lab")
                          
#example use for CC36 Pheatmap
library("RColorBrewer")
display.brewer.all() # to visuallise all palet of Brewer

brewer.pal(9, "BuPu") # Pick 9 color from a the pallet BuPu and return Hexa code
colR(5) # will give an output of 5 ramping color

colR <- colorRampPalette(brewer.pal(9, "BuPu")) # Output is a function that make a ramping of the color provided.
ramp <- colR (4) # loading the colors in a vector. here 4 category of Met_status
#OR
ramp <- colR (length(unique(ann_col$Met_status))) # storing the colors in a vector
names(ramp) <- unique(ann_col$Met_status) # changing the names of the vector as "attribute" because it is a vector / it is like adding col names on a table of row = 1
ann_colors <-  list ( Met_status = ramp) # making it a list



 ### TOOOOONES of exemple of palette you can choose
https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html#the-color-scales
scale_fill_viridis_d(option = "turbo") # d is for discrete variable (categorie or name or gene without value...)
scale_fill_viridis_c() # c is for continous variable (with value)

scale_color_viridis_c()
scale_color_viridis_d() option = "inferno" / "turbo" / "mako" / "rocket" etc !!!!

############ Import all files at ones and Working on LIST ######

blablabla <- list@df.name$colname
blablabla <- list[[1]][,1] = column 1 of dataframe 1

rnames <- list.files(all.files = T) # creates a charater vectors with all name of files / 
#All files include "."and ".." which is folders I guess so not cool !!
list.files(path= ".....", pattern = ".txt",  ) # You can specify what you want.

rnames <-  list.files(pattern="*.txt")
rList_delim <-  lapply(rnames, read.delim) # work but you can't choose the row.names etc....

rnames <- list.files(pattern = "*.txt")
rList <- lapply(rnames, function(x) read.table(x, sep="\t", dec=",", header= T, ))
names(rList) <- rnames

# Extracting the Union from the list 
i <- 1
for (i in 1:length(rlist)){ # with "for loop" "i" is incremented automatically 
  if (i==1) rUnion <- rlist[[i]][,1]
  else rUnion <- union(rUnion, rlist[[i]][,1] )
  # i+1 do not do +1, with "for loop" "i" is incremented automatically 
}
```

##### Simplified code
On the contraru to CC36 the gene names are stored in rownames
```{r warning=FALSE}

rUnion <- character() # Here I am priming a vector because I am only extracting a list a gene names
for (i in 1:length(rlist)){ 
  rUnion <- union(rUnion, rownames(rlist[[i]]) ) 
}


# Here you need to create an empty dataframe 
# With the good colnames if you want to do the union so the upper method with if / else is better !!! 
lipid_Union <- data.frame(matrix(data= NA, ncol = 12)) # creating the empty dataframe with 12 col (does not work because it does not have the same col.names hihi !!! )
colnames(lipid_Union) <- colnames(lipids_GO_total_list[[i]])

for (i in 1:length(lipids_GO_total_list)){ # with "for loop" "i" is incremented automatically 
  lipid_Union <- union(lipid_Union, lipids_GO_total_list[[i]] )
}
lipid_Union <- lipid_Union[-1,] # AND I need to eliminate the first line containing nothing !!!
# this method is borring but fun to try coding !!!
 

############ MARKDOWN #######

  Markdown :
  Insert Chunk : Cmd+Alt+i

Go to the next line = "  " double space !!!

echo # Print the code or not
eval # Run the code or not
include # control both PRINT : code&output 
ex : include = FALSE, eval = TRUE --> means that you will display NOTHING but run the code


result = # text result is display / hide = text hidden / asis = no formating by Knit (usefull when already formated by R)
warning # print warnings or not
fig.show # display graphs
fig.cap = "enter your figure title " # add a tittle of a figure written bellow
fig.align 
out.height 
out.width 



Bold : __ text __ or ** text **
Italique : _ text _ or * text *
Hyperlink : [text of the link to display] (hhttp//www.url.com)

Headers :
# Header 1
## Header 2
### Header 3
#### Header 4
##### Header 5

include an image
image![](./folder/Picture.jpg)

Drawing tables manually

Table Header1 | Table Header 2 | Table Header 3
--------------|----------------|---------------
Entry in table|have to be      | alined
--------------|----------------|---------------
  Then you can| just copy paste|
--------------|----------------|---------------












######### Saving the graphs in pdf(), jpeg(), png()  & par() ####

# example of what I did in the CC36 mFuzz script - "yt2" function
setwd("where_to_save")
pdf("name.pdf") #open the container for the futur flux like white sheet
  mfuzz(.......)
dev.off() # stop storing the flux in that file

pdf(onefile = TRUE)#mutiple fig in 1 page
pdf(onefile = FALSE)# 1 fig in 1 page

# It saves automatically the graph in pdf in the good folder.
# IT CONTINU ADDING THE GRAPHS UNTIL YOU WRITE dev.off()

#Example in CC36 mfuzz markdown
setwd("/Volumes/UNIGE/Experiments/Bioinformatic - Seq/RNAseq Data/2022 02 Ramping Clone Analysis CC36/mFuzz")

pdf(paste(sep="_","MFuzz",dfname,"c",n_cluster,"m",m_fuzziness,".pdf"),width = 11.69, height = 8.27)
mfuzz.plot2(eset.s, cl = cl, mfrow = output_diplay, time.labels = colnames(GOI), centre=TRUE, centre.lwd=3, x11=FALSE)
dev.off()



#### par()
old fashion way to parameter all text /size / font / alignment /background /colors /marging etc !!!!

  
  
#### layout()
  layout() 
it allow to specify all the parameter you want in the next export thing, creating white slot for storing futur fig .


layout( , widths = 3, heights = 4, n = c(2,2))
pdf(file =)

--> Here it generate a blank pdf with 2x2 slot for figures of size 3x4 inch


########  Import / Exporting multiple sheet in 1 excel file ######
can try with 
library(xlsx)
library(tidyverse)  
library(readxl) #included in tidiverse



    #Importing Excel sheets
    
    setwd("/Volumes/UNIGE/Experiments/Bioinformatic - Seq/RNAseq Data/2021 10 Ramping clone analysis CC14/R Analysis/Data/Ariel Lists/List")
    
  ## Importing  
    #You can import any sheet by calling it by number or sheet name
    single_sheet <- read_excel(path = "PAME Yann.xlsx", sheet = 3, col_names = F) # here as I have stated the path in setwd() It just miss the file name to complet the path.
    single_sheet <- read_excel(path = "PAME Yann.xlsx", sheet = "PAME up for mFUZZ", col_names = F)
    
    # Importing all the sheets as a list
    sheets_names <- excel_sheets("PAME Yann.xlsx") # Retreive the excel sheet names in a vector
    PAME_list_of_all <- lapply(sheets_names, function(x) read_excel(path = "PAME Yann.xlsx", sheet = x, col_names = F))
    names(PAME_list_of_all) <- sheets_names 
    
    
  ## Exporting
    #the principle is to APPEND data into an existing file !!!
    
    # readxl
    write_sheet()
    sheet_append()
    
    need to make a loop
    for (i = 1:n) 
    if(i==1) write_sheet(x = , ) # create the fisrt file
    else sheet_append () # append data in a new sheet






######## Scale function to center reduce by column ####
  # by COLUMN IT DO IT BY COLUMN !!!
scale(x = ,center = ,scale = ) ### Allow to directly scale the value x by COLUMN, Center = -mean , scale = /SD
center_reduce3 <-  scale(x= t(CC14_nc_mean) ,center = TRUE, scale = TRUE )) # As scale is scaling by col I transpose the table t() 
  CC14_nc_mean_CR <- as.data.frame( t(center_reduce3)) #AND I retranspose the output to have it in the write ordore
  
  
  

  
######## To plot in a time serie with x-Axis scaled propotionnaly to time #####
  ggplot ()+
  scale_x_continuous(minor_breaks = NULL, breaks=c(as.numeric(levels(factor(dmolten$time)))))
  #You force the breaks of x-axis to be propotional to here "time" in hours.
  
  scale_x_continuous( minor_breaks = NULL,
                      breaks= TW_count, # This give the numeric value to add on x-Axis // could have been from an outside data.frame : TW_rank$TW_count
                      labels = TW_rank$clones, # This give the numeric value to add on x-Axis // could have been from an outside data.frame : TW_rank$clones
                      guide = guide_axis(n.dodge=5)) # to prevent overlaping labels, number of dodge is the number of line on which writing
  theme ( plot.title = element_text (hjust=0.5, color="Blue", size=14, face="bold"),
          axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  
########## file.exists() // if(!file.exists()) ####
  Testing if a file exists or not.
  familly of file.blablabla
  file.create()
  file.remove()
  file.append()
  
  setwd(dir = 'bla/bla/bla')
if (file.exists("name_file.txt")){
  do what ever you want
}

  OR ############ #
if (!file.exists("name_file.txt")){ # read it : if file do NOT exists
  write.table(file= , , , )
  } 
YOU DO NOT NEED the else {} # because it just do not execute you function if the file exist and continu....









###### Venn Diagram #####


#### Making a vector from a dataframe - unlist() #####
  exp_ATP11C <- scaled_nc %>%  filter (row.names(scaled_nc) == "ATP11C")

  #vexp_ATP11C2 <- as.vector(exp_ATP11C2) # Still a dataframe !!
  #vexp_ATP11C2 <- as.vector(exp_ATP11C2[1,]) # does not work neither
  vexp_ATP11C <- unlist(exp_ATP11C)
  
###### Printing a list in the same txt file on the top of each other ###### 
  setwd("/Volumes/UNIGE/Experiments/Bioinformatic - Seq/RNAseq Data/2022 04 Single Cell Cluster Markers/Export/Venn Diagram")
  # I am creating a 2 line the dataframe with the names() of my list,
  # AND Thanks to the append = T function it add on the top of each other every gene lists in the same file.
  
  for (i in 1:length(list_blabla)){
    write.table(data.frame(list_blabla[[i]], Category = names(list_blabla)[i]), paste(sep="", "Blablabla.txt" ) ,quote=F, append= T, sep='\t', col.names = T)  
  }
  
  
  
###### Sink OR capture.output functions ###### 
  
  #sink is basicly a "print out" function that save everything you do/plot in middle of "sink()"
    sink("blablabla.txt"))
     print(list_blablabla)
    sink()
    
  # Caputre output is globally the same
    capture.output(print(list_blablabla), file= "blablabla.txt" ))
    
    
    
###### grep familly + dplyr equivalent #####
Used to search a character pattern in a character vector.
grepl ("pattern", vector)

#Combine it with dplyR to sort the results
  exp_gene <- df %>%  filter (grepl("CXC", row.names(df)))


#dplyr have it own version 
  exp_gene <- df %>%  filter (str_detect( row.names(df),"CXC"))# where ever
  exp_gene <- df %>%  filter (str_ends( row.names(df),"CXC")) # end by
  exp_gene <- df %>%  filter (str_starts( row.names(df),"CXC")) # start by



  
###### funprog familly ######
  
# The Reduce function is part of funprog {base}, which includes
  #A straightforward way to do the job is:
    intersect(intersect(a,b),c)
  #More cleverly, and more conveniently if you have a lot of arguments:
    Reduce(intersect, list(a,b,c))
  
  Reduce(f, x, init, right = FALSE, accumulate = FALSE)
  Filter(f, x)
  Find(f, x, right = FALSE, nomatch = NULL)
  Map(f, ...)
  Negate(f)
  Position(f, x, right = FALSE, nomatch = NA_integer_)
  
  ####### Statistics and ploting :stat_compare_means.R #####
  library(stat_compare_means)
  
  stat_compare_mean
  my_list_comparison <-  list(c("E3ctr","E3_treated1"), c("E3ctr","E3_treated2")) # column names
  stat_compare_means(comparisons = my_list_comparison, method = "t.test"
                     label.x = specify the coorinates manually
                     label.y = specify the coorinates manually)
  
  #### Put everything in uppercase ###
  BLABLABLA <- toupper(blablabla)
  
  
############## Generating Trend line with rolling average ######
install.packages("zoo") # Contains rollmean() 
library(zoo)

# The point here is to have a rolling something (sum / mean / Max etc...)
# I will use it for rolling mean.

rollmean(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
         align = c("center", "left", "right"), ...)

rollmax() /rollmedian() / rollsum()

roll3_vec <-  rollmean(x = vec, k = 3, fill = NA)  # fill = NA keeps the the initial number of columns adding the NA value to the left and right of the df (because less than "k" value to calculate the mean)
roll3_vec_left <-  rollmean(x = vec, k = 3, fill = NA, align = "center") #  (could be left right or center !)

###### fill --> necesary to keep the good number of column --> Can be fill with NA or left / center /right value !!

# can do a rollapply for a dataframe.

### rollapply  --> APPLY THE CALCULATION BY COLUMNS !!!!
vec <- 1:16
vec2 <- 17:32
df <- data.frame(vec,vec2)

df_roll <- rollapply (data = df, width = 3, FUN = function(x) mean(x), by.column = TRUE) # by.column = TRUE calculates each col separatly
df_roll2 <- rollapply (df, 3, mean, by.column = TRUE, fill = NA) # fill = NA keeps the the initial number of columns adding the NA value to the left and right of the df (because less than "k" value to calculate the mean)




