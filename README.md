# Prediction-of-poisonous-mushrooms-using-R
Mushroom data Modeling
Ruthwick Kuchibhotla

November 8, 2016

Packages Used
These are some the packages used for data cleaning, data exploration and modeling. If some of these packages are not installed in your system, please remove the hashtags below to install them. For more information about the mlr package please refer the following link https://cran.r-project.org/web/packages/mlr/mlr.pdf.

Please make sure that the version of R and the java installed in the system are the same like 64bit or 32bit.

# install.packages("randomForestSRC")
# install.packages("FSelector")
# install.packages("mlr")
# install.packages("readr")
# install.packages("data.table")
# install.packages("dplyr")
library(mlr)
library(readr)
library(data.table)
library(dplyr)
Introduction and Summary Statistics
The mushrooms data set has been downloaded from https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/expanded.Z.
After this I have tried to clean the data a little in the excel by removing some of the unwanted rows in the beginning.
Please save this code to the system and set the working directory.
I have included a code in the end that saves the cleaned data in the path this code has been saved.
Loading the Data
After cleaning the data in the excel I have created a vector of column names of the data since the data did not have the column names.

colmname <- c("mushroom_type","cap_shape", "cap_surface", "cap_color","bruises","odor",
               "gill_attachment","gill_spacing", "gill_size","gill_color",
               "stalk_shape","stalk_root","stalk_surface_above_ring",
               "stalk_surface_below_ring","stalk_color_above_ring",
               "stalk_color_below_ring","veil_type","veil_color","ring_number",
               "ring_type","spore_print_color","population","habitat")
Reading the Data

raw_data <- read_csv("expanded.csv",col_names = colmname)
Since all the variables are categorical I used sapply to loop over all the variables to convert them to factors and make a matrix.

Totaldata <- sapply(raw_data, as.factor)
I then converted the R object Totaldata into a data table which is a convenient way of data manipulation

Totaldata <- as.data.frame(Totaldata)
Totaldata <- data.table(Totaldata)
I then summarized the columns to see if all the variables are converted to factors and if there are any missing values in any column.

summarizeColumns(Totaldata)
##                        name   type na mean       disp median mad  min  max
## 1             mushroom_type factor  0   NA 0.46673004     NA  NA 3928 4488
## 2                 cap_shape factor  0   NA 0.54895437     NA  NA    4 3796
## 3               cap_surface factor  0   NA 0.61169202     NA  NA    4 3268
## 4                 cap_color factor  0   NA 0.72433460     NA  NA   16 2320
## 5                   bruises factor  0   NA 0.40114068     NA  NA 3376 5040
## 6                      odor factor  0   NA 0.54752852     NA  NA   48 3808
## 7           gill_attachment factor  0   NA 0.02566540     NA  NA  216 8200
## 8              gill_spacing factor  0   NA 0.18916350     NA  NA 1592 6824
## 9                 gill_size factor  0   NA 0.30133080     NA  NA 2536 5880
## 10               gill_color factor  0   NA 0.79467681     NA  NA   24 1728
## 11              stalk_shape factor  0   NA 0.42205323     NA  NA 3552 4864
## 12               stalk_root factor  0   NA 0.54847909     NA  NA  192 3800
## 13 stalk_surface_above_ring factor  0   NA 0.36834601     NA  NA   24 5316
## 14 stalk_surface_below_ring factor  0   NA 0.39686312     NA  NA  296 5076
## 15   stalk_color_above_ring factor  0   NA 0.43631179     NA  NA    8 4744
## 16   stalk_color_below_ring factor  0   NA 0.44866920     NA  NA   24 4640
## 17                veil_type factor  0   NA 0.00000000     NA  NA 8416 8416
## 18               veil_color factor  0   NA 0.02376426     NA  NA    8 8216
## 19              ring_number factor  0   NA 0.07699620     NA  NA   48 7768
## 20                ring_type factor  0   NA 0.52851711     NA  NA   48 3968
## 21        spore_print_color factor  0   NA 0.71197719     NA  NA   48 2424
## 22               population factor  0   NA 0.51711027     NA  NA  352 4064
## 23                  habitat factor  0   NA 0.62452471     NA  NA  192 3160
##    nlevs
## 1      2
## 2      6
## 3      4
## 4     10
## 5      2
## 6      9
## 7      2
## 8      2
## 9      2
## 10    12
## 11     2
## 12     5
## 13     4
## 14     4
## 15     9
## 16     9
## 17     1
## 18     4
## 19     3
## 20     5
## 21     9
## 22     6
## 23     7
There seems to be no missing values and so proceeding further.

Data Exploration
Since all the vaiables are categorical, I have used bar plots as my primary exploratory data analysis plots to understand the data.

I have collapsed some of the levels in some of the variables because if I split the data into training and testing, some of the levels maynot be present in the test data.

Although, this level colapsing is not required if the whole data is considered to build the model.

Cap Shape VS Mushroom Type
We can see in the below bar graph that some of the factors like BELL, CONICAL KNOBBED and SUNKEN are having very minimum count and they need to be colapsed

ggplot(Totaldata, aes(x = cap_shape, fill = mushroom_type))+geom_bar() + labs(title = "cap shape VS mushroom type")


The table below shows the frequency of each factor in the variable

table(Totaldata$cap_shape)
## 
##    BELL CONICAL  CONVEX    FLAT KNOBBED  SUNKEN 
##     452       4    3796    3292     840      32
Doing the level colapsing as mentioned above with the code below

Totaldata$cap_shape <-  ifelse(Totaldata$cap_shape == "CONICAL","CONVEX",
                               ifelse(Totaldata$cap_shape == "SUNKEN","CONVEX",
                                      ifelse(Totaldata$cap_shape == "BELL","CONVEX",Totaldata$cap_shape)))
Renaming the levels to the they were before

Totaldata[,cap_shape := ifelse(cap_shape == "3","CONVEX",
                               ifelse(cap_shape == "4","FLAT",
                                      ifelse(cap_shape == "5","KNOBBED",cap_shape)))]
##       mushroom_type cap_shape cap_surface cap_color bruises   odor
##    1:        EDIBLE    CONVEX      SMOOTH     WHITE BRUISES ALMOND
##    2:        EDIBLE    CONVEX      SMOOTH     WHITE BRUISES ALMOND
##    3:        EDIBLE    CONVEX      SMOOTH     WHITE BRUISES ALMOND
##    4:        EDIBLE    CONVEX      SMOOTH     WHITE BRUISES ALMOND
##    5:        EDIBLE    CONVEX      SMOOTH     WHITE BRUISES ALMOND
##   ---                                                             
## 8412:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO   NONE
## 8413:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO   NONE
## 8414:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO   NONE
## 8415:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO   NONE
## 8416:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO   NONE
##       gill_attachment gill_spacing gill_size gill_color stalk_shape
##    1:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    2:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    3:            FREE      CROWDED    NARROW       PINK    TAPERING
##    4:            FREE      CROWDED    NARROW       PINK    TAPERING
##    5:            FREE      CROWDED    NARROW      BROWN    TAPERING
##   ---                                                              
## 8412:        ATTACHED        CLOSE     BROAD      BROWN   ENLARGING
## 8413:        ATTACHED        CLOSE     BROAD      BROWN   ENLARGING
## 8414:        ATTACHED        CLOSE     BROAD      BROWN   ENLARGING
## 8415:        ATTACHED        CLOSE     BROAD      BROWN   ENLARGING
## 8416:        ATTACHED        CLOSE     BROAD      BROWN   ENLARGING
##       stalk_root stalk_surface_above_ring stalk_surface_below_ring
##    1:    BULBOUS                   SMOOTH                   SMOOTH
##    2:    BULBOUS                   SMOOTH                   SMOOTH
##    3:    BULBOUS                   SMOOTH                   SMOOTH
##    4:    BULBOUS                   SMOOTH                   SMOOTH
##    5:    BULBOUS                   SMOOTH                   SMOOTH
##   ---                                                             
## 8412:          ?                   SMOOTH                   SMOOTH
## 8413:          ?                   SMOOTH                   SMOOTH
## 8414:          ?                   SMOOTH                   SMOOTH
## 8415:          ?                   SMOOTH                   SMOOTH
## 8416:          ?                   SMOOTH                   SMOOTH
##       stalk_color_above_ring stalk_color_below_ring veil_type veil_color
##    1:                  WHITE                  WHITE   PARTIAL      WHITE
##    2:                  WHITE                  WHITE   PARTIAL      WHITE
##    3:                  WHITE                  WHITE   PARTIAL      WHITE
##    4:                  WHITE                  WHITE   PARTIAL      WHITE
##    5:                  WHITE                  WHITE   PARTIAL      WHITE
##   ---                                                                   
## 8412:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8413:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8414:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8415:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8416:                 ORANGE                 ORANGE   PARTIAL      BROWN
##       ring_number ring_type spore_print_color population habitat
##    1:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    2:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    3:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    4:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    5:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##   ---                                                           
## 8412:         ONE   PENDANT             BROWN  CLUSTERED  LEAVES
## 8413:         ONE   PENDANT            ORANGE    SEVERAL  LEAVES
## 8414:         ONE   PENDANT            ORANGE  CLUSTERED  LEAVES
## 8415:         ONE   PENDANT              BUFF    SEVERAL  LEAVES
## 8416:         ONE   PENDANT              BUFF  CLUSTERED  LEAVES
Converting the variable back to factor

Totaldata$cap_shape <- as.factor(Totaldata$cap_shape)
Looking at the bar plot again with the colapsed levels. We can see that the count in each level is a little balanced.

ggplot(Totaldata, aes(x = cap_shape, fill = mushroom_type))+geom_bar()+labs(title = "bar chart between cap shape and mushroom type")


Cap Surface VS Mushroom Type
Here we can see that the factor level frequency or count of GROOVES is close to 0. Hence it has to be colapsed or else it would be a problem while spliting the data into train and test.

ggplot(Totaldata, aes(x = cap_surface, fill = mushroom_type))+geom_bar()+labs(title = "bar chart between cap surface and mushroom type")


the table below shows the frequency of each factor in the variable

table(Totaldata$cap_surface)
## 
## FIBROUS GROOVES   SCALY  SMOOTH 
##    2460       4    3268    2684
Doing the level colapsing as mentioned above with the code below

Totaldata$cap_surface <-  ifelse(Totaldata$cap_surface == "GROOVES","SMOOTH",Totaldata$cap_surface)
Renaming the levels for convenience

Totaldata[,cap_surface := ifelse(cap_surface == "1","FIBROUS",
                                 ifelse(cap_surface == "3","SCALY",
                                        ifelse(cap_surface == "4","SMOOTH",cap_surface)))]
##       mushroom_type cap_shape cap_surface cap_color bruises   odor
##    1:        EDIBLE    CONVEX      SMOOTH     WHITE BRUISES ALMOND
##    2:        EDIBLE    CONVEX      SMOOTH     WHITE BRUISES ALMOND
##    3:        EDIBLE    CONVEX      SMOOTH     WHITE BRUISES ALMOND
##    4:        EDIBLE    CONVEX      SMOOTH     WHITE BRUISES ALMOND
##    5:        EDIBLE    CONVEX      SMOOTH     WHITE BRUISES ALMOND
##   ---                                                             
## 8412:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO   NONE
## 8413:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO   NONE
## 8414:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO   NONE
## 8415:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO   NONE
## 8416:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO   NONE
##       gill_attachment gill_spacing gill_size gill_color stalk_shape
##    1:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    2:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    3:            FREE      CROWDED    NARROW       PINK    TAPERING
##    4:            FREE      CROWDED    NARROW       PINK    TAPERING
##    5:            FREE      CROWDED    NARROW      BROWN    TAPERING
##   ---                                                              
## 8412:        ATTACHED        CLOSE     BROAD      BROWN   ENLARGING
## 8413:        ATTACHED        CLOSE     BROAD      BROWN   ENLARGING
## 8414:        ATTACHED        CLOSE     BROAD      BROWN   ENLARGING
## 8415:        ATTACHED        CLOSE     BROAD      BROWN   ENLARGING
## 8416:        ATTACHED        CLOSE     BROAD      BROWN   ENLARGING
##       stalk_root stalk_surface_above_ring stalk_surface_below_ring
##    1:    BULBOUS                   SMOOTH                   SMOOTH
##    2:    BULBOUS                   SMOOTH                   SMOOTH
##    3:    BULBOUS                   SMOOTH                   SMOOTH
##    4:    BULBOUS                   SMOOTH                   SMOOTH
##    5:    BULBOUS                   SMOOTH                   SMOOTH
##   ---                                                             
## 8412:          ?                   SMOOTH                   SMOOTH
## 8413:          ?                   SMOOTH                   SMOOTH
## 8414:          ?                   SMOOTH                   SMOOTH
## 8415:          ?                   SMOOTH                   SMOOTH
## 8416:          ?                   SMOOTH                   SMOOTH
##       stalk_color_above_ring stalk_color_below_ring veil_type veil_color
##    1:                  WHITE                  WHITE   PARTIAL      WHITE
##    2:                  WHITE                  WHITE   PARTIAL      WHITE
##    3:                  WHITE                  WHITE   PARTIAL      WHITE
##    4:                  WHITE                  WHITE   PARTIAL      WHITE
##    5:                  WHITE                  WHITE   PARTIAL      WHITE
##   ---                                                                   
## 8412:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8413:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8414:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8415:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8416:                 ORANGE                 ORANGE   PARTIAL      BROWN
##       ring_number ring_type spore_print_color population habitat
##    1:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    2:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    3:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    4:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    5:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##   ---                                                           
## 8412:         ONE   PENDANT             BROWN  CLUSTERED  LEAVES
## 8413:         ONE   PENDANT            ORANGE    SEVERAL  LEAVES
## 8414:         ONE   PENDANT            ORANGE  CLUSTERED  LEAVES
## 8415:         ONE   PENDANT              BUFF    SEVERAL  LEAVES
## 8416:         ONE   PENDANT              BUFF  CLUSTERED  LEAVES
Converting the variable to factor

Totaldata$cap_surface <- as.factor(Totaldata$cap_surface)
Colapsed levels plot

ggplot(Totaldata, aes(x = cap_surface, fill = mushroom_type))+geom_bar()+
  labs(title = "bar chart between cap surface and mushroom type")


Here we can see that a lot of variables like BUFF, CINAMMON, GREEN, PINK, PURPLE have very less count and hence need to be colapsed.

ggplot(Totaldata, aes(x = cap_color, fill = mushroom_type))+geom_bar()+labs(title = "bar chart between cap color and mushroom type")


The table below shows the frequency of each factor in the variable

table(Totaldata$cap_color)
## 
##    BROWN     BUFF CINNAMON     GRAY    GREEN     PINK   PURPLE      RED 
##     2320      168       44     2096       16      144       16     1500 
##    WHITE   YELLOW 
##     1040     1072
Colapsing the levels as stated below

Totaldata$cap_color <-  ifelse(Totaldata$cap_color == "BUFF","Comb",
                               ifelse(Totaldata$cap_color == "CINNAMON","Comb",
                                      ifelse(Totaldata$cap_color == "GREEN","Comb",
                                             ifelse(Totaldata$cap_color == "PINK","Comb",
                                      ifelse(Totaldata$cap_color == "PURPLE","Comb",                                                                              ifelse(Totaldata$cap_color == "WHITE","Comb",Totaldata$cap_color))))))
Renaming the levels back to the way they were.

Totaldata[,cap_color := ifelse(cap_color == "1","BROWN",
                               ifelse(cap_color == "10","YELLOW",
                                      ifelse(cap_color == "4","GRAY",
                                             ifelse(cap_color == "8","RED",cap_color))))]
##       mushroom_type cap_shape cap_surface cap_color bruises   odor
##    1:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES ALMOND
##    2:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES ALMOND
##    3:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES ALMOND
##    4:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES ALMOND
##    5:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES ALMOND
##   ---                                                             
## 8412:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO   NONE
## 8413:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO   NONE
## 8414:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO   NONE
## 8415:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO   NONE
## 8416:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO   NONE
##       gill_attachment gill_spacing gill_size gill_color stalk_shape
##    1:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    2:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    3:            FREE      CROWDED    NARROW       PINK    TAPERING
##    4:            FREE      CROWDED    NARROW       PINK    TAPERING
##    5:            FREE      CROWDED    NARROW      BROWN    TAPERING
##   ---                                                              
## 8412:        ATTACHED        CLOSE     BROAD      BROWN   ENLARGING
## 8413:        ATTACHED        CLOSE     BROAD      BROWN   ENLARGING
## 8414:        ATTACHED        CLOSE     BROAD      BROWN   ENLARGING
## 8415:        ATTACHED        CLOSE     BROAD      BROWN   ENLARGING
## 8416:        ATTACHED        CLOSE     BROAD      BROWN   ENLARGING
##       stalk_root stalk_surface_above_ring stalk_surface_below_ring
##    1:    BULBOUS                   SMOOTH                   SMOOTH
##    2:    BULBOUS                   SMOOTH                   SMOOTH
##    3:    BULBOUS                   SMOOTH                   SMOOTH
##    4:    BULBOUS                   SMOOTH                   SMOOTH
##    5:    BULBOUS                   SMOOTH                   SMOOTH
##   ---                                                             
## 8412:          ?                   SMOOTH                   SMOOTH
## 8413:          ?                   SMOOTH                   SMOOTH
## 8414:          ?                   SMOOTH                   SMOOTH
## 8415:          ?                   SMOOTH                   SMOOTH
## 8416:          ?                   SMOOTH                   SMOOTH
##       stalk_color_above_ring stalk_color_below_ring veil_type veil_color
##    1:                  WHITE                  WHITE   PARTIAL      WHITE
##    2:                  WHITE                  WHITE   PARTIAL      WHITE
##    3:                  WHITE                  WHITE   PARTIAL      WHITE
##    4:                  WHITE                  WHITE   PARTIAL      WHITE
##    5:                  WHITE                  WHITE   PARTIAL      WHITE
##   ---                                                                   
## 8412:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8413:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8414:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8415:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8416:                 ORANGE                 ORANGE   PARTIAL      BROWN
##       ring_number ring_type spore_print_color population habitat
##    1:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    2:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    3:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    4:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    5:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##   ---                                                           
## 8412:         ONE   PENDANT             BROWN  CLUSTERED  LEAVES
## 8413:         ONE   PENDANT            ORANGE    SEVERAL  LEAVES
## 8414:         ONE   PENDANT            ORANGE  CLUSTERED  LEAVES
## 8415:         ONE   PENDANT              BUFF    SEVERAL  LEAVES
## 8416:         ONE   PENDANT              BUFF  CLUSTERED  LEAVES
Converting the variable to factor

Totaldata$cap_color <- as.factor(Totaldata$cap_color)
Looking at the levels again

ggplot(Totaldata, aes(x = cap_color, fill = mushroom_type))+geom_bar()+
  labs(title = "bar chart between cap color and mushroom type")


Bruises VS Mushroom Type
We can see that bruises seem to be having good count

ggplot(Totaldata, aes(x = bruises, fill = mushroom_type))+geom_bar()+labs(title = "bar chart between bruises and mushroom type")
 Frequency of each level in Bruises

table(Totaldata$bruises)
## 
## BRUISES      NO 
##    3376    5040
Odor VS Mushroom Type
Here we can see that some of the levels in the variables are perfect predictors and it may lead to quasi complete seperation if used in logistic regression because log value of 0 is inf and since I would be using randomforest, I would be keeping this variable in the model

ggplot(Totaldata, aes(x = odor, fill = mushroom_type))+geom_bar()+labs(title = "bar chart between odor and mushroom type")


the table below shows the frequency of each factor in the variable

table(Totaldata$odor)
## 
##   ALMOND    ANISE CREOSOTE    FISHY     FOUL    MUSTY     NONE  PUNGENT 
##      400      400      192      576     2160       48     3808      256 
##    SPICY 
##      576
Colapsing the levels as mentioned above.

Totaldata$odor <-  ifelse(Totaldata$odor == "CREOSOTE","Comb",
                          ifelse(Totaldata$odor == "MUSTY","Comb",
                                 ifelse(Totaldata$odor == "PUNGENT","Comb",
                                        ifelse(Totaldata$odor == "SPICY","Comb",
                                               ifelse(Totaldata$odor == "FISHY","Comb",
                                                      ifelse(Totaldata$odor == "FOUL","Comb",
                                                             ifelse(Totaldata$odor == "ALMOND","Comb1",
                                                                    ifelse(Totaldata$odor == "ANISE","Comb1",
                                                                           ifelse(Totaldata$odor == "NONE","Comb1",
                                                                                  Totaldata$odor)))))))))
Another look at the plot with the colapsed levels.The count now seems high enough for split.

ggplot(Totaldata, aes(x = odor, fill = mushroom_type))+geom_bar()+labs(title = "bar chart between odor and mushroom type")


Gill Attachment VS Mushroom Type
Here we can see another instance of a un balanced levels and so I would be removing this from the model since its significant would not be great

ggplot(Totaldata, aes(x = gill_attachment,fill = mushroom_type))+geom_bar()+
  labs(title = "bar chart between gill attachment and mushroom type")


Gill Spacing VS Mushroom Type
Another instance of a perfect prediction. Although, there seems to be some obseravtions in each level, it can be of some significance but not much

ggplot(Totaldata, aes(x = gill_size,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between gill size and mushroom type")


Gill Color VS Mushroom Type
We can see that some of the levels have very less count and hence they need to be colapsed

ggplot(Totaldata, aes(x = gill_color,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between gill color and mushroom type")


Colapsing the levels as mentioned above

Totaldata$gill_color <-  ifelse(Totaldata$gill_color == "GREEN","BUFF",
                                ifelse(Totaldata$gill_color == "ORANGE","WHITE",
                                       ifelse(Totaldata$gill_color == "RED","WHITE",
                                              ifelse(Totaldata$gill_color == "YELLOW","WHITE",
                                                     ifelse(Totaldata$gill_color == "BLACK","WHITE",
                                                            ifelse(Totaldata$gill_color == "BROWN","WHITE",
                                                                   Totaldata$gill_color))))))
Renaming the levels back to the way they were for convenience

Totaldata[,gill_color := ifelse(gill_color == "11","WHITE",
                                ifelse(gill_color == "3","BUFF",
                                       ifelse(gill_color == "4","CHOCOLATE",
                                              ifelse(gill_color == "5","GRAY",
                                                     ifelse(gill_color == "8","PINK",
                                                            ifelse(gill_color == "9","PURPLE",gill_color))))))]
##       mushroom_type cap_shape cap_surface cap_color bruises  odor
##    1:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    2:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    3:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    4:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    5:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##   ---                                                            
## 8412:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8413:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8414:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8415:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8416:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
##       gill_attachment gill_spacing gill_size gill_color stalk_shape
##    1:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    2:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    3:            FREE      CROWDED    NARROW       PINK    TAPERING
##    4:            FREE      CROWDED    NARROW       PINK    TAPERING
##    5:            FREE      CROWDED    NARROW      WHITE    TAPERING
##   ---                                                              
## 8412:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8413:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8414:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8415:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8416:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
##       stalk_root stalk_surface_above_ring stalk_surface_below_ring
##    1:    BULBOUS                   SMOOTH                   SMOOTH
##    2:    BULBOUS                   SMOOTH                   SMOOTH
##    3:    BULBOUS                   SMOOTH                   SMOOTH
##    4:    BULBOUS                   SMOOTH                   SMOOTH
##    5:    BULBOUS                   SMOOTH                   SMOOTH
##   ---                                                             
## 8412:          ?                   SMOOTH                   SMOOTH
## 8413:          ?                   SMOOTH                   SMOOTH
## 8414:          ?                   SMOOTH                   SMOOTH
## 8415:          ?                   SMOOTH                   SMOOTH
## 8416:          ?                   SMOOTH                   SMOOTH
##       stalk_color_above_ring stalk_color_below_ring veil_type veil_color
##    1:                  WHITE                  WHITE   PARTIAL      WHITE
##    2:                  WHITE                  WHITE   PARTIAL      WHITE
##    3:                  WHITE                  WHITE   PARTIAL      WHITE
##    4:                  WHITE                  WHITE   PARTIAL      WHITE
##    5:                  WHITE                  WHITE   PARTIAL      WHITE
##   ---                                                                   
## 8412:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8413:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8414:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8415:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8416:                 ORANGE                 ORANGE   PARTIAL      BROWN
##       ring_number ring_type spore_print_color population habitat
##    1:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    2:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    3:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    4:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    5:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##   ---                                                           
## 8412:         ONE   PENDANT             BROWN  CLUSTERED  LEAVES
## 8413:         ONE   PENDANT            ORANGE    SEVERAL  LEAVES
## 8414:         ONE   PENDANT            ORANGE  CLUSTERED  LEAVES
## 8415:         ONE   PENDANT              BUFF    SEVERAL  LEAVES
## 8416:         ONE   PENDANT              BUFF  CLUSTERED  LEAVES
looking at the levels again.The count now seems high enough for split.

ggplot(Totaldata, aes(x = gill_color,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between gill color and mushroom type")


The table above shows the frequency of each factor in the variable

table(Totaldata$gill_color)
## 
##      BUFF CHOCOLATE      GRAY      PINK    PURPLE     WHITE 
##      1752       796       752      1556       492      3068
Stalk Shape VS Mushroom Type
Here the mushroom type levels are spread across in almost equal proportions suggesting a higher entropy and lower information gain which tells us that the variable is not that significant

ggplot(Totaldata, aes(x = stalk_shape,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between stalk shape and mushroom type")


Stalk Root VS Mushroom Type
Here we can see that some of the levels like CLUB and ROOTED seem to have very less count and hence can be combined with the level EQUAL since they are close to perfect predictors and can give higher information gain.

ggplot(Totaldata, aes(x = stalk_root,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between stalk root and mushroom type")


The table above shows the frequency of each factor in the variable

table(Totaldata$stalk_root)
## 
##       ? BULBOUS    CLUB   EQUAL  ROOTED 
##    2480    3800     568    1376     192
Colapsing the levels as mentioned above

Totaldata$stalk_root<-  ifelse(Totaldata$stalk_root == "CLUB","BULBOUS",
                               ifelse(Totaldata$stalk_root == "EQUAL","BULBOUS",
                                      ifelse(Totaldata$stalk_root == "ROOTED","BULBOUS",Totaldata$stalk_root)))
Renaming the levels for convenience

Totaldata[,stalk_root := ifelse(stalk_root == "1","NONE",
                                ifelse(stalk_root == "2","BULBOUS",stalk_root))]
##       mushroom_type cap_shape cap_surface cap_color bruises  odor
##    1:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    2:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    3:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    4:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    5:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##   ---                                                            
## 8412:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8413:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8414:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8415:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8416:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
##       gill_attachment gill_spacing gill_size gill_color stalk_shape
##    1:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    2:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    3:            FREE      CROWDED    NARROW       PINK    TAPERING
##    4:            FREE      CROWDED    NARROW       PINK    TAPERING
##    5:            FREE      CROWDED    NARROW      WHITE    TAPERING
##   ---                                                              
## 8412:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8413:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8414:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8415:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8416:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
##       stalk_root stalk_surface_above_ring stalk_surface_below_ring
##    1:    BULBOUS                   SMOOTH                   SMOOTH
##    2:    BULBOUS                   SMOOTH                   SMOOTH
##    3:    BULBOUS                   SMOOTH                   SMOOTH
##    4:    BULBOUS                   SMOOTH                   SMOOTH
##    5:    BULBOUS                   SMOOTH                   SMOOTH
##   ---                                                             
## 8412:       NONE                   SMOOTH                   SMOOTH
## 8413:       NONE                   SMOOTH                   SMOOTH
## 8414:       NONE                   SMOOTH                   SMOOTH
## 8415:       NONE                   SMOOTH                   SMOOTH
## 8416:       NONE                   SMOOTH                   SMOOTH
##       stalk_color_above_ring stalk_color_below_ring veil_type veil_color
##    1:                  WHITE                  WHITE   PARTIAL      WHITE
##    2:                  WHITE                  WHITE   PARTIAL      WHITE
##    3:                  WHITE                  WHITE   PARTIAL      WHITE
##    4:                  WHITE                  WHITE   PARTIAL      WHITE
##    5:                  WHITE                  WHITE   PARTIAL      WHITE
##   ---                                                                   
## 8412:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8413:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8414:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8415:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8416:                 ORANGE                 ORANGE   PARTIAL      BROWN
##       ring_number ring_type spore_print_color population habitat
##    1:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    2:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    3:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    4:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    5:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##   ---                                                           
## 8412:         ONE   PENDANT             BROWN  CLUSTERED  LEAVES
## 8413:         ONE   PENDANT            ORANGE    SEVERAL  LEAVES
## 8414:         ONE   PENDANT            ORANGE  CLUSTERED  LEAVES
## 8415:         ONE   PENDANT              BUFF    SEVERAL  LEAVES
## 8416:         ONE   PENDANT              BUFF  CLUSTERED  LEAVES
The table above shows the frequency of each factor in the variable

table(Totaldata$stalk_root)
## 
## BULBOUS    NONE 
##    5936    2480
looking at the levels again.The count now seems high enough for split.

ggplot(Totaldata, aes(x = stalk_root,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between stalk root and mushroom type")


Stalk Surface Above Ring VS Mushroom Type
Here we can see that some of the levels like FIBROUS SCALY have very less count and hence need to combined with SMOOTH level since it has more proportion of EDIBLE level

ggplot(Totaldata, aes(x = stalk_surface_above_ring,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between stalk surface above the ring and mushroom type")


The table above shows the frequency of each factor in the variable

table(Totaldata$stalk_surface_above_ring)
## 
## FIBROUS   SCALY   SILKY  SMOOTH 
##     692      24    2384    5316
Colapsing the levels as mentioned above

Totaldata$stalk_surface_above_ring <-  ifelse(Totaldata$stalk_surface_above_ring == "SCALY","SILKY",
                                      ifelse(Totaldata$stalk_surface_above_ring == "FIBROUS","SMOOTH",
                                             Totaldata$stalk_surface_above_ring))
Renaming the levels for convenience

Totaldata[,stalk_surface_above_ring := ifelse(stalk_surface_above_ring == "3","SILKY",
                                              ifelse(stalk_surface_above_ring == "4","SMOOTH",
                                                     stalk_surface_above_ring))]
##       mushroom_type cap_shape cap_surface cap_color bruises  odor
##    1:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    2:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    3:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    4:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    5:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##   ---                                                            
## 8412:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8413:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8414:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8415:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8416:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
##       gill_attachment gill_spacing gill_size gill_color stalk_shape
##    1:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    2:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    3:            FREE      CROWDED    NARROW       PINK    TAPERING
##    4:            FREE      CROWDED    NARROW       PINK    TAPERING
##    5:            FREE      CROWDED    NARROW      WHITE    TAPERING
##   ---                                                              
## 8412:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8413:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8414:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8415:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8416:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
##       stalk_root stalk_surface_above_ring stalk_surface_below_ring
##    1:    BULBOUS                   SMOOTH                   SMOOTH
##    2:    BULBOUS                   SMOOTH                   SMOOTH
##    3:    BULBOUS                   SMOOTH                   SMOOTH
##    4:    BULBOUS                   SMOOTH                   SMOOTH
##    5:    BULBOUS                   SMOOTH                   SMOOTH
##   ---                                                             
## 8412:       NONE                   SMOOTH                   SMOOTH
## 8413:       NONE                   SMOOTH                   SMOOTH
## 8414:       NONE                   SMOOTH                   SMOOTH
## 8415:       NONE                   SMOOTH                   SMOOTH
## 8416:       NONE                   SMOOTH                   SMOOTH
##       stalk_color_above_ring stalk_color_below_ring veil_type veil_color
##    1:                  WHITE                  WHITE   PARTIAL      WHITE
##    2:                  WHITE                  WHITE   PARTIAL      WHITE
##    3:                  WHITE                  WHITE   PARTIAL      WHITE
##    4:                  WHITE                  WHITE   PARTIAL      WHITE
##    5:                  WHITE                  WHITE   PARTIAL      WHITE
##   ---                                                                   
## 8412:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8413:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8414:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8415:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8416:                 ORANGE                 ORANGE   PARTIAL      BROWN
##       ring_number ring_type spore_print_color population habitat
##    1:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    2:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    3:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    4:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    5:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##   ---                                                           
## 8412:         ONE   PENDANT             BROWN  CLUSTERED  LEAVES
## 8413:         ONE   PENDANT            ORANGE    SEVERAL  LEAVES
## 8414:         ONE   PENDANT            ORANGE  CLUSTERED  LEAVES
## 8415:         ONE   PENDANT              BUFF    SEVERAL  LEAVES
## 8416:         ONE   PENDANT              BUFF  CLUSTERED  LEAVES
looking at the frequency again

table(Totaldata$stalk_surface_above_ring)
## 
##  SILKY SMOOTH 
##   2408   6008
looking at the levels again.The count now seems high enough for split.

ggplot(Totaldata, aes(x = stalk_surface_above_ring,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between stalk surface above the ring and mushroom type")


Stalk Surface Below Ring VS Mushroom Type
Here we can see that some of the levels like FIBROUS SCALY have very less count and hence need to combined with SMOOTH level since it has more proportion of EDIBLE level

ggplot(Totaldata, aes(x = stalk_surface_below_ring,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between stalk surface below the ring and mushroom type")


The table below shows the frequency of each factor in the variable

table(Totaldata$stalk_surface_below_ring)
## 
## FIBROUS   SCALY   SILKY  SMOOTH 
##     740     296    2304    5076
Colapsing the levels as mentioned above

Totaldata$stalk_surface_below_ring <-  ifelse(Totaldata$stalk_surface_below_ring == "SCALY","SMOOTH",
                                              ifelse(Totaldata$stalk_surface_below_ring == "FIBROUS","SMOOTH",
                                                     Totaldata$stalk_surface_below_ring))
Renaming the levels for convenience

Totaldata[,stalk_surface_below_ring := ifelse(stalk_surface_below_ring == "3","SILKY",
                                              ifelse(stalk_surface_below_ring == "4","SMOOTH",
                                                     stalk_surface_below_ring))]
##       mushroom_type cap_shape cap_surface cap_color bruises  odor
##    1:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    2:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    3:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    4:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    5:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##   ---                                                            
## 8412:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8413:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8414:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8415:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8416:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
##       gill_attachment gill_spacing gill_size gill_color stalk_shape
##    1:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    2:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    3:            FREE      CROWDED    NARROW       PINK    TAPERING
##    4:            FREE      CROWDED    NARROW       PINK    TAPERING
##    5:            FREE      CROWDED    NARROW      WHITE    TAPERING
##   ---                                                              
## 8412:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8413:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8414:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8415:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8416:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
##       stalk_root stalk_surface_above_ring stalk_surface_below_ring
##    1:    BULBOUS                   SMOOTH                   SMOOTH
##    2:    BULBOUS                   SMOOTH                   SMOOTH
##    3:    BULBOUS                   SMOOTH                   SMOOTH
##    4:    BULBOUS                   SMOOTH                   SMOOTH
##    5:    BULBOUS                   SMOOTH                   SMOOTH
##   ---                                                             
## 8412:       NONE                   SMOOTH                   SMOOTH
## 8413:       NONE                   SMOOTH                   SMOOTH
## 8414:       NONE                   SMOOTH                   SMOOTH
## 8415:       NONE                   SMOOTH                   SMOOTH
## 8416:       NONE                   SMOOTH                   SMOOTH
##       stalk_color_above_ring stalk_color_below_ring veil_type veil_color
##    1:                  WHITE                  WHITE   PARTIAL      WHITE
##    2:                  WHITE                  WHITE   PARTIAL      WHITE
##    3:                  WHITE                  WHITE   PARTIAL      WHITE
##    4:                  WHITE                  WHITE   PARTIAL      WHITE
##    5:                  WHITE                  WHITE   PARTIAL      WHITE
##   ---                                                                   
## 8412:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8413:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8414:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8415:                 ORANGE                 ORANGE   PARTIAL      BROWN
## 8416:                 ORANGE                 ORANGE   PARTIAL      BROWN
##       ring_number ring_type spore_print_color population habitat
##    1:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    2:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    3:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    4:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    5:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##   ---                                                           
## 8412:         ONE   PENDANT             BROWN  CLUSTERED  LEAVES
## 8413:         ONE   PENDANT            ORANGE    SEVERAL  LEAVES
## 8414:         ONE   PENDANT            ORANGE  CLUSTERED  LEAVES
## 8415:         ONE   PENDANT              BUFF    SEVERAL  LEAVES
## 8416:         ONE   PENDANT              BUFF  CLUSTERED  LEAVES
looking at the frequency again

table(Totaldata$stalk_surface_below_ring)
## 
##  SILKY SMOOTH 
##   2304   6112
looking at the colapsed levels.The count now seems high enough for split.

ggplot(Totaldata, aes(x = stalk_surface_below_ring,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between stalk surface below the ring and mushroom type")


Stalk Color Above Ring VS Mushroom Type
Here we can see that some of the levels like BROWN, BUFF, CINNAMON, GRAY, ORANGE, RED have very less count and hence need to be combined with WHITE level since it has more EDIBLE level.

ggplot(Totaldata, aes(x = stalk_color_above_ring,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between stalk color above the ring and mushroom type")


The table below shows the frequency of each factor in the variable

table(Totaldata$stalk_color_above_ring)
## 
##    BROWN     BUFF CINNAMON     GRAY   ORANGE     PINK      RED    WHITE 
##      448      432       48      576      192     1872       96     4744 
##   YELLOW 
##        8
Colapsing the levels as mentioned above

Totaldata$stalk_color_above_ring <-  ifelse(Totaldata$stalk_color_above_ring == "BUFF","PINK",
                                            ifelse(Totaldata$stalk_color_above_ring == "CINNAMON","PINK",
                                                   ifelse(Totaldata$stalk_color_above_ring == "ORANGE","WHITE",
                                        ifelse(Totaldata$stalk_color_above_ring == "RED","WHITE",
                                              ifelse(Totaldata$stalk_color_above_ring == "YELLOW","PINK",
                                               ifelse(Totaldata$stalk_color_above_ring == "BROWN","PINK",
                                          ifelse(Totaldata$stalk_color_above_ring == "GRAY","WHITE",
                                            Totaldata$stalk_color_above_ring)))))))
Renaming the levels for convenience

Totaldata[,stalk_color_above_ring := ifelse(stalk_color_above_ring == "6","PINK",
                                            ifelse(stalk_color_above_ring == "8","WHITE",stalk_color_above_ring))]
##       mushroom_type cap_shape cap_surface cap_color bruises  odor
##    1:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    2:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    3:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    4:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    5:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##   ---                                                            
## 8412:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8413:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8414:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8415:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8416:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
##       gill_attachment gill_spacing gill_size gill_color stalk_shape
##    1:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    2:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    3:            FREE      CROWDED    NARROW       PINK    TAPERING
##    4:            FREE      CROWDED    NARROW       PINK    TAPERING
##    5:            FREE      CROWDED    NARROW      WHITE    TAPERING
##   ---                                                              
## 8412:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8413:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8414:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8415:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8416:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
##       stalk_root stalk_surface_above_ring stalk_surface_below_ring
##    1:    BULBOUS                   SMOOTH                   SMOOTH
##    2:    BULBOUS                   SMOOTH                   SMOOTH
##    3:    BULBOUS                   SMOOTH                   SMOOTH
##    4:    BULBOUS                   SMOOTH                   SMOOTH
##    5:    BULBOUS                   SMOOTH                   SMOOTH
##   ---                                                             
## 8412:       NONE                   SMOOTH                   SMOOTH
## 8413:       NONE                   SMOOTH                   SMOOTH
## 8414:       NONE                   SMOOTH                   SMOOTH
## 8415:       NONE                   SMOOTH                   SMOOTH
## 8416:       NONE                   SMOOTH                   SMOOTH
##       stalk_color_above_ring stalk_color_below_ring veil_type veil_color
##    1:                  WHITE                  WHITE   PARTIAL      WHITE
##    2:                  WHITE                  WHITE   PARTIAL      WHITE
##    3:                  WHITE                  WHITE   PARTIAL      WHITE
##    4:                  WHITE                  WHITE   PARTIAL      WHITE
##    5:                  WHITE                  WHITE   PARTIAL      WHITE
##   ---                                                                   
## 8412:                  WHITE                 ORANGE   PARTIAL      BROWN
## 8413:                  WHITE                 ORANGE   PARTIAL      BROWN
## 8414:                  WHITE                 ORANGE   PARTIAL      BROWN
## 8415:                  WHITE                 ORANGE   PARTIAL      BROWN
## 8416:                  WHITE                 ORANGE   PARTIAL      BROWN
##       ring_number ring_type spore_print_color population habitat
##    1:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    2:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    3:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    4:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    5:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##   ---                                                           
## 8412:         ONE   PENDANT             BROWN  CLUSTERED  LEAVES
## 8413:         ONE   PENDANT            ORANGE    SEVERAL  LEAVES
## 8414:         ONE   PENDANT            ORANGE  CLUSTERED  LEAVES
## 8415:         ONE   PENDANT              BUFF    SEVERAL  LEAVES
## 8416:         ONE   PENDANT              BUFF  CLUSTERED  LEAVES
looking at the frequency again

table(Totaldata$stalk_color_above_ring)
## 
##  PINK WHITE 
##  2808  5608
Ploting the colpased levels.The count now seems high enough for split.

ggplot(Totaldata, aes(x = stalk_color_above_ring,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between stalk color above the ring and mushroom type")


Stalk Color Below Ring VS Mushroom Type
Here we can see that some of the levels like BROWN, BUFF, CINNAMON, GRAY, ORANGE, RED have very less count and hence need to be combined with WHITE level since it has more EDIBLE level.

ggplot(Totaldata, aes(x = stalk_color_below_ring,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between stalk color below the ring and mushroom type")


The table above shows the frequency of each factor in the variable

table(Totaldata$stalk_color_below_ring)
## 
##    BROWN     BUFF CINNAMON     GRAY   ORANGE     PINK      RED    WHITE 
##      536      432       48      576      192     1872       96     4640 
##   YELLOW 
##       24
Colapsing the levels as mentioned above

Totaldata$stalk_color_below_ring <-  ifelse(Totaldata$stalk_color_below_ring == "BUFF","PINK",
                                      ifelse(Totaldata$stalk_color_below_ring == "CINNAMON","PINK",
                                         ifelse(Totaldata$stalk_color_below_ring == "ORANGE","WHITE",
                                           ifelse(Totaldata$stalk_color_below_ring == "RED","WHITE",
                                              ifelse(Totaldata$stalk_color_below_ring == "YELLOW","PINK",
                                                ifelse(Totaldata$stalk_color_below_ring == "BROWN","PINK",
                                                 ifelse(Totaldata$stalk_color_below_ring == "GRAY","WHITE",
                                                        Totaldata$stalk_color_below_ring)))))))
Renaming the levels for convenience

Totaldata[,stalk_color_below_ring := ifelse(stalk_color_below_ring == "6","PINK",
                                            ifelse(stalk_color_below_ring == "8","WHITE",stalk_color_below_ring))]
##       mushroom_type cap_shape cap_surface cap_color bruises  odor
##    1:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    2:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    3:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    4:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    5:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##   ---                                                            
## 8412:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8413:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8414:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8415:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8416:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
##       gill_attachment gill_spacing gill_size gill_color stalk_shape
##    1:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    2:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    3:            FREE      CROWDED    NARROW       PINK    TAPERING
##    4:            FREE      CROWDED    NARROW       PINK    TAPERING
##    5:            FREE      CROWDED    NARROW      WHITE    TAPERING
##   ---                                                              
## 8412:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8413:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8414:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8415:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8416:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
##       stalk_root stalk_surface_above_ring stalk_surface_below_ring
##    1:    BULBOUS                   SMOOTH                   SMOOTH
##    2:    BULBOUS                   SMOOTH                   SMOOTH
##    3:    BULBOUS                   SMOOTH                   SMOOTH
##    4:    BULBOUS                   SMOOTH                   SMOOTH
##    5:    BULBOUS                   SMOOTH                   SMOOTH
##   ---                                                             
## 8412:       NONE                   SMOOTH                   SMOOTH
## 8413:       NONE                   SMOOTH                   SMOOTH
## 8414:       NONE                   SMOOTH                   SMOOTH
## 8415:       NONE                   SMOOTH                   SMOOTH
## 8416:       NONE                   SMOOTH                   SMOOTH
##       stalk_color_above_ring stalk_color_below_ring veil_type veil_color
##    1:                  WHITE                  WHITE   PARTIAL      WHITE
##    2:                  WHITE                  WHITE   PARTIAL      WHITE
##    3:                  WHITE                  WHITE   PARTIAL      WHITE
##    4:                  WHITE                  WHITE   PARTIAL      WHITE
##    5:                  WHITE                  WHITE   PARTIAL      WHITE
##   ---                                                                   
## 8412:                  WHITE                  WHITE   PARTIAL      BROWN
## 8413:                  WHITE                  WHITE   PARTIAL      BROWN
## 8414:                  WHITE                  WHITE   PARTIAL      BROWN
## 8415:                  WHITE                  WHITE   PARTIAL      BROWN
## 8416:                  WHITE                  WHITE   PARTIAL      BROWN
##       ring_number ring_type spore_print_color population habitat
##    1:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    2:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    3:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    4:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    5:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##   ---                                                           
## 8412:         ONE   PENDANT             BROWN  CLUSTERED  LEAVES
## 8413:         ONE   PENDANT            ORANGE    SEVERAL  LEAVES
## 8414:         ONE   PENDANT            ORANGE  CLUSTERED  LEAVES
## 8415:         ONE   PENDANT              BUFF    SEVERAL  LEAVES
## 8416:         ONE   PENDANT              BUFF  CLUSTERED  LEAVES
looking at the frequency again

table(Totaldata$stalk_color_below_ring)
## 
##  PINK WHITE 
##  2912  5504
Ploting the colapsed levels.The count now seems high enough for split.

ggplot(Totaldata, aes(x = stalk_color_below_ring,fill = mushroom_type))+geom_bar()+
  labs(title = "bar chart between stalk color below the ring and mushroom type") 


Veil Type VS Mushroom Type
Here we can see that there is only one level and hence will not have any significance. This will be removed from the model. Although you can divide this level into two levels which can have a high significance.

ggplot(Totaldata, aes(x = veil_type,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between veil type and mushroom type")


Veil Color VS Mushroom Type
Here we can see that there are levels like BROWN, ORANGE and YELLOW have very less count and hence will not have any significance. This will be removed from the model.

ggplot(Totaldata, aes(x = veil_color,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between veil color and mushroom type")


The table below shows the frequency of each factor in the variable

table(Totaldata$veil_color)
## 
##  BROWN ORANGE  WHITE YELLOW 
##     96     96   8216      8
Ring Number VS Mushroom Type
Here we can see that the levels NONE and TWO have very little count and colapsing them into one level will not do any good to the accuracy of the model. Hence I will be removing this from the model.

ggplot(Totaldata, aes(x = ring_number,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between ring number and mushroom type")


The table above shows the frequency of each factor in the variable

table(Totaldata$ring_number)
## 
## NONE  ONE  TWO 
##   48 7768  600
Ring Type VS Mushroom Type
Here we can combine the levels FLARING with PENDANT and LARGE,NONE with EVANESCENT to reduce the entropy and there by improve the information gain.

ggplot(Totaldata, aes(x = ring_type,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between ring type and mushroom type")


The table below shows the frequency of each factor in the variable

table(Totaldata$ring_type)
## 
## EVANESCENT    FLARING      LARGE       NONE    PENDANT 
##       3056         48       1296         48       3968
Colapsing the levels as mentioned above

Totaldata$ring_type <- ifelse(Totaldata$ring_type == "FLARING","PENDANT",
                              ifelse(Totaldata$ring_type == "NONE","EVANESCENT",
                                     ifelse(Totaldata$ring_type == "LARGE","EVANESCENT",
                                            ifelse(Totaldata$ring_type == "LARGE","EVANESCENT",
                                                   Totaldata$ring_type))))
Renaming the levels for convenience

Totaldata[,ring_type := ifelse(ring_type == "1","EVANESCENT",
                               ifelse(ring_type == "5","PENDANT",ring_type))]
##       mushroom_type cap_shape cap_surface cap_color bruises  odor
##    1:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    2:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    3:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    4:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    5:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##   ---                                                            
## 8412:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8413:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8414:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8415:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8416:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
##       gill_attachment gill_spacing gill_size gill_color stalk_shape
##    1:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    2:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    3:            FREE      CROWDED    NARROW       PINK    TAPERING
##    4:            FREE      CROWDED    NARROW       PINK    TAPERING
##    5:            FREE      CROWDED    NARROW      WHITE    TAPERING
##   ---                                                              
## 8412:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8413:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8414:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8415:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8416:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
##       stalk_root stalk_surface_above_ring stalk_surface_below_ring
##    1:    BULBOUS                   SMOOTH                   SMOOTH
##    2:    BULBOUS                   SMOOTH                   SMOOTH
##    3:    BULBOUS                   SMOOTH                   SMOOTH
##    4:    BULBOUS                   SMOOTH                   SMOOTH
##    5:    BULBOUS                   SMOOTH                   SMOOTH
##   ---                                                             
## 8412:       NONE                   SMOOTH                   SMOOTH
## 8413:       NONE                   SMOOTH                   SMOOTH
## 8414:       NONE                   SMOOTH                   SMOOTH
## 8415:       NONE                   SMOOTH                   SMOOTH
## 8416:       NONE                   SMOOTH                   SMOOTH
##       stalk_color_above_ring stalk_color_below_ring veil_type veil_color
##    1:                  WHITE                  WHITE   PARTIAL      WHITE
##    2:                  WHITE                  WHITE   PARTIAL      WHITE
##    3:                  WHITE                  WHITE   PARTIAL      WHITE
##    4:                  WHITE                  WHITE   PARTIAL      WHITE
##    5:                  WHITE                  WHITE   PARTIAL      WHITE
##   ---                                                                   
## 8412:                  WHITE                  WHITE   PARTIAL      BROWN
## 8413:                  WHITE                  WHITE   PARTIAL      BROWN
## 8414:                  WHITE                  WHITE   PARTIAL      BROWN
## 8415:                  WHITE                  WHITE   PARTIAL      BROWN
## 8416:                  WHITE                  WHITE   PARTIAL      BROWN
##       ring_number ring_type spore_print_color population habitat
##    1:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    2:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    3:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##    4:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    5:         ONE   PENDANT            PURPLE    SEVERAL   WOODS
##   ---                                                           
## 8412:         ONE   PENDANT             BROWN  CLUSTERED  LEAVES
## 8413:         ONE   PENDANT            ORANGE    SEVERAL  LEAVES
## 8414:         ONE   PENDANT            ORANGE  CLUSTERED  LEAVES
## 8415:         ONE   PENDANT              BUFF    SEVERAL  LEAVES
## 8416:         ONE   PENDANT              BUFF  CLUSTERED  LEAVES
looking at the frequency again

table(Totaldata$ring_type)
## 
## EVANESCENT    PENDANT 
##       4400       4016
Ploting the colapsed levels again.The count now seems high enough for split.

ggplot(Totaldata, aes(x = ring_type,fill = mushroom_type))+geom_bar()+
  labs(title = "bar chart between ring type and mushroom type")


Spore Print Color VS Mushroom Type
Here we can comibine the levels BLACK, BROWN, BUFF, ORANGE, PURPLE and YELLOW together and CHOCOLATE and WHITE together to reduce the entropy in the model and there by improve the information gain

ggplot(Totaldata, aes(x = spore_print_color,fill = mushroom_type))+geom_bar()+
  labs(title = "bar chart between spore print color and mushroom type")


The table above shows the frequency of each factor in the variable

table(Totaldata$spore_print_color)
## 
##     BLACK     BROWN      BUFF CHOCOLATE     GREEN    ORANGE    PURPLE 
##      2000      2096        48      1632        72        48        48 
##     WHITE    YELLOW 
##      2424        48
Colapsing the levels as mentioned above

Totaldata$spore_print_color <- ifelse(Totaldata$spore_print_color == "BUFF","BROWN",
                                ifelse(Totaldata$spore_print_color == "GREEN","WHITE",
                                 ifelse(Totaldata$spore_print_color == "ORANGE","BROWN",
                                  ifelse(Totaldata$spore_print_color == "PURPLE","BROWN",
                                    ifelse(Totaldata$spore_print_color == "YELLOW","BROWN",
                                      ifelse(Totaldata$spore_print_color == "CHOCOLATE","WHITE",
                                        ifelse(Totaldata$spore_print_color == "BLACK","BROWN",
                                          Totaldata$spore_print_color)))))))
Renaming the levels for convenience

Totaldata[,spore_print_color := ifelse(spore_print_color == "2","BROWN",
                                       ifelse(spore_print_color == "8","WHITE",spore_print_color))]
##       mushroom_type cap_shape cap_surface cap_color bruises  odor
##    1:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    2:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    3:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    4:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    5:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##   ---                                                            
## 8412:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8413:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8414:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8415:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8416:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
##       gill_attachment gill_spacing gill_size gill_color stalk_shape
##    1:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    2:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    3:            FREE      CROWDED    NARROW       PINK    TAPERING
##    4:            FREE      CROWDED    NARROW       PINK    TAPERING
##    5:            FREE      CROWDED    NARROW      WHITE    TAPERING
##   ---                                                              
## 8412:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8413:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8414:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8415:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8416:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
##       stalk_root stalk_surface_above_ring stalk_surface_below_ring
##    1:    BULBOUS                   SMOOTH                   SMOOTH
##    2:    BULBOUS                   SMOOTH                   SMOOTH
##    3:    BULBOUS                   SMOOTH                   SMOOTH
##    4:    BULBOUS                   SMOOTH                   SMOOTH
##    5:    BULBOUS                   SMOOTH                   SMOOTH
##   ---                                                             
## 8412:       NONE                   SMOOTH                   SMOOTH
## 8413:       NONE                   SMOOTH                   SMOOTH
## 8414:       NONE                   SMOOTH                   SMOOTH
## 8415:       NONE                   SMOOTH                   SMOOTH
## 8416:       NONE                   SMOOTH                   SMOOTH
##       stalk_color_above_ring stalk_color_below_ring veil_type veil_color
##    1:                  WHITE                  WHITE   PARTIAL      WHITE
##    2:                  WHITE                  WHITE   PARTIAL      WHITE
##    3:                  WHITE                  WHITE   PARTIAL      WHITE
##    4:                  WHITE                  WHITE   PARTIAL      WHITE
##    5:                  WHITE                  WHITE   PARTIAL      WHITE
##   ---                                                                   
## 8412:                  WHITE                  WHITE   PARTIAL      BROWN
## 8413:                  WHITE                  WHITE   PARTIAL      BROWN
## 8414:                  WHITE                  WHITE   PARTIAL      BROWN
## 8415:                  WHITE                  WHITE   PARTIAL      BROWN
## 8416:                  WHITE                  WHITE   PARTIAL      BROWN
##       ring_number ring_type spore_print_color population habitat
##    1:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    2:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    3:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    4:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    5:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##   ---                                                           
## 8412:         ONE   PENDANT             BROWN  CLUSTERED  LEAVES
## 8413:         ONE   PENDANT             BROWN    SEVERAL  LEAVES
## 8414:         ONE   PENDANT             BROWN  CLUSTERED  LEAVES
## 8415:         ONE   PENDANT             BROWN    SEVERAL  LEAVES
## 8416:         ONE   PENDANT             BROWN  CLUSTERED  LEAVES
Looking at the frequency again

table(Totaldata$spore_print_color)
## 
## BROWN WHITE 
##  4288  4128
Ploting the colapsed levels again.The count now seems high enough for split.

ggplot(Totaldata, aes(x = spore_print_color,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between spore print color and mushroom type")


Population VS Mushroom Type
Here we can combine the levels ABUNDANT, CLUSTERED, NUMEROUS, SCATTERED and SOLITARY together to improve the information gain.

ggplot(Totaldata, aes(x = population,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between population and mushroom type")


The table below shows the frequency of each factor in the variable

table(Totaldata$population)
## 
##  ABUNDANT CLUSTERED  NUMEROUS SCATTERED   SEVERAL  SOLITARY 
##       512       352       400      1376      4064      1712
Colapsing the levels as mentioned above

Totaldata$population <- ifelse(Totaldata$population == "ABUNDANT","SOLITARY",
                          ifelse(Totaldata$population == "CLUSTERED","SOLITARY",
                            ifelse(Totaldata$population == "NUMEROUS","SOLITARY",
                              ifelse(Totaldata$population == "SCATTERED","SOLITARY",Totaldata$population))))
Renaming the levels for convenience

Totaldata[,population := ifelse(population == "5","SEVERAL",
                                ifelse(population == "6","SOLITARY",population))]
##       mushroom_type cap_shape cap_surface cap_color bruises  odor
##    1:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    2:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    3:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    4:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    5:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##   ---                                                            
## 8412:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8413:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8414:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8415:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8416:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
##       gill_attachment gill_spacing gill_size gill_color stalk_shape
##    1:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    2:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    3:            FREE      CROWDED    NARROW       PINK    TAPERING
##    4:            FREE      CROWDED    NARROW       PINK    TAPERING
##    5:            FREE      CROWDED    NARROW      WHITE    TAPERING
##   ---                                                              
## 8412:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8413:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8414:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8415:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8416:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
##       stalk_root stalk_surface_above_ring stalk_surface_below_ring
##    1:    BULBOUS                   SMOOTH                   SMOOTH
##    2:    BULBOUS                   SMOOTH                   SMOOTH
##    3:    BULBOUS                   SMOOTH                   SMOOTH
##    4:    BULBOUS                   SMOOTH                   SMOOTH
##    5:    BULBOUS                   SMOOTH                   SMOOTH
##   ---                                                             
## 8412:       NONE                   SMOOTH                   SMOOTH
## 8413:       NONE                   SMOOTH                   SMOOTH
## 8414:       NONE                   SMOOTH                   SMOOTH
## 8415:       NONE                   SMOOTH                   SMOOTH
## 8416:       NONE                   SMOOTH                   SMOOTH
##       stalk_color_above_ring stalk_color_below_ring veil_type veil_color
##    1:                  WHITE                  WHITE   PARTIAL      WHITE
##    2:                  WHITE                  WHITE   PARTIAL      WHITE
##    3:                  WHITE                  WHITE   PARTIAL      WHITE
##    4:                  WHITE                  WHITE   PARTIAL      WHITE
##    5:                  WHITE                  WHITE   PARTIAL      WHITE
##   ---                                                                   
## 8412:                  WHITE                  WHITE   PARTIAL      BROWN
## 8413:                  WHITE                  WHITE   PARTIAL      BROWN
## 8414:                  WHITE                  WHITE   PARTIAL      BROWN
## 8415:                  WHITE                  WHITE   PARTIAL      BROWN
## 8416:                  WHITE                  WHITE   PARTIAL      BROWN
##       ring_number ring_type spore_print_color population habitat
##    1:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    2:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    3:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    4:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    5:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##   ---                                                           
## 8412:         ONE   PENDANT             BROWN   SOLITARY  LEAVES
## 8413:         ONE   PENDANT             BROWN    SEVERAL  LEAVES
## 8414:         ONE   PENDANT             BROWN   SOLITARY  LEAVES
## 8415:         ONE   PENDANT             BROWN    SEVERAL  LEAVES
## 8416:         ONE   PENDANT             BROWN   SOLITARY  LEAVES
Looking at the frequency again

table(Totaldata$population)
## 
##  SEVERAL SOLITARY 
##     4064     4352
Ploting the colapsed levels again.The count now seems high enough for split.

ggplot(Totaldata, aes(x = population,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between population and mushroom type")


Habitat VS Mushroom Type
We can see that some of the levels like WASTE and MEADOWS need to be combined with WOODS since they have a very less count.

ggplot(Totaldata, aes(x = habitat,fill = mushroom_type))+geom_bar()+
  labs(title = "bar chart between habitat and mushroom type")


the table below shows the frequency of each factor in the variable

table(Totaldata$habitat)
## 
## GRASSES  LEAVES MEADOWS   PATHS   URBAN   WASTE   WOODS 
##    2404     856     292    1144     368     192    3160
Colapsing the levels as mentioned above

Totaldata$habitat <- ifelse(Totaldata$habitat == "MEADOWS","WOODS",
                            ifelse(Totaldata$habitat == "URBAN","PATHS",
                                   ifelse(Totaldata$habitat == "WASTE","WOODS",Totaldata$habitat)))
Renaming the levels for convenience

Totaldata[,habitat := ifelse(habitat == "1","GRASSES",
                             ifelse(habitat == "2","LEAVES",
                                    ifelse(habitat == "4","PATHS",
                                           ifelse(habitat == "7","WOODS",habitat))))]
##       mushroom_type cap_shape cap_surface cap_color bruises  odor
##    1:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    2:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    3:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    4:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##    5:        EDIBLE    CONVEX      SMOOTH      Comb BRUISES Comb1
##   ---                                                            
## 8412:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8413:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8414:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8415:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
## 8416:        EDIBLE   KNOBBED      SMOOTH     BROWN      NO Comb1
##       gill_attachment gill_spacing gill_size gill_color stalk_shape
##    1:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    2:            FREE      CROWDED    NARROW      WHITE    TAPERING
##    3:            FREE      CROWDED    NARROW       PINK    TAPERING
##    4:            FREE      CROWDED    NARROW       PINK    TAPERING
##    5:            FREE      CROWDED    NARROW      WHITE    TAPERING
##   ---                                                              
## 8412:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8413:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8414:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8415:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
## 8416:        ATTACHED        CLOSE     BROAD      WHITE   ENLARGING
##       stalk_root stalk_surface_above_ring stalk_surface_below_ring
##    1:    BULBOUS                   SMOOTH                   SMOOTH
##    2:    BULBOUS                   SMOOTH                   SMOOTH
##    3:    BULBOUS                   SMOOTH                   SMOOTH
##    4:    BULBOUS                   SMOOTH                   SMOOTH
##    5:    BULBOUS                   SMOOTH                   SMOOTH
##   ---                                                             
## 8412:       NONE                   SMOOTH                   SMOOTH
## 8413:       NONE                   SMOOTH                   SMOOTH
## 8414:       NONE                   SMOOTH                   SMOOTH
## 8415:       NONE                   SMOOTH                   SMOOTH
## 8416:       NONE                   SMOOTH                   SMOOTH
##       stalk_color_above_ring stalk_color_below_ring veil_type veil_color
##    1:                  WHITE                  WHITE   PARTIAL      WHITE
##    2:                  WHITE                  WHITE   PARTIAL      WHITE
##    3:                  WHITE                  WHITE   PARTIAL      WHITE
##    4:                  WHITE                  WHITE   PARTIAL      WHITE
##    5:                  WHITE                  WHITE   PARTIAL      WHITE
##   ---                                                                   
## 8412:                  WHITE                  WHITE   PARTIAL      BROWN
## 8413:                  WHITE                  WHITE   PARTIAL      BROWN
## 8414:                  WHITE                  WHITE   PARTIAL      BROWN
## 8415:                  WHITE                  WHITE   PARTIAL      BROWN
## 8416:                  WHITE                  WHITE   PARTIAL      BROWN
##       ring_number ring_type spore_print_color population habitat
##    1:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    2:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    3:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    4:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##    5:         ONE   PENDANT             BROWN    SEVERAL   WOODS
##   ---                                                           
## 8412:         ONE   PENDANT             BROWN   SOLITARY  LEAVES
## 8413:         ONE   PENDANT             BROWN    SEVERAL  LEAVES
## 8414:         ONE   PENDANT             BROWN   SOLITARY  LEAVES
## 8415:         ONE   PENDANT             BROWN    SEVERAL  LEAVES
## 8416:         ONE   PENDANT             BROWN   SOLITARY  LEAVES
Looking at the frequency again

table(Totaldata$habitat)
## 
## GRASSES  LEAVES   PATHS   WOODS 
##    2404     856    1512    3644
Ploting the colapsed levels. The count now seems high enough for split

ggplot(Totaldata, aes(x = habitat,fill = mushroom_type))+geom_bar()+labs(title = "bar chart between habitat and mushroom type")


Columns Removed
Some of the variables removed are as follows.

Totaldata$veil_type <- NULL
Totaldata$ring_number <- NULL
Totaldata$veil_color <- NULL
After the manipulation some of the variables had to be converted back to factors.

Totaldata <- as.data.frame(Totaldata)
Totaldata <- sapply(Totaldata, as.factor)
Totaldata <- as.data.frame(Totaldata)
Data Partition
Now I am shuffling the data by number of rows so that the last 20% of the rows can be taken as validation data.

Totaldata <- Totaldata[sample(nrow(Totaldata)),]
dim(Totaldata)
## [1] 8416   20
Dividing the data into 80% train and 20% of validation data or test data

Traindata <- Totaldata[1:6740,]
Testdata <- Totaldata[6741:8416,]
str(Traindata)
## 'data.frame':    6740 obs. of  20 variables:
##  $ mushroom_type           : Factor w/ 2 levels "EDIBLE","POISONOUS": 2 1 2 1 2 1 1 2 1 1 ...
##  $ cap_shape               : Factor w/ 3 levels "CONVEX","FLAT",..: 1 2 1 1 2 1 1 2 1 2 ...
##  $ cap_surface             : Factor w/ 3 levels "FIBROUS","SCALY",..: 2 3 2 3 2 2 1 2 2 1 ...
##  $ cap_color               : Factor w/ 5 levels "BROWN","Comb",..: 2 3 3 2 1 2 2 3 5 2 ...
##  $ bruises                 : Factor w/ 2 levels "BRUISES","NO": 1 2 2 1 2 1 2 2 1 2 ...
##  $ odor                    : Factor w/ 2 levels "Comb","Comb1": 1 2 1 2 1 2 2 1 2 2 ...
##  $ gill_attachment         : Factor w/ 2 levels "ATTACHED","FREE": 2 2 2 2 2 2 2 2 2 2 ...
##  $ gill_spacing            : Factor w/ 2 levels "CLOSE","CROWDED": 1 2 1 1 1 1 2 1 1 2 ...
##  $ gill_size               : Factor w/ 2 levels "BROAD","NARROW": 2 1 1 1 2 1 1 1 1 1 ...
##  $ gill_color              : Factor w/ 6 levels "BUFF","CHOCOLATE",..: 6 6 4 6 1 3 6 2 6 6 ...
##  $ stalk_shape             : Factor w/ 2 levels "ENLARGING","TAPERING": 1 2 1 1 2 1 1 1 1 2 ...
##  $ stalk_root              : Factor w/ 2 levels "BULBOUS","NONE": 1 1 1 1 2 1 2 1 1 1 ...
##  $ stalk_surface_above_ring: Factor w/ 2 levels "SILKY","SMOOTH": 2 2 1 2 1 2 1 1 2 2 ...
##  $ stalk_surface_below_ring: Factor w/ 2 levels "SILKY","SMOOTH": 2 2 1 2 2 2 2 1 2 2 ...
##  $ stalk_color_above_ring  : Factor w/ 2 levels "PINK","WHITE": 2 2 1 2 1 2 2 1 2 2 ...
##  $ stalk_color_below_ring  : Factor w/ 2 levels "PINK","WHITE": 2 2 1 2 1 2 2 1 2 2 ...
##  $ ring_type               : Factor w/ 2 levels "EVANESCENT","PENDANT": 2 1 1 2 1 2 2 1 2 1 ...
##  $ spore_print_color       : Factor w/ 2 levels "BROWN","WHITE": 1 1 2 1 2 1 2 2 1 1 ...
##  $ population              : Factor w/ 2 levels "SEVERAL","SOLITARY": 2 2 2 2 1 2 2 2 2 2 ...
##  $ habitat                 : Factor w/ 4 levels "GRASSES","LEAVES",..: 1 1 4 4 2 4 1 4 4 1 ...
str(Testdata)
## 'data.frame':    1676 obs. of  20 variables:
##  $ mushroom_type           : Factor w/ 2 levels "EDIBLE","POISONOUS": 1 1 2 1 2 1 1 2 1 1 ...
##  $ cap_shape               : Factor w/ 3 levels "CONVEX","FLAT",..: 2 2 1 1 3 2 2 1 1 2 ...
##  $ cap_surface             : Factor w/ 3 levels "FIBROUS","SCALY",..: 3 1 1 3 3 1 2 1 3 1 ...
##  $ cap_color               : Factor w/ 5 levels "BROWN","Comb",..: 1 4 5 1 1 3 4 3 2 3 ...
##  $ bruises                 : Factor w/ 2 levels "BRUISES","NO": 2 1 2 2 2 1 1 2 1 2 ...
##  $ odor                    : Factor w/ 2 levels "Comb","Comb1": 2 2 1 2 1 2 2 1 2 2 ...
##  $ gill_attachment         : Factor w/ 2 levels "ATTACHED","FREE": 2 2 2 2 2 2 2 2 2 2 ...
##  $ gill_spacing            : Factor w/ 2 levels "CLOSE","CROWDED": 2 1 1 2 1 1 1 1 1 2 ...
##  $ gill_size               : Factor w/ 2 levels "BROAD","NARROW": 1 1 1 1 2 1 1 1 1 1 ...
##  $ gill_color              : Factor w/ 6 levels "BUFF","CHOCOLATE",..: 4 4 4 6 1 4 6 2 6 4 ...
##  $ stalk_shape             : Factor w/ 2 levels "ENLARGING","TAPERING": 2 2 1 2 2 2 2 1 1 2 ...
##  $ stalk_root              : Factor w/ 2 levels "BULBOUS","NONE": 1 1 1 1 2 1 1 1 1 1 ...
##  $ stalk_surface_above_ring: Factor w/ 2 levels "SILKY","SMOOTH": 2 2 1 2 1 2 2 1 2 2 ...
##  $ stalk_surface_below_ring: Factor w/ 2 levels "SILKY","SMOOTH": 2 2 1 2 1 2 2 1 2 2 ...
##  $ stalk_color_above_ring  : Factor w/ 2 levels "PINK","WHITE": 2 2 1 2 1 1 2 1 2 2 ...
##  $ stalk_color_below_ring  : Factor w/ 2 levels "PINK","WHITE": 2 2 1 2 2 2 2 1 2 2 ...
##  $ ring_type               : Factor w/ 2 levels "EVANESCENT","PENDANT": 1 2 1 1 1 2 2 1 2 1 ...
##  $ spore_print_color       : Factor w/ 2 levels "BROWN","WHITE": 1 1 2 1 2 1 1 2 1 1 ...
##  $ population              : Factor w/ 2 levels "SEVERAL","SOLITARY": 2 1 1 2 1 2 2 2 2 2 ...
##  $ habitat                 : Factor w/ 4 levels "GRASSES","LEAVES",..: 1 4 4 1 2 4 4 4 1 1 ...
Modelling Using Random Forest
The basic steps followed by me are as follows

Create a task
Make a learner
Set up the tuning parameters
Tune the parameters
Train the model with these parameters
Predict the Test data
Creating a Task for the mlr package. This is basically setting up the data before it can be trained

Train_Task <- makeClassifTask(data = Traindata, target = "mushroom_type")
Test_Task <- makeClassifTask(data = Testdata, target = "mushroom_type")
This Fuction gives the relative importance of each variable by taking information gain into consideration which is 1- entropy. You can consider other parameters like Chi-Square and Importance for calculating the relative importance.
Here we can see that the variable odor has a very high information gain which would greatly help the model to do a better prediction considering the other variables having a high information gain.
im_feat1 <- generateFilterValuesData(Train_Task, method = c("information.gain"))
plotFilterValues(im_feat1,n.show = 24)


Let’s look at some of the tunable parameters for randomforest. Depending on the knowledge on these parameters, we can use them for the tuning purpose.

getParamSet("classif.randomForest")
##                      Type  len   Def   Constr Req Tunable Trafo
## ntree             integer    -   500 1 to Inf   -    TRUE     -
## mtry              integer    -     - 1 to Inf   -    TRUE     -
## replace           logical    -  TRUE        -   -    TRUE     -
## classwt     numericvector <NA>     - 0 to Inf   -    TRUE     -
## cutoff      numericvector <NA>     -   0 to 1   -    TRUE     -
## strata            untyped    -     -        -   -    TRUE     -
## sampsize    integervector <NA>     - 1 to Inf   -    TRUE     -
## nodesize          integer    -     1 1 to Inf   -    TRUE     -
## maxnodes          integer    -     - 1 to Inf   -    TRUE     -
## importance        logical    - FALSE        -   -    TRUE     -
## localImp          logical    - FALSE        -   -    TRUE     -
## proximity         logical    - FALSE        -   -   FALSE     -
## oob.prox          logical    -     -        -   Y   FALSE     -
## norm.votes        logical    -  TRUE        -   -   FALSE     -
## do.trace          logical    - FALSE        -   -   FALSE     -
## keep.forest       logical    -  TRUE        -   -   FALSE     -
## keep.inbag        logical    - FALSE        -   -   FALSE     -
Making a classification learner. This tells the mlr package that a randomforest model needs to be built.

logit_mush <- makeLearner("classif.randomForest", predict.type = "response")
Setting up the tunable parameters for hyper tuning. The model takes random values for the parameters between the lower and upper bounds provided. These parameters are integer parameters and they are bounded.

rf_parm <- makeParamSet(makeIntegerParam("mtry", lower = 1, upper = 5),
                        makeIntegerParam("ntree", lower = 40, upper = 400 ),
                        makeIntegerParam("nodesize", lower = 10, upper = 50))
A few more parameters that are not bounded.

rf_parm$par.vals <- list(mtry = 5, ntree = 200, information.gain = TRUE)
Setting up a random control parameter will help in picking up values from tunable parameters randomly and build a model upon each randomly selected set of parameters and then combine these models to build a better model which is basically known as Bagging.These max iterations tell the model to iterate 10 times.
The number of iterations can be changed by keeping an eye on the overfitting of the data.
Rand <- makeTuneControlRandom(maxit = 10L)
Making a crossvalidation parameter so that the algorithm uses it to predict the mean accuracy of the model on the validation data set it automatically creates.

rf_cv <- makeResampleDesc("CV",iters = 3L )
Here the parameters are tuned by providing the learner, task, crossvalidation, measure to be used for deciding the model performance, parameter set and random control parameter to the function tuneParams()
I am using the measure accuracy because it is a classification model.
rf_tune <- tuneParams(learner = logit_mush, task = Train_Task,
                      resampling = rf_cv, measures = acc, 
                      par.set = rf_parm, control = Rand )
## [Tune] Started tuning learner classif.randomForest for parameter set:
##             Type len Def    Constr Req Tunable Trafo
## mtry     integer   -   -    1 to 5   -    TRUE     -
## ntree    integer   -   - 40 to 400   -    TRUE     -
## nodesize integer   -   -  10 to 50   -    TRUE     -
## With control class: TuneControlRandom
## Imputation value: -0
## [Tune-x] 1: mtry=3; ntree=47; nodesize=46
## [Tune-y] 1: acc.test.mean=0.998; time: 0.0 min; memory: 89Mb use, 173Mb max
## [Tune-x] 2: mtry=2; ntree=327; nodesize=41
## [Tune-y] 2: acc.test.mean=0.997; time: 0.0 min; memory: 89Mb use, 173Mb max
## [Tune-x] 3: mtry=4; ntree=274; nodesize=45
## [Tune-y] 3: acc.test.mean=0.999; time: 0.0 min; memory: 89Mb use, 173Mb max
## [Tune-x] 4: mtry=2; ntree=202; nodesize=31
## [Tune-y] 4: acc.test.mean=0.998; time: 0.0 min; memory: 89Mb use, 173Mb max
## [Tune-x] 5: mtry=2; ntree=307; nodesize=14
## [Tune-y] 5: acc.test.mean=0.997; time: 0.0 min; memory: 89Mb use, 179Mb max
## [Tune-x] 6: mtry=1; ntree=55; nodesize=12
## [Tune-y] 6: acc.test.mean=0.971; time: 0.0 min; memory: 89Mb use, 180Mb max
## [Tune-x] 7: mtry=4; ntree=164; nodesize=32
## [Tune-y] 7: acc.test.mean=0.999; time: 0.0 min; memory: 89Mb use, 180Mb max
## [Tune-x] 8: mtry=1; ntree=219; nodesize=18
## [Tune-y] 8: acc.test.mean=0.977; time: 0.0 min; memory: 89Mb use, 180Mb max
## [Tune-x] 9: mtry=2; ntree=71; nodesize=30
## [Tune-y] 9: acc.test.mean=0.999; time: 0.0 min; memory: 89Mb use, 180Mb max
## [Tune-x] 10: mtry=5; ntree=278; nodesize=26
## [Tune-y] 10: acc.test.mean=0.999; time: 0.0 min; memory: 89Mb use, 180Mb max
## [Tune] Result: mtry=5; ntree=278; nodesize=26 : acc.test.mean=0.999
rf_tune$x
## $mtry
## [1] 5
## 
## $ntree
## [1] 278
## 
## $nodesize
## [1] 26
Above are the tuned parameters after hyper tuning.

Setting the tuned parameters to the learner parameters with the following code.

rf_hyper <- setHyperPars(learner =  logit_mush, par.vals = rf_tune$x)
Building the model using the train data set with the learner.

rf_Train <- train(learner = logit_mush, task = Train_Task)
Now let’s Predict the Test data.

rf_Test_pred <- predict(rf_Train, Test_Task)
Making a confusion matrix

getConfMatrix(rf_Test_pred)
##            predicted
## true        EDIBLE POISONOUS -SUM-
##   EDIBLE       881         0     0
##   POISONOUS      0       795     0
##   -SUM-          0         0     0
The reason this data set has 100% accuracy is because of the information gain that the top two significant variables have a total information gain of 1 and hence the entropy would be 0. Since entropy is 0 the variables are able to explain the variance in the data completely.

This line basically exports the final cleaned data into the current working directory in the system. Please refer this data if needed so. Note : The data in the Train and Test files may change everytime this code is run since the sample() function from line 642 will shuffle the total data everytime it is executed.

write_csv(Totaldata,"Totaldata.csv")
write_csv(Traindata,"Traindata.csv")
write_csv(Testdata,"Testdata.csv")
Note that the echo = FALSE parameter was added to the code chunk to prevent printing of the R code that generated the plot.
