# Expedia_Hotel_Recommendor
EECS 6893 Big Data Analytics Group 201612-39

There are three R files and one outcome csv file in this repository.

1) weights estimation.R
In this file, we estimate the variables' weights by applying random forest algorithm. And we can save the image (data enviroment and weights value) for later use, because the file reading process can take a while. 

2) run.501_1000.R
This is the file we will run over SparkR. When we get into SparkR interface, fist thing to do is to load the image that we saved in previous step, and source this run.501_1000.R file. 

501_1000 stands for user 500 to user 1000, which is a subset of our aim user. The running time for this file should take about 48 hours over SparkR platform. And it will returns a outcome matrix with size of 500*100, each row stands for 100 sorted clusters recommended for a single user. 

3) data.501_1000
This is csv output data file which is generated by the previous step. The first column stands for the user id, and the rest part is a 500*100 matrix. We will use this for model evaluation. 

4) accuracy_check.R
This code provides the leave-one-out cross validation evaluation over the training set. 
