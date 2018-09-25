# R script to convert a confusion matrix in to a nice graphical display.

library(ggplot2)

flag_file = 6
#my_counts = read.csv("/Users/mjensen1/Dropbox/Variational_Autoencoders/Pictures/LDA_Confusion01.csv", header = FALSE)

if(flag_file == 1){
	my_counts = read.csv("/Users/mjensen1/Dropbox/Variational_Autoencoders/Pictures/LDA_Confusion02.csv", header = FALSE)
	mytitle = paste("LDA extraction")
	x_lab = "LDA Topic"

}else if(flag_file == 2) {
	my_counts = read.csv("/Users/mjensen1/Dropbox/Variational_Autoencoders/Pictures/c_matrix20dim.csv", header = FALSE)
	mytitle = paste("K-means 20 dimension")
	x_lab = "K-means Class"

} else if (flag_file == 3){
	my_counts = read.csv("/Users/mjensen1/Dropbox/Variational_Autoencoders/Pictures/c_matrix50dim.csv", header = FALSE)
	mytitle = paste("K-means 50 dimension")
	x_lab = "K-means Class"
}else if (flag_file == 4){
		my_counts = read.csv("/Users/mjensen1/Dropbox/Variational_Autoencoders/Pictures/lda_20_topics.csv", header = FALSE)
	mytitle = paste("LDA extraction - 20 topics")
	x_lab = "LDA Topic"
	
	}else if (flag_file == 5){
		my_counts = read.csv("/Users/mjensen1/Dropbox/Variational_Autoencoders/Pictures/lda_012_10.csv", header = FALSE)
	mytitle = paste("LDA extraction for 0,1,2")
	x_lab = "LDA Topic"
	}else if (flag_file == 6){
		my_counts = read.csv("/Users/mjensen1/Dropbox/Variational_Autoencoders/Pictures/Cmatrix_012_3topic.csv", header = FALSE)
	mytitle = paste("LDA extraction for 0,1,2- 3 Topics")
	x_lab = "LDA Topic"
	}
	
	
	
	
LDA_class = c()
actual_class_all = c()

for(j in 1:(dim(my_counts)[1]) ){	
	actual_class_all  = c(actual_class_all,rep(j -1, times = sum(my_counts[j,])))
	for (k in 1:(dim(my_counts)[2])){
		LDA_class = c(LDA_class, rep(k-1, times = my_counts[j,k]))		
	}
	
}

LDA_fac = as.factor(LDA_class)

class_df = data.frame(truth = actual_class_all, LDA = LDA_class, LDAfac = LDA_fac)


LDA_lab =c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")

if(flag_file <= 5){
plot_setup = ggplot(class_df, aes(y = truth, x =LDA)) 
print(plot_setup+ geom_jitter(alpha = 0.1, colour = "black") + scale_x_continuous(x_lab,breaks=c(0:9), label = LDA_lab)+ scale_y_continuous("True Digit", breaks=c(0:9)) + ggtitle(mytitle) + theme(plot.title = element_text(hjust = 0.5)))
}


ggplot(class_df, aes(y = truth, x =LDA)) + geom_jitter(alpha = 0.1, colour = "black") 

if (flag_file == 6 ){
plot_setup = ggplot(class_df, aes(y = truth, x =LDA)) 
print(plot_setup + geom_jitter(alpha = 0.1, colour = "black") + scale_x_continuous(x_lab,breaks=c(0:2), label = LDA_lab[1:3])+ scale_y_continuous("True Digit", breaks=c(0:2)) + ggtitle(mytitle) + theme(plot.title = element_text(hjust = 0.5)))
	
}

dim(my_counts)
apply(my_counts, 1, which.max)

# # 
# actual_digit = list()

# actual_digit$"0" = c(16,   1,  52 ,  1 ,684 ,131,  22,   0 , 71 ,  2)
# actual_digit$"1" = c( 2 ,  1 ,  5 ,489 ,  0 , 13 ,  3 ,600 , 22 ,  0)
# actual_digit$"2" = c(327 ,  8 ,383 , 23 , 31 , 22  , 9  , 5 ,201 , 23)
# actual_digit$"3" =  c(156 , 15 , 10  , 0 ,  6 ,682 , 25 , 12 , 87 , 17)
# actual_digit$"4"=  c(10, 593 , 52  , 6 , 61 ,  2, 241 , 13 ,  1 ,  3)
# actual_digit$"5"=  c( 59 , 56,  13  , 0 , 14 ,415, 292 ,  5 , 36 ,  2)
 # actual_digit$"6" = c( 14 ,  1, 795  , 4 , 95 , 27 ,  4 ,  9 ,  9 ,  0)
# actual_digit$"7" =  c( 6 , 42 ,  7  ,31 , 18 ,  2 ,198 , 37 ,  6, 681)
# actual_digit$"8" = c(76 , 91 , 18  , 7 , 10 ,454 ,278 ,  0,  20 , 20)
# actual_digit$"9" =  c(8, 566 ,  7  , 2 , 31 , 22, 199 , 16 ,  3, 155)




print(paste("Accuracy = ",round(sum(diag(as.matrix(my_counts)))/sum(apply(my_counts, 1 ,sum)),4)))

