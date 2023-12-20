#' @title Outlier Detect
#'
#' @description Detects outliers in two columns of a tibble. The control and the
#' experimental. When assigned to a new object returns a tibble that has the
#' control variable in the first column (output_data[1)] and the experimental
#' variable in the second column (output_data[2]).
#'
#' @param data Data collected in tibble format that will be analyzed for
#'  outliers.
#' @param col_c The number of the column that contains the control variable
#' (as.numeric).
#' @param col_x The number of the column that contains the experimental variable
#'  (as.numeric) .
#' @param paired Is the data is paired or not (T/F).
#' @param REMOVE Outliers should be removed from the data (T/F).
#' @param plot_type What type of plot to be created "box" for independent data
#' or "QQ" for paired data.
#'
#' @return A graphical depiction of the data with or without outliers.
#' @export
#'
#' @examples output_data <- Outlier_Detect(data, col_c = 1, col_x = 2,
#' paired = F, REMOVE = F, plot_type = "box")
#' @import tibble dplyr ggplot2
Outlier_Detect <- function(data, col_c, col_x, paired, REMOVE, plot_type){
  if (!is_tibble(data)) {
    stop("Data must be a tibble.")
  }
  #Establishing function global variables

  ##Getting names of Columns
  Control_Name <- colnames(data[col_c])
  Experimental_Name <- colnames(data[col_x])

  ##Getting data
  Control <- data[[col_c]]
  Experimental <- data[[col_x]]

  ##Calculate each column's QR
  Q1_C   <- quantile(Control, 0.25)
  Q3_C   <- quantile(Control, 0.75)
  IQR_C  <- Q3_C - Q1_C
  lowB_C <- Q1_C - 1.5 * IQR_C
  upB_C  <- Q3_C + 1.5 * IQR_C

  Q1_E   <- quantile(Experimental, 0.25)
  Q3_E   <- quantile(Experimental, 0.75)
  IQR_E  <- Q3_E - Q1_E
  lowB_E <- Q1_E - 1.5 * IQR_E
  upB_E  <- Q3_E + 1.5 * IQR_E

  print(paste("The range for", Control_Name, "(lower bound-upper bound) is (",
              lowB_C, ") - (", upB_C, ").", sep=" "))
  print(paste("The range for", Experimental_Name, "(lower bound-upper bound) is (",
              lowB_E, ") - (", upB_E, ").", sep=" "))

  print("Anything outside of the lower or upper bounds is considered an outlier.")

  #Taking Out outlier portion
  if(paired==T){
    if(REMOVE==T){

      #Fill extra spaces with NA ##SHOULD NOT HAPPEN
      maxLength <- max(length(Control), length(Experimental))
      Control <- c(Control, rep(NA, maxLength-length(Control)))
      Experimental <- c(Experimental, rep(NA, maxLength-length(Experimental)))

      #Create combined tibble
      tibble_Combined <- tibble(Control, Experimental)


      #Make lists of outliers
      outliers_C <- as.list(tibble_Combined %>%
                              filter(tibble_Combined[1]    < lowB_C |
                                       tibble_Combined[1]  > upB_C))

      outliers_E <- (Experimental[Experimental<lowB_E | Experimental>upB_E])
      #cat("The outliers in", Control_Name, "dataset are:", outliers_C, sep=" ")
      #cat("The outliers in", Experimental_Name, "dataset are:", outliers_E, sep=" ")

      #Filter out tibbles
      tibble_Updated <- tibble_Combined %>%
        filter(tibble_Combined[1]  > lowB_C &
                 tibble_Combined[1]  < upB_C)
      tibble_Updated <- tibble_Combined %>%
        filter(tibble_Combined[2]  > lowB_E &
                 tibble_Combined[2]  < upB_E)
      #Rename columns
      colnames(tibble_Updated)[1] <- Control_Name
      colnames(tibble_Updated)[2] <- Experimental_Name

      #Update data to filtered data
      data <- tibble_Updated

    }else if(REMOVE==F){
      #Create combined tibble
      data <- tibble(Control, Experimental)
      #Rename columns
      colnames(data)[1] <- Control_Name
      colnames(data)[2] <- Experimental_Name

    }else{ #Wrong input
      print("Error in input")
    }
  }else if (paired==F){
    if(REMOVE==T){
      #Make lists of outliers
      outliers_C <- (Control[Control<lowB_C | Control>upB_C])
      outliers_E <- (Experimental[Experimental<lowB_E | Experimental>upB_E])
      cat("The outliers in", Control_Name, "dataset are:", outliers_C, ".","\n", sep=" ")
      cat("The outliers in", Experimental_Name, "dataset are:", outliers_E, ".","\n",  sep=" ")

      #Filter out the suspected outliers
      filtered_C <- Control[Control>lowB_C           & Control<upB_C]
      filtered_E <- Experimental[Experimental>lowB_E & Experimental<upB_E]

      #Fill extra spaces with NA
      maxLength <- max(length(filtered_C), length(filtered_E))
      filtered_C <- c(filtered_C, rep(NA, maxLength-length(filtered_C)))
      filtered_E <- c(filtered_E, rep(NA, maxLength-length(filtered_E)))

      #Combine data into a tibble and return
      data_updated <- tibble(filtered_C, filtered_E)

      colnames(data_updated)[1] <- Control_Name
      colnames(data_updated)[2] <- Experimental_Name

      data <- data_updated
    }else if(REMOVE==F){
      data <- tibble(Control, Experimental)
      colnames(data)[1] <- Control_Name
      colnames(data)[2] <- Experimental_Name
    }
  }
  print(data)
  if(plot_type=="QQ"){
    qqplot( x    = data[[1]],
            y    = data[[2]],
            main = "Normal Q-Q Plot",
            xlab = Control_Name,
            ylab = Experimental_Name)
    return(data)
  }else if(plot_type=="box"){
    boxplot(data, col="darkred")
    title("Boxplot")
    return(data)
  }else{
    print("Invalid plot type given.")
    return(data)
  }
}
#' @title Check Normality
#'
#' @description Conducts a Shapiro-Wilk test for the control and experimental
#' columns as well as generating either a histogram or QQ plot so that students
#' can visualize their data distributions and quantify normality.
#'
#' @param data Data collected in tibble format that will be visualized and
#' tested for normality
#' @param plot_type Plot used to visualize data "histogram" or "QQ".
#'
#' @return Either a histogram or QQ plot and a Shapiro-Wilk test for normality.
#' @export
#'
#' @examples Check_Normal(data, "histogram")
#' @import tibble ggplot2
Check_Normal <- function(data, plot_type){
  if (!is_tibble(data)) {
    stop("Data must be a tibble.")
  }
  # Establish parameters
  col_c <- data[[1]]
  col_x <- data[[2]]

  name_col_c <- data[1]
  name_col_x <- data[2]

  if(plot_type=="histogram"){print(ggplot(data, aes(col_c)) +
                                     geom_histogram(bins = round(sqrt(length(col_c))), color = "goldenrod", fill = "darkred") +
                                     ggtitle("Histogram of", names(name_col_c)) +
                                     labs(y = "Frequency", x = names(name_col_c)))

    print(ggplot(data, aes(col_x)) +
            geom_histogram(bins = round(sqrt(length(col_x))), color = "goldenrod", fill = "darkred") +
            ggtitle("Histogram of", names(name_col_x)) +
            labs(y = "Frequency", x = names(name_col_x)))
    cat("Two histograms were created. Use the arrows in the plot tab to\nswitch between the two.\n")

    # Shaprio-Wilk Normal Test

    norm_test <- shapiro.test(data[[1]])$p.value
    cat(paste("The shapiro test for normality was conducted in addition to plotting histograms. \nThe p-value is",
              if(norm_test<0.00005){
                norm_test <-  format(norm_test, scientific = T)
              } else if(norm_test>0.00005 | norm_test==0.00005){

                norm_test <- format(round(norm_test, 4))
              },
              "for", names(name_col_c[1]),"\n---\nSignifigance (\u03B1) is set at 0.05 for this test. \nAny value less than 0.05 indicates that the sample is not normally distributed, and should be tested as 'nonparametric'\nin Test_Data\n--"))

    norm_test <- shapiro.test(data[[2]])$p.value
    cat(paste("\nThe shapiro test for normality was conducted in addition to plotting histograms. \nThe p-value is",
              if(norm_test<0.00005){
                norm_test <-  format(norm_test, scientific = T)
              } else if(norm_test>0.00005 | norm_test==0.00005){

                norm_test <- format(round(norm_test, 4))
              },
              "for", names(name_col_x[1]),"\n---\nSignifigance (\u03B1) is set at 0.05 for this test. \nAny value less than 0.05 indicates that the sample is not normally distributed, and should be tested as 'nonparametric'\nin Test_Data\n"))

  }else if(plot_type=="QQ"){
    print(ggplot(data, aes(sample = data[[1]])) +
            geom_qq() +
            stat_qq_line() +
            ggtitle("QQ plot of", names(name_col_c)) +
            ylab(names(name_col_c)) +
            xlab("Theoritical Quantities"))

    print(ggplot(data, aes(sample = data[[2]])) +
            geom_qq() +
            stat_qq_line() +
            ggtitle("QQ plot of", names(name_col_x)) +
            ylab(names(name_col_x)) +
            xlab("Theoritical Quantities"))

    cat("Two QQ plots were created. Use the arrows in the plot tab to\nswitch between the two.\n")

    # Shapiro Wilk Test

    norm_test <- shapiro.test(data[[1]])$p.value
    cat(paste("The shapiro test for normality was conducted in addition to making QQ plots. \nThe p-value is",
              if(norm_test<0.00005){
                norm_test <-  format(norm_test, scientific = T)
              } else if(norm_test>0.00005 | norm_test==0.00005){

                norm_test <- format(round(norm_test, 4))
              },
              "for", names(name_col_c[1]),"\n---\nSignifigance (\u03B1) is set at 0.05 for this test. \nAny value less than 0.05 indicates that the sample is not normally distributed and should be tested as 'nonparametric'\nin Test_Data\n--"))

    norm_test <- shapiro.test(data[[2]])$p.value
    cat(paste("\nThe shapiro test for normality was conducted in addition to making QQ plots. \nThe p-value is",
              if(norm_test<0.00005){
                norm_test <-  format(norm_test, scientific = T)
              } else if(norm_test>0.00005 | norm_test==0.00005){

                norm_test <- format(round(norm_test, 4))
              },
              "for", names(name_col_x[1]),"\n---\nSignifigance (\u03B1) is set at 0.05 for this test. \nAny value less than 0.05 indicates that the sample is not normally distributed and should be tested as 'nonparametric'\nin Test_Data\n"))

  }

}
#' @title Test Data
#'
#' @description Conducts various tests based off of user input. The test may be
#' parametric or nonparametric. It also takes into account if the variables are
#' paired, the alternative hypothesis "less", "two.sided", or "greater", and the
#' difference in mu which is typically zero.
#'
#' @param data Data collected in tibble format that will be tested for
#' significance.
#' @param paired Is the data is paired or not (T/F).
#' @param test_level Either "parametric" or "nonparametric" determined when
#' using the Check_Normal function
#' @param input_alternative The sidedness of test used for the alternative
#'  hypothesis "less"/"two.sided"/"greater".
#' @param mu_given The null hypothesis of the given test. Most often the null
#' hypothesis is that the difference between means is 0.
#'
#' @return A p-value of a statistical test used based off the inputs into the
#' function.
#' @export
#'
#' @examples Test_Data(data, paired = F, test_level = "parametric",
#' input_alternative = "two.sided", mu_given = 0)
#' @import tibble stats
Test_Data <- function(data, paired, test_level, input_alternative, mu_given){
  if (!is_tibble(data)) {
    stop("Data must be a tibble.")
  }
  col_c <- data[[1]]
  col_x <- data[[2]]
  name_c<- colnames(data[1])
  name_x <- colnames(data[2])


  if(paired==T){
    n<- length(col_c)
  }else{
    n<- max(length(col_c), length(col_x))
  }

  if(test_level=="parametric"){

    if(paired==T){

      t.test(col_c, col_x,
             alternative=input_alternative,
             paired=T, mu=mu_given)
    }else if (paired==F){

      t.test(col_c, col_x,
             alternative=input_alternative,
             paired=F, mu=mu_given)
    }

  }else if(test_level=="nonparametric"){

    if(paired==T & n>50){
      wilcox.test(col_c, col_x,
                  alternative=input_alternative,
                  paired=T, mu=mu_given)

    }else if (paired==F & n>50){
      wilcox.test(col_c, col_x,
                  alternative=input_alternative,
                  paired=F, mu=mu_given)

    }else if(paired==T & n<50){
      wilcox.test(col_c, col_x,
                  alternative=input_alternative,
                  paired=T, mu=mu_given, exact=F)

    }else if(paired==F & n<50){
      wilcox.test(col_c, col_x,
                  alternative=input_alternative,
                  paired=F, mu=mu_given, exact=F)
    }

  }

}
#' @title Create Figure
#'
#' @description  A standardized figure that has no set theme. theme_set() from
#' ggplot2 can be used in order to customize the figure. theme_set(minimal) is a
#' common theme.
#'
#' @param data Data collected in tibble format that will be used to create a
#' figure.
#' @param errorbars Error bars for control and experimental variable with
#' options of standard deviation "sd" or standard error "se"
#' @param test_level Either "parametric" or "nonparametric" determined when
#' using the Check_Normal function
#' @param yaxis_label A "string" that labels the y-axis. If no y-axis label is
#' desired then enter NULL
#' @param group A "string" that labels the control and experimental variables.
#' If the experiment tested different antibiotic treatments the the group would
#'  be "Antibiotic Treatment". If no group label is desired then enter NULL.
#'
#' @return A figure with no theme that shows averages for parametric data or
#' medians for nonparametric data with error bars of sd or se.
#' @export
#'
#' @examples Create_Figure(data, errorbars = "se", test_level = "parametric",
#' yaxis_label = "Average of an Outcome",
#' group = "Commonality of the Two Variables")
#' @import tibble dplyr ggplot2 graphics
Create_Figure <- function(data, errorbars, test_level, yaxis_label, group){
  if (!is_tibble(data)) {
    stop("Data must be a tibble.")
  }
  # Create common variables
  yaxis_lab <- yaxis_label
  xaxis_lab <- group

  name_col_c <- names(data[1])
  col_c <- data[1]
  avg_col_c <- col_c %>% summarise(avg = round(mean(col_c[[1]]), 3))
  median_col_c <- col_c %>% summarise(median = median(col_c[[1]]))
  sd_col_c <- col_c %>% summarise(sd = round(sd(col_c[[1]]), 3))
  se_col_c <- col_c %>% summarise(se = round((sd(col_c[[1]])/sqrt(length(col_c[[1]]))), 3))

  name_col_x <- names(data[2])
  col_x <- data[2]
  avg_col_x <- col_x %>% summarise(avg = round(mean(col_x[[1]]), 3))
  median_col_x <- col_x %>% summarise(median = median(col_x[[1]]))
  sd_col_x <- col_x %>% summarise(sd = round(sd(col_x[[1]]), 3))
  se_col_x <- col_x %>% summarise(se = round((sd(col_x[[1]])/sqrt(length(col_x[[1]]))), 3))

  avg_sd_pos_errorbar <- tibble(avg_sd_pos = c(avg_col_c[[1]] + sd_col_c$sd, avg_col_x + sd_col_x$sd))
  avg_sd_neg_errorbar <- tibble(avg_sd_neg = c(avg_col_c[[1]] - sd_col_c$sd, avg_col_x - sd_col_x$sd))

  avg_se_pos_errorbar <- tibble(avg_se_pos = c(avg_col_c[[1]] + se_col_c$se, avg_col_x + se_col_c$se))
  avg_se_neg_errorbar <- tibble(avg_se_neg = c(avg_col_c[[1]] - se_col_x$se, avg_col_x - se_col_x$se))

  med_sd_pos_errorbar <- tibble(med_sd_pos = c(median_col_c[[1]] + sd_col_c$sd, median_col_x + sd_col_x$sd))
  med_sd_neg_errorbar <- tibble(med_sd_neg = c(median_col_c[[1]] - sd_col_c$sd, median_col_x - sd_col_x$sd))

  med_se_pos_errorbar <- tibble(med_se_pos = c(median_col_c[[1]] + se_col_c$se, median_col_x + se_col_c$se))
  med_se_neg_errorbar <- tibble(med_se_neg = c(median_col_c[[1]] - se_col_x$se, median_col_x - se_col_x$se))

  avg <- tibble(avg = c(avg_col_c, avg_col_x))
  median <- tibble(median = c(median_col_c, median_col_x))
  var_names <- tibble(var_names = c(name_col_c, name_col_x))
  # Create tibble of all common variables
  figure <- tibble(avg = as.numeric(unlist(avg)), median = as.numeric(unlist(median)), var_names, avg_sd_pos = as.numeric(unlist(avg_sd_pos_errorbar)),
                   avg_sd_neg = as.numeric(unlist(avg_sd_neg_errorbar)), avg_se_pos = as.numeric(unlist(avg_se_pos_errorbar)), avg_se_neg = as.numeric(unlist(avg_se_neg_errorbar)),
                   med_sd_pos = as.numeric(unlist(med_sd_pos_errorbar)), med_sd_neg = as.numeric(unlist(med_sd_neg_errorbar)), med_se_pos = as.numeric(unlist(med_se_pos_errorbar)),
                   med_se_neg = as.numeric(unlist(med_se_neg_errorbar)))
  if(test_level=="parametric" & errorbars=="sd"){
    figure <- figure %>% select(1, 5:3) # Filter undesired columns
  print(figure %>% ggplot(aes(x = var_names, y = figure[[1]])) +
        geom_col(fill = "darkred", width = 0.5) +
        geom_errorbar(aes(x = var_names, ymin = figure[[2]], ymax = figure[[3]]), width = 0.1) +
        labs(y = yaxis_lab, x = xaxis_lab) +
        scale_y_continuous(breaks = seq(round(1.05 * min(figure[[2]]), 0), round(1.1 * max(figure[[3]]), 2))))

  }else if(test_level=="parametric" & errorbars=="se"){
    figure <- figure %>% select(1, 7:6, 3) # Filter undesired columns
    print(figure %>% ggplot(aes(x= var_names, y= figure[[1]])) +
            geom_col(fill = "darkred", width = 0.5) +
            geom_errorbar(aes(x=var_names, ymin = figure[[2]], ymax = figure[[3]]), width = 0.1) +
            labs(y = yaxis_lab, x=xaxis_lab) +
            scale_y_continuous(breaks = seq(round(1.05*min(figure[[2]]), 0), round(1.1*(max(figure[[3]])), 2))))
  }else if (test_level=="nonparametric" & errorbars=="sd"){
    figure <- figure %>% select(2, 9:8, 3) # Filter undesired columns
    print(figure %>% ggplot(aes(x= var_names, y= figure[[1]])) +
            geom_col(fill = "darkred", width = 0.5) +
            geom_errorbar(aes(x=var_names, ymin = figure[[2]], ymax = figure[[3]]), width = 0.1) +
            labs(y = yaxis_lab, x=xaxis_lab) +
            scale_y_continuous(breaks = seq(round(1.05*min(figure[[2]]), 0), round(1.1*(max(figure[[3]])), 2))))
  }else if(test_level=="nonparametric" & errorbars=="se"){
    figure <- figure %>% select(2, 11:10, 3) # Filter undesired columns
    print(figure %>% ggplot(aes(x= var_names, y= figure[[1]])) +
            geom_col(fill = "darkred", width = 0.5) +
            geom_errorbar(aes(x=var_names, ymin = figure[[2]], ymax = figure[[3]]), width = 0.1) +
            labs(y = yaxis_lab, x=xaxis_lab) +
            scale_y_continuous(breaks = seq(round(1.05*min(figure[[2]]), 0), round(1.1*(max(figure[[3]])), 2))))
  }
}
