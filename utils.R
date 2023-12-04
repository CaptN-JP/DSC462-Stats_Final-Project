# Import required libraries
library(ggplot2)
library(moments)
library(RColorBrewer)

#' Expand Nested Categorical Data
#'
#' This function deconstructs a column containing nested categorical data, extracts individual values, 
#' calculates their frequencies, and returns a dataframe with the most frequent values and their frequencies.
#'
#' @param data A dataframe containing the data to be expanded.
#' @param cat_col Specifies the column from the dataframe where nested categorical data is stored.
#' @param n The number of unique values with maximum frequency to include in the output dataframe.
#'
#' @return A dataframe with two columns: 'cat_col' containing the most frequent values and 
#' 'frequency' containing their corresponding frequencies.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' expand_nested_data(my_data, "nested_categorical_column", n = 10)
#' }
#'
#' @importFrom base strsplit trimws unlist table head sort data.frame as.numeric
#' @export
expand_nested_data <- function(data, cat_col, n=20) {
  
  # Split and trim the values
  cat_vals_list <- strsplit(trimws(data[[cat_col]]), ",")
  
  # Flatten the list of values
  all_vals <- unlist(cat_vals_list)
  
  # Calculate frequencies and sort
  freq_table <- table(all_vals)
  sorted_vals <- names(head(sort(freq_table, decreasing = TRUE), n))
  
  # Filter the data frame to include only the top n values
  exp_df <- data.frame(cat_col = sorted_vals, frequency = as.numeric(freq_table[sorted_vals]), stringsAsFactors = FALSE)
  
  return(exp_df)
}


#' Plot Histogram of Input Data
#'
#' This function generates and displays a histogram based on the specified column from the input dataframe.
#' The histogram can be created for numeric, nested categorical, or categorical data types.
#'
#' @param data A dataframe containing the data to be plotted.
#' @param column_name The column from the dataframe to be used for plotting the histogram.
#' @param x_label Label for the x-axis of the histogram.
#' @param y_label Label for the y-axis of the histogram.
#' @param title Title of the histogram.
#' @param type Type of column data to be plotted. Values can be:
#'              0 - Numeric Data
#'              1 - Nested Categorical Data
#'              2 - Categorical Data
#'
#' @return This function does not explicitly return a value. It prints and saves the histogram plot.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' plot_histogram(my_data, "numeric_column", x_label = "Values", y_label = "Frequency", title = "Numeric Histogram", type = 0)
#' }
#'
#' @import ggplot2
#' @importFrom ggplot2 aes geom_histogram scale_x_continuous theme axis.text.x element_text
#' @importFrom ggplot2 geom_bar expand_nested_data stat identity position dodge
#' @importFrom stats ceiling
#' @importFrom base dir.create ggsave
#' @export
plot_histogram <- function(data, column_name, x_label="Data", y_label="Frequency", title="Histogram", type=1) {
  
  # Extract the specified column from the data frame
  column_data <- data[[column_name]]
  
  if (type == 0) {
    # For Numeric Data
    
    # Sturges' formula : No. of bins = [log2(n)] + 1
    length_of_seasons_data <- length(column_data)
    num_of_bins <- ceiling(log2(length_of_seasons_data)) + 1
    bin_width <- (max(column_data) - min(column_data)) / num_of_bins # Set the bin width
    
    # Plot the histogram
    histplot <- ggplot(data, aes(x = column_data)) + 
      geom_histogram(binwidth = bin_width, colour = "black", fill = "white", boundary = 0) +
      labs(title = title, x = x_label, y = y_label) +
      scale_x_continuous(breaks = seq(min(column_data), max(column_data), by = bin_width),
                         labels = function(x) round(x, 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
    
    # Adding Mean
    mean_value <- mean(data[[column_name]])
    histplot <- histplot + geom_vline(aes(xintercept=mean_value) ,colour="red", show.legend = F) + annotate("text", x=mean_value, y= 0.9 * max(table(column_data)), 
                label=substitute(paste(bar(x),"=",m), list(m=sprintf("%.02f",mean_value))), colour="red")
    
    # Adding Median  
    median_value <- median(data[[column_name]])
#    max_freq <- max(table(column_data))
#    yv <- (0.7 * max_freq)
#    cat("Median: ", median_value, "\n")
#    cat("Y-value: ", str(yv), "\n")
    
    histplot <- histplot + geom_vline(aes(xintercept = median_value), colour = "blue", show.legend = FALSE) #+
#      annotate("text", x = median_value, y = 5000, 
#              label = substitute(paste(tilde(x), "=", m)), 
#               list(m = sprintf("%.02f", median_value)), colour = "blue")
  }
  
  else if(type == 1) {
    # For Nested Categorical Data
    
    # Modify the data
    modified_data = expand_nested_data(data, column_name, 20)
    
    histplot <- ggplot(modified_data, aes(x = cat_col, y = frequency)) + 
    geom_bar(stat = "identity", position = "dodge", color = "black", fill = "white") +
    labs(title = title, x = x_label, y = y_label) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  }
  
  else {
    # For Categorical Data
    histplot <- ggplot(data, aes(x = column_data)) + 
      geom_bar(stat="count", colour = "black", fill = "white") +
      labs(title = title, x = x_label, y = y_label)
  }
  
  # Check if "histograms/" directory exists, create if not
  if (!dir.exists("histograms")) {
    dir.create("histograms")
  }
  
  # Print and Save the plot as an image file
  print(histplot)
  file_name <- paste("histograms/", title, ".png")
  ggsave(file_name, plot = histplot, width = 8, height = 6, units = "in", dpi = 300)

}


#' Display summary statistics of specified columns
#'
#' This function takes a vector of column names and displays summary statistics for each column. 
#' For numeric columns, it shows a numeric summary, including the interquartile range (IQR), variance, 
#' coefficient of variance, skewness, and a trimmed mean. For non-numeric columns, it displays the 
#' top and bottom values along with their frequencies.
#'
#' @param cols A character vector containing column names for which statistics will be displayed.
#'
#' @return This function does not explicitly return a value. It prints the summary statistics for each specified column.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' col_stats(c("numeric_column", "categorical_column"))
#' }
#'
#' @importFrom stats summary var sd mean skewness
#' @importFrom base table head tail cbind
#' @export
col_stats <- function(cols) { 
  
  for (col in cols) {
    cat(paste("Table for ", col, "\n"))
    col_data <- (tmdb_data[[col]])
    
    # Check if the column is numeric
    if (is.numeric(tmdb_data[[col]])) {
      cat("\nNumeric Summary:\n")
      numeric_summary <- summary(col_data)
      print(numeric_summary)
      
      # Calculate the IQR, variance, Coefficient of variance, skewness
      iqr <- (numeric_summary["3rd Qu."] - numeric_summary["1st Qu."])
      variance<- var(col_data)
      cov <- (sd(col_data)/mean(col_data))
      sk <- skewness(col_data)
      trimmed_mean <- mean(col_data, trim=0.1)
      
      cat(paste("\nCoefficient of Variance=", cov, "\nVariance=", variance))
      cat(paste("\nSkewness=", sk, "\nIQR=", iqr, "\n10% Trimmed Mean", trimmed_mean))
      
    } else {
      tbl <- table(col_data)
      
      # if length of tbl is greater than 10
      
      # else display the full frequency table
      
      cat("\nTop 5 values with serial number:\n")
      top_values <- head(tbl, 5)
      print(cbind(Value = top_values))
      
      cat("\nBottom 5 values with serial number:\n")
      bottom_values <- tail(tbl, 5)
      print(cbind(Value = bottom_values))
    }
    cat("\n____________________-------------_______________________\n")
  }
}


#' Display a scatter plot
#'
#' This function creates and displays a scatter plot using ggplot2.
#'
#' @param data A data frame containing the columns specified in x_col and y_col.
#' @param x_col The column name for the x-axis.
#' @param y_col The column name for the y-axis.
#' @param equal_aspect Logical indicating whether to maintain equal aspect ratio.
#' @param xlim A numeric vector of length 2 specifying the x-axis limits.
#' @param ylim A numeric vector of length 2 specifying the y-axis limits.
#' @param x_label Label for the x-axis.
#' @param y_label Label for the y-axis.
#' @param title Title for the scatter plot.
#'
#' @return A ggplot2 scatter plot object is printed to the console.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' display_scatter_plot(my_data, "column_x", "column_y", equal_aspect = TRUE, xlim = c(0, 10), ylim = c(0, 20))
#' }
#'
#' @import ggplot2
#' @importFrom ggplot2 aes_string geom_point labs coord_fixed coord_cartesian
#' @export
display_scatter_plot <- function(data, x_col, y_col, equal_aspect = FALSE, xlim = NULL, ylim = NULL, x_label = "X Column data", y_label = "Y Column data", title = "Scatter Plot") {
  
  # Check if the specified columns exist in the data frame
  if (!all(c(x_col, y_col) %in% names(data))) {
    stop("Specified columns do not exist in the data frame.")
  }
  
  # Check data types of the specified columns
  if (!is.numeric(data[[x_col]]) || !is.numeric(data[[y_col]])) {
    stop("Both x_col and y_col must be numeric columns.")
  }
  
  # Remove rows with missing values in either of the columns
  data <- na.omit(data, cols = c(x_col, y_col))
  
  scatter_plot <- ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_point() +
    labs(x = x_label, y = y_label, title = title)
  
  # Apply equal aspect ratio if specified
  if (equal_aspect) {
    scatter_plot <- scatter_plot + coord_fixed(ratio = 1)
  }
  
  # Set custom xlim and ylim if specified
  scatter_plot <- scatter_plot + coord_cartesian(xlim = xlim, ylim = ylim)
  
  
  # Check if "scatter_plots/" directory exists, create if not
  if (!dir.exists("scatter_plots")) {
    dir.create("scatter_plots")
  }
  
  # Print and Save the plot as an image file
  print(scatter_plot)
  file_name <- paste("scatter_plots/", title, ".png")
  ggsave(file_name, plot = scatter_plot, width = 8, height = 6, units = "in", dpi = 300)
}


#' Process episode data in a data frame by updating "episode_run_time" values and removing rows with missing values.
#'
#' This function iterates through the "episode_run_time" column of a data frame and performs
#' the following steps:
#'   1. Remove rows with missing values in "episode_run_time".
#'   2. Update values outside the specified range directly.
#'   3. Update values by dividing by "number_of_episodes" where applicable.
#'   4. Update values by dividing by 60 where applicable.
#'   5. Assign a default value of -1 for values that don't meet the conditions.
#'   6. Remove rows with the default value.
#'   7. Discard entire rows for values that don't meet the conditions.
#'
#' The number of discarded values is printed.
#'
#' @param df A data frame containing the episode data.
#' @param min_lim The minimum limit for the "episode_run_time" values.
#' @param max_lim The maximum limit for the "episode_run_time" values.
#'
#' @return A data frame with updated "episode_run_time" values and missing values removed.
#'
#' @examples
#' df <- data.frame(episode_run_time = c(60, 120, 180, NA), number_of_episodes = c(10, 5, 8, 3))
#' updated_df <- process_episode_data(df, 10, 90)
#' @export
process_episode_data <- function(df, min_lim, max_lim) {
  discarded_count <- 0
  default_value <- -1
  
  # Remove rows with missing values in "episode_run_time"
  df <- df[complete.cases(df$episode_run_time), ]
  
  # Update values directly outside the specified range
  df$episode_run_time <- ifelse(df$episode_run_time < min_lim | df$episode_run_time > max_lim, default_value, df$episode_run_time)
  
  # Remove rows with the default value
  df <- df[df$episode_run_time != default_value, ]
  
  # Update values by dividing by "number_of_episodes" where applicable
  update_indices <- with(df, episode_run_time < min_lim | episode_run_time > max_lim)
  valid_indices <- complete.cases(df$number_of_episodes[update_indices])
  df$episode_run_time[update_indices] <- ifelse(valid_indices, df$episode_run_time[update_indices] / df$number_of_episodes[update_indices], default_value)
  
  # Remove rows with the default value
  df <- df[df$episode_run_time != default_value, ]
  
  # Update values by dividing by 60 where applicable
  update_indices <- with(df, episode_run_time < min_lim | episode_run_time > max_lim)
  df$episode_run_time[update_indices] <- ifelse(update_indices, df$episode_run_time[update_indices] / 60, default_value)
  
  # Discard entire rows for values that don't meet the conditions
  df <- df[df$episode_run_time != default_value, ]
  discarded_count <- nrow(df) - sum(complete.cases(df$episode_run_time))
  
  cat("Number of discarded values:", discarded_count, "\n")
  return(df)
}


#' Compare Box Plot
#'
#' Generate a side-by-side box plot for comparing the distribution of a numeric column
#' (\code{y_col}) across different categories in a factor column (\code{fill_column}).
#' Optionally, you can specify the categories to include, set a custom y-axis limit,
#' and customize the y-axis label and plot title.
#'
#' @param data A data frame containing the relevant columns.
#' @param y_col The name of the numeric column to be plotted on the y-axis.
#' @param fill_column The name of the factor column defining the categories for comparison.
#' @param categories A vector containing the specific categories to include in the plot. 
#'                   If \code{NULL}, the top categories from \code{fill_column} will be used.
#' @param ylim A numeric vector of length 2 specifying the y-axis limits. 
#'             Default is \code{NULL}, resulting in automatic scaling.
#' @param y_label The label for the y-axis. Default is "Y Column data".
#' @param title The title for the plot. Default is "Box Plot".
#'
#' @return A box plot visualizing the distribution of \code{y_col} across categories.
#'
#' @examples
#' # Example usage:
#' compare_box_plot(data = your_data_frame, y_col = "popularity", fill_column = "type",
#'                  categories = c("Scripted", "MiniSeries"), ylim = c(0, 10),
#'                  y_label = "Popularity", title = "Comparison of Popularity")
#'
#' @export
compare_box_plot <- function(data, y_col, fill_column, categories = NULL, ylim = NULL, 
                             y_label = "Y Column data", title = "Box Plot") {
  
  # Check if the specified columns exist in the data frame
  stopifnot(y_col %in% names(data) && fill_column %in% names(data))
  
  # Check data type of the specified columns
  stopifnot(is.numeric(data[[y_col]]))
  
  # Convert fill_column to factor
  data[[fill_column]] <- as.factor(data[[fill_column]])
  
  # Remove rows with missing values in the specified columns
  data <- na.omit(data, cols = c(y_col, fill_column))
  
  # Handle categories with fewer than three values
  if (!is.null(categories)) {
#    if (length(unique(categories)) < 3) {
#      stop("categories must contain at least three unique values.")
    
    if (length(categories) > 0 && length(categories) < 3) {
      # Update categories with dummy string values to make its length 3
      categories <- c(categories, rep("dummy", 3 - length(categories)))
      warning("Categories vector has been updated with dummy values to make its length 3.")
      }
    
    data <- data[data[[fill_column]] %in% categories, ]
  } else {
    # Select top n most frequent unique categorical values from fill_column
    n_top_categories <- 5  # You can adjust this value as needed
    top_categories <- names(sort(table(data[[fill_column]]), decreasing = TRUE))[1:n_top_categories]
    data <- data[data[[fill_column]] %in% top_categories, ]
    categories <- top_categories
  }
  
  # Define color palette
  color_palette <- setNames(brewer.pal(length(categories), "Set3"), categories)
  
  # Create the box plot
  box_plot <- ggplot(data, aes_string(x = fill_column, y = y_col, fill = fill_column)) +
    geom_boxplot(position = "dodge") +
    labs(x = fill_column, y = y_label, title = title) +
    scale_fill_manual(values = color_palette) +
    theme_minimal()
  
  # Set custom ylim if specified
  if (!is.null(ylim)) {
    box_plot <- box_plot + coord_cartesian(ylim = ylim)
  }
  
  print(box_plot)
  file_name <- paste("box_plots/", gsub(" ", "_", title), ".png", sep="")
  ggsave(file_name, plot = box_plot, width = 8, height = 6, units = "in", dpi = 300)
}