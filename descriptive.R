source("./utils.R")


# Import the dataset
tmdb_data <- read.csv("TMDB_tv_dataset_v3.csv", header = TRUE)
summary(tmdb_data)


### PLOTTING HISTOGRAMS(Numeric) AND BAR CHARTS(Categorical) ###

#  NUMBER OF SEASONS
plot_histogram(
  data = tmdb_data, 
  column_name = "number_of_seasons", 
  x_label = "Range of Season Count",
  y_label = "Number of TV Series",
  title = "Histogram of TV Series Season Count",
  type = 0)

# NUMBER OF EPISODES
plot_histogram(
  data = tmdb_data, 
  column_name = "number_of_episodes", 
  x_label = "Range of Episode Count",
  y_label = "Number of TV Series",
  title = "Histogram of TV Series Episode Count",
  type = 0)

# VOTE COUNT
plot_histogram(
  data = tmdb_data, 
  column_name = "vote_count", 
  x_label = "Vote Count",
  y_label = "Number of TV Series",
  title = "Histogram of TV Series Vote Count",
  type = 0)

# VOTE AVERAGE
plot_histogram(
  data = tmdb_data, 
  column_name = "vote_average", 
  x_label = "Vote Average",
  y_label = "Number of TV Series",
  title = "Histogram of TV Series Vote Average",
  type = 0)

# EPISODE RUN TIME
plot_histogram(
  data = tmdb_data, 
  column_name = "episode_run_time", 
  x_label = "Episode Run Time (in minutes)",
  y_label = "Number of TV Series",
  title = "Histogram of Runtime of TV Series Episodes",
  type = 0)

print(process_episode_data(tmdb_data, 10, 90))
plot_histogram(
  data = tmdb_data, 
  column_name = "episode_run_time", 
  x_label = "Episode Run Time (in minutes)",
  y_label = "Number of TV Series",
  title = "Histogram of Corrected Runtime of TV Series Episodes",
  type = 0)

# PRODUCTION COMPANIES
plot_histogram(
  data = tmdb_data, 
  column_name = "production_companies", 
  x_label = "Production Company",
  y_label = "Number of TV Series",
  title = "Production Companies TV Series Portfolio Count",
  type = 1)

# GENRES
plot_histogram(
  data = tmdb_data, 
  column_name = "genres", 
  x_label = "Genre",
  y_label = "Number of TV Series",
  title = "Genres of TV Series",
  type = 1)

# LANGUAGES
plot_histogram(
  data = tmdb_data, 
  column_name = "languages", 
  x_label = "Language",
  y_label = "Number of TV Series",
  title = "Languages of TV Series",
  type = 1)

# NETWORKS
plot_histogram(
  data = tmdb_data, 
  column_name = "networks", 
  x_label = "Network",
  y_label = "Number of TV Series",
  title = "Dominant Networks of TV Series",
  type = 1)

# ORIGIN COUNTRY
plot_histogram(
  data = tmdb_data, 
  column_name = "origin_country", 
  x_label = "Origin Country",
  y_label = "Number of TV Series",
  title = "TV Series Origin Countries",
  type = 1)

# SPOKEN LANGUAGE
plot_histogram(
  data = tmdb_data, 
  column_name = "spoken_languages", 
  x_label = "Spoken Language",
  y_label = "Number of TV Series",
  title = "Spoken Languages of TV Series",
  type = 1)

# PRODUCTION COUNTRIES
plot_histogram(
  data = tmdb_data, 
  column_name = "production_countries", 
  x_label = "Production Country",
  y_label = "Number of TV Series",
  title = "TV Series Production Countries",
  type = 1)

# ORIGINAL LANGUAGE
plot_histogram(
  data = tmdb_data, 
  column_name = "original_language", 
  x_label = "Language",
  y_label = "Number of TV Series",
  title = "Original Language(ISO 639-1 Code) of TV Series",
  type = 2)

# STATUS
plot_histogram(
  data = tmdb_data, 
  column_name = "status", 
  x_label = "Status",
  y_label = "Number of TV Series",
  title = "Status of TV Series",
  type = 2)

# ADULT
plot_histogram(
  data = tmdb_data, 
  column_name = "adult", 
  x_label = "Adult Status",
  y_label = "Number of TV Series",
  title = "Adult Status of TV Series",
  type = 2)

# IN PRODUCTION
plot_histogram(
  data = tmdb_data, 
  column_name = "in_production", 
  x_label = "Production Status",
  y_label = "Number of TV Series",
  title = "Production Status of TV Series",
  type = 2)

# POPULARITY
plot_histogram(
  data = tmdb_data, 
  column_name = "popularity", 
  x_label = "Popularity",
  y_label = "Number of TV Series",
  title = "Popularity of TV Series",
  type = 2)

# TYPE
plot_histogram(
  data = tmdb_data, 
  column_name = "type", 
  x_label = "Series Type",
  y_label = "Number of TV Series",
  title = "Series Type of TV Series",
  type = 2)


### Summary Statistics ###

# Relevant columns
cols <- c("number_of_seasons", "number_of_episodes", "vote_count", "vote_average", 
          "episode_run_time", "production_companies", "genres", "languages", "networks", 
          "origin_country", "spoken_languages", "production_countries", "original_language", 
          "status", "adult", "in_production", "popularity", "type")

col_stats(cols)


### SCATTER PLOTS ###

#----> Vote average vs Popularity <----
df1 <- tmdb_data[tmdb_data$type == "Scripted", ] # Can also use: df <- subset(df, type == "Scripted")
display_scatter_plot(
  data = df1, 
  x_col = "popularity", 
  y_col = "vote_average", 
  x_label = "Popularity", 
  y_label = "Average Vote", 
  title = "Scatter plot of average votes of a scripted TV series with popularity"
  )
# Trimming all data points with popularity greater than 10 
display_scatter_plot(
  data = df1, 
  x_col = "popularity", 
  y_col = "vote_average", 
  x_label = "Popularity (restricted to max=10)", 
  y_label = "Average Vote", 
  title = "Scatter plot of average votes of a scripted TV series with popularity(cleaned)", 
  xlim = c(0, 10), 
  ylim = c(0, 10)
  )


#----> Vote average vs Popularity <----
