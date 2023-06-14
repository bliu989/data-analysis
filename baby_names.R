library(plotly)
library(dplyr)

# prepare data
df <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(df) <- c("state", "year", "top_girl", "top_boy",
                  "avg_len", "top_4_combo", "top_5_combo")

r <- 1 # row number

# cycle through each state and each year
for (st in state.abb){
  filename <- paste("lib/state/STATE.", st, ".TXT", sep = "")
  state_df <- read.table(filename, sep = ",")
  
  for (yr in 1910:2022){
    state_yr_df <- state_df[state_df$V3 == yr,]
    
    # get top girl and boy name and average name length
    top_g <- state_yr_df[1, "V4"]
    top_b <- state_yr_df[state_yr_df$V2 == "M",][1, "V4"]
    len <- sum(nchar(as.character(state_yr_df$V4)) * state_yr_df$V5)/
      sum(state_yr_df$V5)
    
    # count 4 and 5 letter combinations
    # the count takes into account how many babies have that combination in 
    # their name
    
    # counts every occurrence, so if a name has the same combination twice,
    # the code will count this as 2 occurrences 
    top_c <- c()
    for (l in 4:5) {
      combos <- c()
      c_length <- l
      
      for (i in 1:nrow(state_yr_df)) {
        name = tolower(as.character(state_yr_df$V4[i]))
        
        if (nchar(name) >= c_length) {    
          for (j in 1:(nchar(name)-c_length+1)) {
            combo = substr(name, j, j+c_length-1)
            
            if (combo %in% names(combos)) {
              combos[combo] <- combos[combo] + state_yr_df$V5[i]
            } else {
              combos[combo] <- state_yr_df$V5[i]
            }   
          }
        }
      }
      top_c <- append(top_c, names(combos)[which.max(combos)])
    }
    
    # add entry to data frame
    row_data <- list(st, yr, top_g, top_b, len)
    row_data <- append(row_data, top_c) 
    df[r, ] <- row_data
    
    r <- r + 1
  }
}

# get ranks of each feature
top_girl_ranks <- dense_rank(df$top_girl)
top_boy_ranks <- dense_rank(df$top_boy)
top_4_combo_ranks <- dense_rank(df$top_4_combo)
top_5_combo_ranks <- dense_rank(df$top_5_combo)

# average length is group by year to pick out top 10 for each year
df <- df %>% group_by(year) %>% 
  mutate(len_rank = dense_rank(avg_len))


# create map for most popular baby girl names
plot_ly(df,
        type = "choropleth",
        locationmode = "USA-states",
        frame = ~year) %>% 
  add_trace(locations = ~state,
            z = top_girl_ranks,
            zmin = 0,
            zmax = max(top_girl_ranks),
            colorscale = list(list(0, "#ecc19c"),
                              list(0.25, "#79a7d3"),
                              list(0.5, "#ffea04"),
                              list(0.75, "#a2d5c6"),
                              list(1, "red")),
            hoverinfo = "skip",
            showscale = FALSE) %>%
  add_trace(type = "scattergeo",
            locations = ~state, text = ~top_girl, frame = ~year,
            mode = "text",
            textfont = list(color=rgb(0,0,0), size = 10),
            hoverinfo = "text",
            hovertext = paste(df$state, ": ", df$top_girl, 
                              "\nYear : ", df$year),
            showlegend = FALSE) %>%
  animation_opts(250) %>%
  animation_slider(currentvalue = list(prefix = "Year: ")) %>% 
  layout(geo = list(
    scope = "usa",
    projection = list(type = "albers usa"),
    showlakes = FALSE),
    title = "Most popular baby girl names by state each year 1910-2022"
  ) 

# create map for most popular baby boy names
plot_ly(df,
        type = "choropleth",
        locationmode = "USA-states",
        frame = ~year) %>% 
  add_trace(locations = ~state,
            z = top_boy_ranks,
            zmin = 0,
            zmax = max(top_boy_ranks),
            colorscale = list(list(0, "#ecc19c"),
                              list(0.25, "#79a7d3"),
                              list(0.375, "#212b66"),
                              list(0.5, "#ffea04"),
                              list(0.75, "#a2d5c6"),
                              list(1, "red")),
            hoverinfo = "skip",
            showscale = FALSE) %>%
  add_trace(type = "scattergeo",
            locations = ~state, text = ~top_boy, frame = ~year,
            mode = "text",
            textfont = list(color=rgb(0,0,0), size = 10),
            hoverinfo = "text",
            hovertext = paste(df$state, ": ", df$top_boy,
                              "\nYear : ", df$year) ,
            showlegend = FALSE) %>%
  animation_opts(250) %>%
  animation_slider(currentvalue = list(prefix = "Year: ")) %>% 
  layout(geo = list(
    scope = "usa",
    projection = list(type = "albers usa"),
    showlakes = FALSE),
    title = "Most popular baby boy names by state each year 1910-2022"
  ) 

# create map for most popular 4 letter combinations in baby names
plot_ly(df,
        type = "choropleth",
        locationmode = "USA-states",
        frame = ~year) %>% 
  add_trace(locations = ~state,
            z = top_4_combo_ranks,
            zmin = 0,
            zmax = max(top_4_combo_ranks),
            colorscale = list(list(0, "#ecc19c"),
                              list(0.25, "#79a7d3"),
                              list(0.375, "#212b66"),
                              list(0.5, "#ffea04"),
                              list(0.75, "#a2d5c6"),
                              list(1, "red")),
            hoverinfo = "skip",
            showscale = FALSE) %>%
  add_trace(type = "scattergeo",
            locations = ~state, text = ~top_4_combo, frame = ~year,
            mode = "text",
            textfont = list(color=rgb(0,0,0), size = 10),
            hoverinfo = "text",
            hovertext = paste(df$state, ": ", df$top_4_combo,
                              "\nYear : ", df$year) ,
            showlegend = FALSE) %>%
  animation_opts(250) %>%
  animation_slider(currentvalue = list(prefix = "Year: ")) %>% 
  layout(geo = list(
    scope = "usa",
    projection = list(type = "albers usa"),
    showlakes = FALSE),
    title = "Most popular 4 letter combinations in baby names
    by state each year 1910-2022"
  ) 

# create map for most popular 5 letter combinations in baby names
plot_ly(df,
        type = "choropleth",
        locationmode = "USA-states",
        frame = ~year) %>% 
  add_trace(locations = ~state,
            z = top_5_combo_ranks,
            zmin = 0,
            zmax = max(top_5_combo_ranks),
            colorscale = list(list(0, "#ecc19c"),
                              list(0.25, "#79a7d3"),
                              list(0.375, "#212b66"),
                              list(0.5, "#ffea04"),
                              list(0.75, "#a2d5c6"),
                              list(1, "red")),
            hoverinfo = "skip",
            showscale = FALSE) %>%
  add_trace(type = "scattergeo",
            locations = ~state, text = ~top_5_combo, frame = ~year,
            mode = "text",
            textfont = list(color=rgb(0,0,0), size = 10),
            hoverinfo = "text",
            hovertext = paste(df$state, ": ", df$top_5_combo,
                              "\nYear : ", df$year) ,
            showlegend = FALSE) %>%
  animation_opts(250) %>%
  animation_slider(currentvalue = list(prefix = "Year: ")) %>% 
  layout(geo = list(
    scope = "usa",
    projection = list(type = "albers usa"),
    showlakes = FALSE),
    title = "Most popular 5 letter combinations in baby names
    by state each year 1910-2022"
  ) 


# create map for longest baby names
plot_ly(df,
        type = "choropleth",
        locationmode = "USA-states",
        frame = ~year) %>% 
  add_trace(locations = ~state,
            z = 11 - (df$len_rank - 40)*(df$len_rank >= 40),
            colorscale = list(list(0, "#F27A7D"),
                              list(0.5, "#F7D486"),
                              list(1, "white")),
            colorbar = list(title = "Rank",
                            orientation = "h",
                            y = 0,
                            tickvals = seq(1, 11, 1),
                            ticktext = append(seq(1, 10, 1), "11+")),
            hoverinfo = "text",
            hovertext = paste(df$state, "\n",
                              "Year:", df$year, "\n",
                              "Length:", round(df$avg_len, 4), "letters\n",
                              "Length rank:", 51 - df$len_rank)) %>%
  animation_opts(250) %>%
  animation_slider(currentvalue = list(prefix = "Year: ")) %>% 
  layout(geo = list(
    scope = "usa",
    projection = list(type = "albers usa"),
    showlakes = FALSE),
    title = "States with the longest average \nbaby name length each year 1910-2022"
  ) 

