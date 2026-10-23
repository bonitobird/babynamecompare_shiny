
library(shiny)
library(ggplot2)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# ---- GitHub configuration ----

github_owner  <- "bonitobird"
github_repo   <- "babynamecompare_shiny"
github_branch <- "main"
github_folder <- "raw_data/names"


# ---- Load and combine GitHub data ----

load_github_data <- function() {
  
  tree_url <- paste0(
    "https://api.github.com/repos/",
    github_owner, "/",
    github_repo, "/git/trees/",
    github_branch,
    "?recursive=1"
  )
  
  response <- httr::GET(tree_url)
  httr::stop_for_status(response)
  
  tree <- jsonlite::fromJSON(
    httr::content(
      response,
      as = "text",
      encoding = "UTF-8"
    ),
    simplifyVector = FALSE
  )
  
  # Get all file paths in the repository.
  files <- purrr::map_chr(
    tree$tree,
    function(item) {
      if (is.null(item$path)) "" else item$path
    }
  )
  
  # Select TXT files in raw_data/names.
  txt_files <- files[
    grepl(
      paste0("^", github_folder, "/.*\\.txt$"),
      files,
      ignore.case = TRUE
    )
  ]
  
  if (length(txt_files) == 0) {
    stop("No .txt files found in raw_data/names.")
  }
  
  # Extract four-digit years from filenames.
  years <- regmatches(
    basename(txt_files),
    regexpr(
      "[0-9]{4}(?=\\.txt$)",
      basename(txt_files),
      perl = TRUE
    )
  )
  
  if (any(years == "")) {
    stop("Some filenames do not contain a four-digit year.")
  }
  
  # Read each file.
  data_list <- purrr::map2(
    txt_files,
    years,
    function(file_path, file_year) {
      
      file_url <- paste0(
        "https://raw.githubusercontent.com/",
        github_owner, "/",
        github_repo, "/",
        github_branch, "/",
        file_path
      )
      
      file_response <- httr::GET(file_url)
      httr::stop_for_status(file_response)
      
      file_text <- httr::content(
        file_response,
        as = "text",
        encoding = "UTF-8"
      )
      
      # Expected file format:
      # name, sex, count
      df <- read.csv(
        text = file_text,
        header = FALSE,
        col.names = c("name", "sex", "count"),
        stringsAsFactors = FALSE
      )
      
      df$year <- as.integer(file_year)
      
      df
    }
  )
  
  # Combine files into one data frame.
  combined <- dplyr::bind_rows(data_list)
  
  combined %>%
    mutate(
      name = as.character(name),
      sex = factor(
        sex,
        levels = c("M", "F"),
        labels = c("Boys", "Girls")
      ),
      year = as.integer(year),
      count = as.numeric(count)
    ) %>%
    filter(
      !is.na(name),
      !is.na(sex),
      !is.na(year),
      !is.na(count)
    )
}


# ---- Shiny server ----

server <- function(input, output, session) {
  
  # Reactive storage for the combined data.
  data <- reactiveVal(NULL)
  
  # Load data when the app starts.
  observe({
    tryCatch(
      {
        data(load_github_data())
      },
      error = function(e) {
        showNotification(
          paste(
            "Data loading failed:",
            conditionMessage(e)
          ),
          type = "error",
          duration = NULL
        )
      }
    )
  })
  
  
  # ---- Populate dropdowns once data is available ----
  
  observeEvent(data(), {
    
    df <- data()
    
    req(!is.null(df), nrow(df) > 0)
    
    all_names <- sort(unique(df$name))
    
    # Preferred initial selections.
    preferred <- c(
      "James", "Mary", "John", "Patricia", "Robert"
    )
    
    defaults <- preferred[preferred %in% all_names]
    
    # Fill any missing defaults with other available names.
    if (length(defaults) < 5) {
      
      remaining <- setdiff(all_names, defaults)
      
      defaults <- c(
        defaults,
        head(remaining, 5 - length(defaults))
      )
    }
    
    # Populate each dropdown.
    for (i in seq_len(5)) {
      
      updateSelectInput(
        session,
        inputId = paste0("name", i),
        choices = all_names,
        selected = defaults[i]
      )
    }
    
  }, ignoreInit = FALSE)
  
  
  # ---- Data loading status ----
  
  output$data_status <- renderText({
    
    df <- data()
    
    if (is.null(df)) {
      return("Loading baby-name data...")
    }
    
    paste0(
      "Loaded ",
      format(nrow(df), big.mark = ","),
      " rows and ",
      format(length(unique(df$name)), big.mark = ","),
      " unique names."
    )
  })
  
  
  # ---- Filter data by selected names ----
  
  selected_data <- reactive({
    
    req(data())
    
    ids <- paste0("name", 1:5)
    
    selected_names <- vapply(
      ids,
      function(id) {
        value <- input[[id]]
        
        if (is.null(value)) "" else value
      },
      character(1)
    )
    
    # Wait until all five dropdowns have selections.
    req(all(nzchar(selected_names)))
    
    data() %>%
      filter(name %in% selected_names) %>%
      arrange(name, sex, year)
  })
  
  
  # ---- Draw the XY line plot ----
  
  output$xy_plot <- renderPlot({
    
    df <- selected_data()
    
    validate(
      need(
        nrow(df) > 0,
        "No data available for the selected names."
      )
    )
    
    ggplot(
      df,
      aes(
        x = year,
        y = count,
        color = sex,
        linetype = name,
        group = interaction(name, sex)
      )
    ) +
      
      geom_line(linewidth = 0.9) +
      
      scale_color_manual(
        name = "Gender",
        values = c(
          "Boys" = "#2878B5",
          "Girls" = "#D65A9E"
        ),
        drop = FALSE
      ) +
      
      scale_linetype_discrete(
        name = "Name"
      ) +
      
      scale_x_continuous(
        breaks = scales::breaks_pretty(n = 10)
      ) +
      
      scale_y_continuous(
        labels = scales::label_comma()
      ) +
      
      labs(
        title = "Baby Name Popularity Over Time",
        x = "Year",
        y = "Number of Babies"
      ) +
      
      theme_minimal(base_size = 14) +
      
      theme(
        legend.position = "right",
        plot.title = element_text(face = "bold")
      )
  })
}
