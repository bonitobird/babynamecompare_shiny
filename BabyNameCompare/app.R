library(shiny)
library(ggplot2)
library(dplyr)

# Replace with your own dataframe if needed; it must have columns: name, year, sex ("M"/"F"), n (count)
# library(babynames)
# df <- babynames %>% select(name, year, sex, n)

df <- read.csv(
  "https://raw.githubusercontent.com/bonitobird/babynamecompare_shiny/main/combined_data/babynames_national.csv",
  stringsAsFactors = FALSE
)


all_names <- sort(unique(df$name))
line_types <- c("solid", "dashed", "dotted", "dotdash", "longdash")
n_dropdowns <- 5

ui <- fluidPage(
  titlePanel("Baby Name Trends"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      lapply(seq_len(n_dropdowns), function(i) {
        tags$div(
          style = "display: flex; align-items: flex-end; gap: 8px;",
          tags$div(
            style = "flex: 1; min-width: 0;",
            selectizeInput(
              paste0("name", i),
              label = paste0("Name ", i, " (", line_types[i], ")"),
              choices = NULL,
              options = list(placeholder = "Type a name...")
            )
          ),
          actionButton(paste0("clear", i), label = NULL, icon = icon("xmark"),
                       title = paste0("Clear name ", i),
                       `aria-label` = paste0("Clear name ", i),
                       style = "margin-bottom: 15px;")
        )
      })
    ),
    mainPanel(
      width = 9,
      tags$div(
        style = "position: relative;",
        plotOutput("trend_plot", height = "600px",
                   hover = hoverOpts("trend_hover", delay = 75, nullOutside = TRUE)),
        uiOutput("trend_label",
                 style = "position: absolute; inset: 0; pointer-events: none;")
      )
    )
  )
)

server <- function(input, output, session) {
  # Server-side selectize keeps the large name list fast
  for (i in seq_len(n_dropdowns)) {
    updateSelectizeInput(session, paste0("name", i),
                         choices = c("", all_names), selected = "", server = TRUE)
  }

  lapply(seq_len(n_dropdowns), function(i) {
    observeEvent(input[[paste0("clear", i)]], {
      updateSelectizeInput(session, paste0("name", i), selected = character(0))
    })
  })

  selected_names <- reactive({
    selected <- vapply(seq_len(n_dropdowns),
                       function(i) {
                         x <- input[[paste0("name", i)]]
                         if (is.null(x)) "" else x
                       }, character(1))
    keep <- selected != "" & !duplicated(selected)
    selected <- selected[keep]
    validate(need(length(selected) > 0, "Select at least one name."))

    setNames(selected, line_types[which(keep)])
  })

  plot_data <- reactive({
    selected <- unname(selected_names())
    df %>%
      filter(name %in% selected, !is.na(year), !is.na(n)) %>%
      mutate(name = factor(name, levels = selected)) %>%
      arrange(name, sex, year) %>%
      group_by(name, sex) %>%
      mutate(segment = cumsum(year - lag(year, default = first(year)) != 1L)) %>%
      group_by(name, sex, segment) %>%
      mutate(run_size = n(), endpoint = row_number() %in% c(1L, n())) %>%
      ungroup()
  })

  output$trend_label <- renderUI({
    hover <- input$trend_hover
    req(hover)
    point <- nearPoints(plot_data(), hover,
                        xvar = "year", yvar = "n", threshold = 12, maxpoints = 1)
    if (!nrow(point)) return(NULL)

    domain <- hover$domain
    range <- hover$range
    ratio <- hover$img_css_ratio
    point_x <- (range$left + (point$year[[1]] - domain$left) /
      (domain$right - domain$left) * (range$right - range$left)) / ratio$x
    point_y <- (range$bottom + (point$n[[1]] - domain$bottom) /
      (domain$top - domain$bottom) * (range$top - range$bottom)) / ratio$y
    label_width <- min(180, (range$right - range$left) / ratio$x)
    label_left <- max(range$left / ratio$x,
                      min(point_x + 24, range$right / ratio$x - label_width))
    label_top <- max(0, point_y - 128)
    line_x <- label_left + label_width / 2
    line_y <- label_top + 108
    line_length <- sqrt((line_x - point_x)^2 + (line_y - point_y)^2)
    line_angle <- atan2(line_y - point_y, line_x - point_x)

    tagList(
      tags$div(style = sprintf(paste0(
        "position: absolute; left: %.2fpx; top: %.2fpx; width: %.2fpx; ",
        "height: 1px; background: #555; transform-origin: left center; ",
        "transform: rotate(%.6frad);"
      ), point_x, point_y, line_length, line_angle)),
      tags$div(
        role = "tooltip",
        style = sprintf(paste0(
          "position: absolute; left: %.2fpx; top: %.2fpx; width: %.2fpx; ",
          "min-height: 108px; box-sizing: border-box; padding: 8px 10px; ",
          "background: white; border: 1px solid #999; border-radius: 4px; ",
          "font-size: 13px; line-height: 18px; overflow-wrap: anywhere;"
        ), label_left, label_top, label_width),
        tags$strong(as.character(point$name[[1]])),
        tags$div("Count: ", format(point$n[[1]], big.mark = ",")),
        tags$div("Year: ", as.character(point$year[[1]])),
        tags$div("Sex: ", if (point$sex[[1]] == "M") "Male" else "Female")
      )
    )
  })

  output$trend_plot <- renderPlot({
    selected <- selected_names()
    lt_map <- setNames(names(selected), unname(selected))
    plot_df <- plot_data()

    ggplot(plot_df, aes(x = year, y = n, color = sex, linetype = name,
                        group = interaction(name, sex, segment))) +
      geom_line(data = filter(plot_df, run_size > 1L), linewidth = 1) +
      geom_point(data = filter(plot_df, endpoint), size = 2, show.legend = FALSE) +
      scale_x_continuous(breaks = scales::breaks_width(10),
             minor_breaks = scales::breaks_width(5)) +
      scale_color_manual(values = c(M = "blue", F = "red"),
                         labels = c(M = "Male", F = "Female"), name = "Sex") +
      scale_linetype_manual(values = lt_map, name = "Name") +
      labs(x = "Year", y = "Count") +
      theme_minimal(base_size = 14) +
      theme(legend.key.width = unit(2, "cm"),
        axis.ticks.x = element_line(color = "grey60"),
        panel.grid.minor.x = element_line(color = "grey92", linewidth = 0.3))
  })
}

shinyApp(ui, server)
