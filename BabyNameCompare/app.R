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
      plotOutput("trend_plot", height = "600px")
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

  output$trend_plot <- renderPlot({
    selected <- vapply(seq_len(n_dropdowns),
                       function(i) {
                         x <- input[[paste0("name", i)]]
                         if (is.null(x)) "" else x
                       }, character(1))
    keep <- selected != "" & !duplicated(selected)
    selected <- selected[keep]
    validate(need(length(selected) > 0, "Select at least one name."))

    lt_map <- setNames(line_types[which(keep)], selected)

    plot_df <- df %>%
      filter(name %in% selected) %>%
      mutate(name = factor(name, levels = selected))

    ggplot(plot_df, aes(x = year, y = n, color = sex, linetype = name,
                        group = interaction(name, sex))) +
      geom_line(linewidth = 1) +
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
