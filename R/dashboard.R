# ------------------ Dashboard server outputs ------------------
# Call dashboard_server(input, output, session, con, db_tick) inside server()

dashboard_server <- function(input, output, session, con, db_tick) {

  dash_data <- reactive({
  db_tick()
  db_get_transactions(con) |>
    dplyr::filter(
      is.na(nature) | nature != "Transfert",
      !is.na(amount),
      amount < 0
    ) |>
    dplyr::mutate(
      amount_pos = -amount,
      month      = lubridate::floor_date(date, "month"),
      category   = dplyr::if_else(
        is.na(category) | category == "",
        "Non classe", as.character(category)
      )
    )
  })

  output$dash_this_month <- renderText({
    df    <- dash_data()
    cur   <- lubridate::floor_date(Sys.Date(), "month")
    total <- df |>
      dplyr::filter(month == cur) |>
      dplyr::summarise(s = sum(amount_pos, na.rm = TRUE)) |>
      dplyr::pull(s)
    paste0("$", format(round(total, 2), big.mark = " ", nsmall = 2))
  })

  output$dash_last_month <- renderText({
    df    <- dash_data()
    last  <- lubridate::floor_date(Sys.Date(), "month") - months(1)
    total <- df |>
      dplyr::filter(month == last) |>
      dplyr::summarise(s = sum(amount_pos, na.rm = TRUE)) |>
      dplyr::pull(s)
    paste0("$", format(round(total, 2), big.mark = " ", nsmall = 2))
  })

  output$dash_top_cat <- renderText({
    df  <- dash_data()
    cur <- lubridate::floor_date(Sys.Date(), "month")
    top <- df |>
      dplyr::filter(month == cur) |>
      dplyr::summarise(s = sum(amount_pos, na.rm = TRUE), .by = category) |>
      dplyr::slice_max(s, n = 1, with_ties = FALSE)
    if (nrow(top) == 0) return("--")
    paste0(top$category, " ($", format(round(top$s, 2), big.mark = " ", nsmall = 2), ")")
  })

  output$dash_bar <- renderPlot({
    df      <- dash_data()
    cutoff  <- lubridate::floor_date(Sys.Date(), "month") - months(5)
    plot_df <- df |>
      dplyr::filter(month >= cutoff) |>
      dplyr::summarise(total = sum(amount_pos, na.rm = TRUE), .by = c(month, category)) |>
      dplyr::mutate(month_lbl = format(month, "%b %Y"))

    if (nrow(plot_df) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Aucune donnee") +
          ggplot2::theme_void()
      )
    }

    top_cats <- plot_df |>
      dplyr::summarise(s = sum(total), .by = category) |>
      dplyr::slice_max(s, n = 7, with_ties = FALSE) |>
      dplyr::pull(category)

    plot_df <- plot_df |>
      dplyr::mutate(category = dplyr::if_else(category %in% top_cats, category, "Autres"))

    ggplot2::ggplot(plot_df, ggplot2::aes(x = month_lbl, y = total, fill = category)) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::scale_y_continuous(
        labels = scales::dollar_format(prefix = "$", big.mark = " ", decimal.mark = ",")
      ) +
      ggplot2::labs(x = NULL, y = "Depenses ($)", fill = "Categorie") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  })

  output$dash_pie <- renderPlot({
    df  <- dash_data()
    cur <- lubridate::floor_date(Sys.Date(), "month")
    pie_df <- df |>
      dplyr::filter(month == cur) |>
      dplyr::summarise(total = sum(amount_pos, na.rm = TRUE), .by = category) |>
      dplyr::arrange(dplyr::desc(total))

    if (nrow(pie_df) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Aucune donnee ce mois") +
          ggplot2::theme_void()
      )
    }

    top_cats <- dplyr::slice_max(pie_df, total, n = 6, with_ties = FALSE)$category
    pie_df   <- pie_df |>
      dplyr::mutate(category = dplyr::if_else(category %in% top_cats, category, "Autres")) |>
      dplyr::summarise(total = sum(total), .by = category)

    ggplot2::ggplot(pie_df, ggplot2::aes(x = "", y = total, fill = category)) +
      ggplot2::geom_col(width = 1, color = "white") +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::labs(fill = "Categorie") +
      ggplot2::theme_void(base_size = 13)
  })

}
