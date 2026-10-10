# =====================================================================
# helpers.R
# ---------------------------------------------------------------------
# Small reusable functions shared by the login page, the three role
# dashboards (Patient / Doctor / Administrator) and the charts.
#
# Shiny automatically loads every file inside the R/ folder when the
# app starts, so no source() call is needed.
# =====================================================================

# ---------------------------------------------------------------------
# Colours and currency (change CURRENCY to "Rs. " or an INR symbol if you prefer)
# ---------------------------------------------------------------------
CURRENCY <- "$"

HIQ_COL <- list(
  navy  = "#0B2A4A", blue = "#2B6CB0", teal = "#12A8A3", cyan = "#38BDF8",
  green = "#2E9E6B", amber = "#E9A23B", red = "#D64550", purple = "#7C5CBF",
  slate = "#64748B", grid = "#EEF2F6"
)

# ---------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------

# 630000 -> "$630,000"
fmt_currency <- function(x) {
  paste0(CURRENCY, format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE))
}

# "14:00" -> "02:00 PM"
fmt_time12 <- function(t) {
  format(strptime(t, "%H:%M"), "%I:%M %p")
}

# Date -> "05 Oct 2026"
fmt_date <- function(d) format(d, "%d %b %Y")

# Combine a Date and an "HH:MM" string into a date-time
to_datetime <- function(date, time) {
  as.POSIXct(paste(format(date, "%Y-%m-%d"), time), format = "%Y-%m-%d %H:%M")
}

# Minutes from "now" until a date-time (negative = already passed)
minutes_until <- function(dt, now = Sys.time()) {
  as.numeric(difftime(dt, now, units = "mins"))
}

# "today", "tomorrow", "in 5 days"
days_label <- function(date, today = Sys.Date()) {
  n <- as.numeric(date - today)
  if (n == 0) "today" else if (n == 1) "tomorrow" else if (n > 1) paste("in", n, "days") else paste(abs(n), "days ago")
}

# ---------------------------------------------------------------------
# Hospital Health Score  (PROJECT-SPECIFIC OPERATIONAL SCORE)
# ---------------------------------------------------------------------
classify_score <- function(score) {
  if (score >= 80) {
    list(status = "Excellent", hex = "#2E9E6B")
  } else if (score >= 60) {
    list(status = "Good", hex = "#2B6CB0")
  } else if (score >= 40) {
    list(status = "Needs Attention", hex = "#E9A23B")
  } else {
    list(status = "Critical", hex = "#D64550")
  }
}

# ---------------------------------------------------------------------
# Input validation for the Administrator data panel
# Returns a character vector of friendly messages (empty = all valid)
# ---------------------------------------------------------------------
check_inputs <- function(i) {
  msgs <- character(0)

  numeric_vals <- c(
    i$total_beds, i$occupied_beds, i$icu_beds, i$occupied_icu,
    i$total_patients, i$doctors, i$nurses, i$emergency, i$cardiology,
    i$neurology, i$orthopedics, i$pediatrics, i$revenue_pp
  )

  # A numericInput that is empty or invalid returns NULL or NA
  if (length(numeric_vals) < 13 || any(is.na(numeric_vals))) {
    return("Some fields are empty. Please enter a number in every field.")
  }
  if (any(numeric_vals < 0)) {
    msgs <- c(msgs, "Negative values are not allowed. Please enter zero or positive numbers.")
  }
  if (i$total_beds <= 0) {
    msgs <- c(msgs, "Total Beds must be greater than 0 to calculate Bed Occupancy.")
  }
  if (i$icu_beds <= 0) {
    msgs <- c(msgs, "ICU Beds must be greater than 0 to calculate ICU Utilization.")
  }
  if (i$total_patients <= 0) {
    msgs <- c(msgs, "Total Patients must be greater than 0 to calculate the Staff Score.")
  }
  if (i$occupied_beds > i$total_beds) {
    msgs <- c(msgs, "Occupied Beds cannot be greater than Total Beds.")
  }
  if (i$occupied_icu > i$icu_beds) {
    msgs <- c(msgs, "Occupied ICU Beds cannot be greater than ICU Beds.")
  }
  if (i$icu_beds > i$total_beds) {
    msgs <- c(msgs, "ICU Beds cannot be greater than Total Beds.")
  }
  msgs
}

# ---------------------------------------------------------------------
# Plotly styling shared by every chart
# ---------------------------------------------------------------------
hiq_plot <- function(p, xlab = "", ylab = "", legend = FALSE) {
  p %>%
    layout(
      xaxis = list(title = xlab, gridcolor = HIQ_COL$grid),
      yaxis = list(title = ylab, gridcolor = HIQ_COL$grid, zeroline = FALSE),
      showlegend = legend,
      legend = list(orientation = "h", y = -0.25),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Inter, Segoe UI, Roboto, sans-serif", color = "#334155"),
      margin = list(t = 20, b = 55, l = 55, r = 15)
    ) %>%
    config(displayModeBar = FALSE, responsive = TRUE)
}

# ---------------------------------------------------------------------
# Reusable UI building blocks (cards, KPI tiles, badges, alerts ...)
# ---------------------------------------------------------------------

# Page title block
page_header <- function(title, subtitle = NULL) {
  div(class = "hiq-page-head",
      h2(title),
      if (!is.null(subtitle)) p(subtitle))
}

# White rounded card with an optional title
hiq_card <- function(title = NULL, ..., subtitle = NULL, class = "") {
  div(class = paste("hiq-card", class),
      if (!is.null(title)) {
        div(class = "hiq-card-head",
            h4(title),
            if (!is.null(subtitle)) span(class = "hiq-card-sub", subtitle))
      },
      div(class = "hiq-card-body", ...))
}

# Large number tile. tone: teal, blue, navy, green, amber, red, purple
kpi_card <- function(label, value, icon_name, sub = NULL, tone = "teal") {
  div(class = paste("hiq-kpi", paste0("tone-", tone)),
      div(class = "hiq-kpi-icon", icon(icon_name)),
      div(class = "hiq-kpi-body",
          div(class = "hiq-kpi-label", label),
          div(class = "hiq-kpi-value", value),
          if (!is.null(sub)) div(class = "hiq-kpi-sub", sub)))
}

# Responsive grids: the browser wraps cards automatically
kpi_grid <- function(...) div(class = "hiq-grid kpi-grid", ...)
card_grid <- function(..., min = 360) {
  div(class = "hiq-grid", style = paste0("grid-template-columns: repeat(auto-fit, minmax(", min, "px, 1fr));"), ...)
}

# Small coloured label. tone: green, amber, red, blue, grey, teal, purple
status_badge <- function(text, tone = "grey") {
  span(class = paste("hiq-badge", paste0("badge-", tone)), text)
}

# Maps common status words to a badge colour
status_tone <- function(x) {
  switch(x,
         "Completed" = "green", "Reviewed" = "green", "Published" = "green",
         "Resolved" = "green", "Available" = "green", "On Duty" = "green",
         "Approved" = "green",
         "Scheduled" = "blue", "Registered" = "blue", "Attending" = "blue",
         "Requested" = "blue", "Ongoing" = "blue",
         "In Progress" = "teal", "In Consultation" = "teal",
         "In Surgery" = "red", "Rejected" = "red", "High" = "red",
         "Waiting" = "amber", "Pending Review" = "amber", "Under Review" = "amber",
         "Submitted" = "amber", "Follow-up" = "amber", "Rescheduled" = "amber",
         "Medium" = "amber", "Low" = "grey", "Cancelled" = "grey", "grey")
}

# Educational / demo disclaimer strip
demo_notice <- function(text) {
  div(class = "hiq-notice", icon("info-circle"), span(text))
}

# ---------------------------------------------------------------------
# Alerts: a list of plain lists so they can be counted AND displayed.
#   level: "danger", "warning", "info" or "success"
# ---------------------------------------------------------------------
make_alert <- function(level, title, text = NULL) {
  list(level = level, title = title, text = text)
}

render_alerts <- function(alerts) {
  if (length(alerts) == 0) {
    return(div(class = "hiq-empty", "No alerts right now."))
  }
  tagList(lapply(alerts, function(a) {
    ic <- switch(a$level,
                 danger = "exclamation-circle", warning = "exclamation-triangle",
                 success = "check-circle", "info-circle")
    div(class = paste("hiq-alert", paste0("alert-", a$level), if (a$level == "danger") "pulse"),
        div(class = "hiq-alert-icon", icon(ic)),
        div(class = "hiq-alert-body",
            div(class = "hiq-alert-title", a$title),
            if (!is.null(a$text)) div(class = "hiq-alert-text", a$text)))
  }))
}

# Number of alerts that need attention (shown on the bell icon)
count_urgent <- function(alerts) {
  sum(vapply(alerts, function(a) a$level %in% c("danger", "warning"), logical(1)))
}

# ---------------------------------------------------------------------
# Timeline item (used by the doctor schedule and the medicine schedule)
#   state: "past", "next" or "later"
# ---------------------------------------------------------------------
timeline_item <- function(time_text, title, detail = NULL, icon_name = "circle",
                          tone = "blue", state = "later", badge = NULL) {
  div(class = paste("hiq-tl-item", paste0("state-", state), paste0("tone-", tone)),
      div(class = "hiq-tl-time", time_text),
      div(class = "hiq-tl-dot", icon(icon_name)),
      div(class = "hiq-tl-content",
          div(class = "hiq-tl-title", title, if (!is.null(badge)) badge),
          if (!is.null(detail)) div(class = "hiq-tl-detail", detail)))
}

# Thin progress bar with a label (used for bed and ICU capacity)
progress_row <- function(label, pct, tone = "teal", right_text = NULL) {
  pct <- max(0, min(100, pct))
  div(class = "hiq-progress",
      div(class = "hiq-progress-top", span(label), span(class = "hiq-progress-val",
                                                         if (is.null(right_text)) paste0(round(pct, 1), "%") else right_text)),
      div(class = "hiq-progress-track",
          div(class = paste("hiq-progress-fill", paste0("fill-", tone)), style = paste0("width:", pct, "%;"))))
}

# Key / value row used on profile cards
info_row <- function(label, value) {
  div(class = "hiq-info-row", span(class = "hiq-info-label", label), span(class = "hiq-info-value", value))
}

# ---------------------------------------------------------------------
# Dashboard shell: sidebar, top bar and logout button
# ---------------------------------------------------------------------

# Left navigation. `items` is a list of list(id, label, icon).
# The Logout button uses the plain id "logout" which is handled in app.R.
hiq_sidebar <- function(ns, role_label, display_name, sub_text, items, selected) {
  initial <- toupper(substr(gsub("^Dr\\. ", "", display_name), 1, 1))
  tags$aside(
    class = "hiq-sidebar",
    div(class = "hiq-brand",
        div(class = "hiq-brand-icon", icon("hospital")),
        div(div(class = "hiq-brand-name", "Hospital IQ"),
            div(class = "hiq-brand-role", role_label))),
    div(class = "hiq-nav",
        lapply(items, function(it) {
          div(class = paste("hiq-nav-item", if (it$id == selected) "active"),
              actionLink(ns(paste0("nav_", it$id)),
                         label = tagList(icon(it$icon), span(it$label)),
                         class = "hiq-nav-link"))
        })),
    div(class = "hiq-sidebar-foot",
        div(class = "hiq-user",
            div(class = "hiq-avatar", initial),
            div(div(class = "hiq-user-name", display_name),
                div(class = "hiq-user-sub", sub_text))),
        actionButton("logout", label = tagList(icon("sign-out-alt"), "Logout"), class = "hiq-logout"))
  )
}

# Top bar: date, demo chip and alert bell
hiq_topbar <- function(n_alerts = 0) {
  div(class = "hiq-topbar",
      div(class = "hiq-top-date", icon("calendar-alt"), format(Sys.Date(), "%A, %d %B %Y")),
      div(class = "hiq-top-right",
          span(class = "hiq-demo-chip", "Demo data only"),
          div(class = "hiq-bell", icon("bell"),
              if (n_alerts > 0) span(class = "hiq-bell-count", n_alerts))))
}

# Whole page layout for a role: sidebar on the left, content on the right
hiq_shell <- function(sidebar, topbar_output, tabs) {
  div(class = "hiq-shell",
      sidebar,
      div(class = "hiq-main", topbar_output, tabs))
}

# Switches the hidden tab panel when a sidebar link is clicked.
#   ids      : tab ids (same as in the sidebar items)
# The `> 0` check ignores the automatic initial value of action links.
setup_navigation <- function(input, session, ids) {
  # Store observers on session so they are not GC'd and work reliably
  if (is.null(session$userData$nav_observers)) session$userData$nav_observers <- list()
  for (tab_id in ids) {
    local({
      tid <- tab_id
      obs <- observeEvent(input[[paste0("nav_", tid)]], {
        updateTabsetPanel(session, "tabs", selected = tid)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      session$userData$nav_observers[[tid]] <- obs
    })
  }
  invisible(NULL)
}

# DataTable defaults used by every table in the project
hiq_table <- function(df, page_length = 8, selection = "none", ...) {
  DT::datatable(
    df,
    rownames = FALSE,
    selection = selection,
    class = "stripe hover compact",
    options = list(pageLength = page_length, order = list(), dom = if (nrow(df) > page_length) "ftp" else "t",
                   scrollX = TRUE, autoWidth = FALSE,
                   language = list(emptyTable = "No records to display")),
    ...
  )
}

# Colours a status column of a DT table
style_status <- function(dt, column) {
  DT::formatStyle(
    dt, column, fontWeight = "600",
    color = DT::styleEqual(
      c("Completed", "Reviewed", "Published", "Resolved", "Approved",
        "Scheduled", "Registered", "Attending", "Ongoing", "In Progress",
        "Follow-up", "Pending Review", "Under Review", "Submitted", "Rescheduled",
        "Cancelled", "High", "Medium", "Low", "Requested", "Rejected", "In Surgery"),
      c("#1F7A52", "#1F7A52", "#1F7A52", "#1F7A52", "#1F7A52",
        "#2B6CB0", "#2B6CB0", "#2B6CB0", "#2B6CB0", "#0E8C88",
        "#B7791F", "#B7791F", "#B7791F", "#B7791F", "#B7791F",
        "#64748B", "#C53030", "#B7791F", "#64748B", "#2B6CB0", "#C53030", "#C53030")
    )
  )
}

# Surgery status that updates with the clock for today's operations
#   (each surgery is assumed to last two hours)
surgery_status_now <- function(surgeries, now = Sys.time()) {
  if (nrow(surgeries) == 0) return(surgeries)
  start <- to_datetime(surgeries$date, surgeries$time)
  mins  <- minutes_until(start, now)
  surgeries$status <- ifelse(surgeries$status == "Completed", "Completed",
                       ifelse(mins < -120, "Completed",
                       ifelse(mins <= 0, "In Surgery", "Scheduled")))
  surgeries
}

# Queue status for today's outpatient appointments
#   (each consultation is assumed to last 30 minutes)
queue_status_now <- function(appts, now = Sys.time()) {
  if (nrow(appts) == 0) return(appts)
  start <- to_datetime(appts$date, appts$time)
  mins  <- minutes_until(start, now)
  appts$queue_status <- ifelse(mins < -30, "Completed",
                         ifelse(mins <= 0, "In Consultation", "Waiting"))
  appts
}

# Push a notification into shared$notifications
push_notification <- function(shared, role, user_id, title, message, level = "info") {
  new_id <- paste0("N", nrow(shared$notifications) + 1)
  shared$notifications <- rbind(shared$notifications, data.frame(
    notif_id = new_id, role = role, user_id = as.character(user_id),
    title = title, message = message, level = level,
    time = Sys.time(), stringsAsFactors = FALSE
  ))
}

# Render a notification list for a role overview
render_notifications <- function(shared, role, user_id, n = 8) {
  nts <- shared$notifications
  nts <- nts[nts$role == role & nts$user_id == as.character(user_id), , drop = FALSE]
  nts <- nts[order(nts$time, decreasing = TRUE), , drop = FALSE]
  nts <- head(nts, n)
  if (nrow(nts) == 0) {
    return(div(class = "hiq-empty", "No notifications yet."))
  }
  alerts <- lapply(seq_len(nrow(nts)), function(i) {
    make_alert(nts$level[i], nts$title[i],
               paste0(nts$message[i], " · ", format(nts$time[i], "%d %b %H:%M")))
  })
  render_alerts(alerts)
}

