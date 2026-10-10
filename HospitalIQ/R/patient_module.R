# =====================================================================
# patient_module.R
# ---------------------------------------------------------------------
# The PATIENT PORTAL.
#
# PRIVACY RULE used throughout this file:
#   Every table is filtered with  patient_id == pid  where `pid` is the
#   ID of the logged-in patient. Because of this, Patient P001 can never
#   see records that belong to Patient P002 (and vice versa).
#   There is also no financial or hospital-wide information here.
#
# DEMONSTRATION ONLY - all data is synthetic.
# =====================================================================

patient_nav_items <- function() {
  list(
    list(id = "overview",  label = "Overview",         icon = "home"),
    list(id = "history",   label = "Medical History",  icon = "notes-medical"),
    list(id = "reports",   label = "Reports",          icon = "file-medical"),
    list(id = "medicines", label = "Medicines",        icon = "pills"),
    list(id = "appts",     label = "Appointments",     icon = "calendar-check"),
    list(id = "trends",    label = "Health Trends",    icon = "heartbeat"),
    list(id = "profile",   label = "Profile",          icon = "id-card")
  )
}

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------
patient_ui <- function(id, user, demo) {
  ns <- NS(id)
  # The department list starts on the assigned doctor's department
  pat_row <- demo$patients[demo$patients$patient_id == user$user_id, , drop = FALSE]
  default_dept <- demo$doctors$department[match(pat_row$doctor_id, demo$doctors$doctor_id)]
  hiq_shell(
    hiq_sidebar(ns, "Patient Portal", user$display_name,
                paste("Patient ID:", user$user_id), patient_nav_items(), "overview"),
    uiOutput(ns("topbar")),
    tabsetPanel(
      id = ns("tabs"), type = "hidden",

      tabPanelBody("overview", uiOutput(ns("overview_ui"))),

      tabPanelBody("history",
        page_header("Medical History", "Your past diagnoses and treatments"),
        hiq_card("Medical History", DT::DTOutput(ns("history_tbl")))),

      tabPanelBody("reports",
        page_header("Reports & Scans", "Blood tests, imaging and other laboratory reports"),
        demo_notice("Demo educational system — report interpretation should be performed by qualified healthcare professionals."),
        hiq_card("My Reports", subtitle = "Click a row to see a short demo summary",
                 DT::DTOutput(ns("reports_tbl"))),
        hiq_card("Report Summary (demo)", uiOutput(ns("report_summary")))),

      tabPanelBody("medicines",
        page_header("My Medicine Schedule", "Your daily medicines and timings"),
        card_grid(min = 380,
          hiq_card("Today's Medicine Timeline", subtitle = "Upcoming doses are highlighted",
                   uiOutput(ns("med_timeline"))),
          hiq_card("My Medicine Schedule", DT::DTOutput(ns("med_tbl"))))),

      tabPanelBody("appts",
        page_header("Appointments", "View your appointments or request a new one"),
        uiOutput(ns("appt_hero")),
        card_grid(min = 380,
          hiq_card("Upcoming Appointments", DT::DTOutput(ns("upcoming_tbl"))),
          hiq_card("Book Appointment",
            subtitle = "Visible to your doctor and administrator immediately",
            selectInput(ns("req_dept"), "Department",
                        choices = c("Cardiology", "Neurology", "Orthopedics", "Pediatrics", "Emergency"),
                        selected = default_dept),
            selectInput(ns("req_doctor"), "Doctor", choices = NULL),
            dateInput(ns("req_date"), "Preferred date", value = Sys.Date() + 7,
                      min = Sys.Date() + 1, format = "dd M yyyy"),
            selectInput(ns("req_time"), "Preferred time",
                        choices = c("09:00", "10:00", "11:00", "12:00", "14:00", "15:00", "16:00")),
            textAreaInput(ns("req_reason"), "Reason for visit", rows = 3,
                          placeholder = "Briefly describe the reason"),
            hr(),
            h5(class = "hiq-section-title", "OP / IP Registration Payment"),
            uiOutput(ns("pay_summary")),
            div(style = "display:flex; gap:10px; flex-wrap:wrap; margin-top:8px;",
              actionButton(ns("req_submit"), "Book Appointment", icon = icon("paper-plane"),
                           class = "hiq-btn"),
              actionButton(ns("pay_now"), "Pay Now", icon = icon("credit-card"), class = "hiq-btn")
            ),
            uiOutput(ns("req_msg")))),
        hiq_card("Appointment History", DT::DTOutput(ns("past_tbl")))),

      tabPanelBody("trends",
        page_header("Health Trends", "Your recorded measurements over the last 12 months"),
        demo_notice("These charts show recorded historical values only. They are not diagnoses or predictions."),
        card_grid(min = 420,
          hiq_card("Weight over time (kg)", plotly::plotlyOutput(ns("weight_plot"), height = "280px"), class = "fade-in"),
          hiq_card("Blood pressure over time (mmHg)", plotly::plotlyOutput(ns("bp_plot"), height = "280px"), class = "fade-in"),
          hiq_card("Heart rate over time (bpm)", plotly::plotlyOutput(ns("hr_plot"), height = "280px"), class = "fade-in"))),

      tabPanelBody("profile",
        page_header("My Profile", "Your registered details"),
        card_grid(min = 380,
          hiq_card("Personal Details", uiOutput(ns("profile_card"))),
          uiOutput(ns("emergency_profile"))))
    )
  )
}

# ---------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------
patient_server <- function(id, user, demo, shared) {
  moduleServer(id, function(input, output, session) {

    setup_navigation(input, session, vapply(patient_nav_items(), function(x) x$id, character(1)))

    # ---- Who is logged in?  (the ONLY key used to look up data) -------
    pid   <- user$user_id
    today <- demo$today

    pat <- demo$patients[demo$patients$patient_id == pid, , drop = FALSE]
    doc <- demo$doctors[demo$doctors$doctor_id == pat$doctor_id, , drop = FALSE]
    doctor_name <- function(ids) demo$doctors$name[match(ids, demo$doctors$doctor_id)]

    # Only THIS patient's rows are ever copied from the demo tables
    my_appts   <- demo$appointments[demo$appointments$patient_id == pid, , drop = FALSE]
    my_history <- demo$history[demo$history$patient_id == pid, , drop = FALSE]
    my_reports <- demo$reports[demo$reports$patient_id == pid, , drop = FALSE]
    my_meds    <- demo$medicines[demo$medicines$patient_id == pid, , drop = FALSE]
    my_health  <- demo$health[demo$health$patient_id == pid, , drop = FALSE]

    my_reports_live <- reactive({
      shared$reports[shared$reports$patient_id == pid, , drop = FALSE]
    })
    my_rx_live <- reactive({
      shared$prescriptions[shared$prescriptions$patient_id == pid, , drop = FALSE]
    })

    observe({
      dept <- input$req_dept
      if (is.null(dept)) return()
      docs <- demo$doctors[demo$doctors$department == dept, , drop = FALSE]
      if (nrow(docs) == 0) return()
      sel <- if (nrow(doc) > 0 && doc$department[1] == dept) doc$doctor_id[1] else docs$doctor_id[1]
      updateSelectInput(session, "req_doctor",
                        choices = setNames(docs$doctor_id, docs$name),
                        selected = sel)
    })

    all_meds <- reactive({
      base <- my_meds
      rx <- my_rx_live()
      if (nrow(rx) == 0) return(base)
      extra_m <- data.frame(
        patient_id = rx$patient_id, medicine = rx$medicine, dosage = rx$dosage,
        morning = rx$morning, afternoon = rx$afternoon, night = rx$night,
        food = "After Food", start_date = rx$start_date, end_date = rx$end_date,
        stringsAsFactors = FALSE
      )
      rbind(base, extra_m)
    })


    # Clock that refreshes every minute (used for "medicine due")
    clock <- reactive({
      invalidateLater(60000, session)
      Sys.time()
    })

    # ---- Requests made in this session by THIS patient ----------------
    my_requests <- reactive({
      r <- shared$requests
      r[r$patient_id == pid, , drop = FALSE]
    })

    # ---- Upcoming appointments (scheduled + pending requests) ---------
    upcoming_df <- reactive({
      u <- my_appts[my_appts$status == "Scheduled" & my_appts$date >= today, , drop = FALSE]
      base <- data.frame(
        Doctor = doctor_name(u$doctor_id), Department = u$department,
        date = u$date, Time = fmt_time12(u$time), Status = u$status,
        stringsAsFactors = FALSE
      )
      r <- my_requests()
      if (nrow(r) > 0) {
        base <- rbind(base, data.frame(
          Doctor = doctor_name(r$doctor_id), Department = r$department,
          date = r$date, Time = fmt_time12(r$time), Status = "Requested",
          stringsAsFactors = FALSE
        ))
      }
      base <- base[order(base$date), , drop = FALSE]
      base
    })

    # ---- Today's medicine doses --------------------------------------
    slot_cols  <- c("morning", "afternoon", "night")
    slot_times <- c("08:00", "14:00", "21:00")
    slot_names <- c("Morning", "Afternoon", "Night")
    slot_meals <- c("Breakfast", "Lunch", "Dinner")

    today_doses <- reactive({
      now <- clock()
      m <- all_meds(); m <- m[m$start_date <= today & m$end_date >= today, , drop = FALSE]
      rows <- list()
      for (i in seq_len(nrow(m))) {
        for (s in 1:3) {
          if (isTRUE(m[[slot_cols[s]]][i])) {
            rows[[length(rows) + 1]] <- data.frame(
              time = slot_times[s], slot = slot_names[s],
              medicine = m$medicine[i], dosage = m$dosage[i],
              food = paste(sub(" Food", "", m$food[i]), slot_meals[s]),
              stringsAsFactors = FALSE
            )
          }
        }
      }
      if (length(rows) == 0) return(NULL)
      d <- do.call(rbind, rows)
      d <- d[order(d$time), , drop = FALSE]
      d$mins <- minutes_until(to_datetime(today, d$time), now)
      d$state <- ifelse(d$mins < 0, "past", "later")
      first_later <- which(d$state == "later")[1]
      if (!is.na(first_later)) d$state[first_later] <- "next"
      d
    })

    # ---- Smart alerts (generated from the data and the clock) ---------
    alerts <- reactive({
      out <- list()
      now <- clock()

      # Medicine due
      d <- today_doses()
      if (is.null(d)) {
        out[[length(out) + 1]] <- make_alert("info", "No medicines today", "You have no active medicines scheduled for today.")
      } else {
        nxt <- d[d$state == "next", , drop = FALSE]
        if (nrow(nxt) == 0) {
          out[[length(out) + 1]] <- make_alert("success", "All doses for today are done", "No more medicines are scheduled for today.")
        } else if (nxt$mins[1] <= 60) {
          out[[length(out) + 1]] <- make_alert("warning", "Medicine due soon",
            paste0(nxt$medicine[1], " ", nxt$dosage[1], " at ", fmt_time12(nxt$time[1]), " (", nxt$food[1], ")"))
        } else {
          out[[length(out) + 1]] <- make_alert("info", "Next medicine",
            paste0(nxt$medicine[1], " at ", fmt_time12(nxt$time[1]), " (", nxt$food[1], ")"))
        }
      }

      # Appointment tomorrow
      up <- upcoming_df()
      up <- up[up$Status == "Scheduled", , drop = FALSE]
      if (nrow(up) > 0) {
        n <- as.numeric(up$date[1] - today)
        txt <- paste0(up$Doctor[1], " - ", up$Department[1], " at ", up$Time[1], " on ", fmt_date(up$date[1]))
        if (n <= 1) {
          out[[length(out) + 1]] <- make_alert("warning", paste("Appointment", days_label(up$date[1], today)), txt)
        } else {
          out[[length(out) + 1]] <- make_alert("info", paste("Next appointment", days_label(up$date[1], today)), txt)
        }
      }

      # Upcoming medical test
      tests <- { tr <- my_reports_live(); tr[tr$status == "Scheduled" & tr$date >= today, , drop = FALSE] }
      if (nrow(tests) > 0) {
        tests <- tests[order(tests$date), , drop = FALSE]
        n <- as.numeric(tests$date[1] - today)
        out[[length(out) + 1]] <- make_alert(if (n <= 7) "warning" else "info",
          paste("Upcoming test", days_label(tests$date[1], today)),
          paste0(tests$report_name[1], " on ", fmt_date(tests$date[1])))
      }
      out
    })

    output$topbar <- renderUI(hiq_topbar(count_urgent(alerts())))

    # ---- Emergency information card (reused on two pages) -------------
    emergency_card <- function() {
      div(class = "hiq-emergency",
          div(class = "hiq-emergency-head", icon("briefcase-medical"), "Emergency Information"),
          info_row("Blood Group", pat$blood_group),
          info_row("Allergies", pat$allergies),
          info_row("Emergency Contact", pat$emergency_contact),
          info_row("Existing Conditions", pat$conditions))
    }

    # ================= OVERVIEW =======================================
    output$overview_ui <- renderUI({
      up <- upcoming_df()
      up_sched <- up[up$Status == "Scheduled", , drop = FALSE]

      past <- my_appts[my_appts$status == "Completed" & my_appts$date < today, , drop = FALSE]
      past <- past[order(past$date, decreasing = TRUE), , drop = FALSE]

      tests <- { tr <- my_reports_live(); tr[tr$status == "Scheduled" & tr$date >= today, , drop = FALSE] }
      tests <- tests[order(tests$date), , drop = FALSE]

      d <- today_doses()
      am <- all_meds(); active_n <- sum(am$start_date <= today & am$end_date >= today)

      tagList(
        div(class = "hiq-welcome fade-in",
            div(h2(paste("Welcome,", pat$name)),
                p("Here is a summary of your care today.")),
            div(class = "hiq-chips",
                span(class = "hiq-chip", icon("id-card"), paste("ID:", pat$patient_id)),
                span(class = "hiq-chip", icon("calendar"), paste("Age:", pat$age)),
                span(class = "hiq-chip", icon("tint"), paste("Blood Group:", pat$blood_group)),
                span(class = "hiq-chip", icon("user-md"), paste("Doctor:", doc$name)),
                span(class = "hiq-chip chip-status", icon("heartbeat"), pat$status))),

        kpi_grid(
          kpi_card("Next Appointment",
                   if (nrow(up_sched) > 0) fmt_date(up_sched$date[1]) else "None",
                   "calendar-check",
                   if (nrow(up_sched) > 0) paste(up_sched$Time[1], "-", up_sched$Doctor[1]) else "No upcoming visit",
                   "teal"),
          kpi_card("Assigned Doctor", doc$name, "user-md", doc$specialization, "blue"),
          kpi_card("Active Medicines", active_n, "pills",
                   if (is.null(d)) "No doses today" else paste(nrow(d), "doses today"), "purple"),
          kpi_card("Upcoming Test",
                   if (nrow(tests) > 0) tests$report_name[1] else "None",
                   "flask",
                   if (nrow(tests) > 0) fmt_date(tests$date[1]) else "Nothing scheduled", "amber"),
          kpi_card("Last Hospital Visit",
                   if (nrow(past) > 0) fmt_date(past$date[1]) else "None",
                   "hospital",
                   if (nrow(past) > 0) past$reason[1] else "No past visits", "navy")
        ),

        card_grid(min = 380,
          hiq_card("Smart Alerts", subtitle = "Generated from your schedule", render_alerts(alerts())),
          hiq_card("Notification Center", render_notifications(shared, "Patient", pid)),
          emergency_card())
      )
    })

    # ================= MEDICAL HISTORY ================================
    output$history_tbl <- DT::renderDT({
      h <- my_history[order(my_history$date, decreasing = TRUE), , drop = FALSE]
      df <- data.frame(
        Date = fmt_date(h$date), Diagnosis = h$diagnosis, Doctor = doctor_name(h$doctor_id),
        Treatment = h$treatment, Status = h$status, stringsAsFactors = FALSE
      )
      style_status(hiq_table(df), "Status")
    })

    # ================= REPORTS ========================================
    reports_df <- reactive({
      r <- my_reports_live(); r <- r[order(r$date, decreasing = TRUE), , drop = FALSE]
      data.frame(
        `Report Name` = r$report_name, Type = r$type, Date = fmt_date(r$date),
        Doctor = doctor_name(r$doctor_id), `Result/Status` = r$status,
        check.names = FALSE, stringsAsFactors = FALSE
      )
    })

    output$reports_tbl <- DT::renderDT({
      style_status(hiq_table(reports_df(), selection = "single"), "Result/Status")
    })

    output$report_summary <- renderUI({
      sel <- input$reports_tbl_rows_selected
      if (length(sel) == 0) {
        return(div(class = "hiq-empty", "Select a report from the table to see a short demo summary."))
      }
      r <- my_reports_live(); r <- r[order(r$date, decreasing = TRUE), , drop = FALSE][sel, , drop = FALSE]
      note <- switch(r$type,
        "Blood Test"        = "A routine blood panel was processed by the laboratory.",
        "X-Ray"             = "An X-ray image was captured and stored in your record.",
        "MRI"               = "An MRI scan was performed and stored in your record.",
        "CT Scan"           = "A CT scan was performed and stored in your record.",
        "ECG"               = "An electrocardiogram trace was recorded.",
        "Laboratory Report" = "A laboratory test report is attached to your record.",
        "A report is attached to your record.")
      tagList(
        h5(r$report_name),
        p(class = "hiq-muted", paste0(r$type, " | ", fmt_date(r$date), " | Ordered by ", doctor_name(r$doctor_id))),
        p(paste("Demo summary:", note, if (r$status == "Scheduled") "This test is scheduled for the date shown."
                else "Please ask your doctor to explain the detailed results.")),
        demo_notice("This is a placeholder summary. No diagnosis is provided by this system.")
      )
    })

    # ================= MEDICINES ======================================
    output$med_timeline <- renderUI({
      d <- today_doses()
      if (is.null(d)) return(div(class = "hiq-empty", "No medicines are scheduled for today."))
      div(class = "hiq-timeline",
          lapply(seq_len(nrow(d)), function(i) {
            timeline_item(
              time_text = fmt_time12(d$time[i]),
              title = paste(d$medicine[i], d$dosage[i]),
              detail = paste(d$food[i], "-", d$slot[i]),
              icon_name = "pills",
              tone = if (d$state[i] == "next") "teal" else "blue",
              state = d$state[i],
              badge = if (d$state[i] == "next") status_badge("Next dose", "teal")
                      else if (d$state[i] == "past") status_badge("Earlier", "grey") else NULL
            )
          }))
    })

    output$med_tbl <- DT::renderDT({
      yes_no <- function(x) ifelse(x, "✔", "—")
      mm <- all_meds()
      df <- data.frame(
        Medicine = mm$medicine, Dosage = mm$dosage,
        Morning = yes_no(mm$morning), Afternoon = yes_no(mm$afternoon),
        Night = yes_no(mm$night), `Before/After Food` = mm$food,
        `Start Date` = fmt_date(mm$start_date), `End Date` = fmt_date(mm$end_date),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      hiq_table(df) %>%
        DT::formatStyle(c("Morning", "Afternoon", "Night"), textAlign = "center", color = "#12A8A3", fontWeight = "700")
    })

    # ================= APPOINTMENTS ===================================
    output$appt_hero <- renderUI({
      up <- upcoming_df()
      up <- up[up$Status == "Scheduled", , drop = FALSE]
      if (nrow(up) == 0) return(NULL)
      div(class = "hiq-hero fade-in",
          div(class = "hiq-hero-icon", icon("calendar-check")),
          div(div(class = "hiq-hero-label", "Upcoming appointment"),
              div(class = "hiq-hero-title", paste(fmt_date(up$date[1]), "at", up$Time[1])),
              div(class = "hiq-hero-sub", paste(up$Doctor[1], "-", up$Department[1], "-", up$Status[1]))))
    })

    output$upcoming_tbl <- DT::renderDT({
      u <- upcoming_df()
      df <- data.frame(Doctor = u$Doctor, Department = u$Department, Date = fmt_date(u$date),
                       Time = u$Time, Status = u$Status, stringsAsFactors = FALSE)
      style_status(hiq_table(df, page_length = 5), "Status")
    })

    output$past_tbl <- DT::renderDT({
      p <- my_appts[my_appts$date < today | my_appts$status != "Scheduled", , drop = FALSE]
      p <- p[order(p$date, decreasing = TRUE), , drop = FALSE]
      df <- data.frame(Date = fmt_date(p$date), Time = fmt_time12(p$time), Doctor = doctor_name(p$doctor_id),
                       Department = p$department, Reason = p$reason, Status = p$status,
                       stringsAsFactors = FALSE)
      style_status(hiq_table(df, page_length = 6), "Status")
    })

    # ---- Request appointment form (with validation) -------------------
    req_msg <- reactiveVal(NULL)
    output$req_msg <- renderUI(req_msg())

    consultation_fee <- 500
    registration_fee <- 200
    output$pay_summary <- renderUI({
      tagList(
        info_row("Consultation Fee", paste0("$", consultation_fee)),
        info_row("Registration Fee", paste0("$", registration_fee)),
        info_row("Total Amount", paste0("$", consultation_fee + registration_fee)),
        p(class = "hiq-muted", "Payment status is stored with your appointment (Pending / Paid).")
      )
    })
    pay_status <- reactiveVal("Pending")
    observeEvent(input$pay_now, {
      req(input$pay_now > 0)
      pay_status("Paid")
      req_msg(div(class = "hiq-form-msg msg-ok", "Payment recorded as Paid. Click Book Appointment to save."))
    })


    observeEvent(input$req_submit, {
      req(input$req_submit > 0)
      problems <- character(0)

      d <- input$req_date
      if (is.null(d) || length(d) == 0 || is.na(d)) {
        problems <- c(problems, "Please choose a preferred date.")
      } else if (as.Date(d) <= today) {
        problems <- c(problems, "Please choose a date from tomorrow onwards.")
      }
      reason <- trimws(if (is.null(input$req_reason)) "" else input$req_reason)
      if (nchar(reason) == 0) problems <- c(problems, "Please enter a reason for your visit.")
      if (is.null(input$req_dept) || input$req_dept == "") problems <- c(problems, "Please select a department.")
      if (is.null(input$req_doctor) || input$req_doctor == "") problems <- c(problems, "Please select a doctor.")

      if (length(problems) == 0) {
        dup <- my_requests()
        if (nrow(dup) > 0 && any(dup$date == as.Date(d) & dup$time == input$req_time & dup$department == input$req_dept)) {
          problems <- c(problems, "You have already requested this date, time and department.")
        }
      }

      if (length(problems) > 0) {
        req_msg(div(class = "hiq-form-msg msg-error", lapply(problems, function(x) div(x))))
        return()
      }

      did_sel <- if (!is.null(input$req_doctor) && input$req_doctor != "") input$req_doctor else {
        if (nrow(doc) > 0 && input$req_dept == doc$department[1]) doc$doctor_id[1] else
          demo$doctors$doctor_id[match(input$req_dept, demo$doctors$department)]
      }

      if (!"payment_status" %in% names(shared$requests)) {
        shared$requests$payment_status <- rep("Pending", nrow(shared$requests))
      }
      new_row <- data.frame(
        request_id = paste0("REQ", nrow(shared$requests) + 1), patient_id = pid, patient_name = pat$name,
        doctor_id = did_sel, department = input$req_dept, date = as.Date(d), time = input$req_time,
        reason = reason, status = "Requested",
        payment_status = pay_status(),
        stringsAsFactors = FALSE
      )
      for (mc in setdiff(names(shared$requests), names(new_row))) new_row[[mc]] <- NA
      shared$requests <- rbind(shared$requests, new_row[, names(shared$requests), drop = FALSE])
      pay_status("Pending")
      shared$activity <- rbind(shared$activity, data.frame(
        time = Sys.time(), actor = pat$name,
        action = paste0("Patient appointment requested (", pid, " - ", input$req_dept, ")"),
        icon = "calendar-plus", stringsAsFactors = FALSE))

      push_notification(shared, "Doctor", did_sel,
                        "New Appointment Request",
                        paste(pat$name, "requested", fmt_date(as.Date(d)), "at", fmt_time12(input$req_time)), "warning")
      push_notification(shared, "Administrator", "ADMIN",
                        "New Appointment Request",
                        paste(pat$name, "(", pid, ") -", input$req_dept), "info")

      updateTextAreaInput(session, "req_reason", value = "")
      req_msg(div(class = "hiq-form-msg msg-ok",
                  "Appointment booked for ", fmt_date(as.Date(d)), " at ", fmt_time12(input$req_time),
                  ". Status: Requested."))
    })

    # ================= HEALTH TRENDS ==================================
    output$weight_plot <- plotly::renderPlotly({
      plotly::plot_ly(my_health, x = ~date, y = ~weight, type = "scatter", mode = "lines+markers",
                      line = list(color = HIQ_COL$teal, width = 3), marker = list(color = HIQ_COL$teal, size = 8),
                      hovertemplate = "%{x|%d %b %Y}<br>Weight: %{y} kg<extra></extra>") %>%
        hiq_plot(ylab = "kg")
    })

    output$bp_plot <- plotly::renderPlotly({
      plotly::plot_ly(my_health, x = ~date) %>%
        plotly::add_trace(y = ~systolic, name = "Systolic", type = "scatter", mode = "lines+markers",
                          line = list(color = HIQ_COL$red, width = 3), marker = list(color = HIQ_COL$red, size = 7),
                          hovertemplate = "%{x|%d %b %Y}<br>Systolic: %{y} mmHg<extra></extra>") %>%
        plotly::add_trace(y = ~diastolic, name = "Diastolic", type = "scatter", mode = "lines+markers",
                          line = list(color = HIQ_COL$blue, width = 3), marker = list(color = HIQ_COL$blue, size = 7),
                          hovertemplate = "%{x|%d %b %Y}<br>Diastolic: %{y} mmHg<extra></extra>") %>%
        hiq_plot(ylab = "mmHg", legend = TRUE)
    })

    output$hr_plot <- plotly::renderPlotly({
      plotly::plot_ly(my_health, x = ~date, y = ~heart_rate, type = "scatter", mode = "lines+markers",
                      line = list(color = HIQ_COL$purple, width = 3), marker = list(color = HIQ_COL$purple, size = 8),
                      hovertemplate = "%{x|%d %b %Y}<br>Heart rate: %{y} bpm<extra></extra>") %>%
        hiq_plot(ylab = "beats per minute")
    })

    # ================= PROFILE ========================================
    output$profile_card <- renderUI({
      tagList(
        info_row("Name", pat$name), info_row("Patient ID", pat$patient_id),
        info_row("Age", pat$age), info_row("Gender", pat$gender),
        info_row("Blood Group", pat$blood_group),
        info_row("Assigned Doctor", paste0(doc$name, " (", doc$specialization, ")")),
        info_row("Current Status", pat$status)
      )
    })
    output$emergency_profile <- renderUI(emergency_card())
  })
}
