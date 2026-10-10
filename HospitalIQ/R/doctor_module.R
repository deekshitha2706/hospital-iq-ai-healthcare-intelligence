# =====================================================================
# doctor_module.R
# ---------------------------------------------------------------------
# The DOCTOR WORKSPACE.
#
# PRIVACY RULE used throughout this file:
#   Every table is filtered with  doctor_id == did  where `did` is the
#   ID of the logged-in doctor, so Dr. Meera Nair (D001) only sees her
#   own schedule, surgeries, meetings and research, and Dr. Arjun Menon
#   (D002) only sees his. Doctors do not see hospital finances.
#
# DEMONSTRATION ONLY - all data is synthetic.
# =====================================================================

doctor_nav_items <- function() {
  list(
    list(id = "overview",  label = "Overview",            icon = "home"),
    list(id = "schedule",  label = "Schedule",            icon = "clock"),
    list(id = "appts",     label = "Appointments",        icon = "calendar-check"),
    list(id = "prescribe", label = "Prescriptions",       icon = "prescription"),
    list(id = "reports",   label = "Patient Reports",     icon = "file-medical"),
    list(id = "surgeries", label = "Surgeries",           icon = "procedures"),
    list(id = "meetings",  label = "Meetings",            icon = "users"),
    list(id = "research",  label = "Research & Seminars", icon = "microscope"),
    list(id = "analytics", label = "Analytics",           icon = "chart-line"),
    list(id = "profile",   label = "Profile",             icon = "id-card")
  )
}

# Icon and colour used for every kind of schedule entry
schedule_style <- function(type) {
  switch(type,
         "Consultation" = list(icon = "stethoscope",       tone = "teal"),
         "Surgery"      = list(icon = "procedures",        tone = "red"),
         "Meeting"      = list(icon = "users",             tone = "blue"),
         "Seminar"      = list(icon = "chalkboard-teacher", tone = "purple"),
         "Research"     = list(icon = "microscope",        tone = "navy"),
         "Break"        = list(icon = "coffee",            tone = "amber"),
         list(icon = "circle", tone = "blue"))
}

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------
doctor_ui <- function(id, user) {
  ns <- NS(id)
  hiq_shell(
    hiq_sidebar(ns, "Doctor Workspace", user$display_name,
                paste("Doctor ID:", user$user_id), doctor_nav_items(), "overview"),
    uiOutput(ns("topbar")),
    tabsetPanel(
      id = ns("tabs"), type = "hidden",

      tabPanelBody("overview", uiOutput(ns("overview_ui"))),

      tabPanelBody("schedule",
        page_header("Daily Schedule", "Your timeline for today"),
        hiq_card("Today's Timeline", subtitle = "Consultations, surgeries, meetings, seminars and research",
                 uiOutput(ns("timeline_ui"))),
        hiq_card("Edit / Cancel Schedule Entry",
          subtitle = "Select a row, change time or cancel",
          DT::DTOutput(ns("sched_edit_tbl")),
          br(),
          div(class = "hiq-input-grid",
              selectInput(ns("se_time"), "New Time",
                          choices = c("08:00","08:30","09:00","09:30","10:00","10:30","11:00","11:30",
                                      "12:00","13:00","14:00","14:30","15:00","15:30","16:00","16:30","17:00")),
              textInput(ns("se_detail"), "Detail / Note (optional)")
          ),
          div(style = "display:flex; gap:10px; flex-wrap:wrap;",
              actionButton(ns("se_update"), "Edit Schedule", icon = icon("edit"), class = "hiq-btn"),
              actionButton(ns("se_cancel"), "Cancel Entry / Meeting", icon = icon("times"), class = "hiq-btn")
          ),
          uiOutput(ns("se_msg"))
        )
      ),

      tabPanelBody("appts",
        page_header("Appointments", "Today's patient queue"),
        uiOutput(ns("queue_summary")),
        hiq_card("Today's Patient Queue", uiOutput(ns("queue_ui"))),
        hiq_card("Upcoming Appointments", DT::DTOutput(ns("future_appts_tbl")))),

      tabPanelBody("prescribe",
        page_header("Create Prescription", "Write a prescription for an assigned patient"),
        hiq_card("New Prescription",
          div(class = "hiq-input-grid",
              selectInput(ns("pr_patient"), "Patient", choices = NULL),
              textInput(ns("pr_medicine"), "Medicine"),
              textInput(ns("pr_dosage"), "Dosage", placeholder = "e.g. 500 mg"),
              checkboxInput(ns("pr_morning"), "Morning", TRUE),
              checkboxInput(ns("pr_afternoon"), "Afternoon", FALSE),
              checkboxInput(ns("pr_night"), "Night", TRUE),
              numericInput(ns("pr_duration"), "Duration (days)", value = 30, min = 1, max = 365)
          ),
          actionButton(ns("pr_submit"), "Save Prescription", icon = icon("save"), class = "hiq-btn"),
          uiOutput(ns("pr_msg"))
        ),
        hiq_card("My Recent Prescriptions", DT::DTOutput(ns("my_rx_tbl")))
      ),

      tabPanelBody("reports",
        page_header("Patient Reports", "Click a Patient ID to view their reports"),
        hiq_card("Assigned Patients",
                 subtitle = "Select a patient ID",
                 DT::DTOutput(ns("doc_patient_ids_tbl"))),
        hiq_card("Patient Reports",
                 subtitle = "Report Name, Date, Status, Summary, PDF",
                 uiOutput(ns("doc_report_detail")))
      ),

      tabPanelBody("surgeries",
        page_header("Surgery Schedule", "Operations assigned to you"),
        hiq_card("My Surgeries", DT::DTOutput(ns("surgery_tbl")))),

      tabPanelBody("meetings",
        page_header("Meetings", "Department, administration, research and committee meetings"),
        hiq_card("My Meetings", DT::DTOutput(ns("meeting_tbl")))),

      tabPanelBody("research",
        page_header("Research & Academic Activity", "Papers, publications, conferences, seminars and workshops"),
        hiq_card("Research & Academic Activity", DT::DTOutput(ns("research_tbl"))),
        hiq_card("Upcoming Academic Events", DT::DTOutput(ns("academic_tbl")))),

      tabPanelBody("analytics",
        page_header("Workload Analytics", "Synthetic history of your clinical and academic workload"),
        card_grid(min = 420,
          hiq_card("Appointments by Day", plotly::plotlyOutput(ns("weekday_plot"), height = "280px"), class = "fade-in"),
          hiq_card("Surgeries by Month", plotly::plotlyOutput(ns("month_plot"), height = "280px"), class = "fade-in"),
          hiq_card("Consultation Distribution", plotly::plotlyOutput(ns("consult_plot"), height = "280px"), class = "fade-in"),
          hiq_card("Workload Distribution (hours per week)", plotly::plotlyOutput(ns("work_plot"), height = "280px"), class = "fade-in"))),

      tabPanelBody("profile",
        page_header("My Profile", "Your professional details"),
        hiq_card("Doctor Details", uiOutput(ns("profile_card"))))
    )
  )
}

# ---------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------
doctor_server <- function(id, user, demo, shared) {
  moduleServer(id, function(input, output, session) {

    setup_navigation(input, session, vapply(doctor_nav_items(), function(x) x$id, character(1)))

    # ---- Who is logged in? ------------------------------------------
    did   <- user$user_id
    today <- demo$today
    doc   <- demo$doctors[demo$doctors$doctor_id == did, , drop = FALSE]
    patient_name <- function(ids) demo$patients$name[match(ids, demo$patients$patient_id)]

    # Only THIS doctor's rows are copied from the demo tables
    my_appts    <- demo$appointments[demo$appointments$doctor_id == did, , drop = FALSE]
    my_meetings <- demo$meetings[demo$meetings$doctor_id == did, , drop = FALSE]
    my_sched    <- demo$schedule[demo$schedule$doctor_id == did, , drop = FALSE]
    my_sched    <- my_sched[order(my_sched$time), , drop = FALSE]
    my_research <- demo$research[demo$research$doctor_id == did, , drop = FALSE]
    my_academic <- demo$academic_events[demo$academic_events$doctor_id == did, , drop = FALSE]
    my_surg_raw <- demo$surgeries[demo$surgeries$doctor_id == did, , drop = FALSE]

    my_patients_live <- reactive({
      shared$patients[shared$patients$doctor_id == did, , drop = FALSE]
    })

    observe({
      p <- my_patients_live()
      updateSelectInput(session, "pr_patient",
                        choices = setNames(p$patient_id, paste0(p$name, " (", p$patient_id, ")")))
    })

    pr_msg <- reactiveVal(NULL)
    output$pr_msg <- renderUI(pr_msg())

    observeEvent(input$pr_submit, {
      req(input$pr_submit > 0)
      if (is.null(input$pr_patient) || input$pr_patient == "") {
        pr_msg(div(class = "hiq-form-msg msg-error", "Select a patient."))
        return()
      }
      med <- trimws(if (is.null(input$pr_medicine)) "" else input$pr_medicine)
      if (med == "") {
        pr_msg(div(class = "hiq-form-msg msg-error", "Medicine name is required."))
        return()
      }
      start <- Sys.Date()
      dur <- as.integer(if (is.null(input$pr_duration)) 30 else input$pr_duration)
      new_rx <- data.frame(
        prescription_id = paste0("RX", nrow(shared$prescriptions) + 1),
        patient_id = input$pr_patient, doctor_id = did,
        medicine = med, dosage = trimws(if (is.null(input$pr_dosage)) "" else input$pr_dosage),
        morning = isTRUE(input$pr_morning), afternoon = isTRUE(input$pr_afternoon),
        night = isTRUE(input$pr_night),
        duration_days = dur,
        start_date = start, end_date = start + dur,
        created_at = Sys.time(), stringsAsFactors = FALSE
      )
      shared$prescriptions <- rbind(shared$prescriptions, new_rx)
      push_notification(shared, "Patient", input$pr_patient,
                        "New Prescription", paste(med, if (is.null(input$pr_dosage)) "" else input$pr_dosage), "info")
      pr_msg(div(class = "hiq-form-msg msg-ok", "Prescription saved and sent to patient."))
    })

    output$my_rx_tbl <- DT::renderDT({
      rx <- shared$prescriptions[shared$prescriptions$doctor_id == did, , drop = FALSE]
      if (nrow(rx) == 0) return(hiq_table(data.frame(Message = "No prescriptions yet")))
      rx <- rx[order(rx$created_at, decreasing = TRUE), ]
      df <- data.frame(
        Patient = rx$patient_id, Medicine = rx$medicine, Dosage = rx$dosage,
        Morning = ifelse(rx$morning, "✔", "—"),
        Afternoon = ifelse(rx$afternoon, "✔", "—"),
        Night = ifelse(rx$night, "✔", "—"),
        Days = rx$duration_days, From = fmt_date(rx$start_date),
        stringsAsFactors = FALSE
      )
      hiq_table(df, page_length = 6)
    })

    output$doc_patient_ids_tbl <- DT::renderDT({
      p <- my_patients_live()
      if (nrow(p) == 0) return(hiq_table(data.frame(`Patient ID` = character(0), check.names = FALSE)))
      df <- data.frame(`Patient ID` = p$patient_id, Name = p$name, Status = p$status,
                       check.names = FALSE, stringsAsFactors = FALSE)
      hiq_table(df, page_length = 8, selection = "single")
    })

    output$doc_report_detail <- renderUI({
      sel <- input$doc_patient_ids_tbl_rows_selected
      if (length(sel) == 0) {
        return(div(class = "hiq-empty", "Select a Patient ID above to view reports."))
      }
      pid_sel <- my_patients_live()$patient_id[sel]
      r <- shared$reports[shared$reports$patient_id == pid_sel, , drop = FALSE]
      r <- r[order(r$date, decreasing = TRUE), , drop = FALSE]
      if (nrow(r) == 0) return(div(class = "hiq-empty", paste("No reports for", pid_sel)))
      tagList(lapply(seq_len(nrow(r)), function(i) {
        pdf_link <- if ("pdf_file" %in% names(r) && !is.na(r$pdf_file[i]) && nzchar(as.character(r$pdf_file[i]))) {
          tags$a(href = paste0("reports/", r$pdf_file[i]), target = "_blank",
                 icon("file-pdf"), " View PDF")
        } else {
          span(class = "hiq-muted", "No PDF uploaded")
        }
        summary_txt <- if ("summary" %in% names(r) && !is.na(r$summary[i]) && nzchar(as.character(r$summary[i]))) {
          r$summary[i]
        } else {
          "No summary provided."
        }
        div(class = "hiq-card", style = "margin-bottom:12px;",
            div(class = "hiq-card-body",
                h5(r$report_name[i], status_badge(r$status[i], status_tone(r$status[i]))),
                p(class = "hiq-muted", paste(r$type[i], "|", fmt_date(r$date[i]))),
                p(strong("Summary: "), summary_txt),
                pdf_link
            ))
      }))
    })


    clock <- reactive({
      invalidateLater(60000, session)
      Sys.time()
    })

    # ---- Reactive tables that depend on the clock ---------------------
    queue <- reactive({
      q <- my_appts[my_appts$date == today, , drop = FALSE]
      q <- q[order(q$time), , drop = FALSE]
      queue_status_now(q, clock())
    })

    surgeries <- reactive({
      s <- my_surg_raw[order(my_surg_raw$date, my_surg_raw$time), , drop = FALSE]
      surgery_status_now(s, clock())
    })

    availability <- reactive({
      s <- surgeries()
      q <- queue()
      if (any(s$status == "In Surgery")) "In Surgery"
      else if (nrow(q) > 0 && any(q$queue_status == "In Consultation")) "In Consultation"
      else "Available"
    })


    my_sched_live <- reactive({
      base <- shared$schedule[shared$schedule$doctor_id == did, , drop = FALSE]
      edits <- shared$schedule_edits
      if (!is.null(edits) && nrow(edits) > 0) {
        canc <- edits[edits$doctor_id == did & edits$cancelled == TRUE, , drop = FALSE]
        if (nrow(canc) > 0) {
          key <- paste(base$time, base$title)
          ckey <- paste(canc$time, canc$title)
          base <- base[!key %in% ckey, , drop = FALSE]
        }
        chg <- edits[edits$doctor_id == did & edits$cancelled == FALSE, , drop = FALSE]
        if (nrow(chg) > 0) {
          for (i in seq_len(nrow(chg))) {
            hit <- which(base$title == chg$title[i])
            if (length(hit) > 0) {
              base$time[hit[1]] <- chg$time[i]
              if (nzchar(as.character(chg$detail[i]))) base$detail[hit[1]] <- chg$detail[i]
            }
          }
        }
      }
      if (nrow(base) == 0) return(base)
      base[order(base$time), , drop = FALSE]
    })

    timeline <- reactive({
      s <- my_sched_live()
      if (nrow(s) == 0) return(s)
      s$mins <- minutes_until(to_datetime(today, s$time), clock())
      s$state <- ifelse(s$mins < -30, "past", "later")
      first_later <- which(s$mins >= -30)[1]
      if (!is.na(first_later)) s$state[first_later] <- "next"
      s
    })

    se_msg <- reactiveVal(NULL)
    output$se_msg <- renderUI(se_msg())

    output$sched_edit_tbl <- DT::renderDT({
      s <- my_sched_live()
      if (nrow(s) == 0) return(hiq_table(data.frame(Message = "No schedule entries")))
      df <- data.frame(Time = fmt_time12(s$time), Title = s$title, Type = s$type,
                       Detail = s$detail, stringsAsFactors = FALSE)
      hiq_table(df, page_length = 8, selection = "single")
    })

    observeEvent(input$se_update, {
      req(input$se_update > 0)
      sel <- input$sched_edit_tbl_rows_selected
      if (length(sel) == 0) {
        se_msg(div(class = "hiq-form-msg msg-error", "Select a schedule row first."))
        return()
      }
      s <- my_sched_live()
      row <- s[sel, , drop = FALSE]
      new_time <- input$se_time
      new_detail <- trimws(if (is.null(input$se_detail)) "" else input$se_detail)
      idx <- which(shared$schedule$doctor_id == did & shared$schedule$title == row$title[1] &
                     shared$schedule$time == row$time[1])
      if (length(idx) > 0) {
        shared$schedule$time[idx[1]] <- new_time
        if (nzchar(new_detail)) shared$schedule$detail[idx[1]] <- new_detail
      }
      if (row$type[1] == "Meeting") {
        mi <- which(shared$meetings$doctor_id == did & shared$meetings$time == row$time[1])
        if (length(mi) > 0) shared$meetings$time[mi[1]] <- new_time
      }
      if (row$type[1] == "Surgery") {
        si <- which(shared$surgeries$doctor_id == did & shared$surgeries$time == row$time[1] &
                      shared$surgeries$date == today)
        if (length(si) > 0) shared$surgeries$time[si[1]] <- new_time
      }
      shared$schedule_edits <- rbind(shared$schedule_edits, data.frame(
        doctor_id = did, time = new_time, title = row$title[1], type = row$type[1],
        detail = if (nzchar(new_detail)) new_detail else as.character(row$detail[1]),
        cancelled = FALSE, stringsAsFactors = FALSE
      ))
      se_msg(div(class = "hiq-form-msg msg-ok", "Schedule updated to ", fmt_time12(new_time), "."))
    })

    observeEvent(input$se_cancel, {
      req(input$se_cancel > 0)
      sel <- input$sched_edit_tbl_rows_selected
      if (length(sel) == 0) {
        se_msg(div(class = "hiq-form-msg msg-error", "Select a schedule row first."))
        return()
      }
      s <- my_sched_live()
      row <- s[sel, , drop = FALSE]
      idx <- which(shared$schedule$doctor_id == did & shared$schedule$title == row$title[1] &
                     shared$schedule$time == row$time[1])
      if (length(idx) > 0) shared$schedule <- shared$schedule[-idx, , drop = FALSE]
      if (row$type[1] == "Meeting") {
        mi <- which(shared$meetings$doctor_id == did & shared$meetings$time == row$time[1])
        if (length(mi) > 0) shared$meetings <- shared$meetings[-mi, , drop = FALSE]
      }
      shared$schedule_edits <- rbind(shared$schedule_edits, data.frame(
        doctor_id = did, time = row$time[1], title = row$title[1], type = row$type[1],
        detail = as.character(row$detail[1]), cancelled = TRUE, stringsAsFactors = FALSE
      ))
      se_msg(div(class = "hiq-form-msg msg-ok", "Entry cancelled: ", row$title[1]))
    })


    # ---- Smart alerts ---------------------------------------------------
    alerts <- reactive({
      out <- list()
      now <- clock()

      # Surgery starting soon
      s <- surgeries()
      s_today <- s[s$date == today, , drop = FALSE]
      for (i in seq_len(nrow(s_today))) {
        mins <- minutes_until(to_datetime(s_today$date[i], s_today$time[i]), now)
        what <- paste0(s_today$procedure[i], " - ", s_today$theatre[i], " at ", fmt_time12(s_today$time[i]))
        if (s_today$status[i] == "In Surgery") {
          out[[length(out) + 1]] <- make_alert("danger", "Surgery in progress", what)
        } else if (s_today$status[i] == "Scheduled" && mins <= 180) {
          out[[length(out) + 1]] <- make_alert("warning", "Surgery starting soon", what)
        } else if (s_today$status[i] == "Scheduled") {
          out[[length(out) + 1]] <- make_alert("info", "Surgery later today", what)
        }
      }

      # Upcoming patient
      q <- queue()
      waiting <- q[q$queue_status != "Completed", , drop = FALSE]
      if (nrow(waiting) > 0) {
        mins <- minutes_until(to_datetime(waiting$date[1], waiting$time[1]), now)
        txt <- paste0("Token ", waiting$token[1], ": ", patient_name(waiting$patient_id[1]), " at ",
                      fmt_time12(waiting$time[1]), " - ", waiting$reason[1])
        out[[length(out) + 1]] <- make_alert(if (mins <= 60) "warning" else "info", "Upcoming patient", txt)
      } else if (nrow(q) > 0) {
        out[[length(out) + 1]] <- make_alert("success", "Queue complete", "All of today's consultations are done.")
      }

      # Department meeting
      m <- my_meetings[my_meetings$date >= today, , drop = FALSE]
      m <- m[order(m$date, m$time), , drop = FALSE]
      if (nrow(m) > 0) {
        mins <- minutes_until(to_datetime(m$date[1], m$time[1]), now)
        out[[length(out) + 1]] <- make_alert(if (mins <= 120) "warning" else "info",
          paste("Meeting", days_label(m$date[1], today)),
          paste0(m$meeting[1], " at ", fmt_time12(m$time[1]), " - ", m$location[1]))
      }

      # Seminar reminder
      a <- my_academic[my_academic$date >= today, , drop = FALSE]
      a <- a[order(a$date, a$time), , drop = FALSE]
      if (nrow(a) > 0) {
        out[[length(out) + 1]] <- make_alert("info", paste("Academic reminder", days_label(a$date[1], today)),
          paste0(a$event[1], " (", a$type[1], ") at ", fmt_time12(a$time[1]), " - ", a$location[1]))
      }
      out
    })

    output$topbar <- renderUI(hiq_topbar(count_urgent(alerts())))

    # ================= OVERVIEW =======================================
    output$overview_ui <- renderUI({
      q <- queue()
      s <- surgeries()
      m <- my_meetings[my_meetings$date >= today, , drop = FALSE]
      m <- m[order(m$date, m$time), , drop = FALSE]
      tl <- timeline()
      nxt <- tl[tl$state %in% c("next", "later"), , drop = FALSE]
      nxt <- head(nxt, 3)

      avail <- availability()
      avail_tone <- switch(avail, "Available" = "green", "In Consultation" = "teal", "red")

      tagList(
        div(class = "hiq-welcome fade-in",
            div(h2(paste("Welcome,", doc$name)),
                p(paste(doc$specialization, "-", doc$department, "Department"))),
            div(class = "hiq-chips",
                span(class = "hiq-chip", icon("id-card"), paste("ID:", doc$doctor_id)),
                span(class = "hiq-chip", icon("stethoscope"), doc$specialization),
                span(class = "hiq-chip", icon("building"), doc$department),
                span(class = "hiq-chip", icon("award"), paste(doc$experience, "years experience")),
                span(class = paste("hiq-chip chip-avail", paste0("avail-", avail_tone)),
                     icon("circle"), avail))),

        kpi_grid(
          kpi_card("Today's Appointments", nrow(q), "calendar-check",
                   paste(sum(q$queue_status == "Completed"), "completed"), "teal"),
          kpi_card("Today's Surgeries", sum(s$date == today), "procedures",
                   paste(sum(s$date == today & s$status == "Completed"), "completed"), "red"),
          kpi_card("Pending Consultations", sum(q$queue_status != "Completed"), "user-clock",
                   "Waiting or in consultation", "amber"),
          kpi_card("Upcoming Meetings", nrow(m), "users",
                   if (nrow(m) > 0) paste("Next:", fmt_date(m$date[1])) else "None scheduled", "blue")
        ),

        card_grid(min = 380,
          hiq_card("Smart Alerts", subtitle = "Generated from your schedule", render_alerts(alerts())),
          hiq_card("Notification Center", render_notifications(shared, "Doctor", did)),
          hiq_card("Coming Up Next", subtitle = "From today's timeline",
            if (nrow(nxt) == 0) div(class = "hiq-empty", "Nothing else scheduled for today.") else
              div(class = "hiq-timeline",
                  lapply(seq_len(nrow(nxt)), function(i) {
                    st <- schedule_style(nxt$type[i])
                    timeline_item(fmt_time12(nxt$time[i]), nxt$title[i], nxt$detail[i],
                                  st$icon, st$tone, nxt$state[i])
                  })))
        )
      )
    })

    # ================= SCHEDULE =======================================
    output$timeline_ui <- renderUI({
      tl <- timeline()
      div(class = "hiq-timeline",
          lapply(seq_len(nrow(tl)), function(i) {
            st <- schedule_style(tl$type[i])
            timeline_item(
              fmt_time12(tl$time[i]), tl$title[i], tl$detail[i], st$icon, st$tone, tl$state[i],
              badge = status_badge(tl$type[i], switch(st$tone, red = "red", teal = "teal", blue = "blue",
                                                       purple = "purple", amber = "amber", "grey")))
          }))
    })

    # ================= APPOINTMENTS ===================================
    output$queue_summary <- renderUI({
      q <- queue()
      kpi_grid(
        kpi_card("Waiting", sum(q$queue_status == "Waiting"), "hourglass-half", NULL, "amber"),
        kpi_card("In Consultation", sum(q$queue_status == "In Consultation"), "stethoscope", NULL, "teal"),
        kpi_card("Completed", sum(q$queue_status == "Completed"), "check-circle", NULL, "green")
      )
    })

    output$queue_ui <- renderUI({
      q <- queue()
      if (nrow(q) == 0) return(div(class = "hiq-empty", "No appointments scheduled for today."))
      dot <- function(st) switch(st, "Waiting" = "dot-amber", "In Consultation" = "dot-teal", "dot-green")
      tags$table(
        class = "hiq-queue",
        tags$thead(tags$tr(lapply(c("Token", "Patient", "Time", "Reason", "Status"), tags$th))),
        tags$tbody(lapply(seq_len(nrow(q)), function(i) {
          tags$tr(
            tags$td(class = "q-token", q$token[i]),
            tags$td(paste0(patient_name(q$patient_id[i]), " (", q$patient_id[i], ")")),
            tags$td(fmt_time12(q$time[i])),
            tags$td(q$reason[i]),
            tags$td(span(class = paste("hiq-dot", dot(q$queue_status[i]))), q$queue_status[i])
          )
        }))
      )
    })

    output$future_appts_tbl <- DT::renderDT({
      f <- my_appts[my_appts$date > today & my_appts$status == "Scheduled", , drop = FALSE]
      f <- f[order(f$date, f$time), , drop = FALSE]
      df <- data.frame(Date = fmt_date(f$date), Time = fmt_time12(f$time),
                       Patient = paste0(patient_name(f$patient_id), " (", f$patient_id, ")"),
                       Reason = f$reason, Status = f$status, stringsAsFactors = FALSE)
      style_status(hiq_table(df, page_length = 5), "Status")
    })

    # ================= SURGERIES ======================================
    output$surgery_tbl <- DT::renderDT({
      s <- surgeries()
      df <- data.frame(
        `Surgery ID` = s$surgery_id, `Patient ID` = s$patient_id, Procedure = s$procedure,
        Date = fmt_date(s$date), Time = fmt_time12(s$time), `Operation Theatre` = s$theatre,
        `Assistant Doctor` = s$assistant, Status = s$status,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      style_status(hiq_table(df), "Status")
    })

    # ================= MEETINGS =======================================
    output$meeting_tbl <- DT::renderDT({
      m <- my_meetings[order(my_meetings$date, my_meetings$time), , drop = FALSE]
      df <- data.frame(Meeting = m$meeting, Date = fmt_date(m$date), Time = fmt_time12(m$time),
                       Location = m$location, Priority = m$priority, stringsAsFactors = FALSE)
      style_status(hiq_table(df), "Priority")
    })

    # ================= RESEARCH & SEMINARS ============================
    output$research_tbl <- DT::renderDT({
      r <- my_research[order(my_research$date, decreasing = TRUE), , drop = FALSE]
      df <- data.frame(Title = r$title, Type = r$type, Date = fmt_date(r$date), Status = r$status,
                       stringsAsFactors = FALSE)
      style_status(hiq_table(df), "Status")
    })

    output$academic_tbl <- DT::renderDT({
      a <- my_academic[my_academic$date >= today, , drop = FALSE]
      a <- a[order(a$date, a$time), , drop = FALSE]
      df <- data.frame(Event = a$event, Type = a$type, Date = fmt_date(a$date), Time = fmt_time12(a$time),
                       Location = a$location, stringsAsFactors = FALSE)
      hiq_table(df, page_length = 5)
    })

    # ================= ANALYTICS ======================================
    pal <- unname(unlist(HIQ_COL[c("teal", "blue", "navy", "amber", "purple")]))

    output$weekday_plot <- plotly::renderPlotly({
      w <- demo$weekday_stats[demo$weekday_stats$doctor_id == did, , drop = FALSE]
      plotly::plot_ly(w, x = ~weekday, y = ~appointments, type = "bar",
                      marker = list(color = HIQ_COL$teal),
                      text = ~appointments, textposition = "outside",
                      hovertemplate = "%{x}<br>Appointments: %{y}<extra></extra>") %>%
        hiq_plot(ylab = "Appointments per week")
    })

    output$month_plot <- plotly::renderPlotly({
      ms <- demo$month_stats[demo$month_stats$doctor_id == did, , drop = FALSE]
      ms <- ms[order(ms$month_order), , drop = FALSE]
      ms$month <- factor(ms$month, levels = ms$month)
      plotly::plot_ly(ms, x = ~month, y = ~surgeries, type = "bar",
                      marker = list(color = HIQ_COL$red),
                      text = ~surgeries, textposition = "outside",
                      hovertemplate = "%{x}<br>Surgeries: %{y}<extra></extra>") %>%
        hiq_plot(ylab = "Surgeries")
    })

    output$consult_plot <- plotly::renderPlotly({
      cs <- demo$consult_stats[demo$consult_stats$doctor_id == did, , drop = FALSE]
      plotly::plot_ly(cs, labels = ~type, values = ~count, type = "pie", hole = 0.55,
                      textinfo = "label+percent", marker = list(colors = pal),
                      hovertemplate = "%{label}<br>%{value} consultations (%{percent})<extra></extra>") %>%
        plotly::layout(showlegend = FALSE, paper_bgcolor = "rgba(0,0,0,0)",
                       margin = list(t = 10, b = 10, l = 10, r = 10)) %>%
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    })

    output$work_plot <- plotly::renderPlotly({
      ws <- demo$workload_stats[demo$workload_stats$doctor_id == did, , drop = FALSE]
      ws <- ws[order(ws$hours), , drop = FALSE]
      ws$category <- factor(ws$category, levels = ws$category)
      plotly::plot_ly(ws, x = ~hours, y = ~category, type = "bar", orientation = "h",
                      marker = list(color = HIQ_COL$blue),
                      text = ~paste(hours, "h"), textposition = "outside",
                      hovertemplate = "%{y}<br>%{x} hours per week<extra></extra>") %>%
        hiq_plot(xlab = "Hours per week") %>%
        plotly::layout(margin = list(l = 140, t = 20, b = 55, r = 40))
    })

    # ================= PROFILE ========================================
    output$profile_card <- renderUI({
      tagList(
        info_row("Name", doc$name), info_row("Doctor ID", doc$doctor_id),
        info_row("Specialization", doc$specialization), info_row("Department", doc$department),
        info_row("Experience", paste(doc$experience, "years")),
        info_row("Current Availability", availability()),
        info_row("Assigned Patients", sum(demo$patients$doctor_id == did))
      )
    })
  })
}
