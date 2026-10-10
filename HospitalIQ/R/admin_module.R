# =====================================================================
# admin_module.R
# ---------------------------------------------------------------------
# The HOSPITAL ADMINISTRATOR dashboard.
#
# This module keeps EVERY feature of the original Hospital IQ project:
#   KPIs, Hospital Health Score, Department pie chart, Bed Status,
#   Staff Availability, Resource Utilization, Revenue Analysis,
#   Summary Table and the input validation messages.
# and adds executive KPIs, bed management, department analytics,
# staff roster, a combined hospital schedule, financial history,
# a smart alert centre and an activity log.
#
# Financial, bed, ICU, staffing and department information exists ONLY
# in this module, so Patients and Doctors can never see it.
#
# DEMONSTRATION ONLY - all data is synthetic.
# =====================================================================

admin_nav_items <- function() {
  list(
    list(id = "exec",       label = "Executive Dashboard", icon = "tachometer-alt"),
    list(id = "patients",   label = "Patient Analytics",   icon = "user-injured"),
    list(id = "patmgmt",    label = "Patient Management",  icon = "user-plus"),
    list(id = "beds",       label = "Room Management",     icon = "door-open"),
    list(id = "depts",      label = "Departments",         icon = "building"),
    list(id = "staff",      label = "Doctors & Staff",     icon = "user-md"),
    list(id = "schedule",   label = "Hospital Schedule",   icon = "calendar-alt"),
    list(id = "finance",    label = "Financial Analytics", icon = "dollar-sign"),
    list(id = "rptmgmt",    label = "Reports Management",  icon = "file-medical"),
    list(id = "apptmgmt",   label = "Appointments",        icon = "calendar-check"),
    list(id = "alerts",     label = "Alerts",              icon = "bell")
  )
}

# Starting values for the data panel (same defaults as the original project)
admin_defaults <- function() {
  list(total_beds = 500, occupied_beds = 300, icu_beds = 50, occupied_icu = 30,
       total_patients = 420, doctors = 85, nurses = 240,
       emergency = 120, cardiology = 80, neurology = 60, orthopedics = 70, pediatrics = 90,
       revenue_pp = 1500)
}

# The input form (static UI, so typing never loses focus)
admin_inputs_card <- function(ns) {
  d <- admin_defaults()
  hiq_card(
    "Hospital Data Inputs",
    subtitle = "Enter the current figures, then click Update Dashboard",
    div(class = "hiq-input-grid",
        div(class = "hiq-input-group",
            h5("Hospital Information"),
            numericInput(ns("total_beds"), "Total Beds", d$total_beds, min = 0, step = 1),
            numericInput(ns("occupied_beds"), "Occupied Beds", d$occupied_beds, min = 0, step = 1),
            numericInput(ns("icu_beds"), "ICU Beds", d$icu_beds, min = 0, step = 1),
            numericInput(ns("occupied_icu"), "Occupied ICU Beds", d$occupied_icu, min = 0, step = 1),
            numericInput(ns("total_patients"), "Total Patients", d$total_patients, min = 0, step = 1)),
        div(class = "hiq-input-group",
            h5("Staff Information"),
            numericInput(ns("doctors"), "Doctors Available", d$doctors, min = 0, step = 1),
            numericInput(ns("nurses"), "Nurses Available", d$nurses, min = 0, step = 1),
            h5("Financial Information"),
            numericInput(ns("revenue_pp"), "Average Revenue Per Patient", d$revenue_pp, min = 0, step = 50)),
        div(class = "hiq-input-group",
            h5("Department Patients"),
            numericInput(ns("emergency"), "Emergency Patients", d$emergency, min = 0, step = 1),
            numericInput(ns("cardiology"), "Cardiology Patients", d$cardiology, min = 0, step = 1),
            numericInput(ns("neurology"), "Neurology Patients", d$neurology, min = 0, step = 1),
            numericInput(ns("orthopedics"), "Orthopedics Patients", d$orthopedics, min = 0, step = 1),
            numericInput(ns("pediatrics"), "Pediatrics Patients", d$pediatrics, min = 0, step = 1))),
    actionButton(ns("update"), "Update Dashboard", icon = icon("sync"), class = "hiq-btn")
  )
}

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------
admin_ui <- function(id, user) {
  ns <- NS(id)
  hiq_shell(
    hiq_sidebar(ns, "Administrator", user$display_name, "Hospital-wide access",
                admin_nav_items(), "exec"),
    tagList(uiOutput(ns("topbar")), uiOutput(ns("warnings"))),
    tabsetPanel(
      id = ns("tabs"), type = "hidden",

      # ---------- Executive dashboard ----------
      tabPanelBody("exec",
        page_header("Executive Dashboard", "Hospital Overview"),
        uiOutput(ns("kpi_ui")),
        admin_inputs_card(ns),
        card_grid(min = 380,
          hiq_card("Hospital Health Score", subtitle = "Project-specific operational score", uiOutput(ns("health_card"))),
          hiq_card("Resource Utilization", plotly::plotlyOutput(ns("util_plot"), height = "300px"), class = "fade-in")),
        card_grid(min = 380,
          hiq_card("Smart Alerts", uiOutput(ns("exec_alerts"))))),

      # ---------- Patient analytics ----------
      tabPanelBody("patients",
        page_header("Patient Analytics", "Inpatients, outpatients and department distribution"),
        uiOutput(ns("patient_kpi")),
        card_grid(min = 420,
          hiq_card("Inpatients", DT::DTOutput(ns("inpatient_tbl"))),
          hiq_card("Outpatients", DT::DTOutput(ns("outpatient_tbl")))),
        card_grid(min = 420,
          hiq_card("Department Distribution", plotly::plotlyOutput(ns("dept_pie"), height = "320px"), class = "fade-in"),
          hiq_card("Summary Table", DT::DTOutput(ns("summary_tbl"))))

      ),

      tabPanelBody("beds",
        page_header("Room Management", "Hospital rooms, capacity and OT availability"),
        uiOutput(ns("capacity_ui")),
        card_grid(min = 420,
          hiq_card("Bed Capacity", plotly::plotlyOutput(ns("capacity_plot"), height = "300px"), class = "fade-in"),
          hiq_card("ICU Capacity", plotly::plotlyOutput(ns("icu_plot"), height = "300px"), class = "fade-in")),

        hiq_card("OT Availability",
          uiOutput(ns("ot_kpi")),
          DT::DTOutput(ns("ot_tbl")),
          br(),
          div(class = "hiq-input-grid",
              selectInput(ns("ot_select"), "OT Room", choices = c("OT 1", "OT 2", "OT 3", "OT 4")),
              selectInput(ns("ot_status"), "Status", c("Available", "Occupied", "Scheduled")),
              textInput(ns("ot_surgery"), "Current / Scheduled Surgery (optional)")
          ),
          actionButton(ns("ot_edit"), "Edit OT Status", icon = icon("edit"), class = "hiq-btn"),
          uiOutput(ns("ot_msg"))
        )
      ),

      # ---------- Departments ----------
      tabPanelBody("depts",
        page_header("Department Analytics", "Patient counts, share and workload"),
        card_grid(min = 420,
          hiq_card("Patients by Department", plotly::plotlyOutput(ns("dept_bar"), height = "300px"), class = "fade-in"),
          hiq_card("Today's Workload (appointments + surgeries)", plotly::plotlyOutput(ns("dept_work"), height = "300px"), class = "fade-in")),
        hiq_card("Department Performance", DT::DTOutput(ns("dept_tbl")))),

      # ---------- Doctors and staff ----------
      tabPanelBody("staff",
        page_header("Doctors & Staff", "Staffing levels and the doctor roster"),
        uiOutput(ns("staff_kpi")),
        card_grid(min = 380,
          hiq_card("Staff Availability", plotly::plotlyOutput(ns("staff_plot"), height = "300px"), class = "fade-in"),
          hiq_card("Doctor Roster", subtitle = "Demo roster", DT::DTOutput(ns("roster_tbl"))))),

      # ---------- Hospital schedule ----------
      tabPanelBody("schedule",
        page_header("Hospital Schedule", "Surgeries, meetings, seminars, conferences and major appointments"),
        hiq_card("Filters",
          div(class = "hiq-input-grid",
              dateRangeInput(ns("sch_dates"), "Date range", start = Sys.Date(), end = Sys.Date() + 14, format = "dd M yyyy"),
              selectInput(ns("sch_dept"), "Department",
                          choices = c("All", "Cardiology", "Neurology", "Orthopedics", "Pediatrics", "Emergency")),
              selectInput(ns("sch_type"), "Event type", multiple = TRUE,
                          choices = c("Surgery", "Meeting", "Seminar", "Conference", "Major Appointment"),
                          selected = c("Surgery", "Meeting", "Seminar", "Conference", "Major Appointment")))),
        uiOutput(ns("sch_msg")),
        hiq_card("Combined Hospital Schedule", DT::DTOutput(ns("sch_tbl")))),

      # ---------- Financial analytics ----------
      tabPanelBody("finance",
        page_header("Financial Analytics", "Confidential - Administrators only"),
        demo_notice("Historical revenue values are synthetic demo data generated around your current inputs."),
        uiOutput(ns("fin_kpi")),
        card_grid(min = 420,
          hiq_card("Revenue Analysis", plotly::plotlyOutput(ns("rev_plot"), height = "300px"), class = "fade-in"),
          hiq_card("Daily Revenue (last 30 days)", plotly::plotlyOutput(ns("daily_rev"), height = "300px"), class = "fade-in"),
          hiq_card("Monthly Revenue (last 12 months)", plotly::plotlyOutput(ns("monthly_rev"), height = "300px"), class = "fade-in"),
          hiq_card("Revenue Per Department", plotly::plotlyOutput(ns("dept_rev"), height = "300px"), class = "fade-in"))),
      # ---------- Patient Management ----------
      tabPanelBody("patmgmt",
        page_header("Patient Management", "Add, edit or remove patients (session only)"),
        hiq_card("Patient Form",
          div(class = "hiq-input-grid",
              textInput(ns("pm_id"), "Patient ID", placeholder = "e.g. P013"),
              textInput(ns("pm_name"), "Full Name"),
              numericInput(ns("pm_age"), "Age", value = 30, min = 0, max = 120),
              selectInput(ns("pm_gender"), "Gender", c("Female", "Male", "Other")),
              selectInput(ns("pm_bg"), "Blood Group",
                          c("A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-")),
              textInput(ns("pm_phone"), "Phone", placeholder = "90000 00000"),
              selectInput(ns("pm_doctor"), "Assigned Doctor", choices = NULL),
              selectInput(ns("pm_dept"), "Department",
                          c("Cardiology", "Neurology", "Orthopedics", "Pediatrics", "Emergency")),
              textInput(ns("pm_diag"), "Diagnosis / Conditions"),
              selectInput(ns("pm_status"), "Status",
                          c("Under Treatment", "Follow-up Care", "Under Observation",
                            "Routine Care", "Pre-procedure", "Post-operative"))
          ),
          div(style = "display:flex; gap:10px; flex-wrap:wrap;",
              actionButton(ns("pm_add"), "Add Patient", icon = icon("user-plus"), class = "hiq-btn"),
              actionButton(ns("pm_update"), "Update Selected", icon = icon("edit"), class = "hiq-btn"),
              actionButton(ns("pm_delete"), "Delete Selected", icon = icon("trash"), class = "hiq-btn")
          ),
          uiOutput(ns("pm_msg"))
        ),
        hiq_card("All Patients (live)", DT::DTOutput(ns("pm_tbl")))
      ),

      # ---------- Reports Management ----------
      tabPanelBody("rptmgmt",
        page_header("Reports Management", "Upload medical reports for any patient"),
        hiq_card("Add Report",
          div(class = "hiq-input-grid",
              selectInput(ns("rm_patient"), "Patient", choices = NULL),
              selectInput(ns("rm_type"), "Report Type",
                          c("Blood Test", "X-Ray", "MRI", "CT Scan", "ECG")),
              textInput(ns("rm_name"), "Report Name", placeholder = "e.g. Complete Blood Count"),
              selectInput(ns("rm_doctor"), "Ordering Doctor", choices = NULL),
              dateInput(ns("rm_date"), "Report Date", value = Sys.Date(), format = "dd M yyyy"),
              selectInput(ns("rm_status"), "Status", c("Reviewed", "Pending Review", "Scheduled")),
              textAreaInput(ns("rm_summary"), "Summary (optional)", rows = 2),
              fileInput(ns("rm_pdf"), "Upload PDF", accept = c(".pdf", "application/pdf"))
          ),
          actionButton(ns("rm_submit"), "Add Report", icon = icon("file-medical"), class = "hiq-btn"),
          uiOutput(ns("rm_msg"))
        ),
        hiq_card("All Reports (live)", DT::DTOutput(ns("rm_tbl")))
      ),

      # ---------- Appointment Management ----------
      tabPanelBody("apptmgmt",
        page_header("Appointment Management", "Approve, reject or reschedule"),
        hiq_card("All Appointments & Requests",
          DT::DTOutput(ns("am_tbl")),
          br(),
          div(class = "hiq-input-grid",
              selectInput(ns("am_status"), "Set Status",
                          c("Approved", "Rejected", "Rescheduled", "Scheduled")),
              dateInput(ns("am_date"), "New Date (reschedule)", value = Sys.Date() + 1),
              selectInput(ns("am_time"), "New Time",
                          c("09:00", "10:00", "11:00", "12:00", "14:00", "15:00", "16:00"))
          ),
          actionButton(ns("am_update"), "Update Selected", icon = icon("sync"), class = "hiq-btn"),
          uiOutput(ns("am_msg"))
        )
      ),

      # ---------- Alerts ----------
      tabPanelBody("alerts",
        page_header("Smart Hospital Alert Center", "Alerts are generated from your inputs and today's schedule"),
        card_grid(min = 400,
          hiq_card("Active Alerts", subtitle = "Operational indicators only", uiOutput(ns("alerts_ui"))),
          hiq_card("Activity Log", subtitle = "Demonstration timeline, not a security audit", uiOutput(ns("activity_ui")))),
        hiq_card("Notification Center", uiOutput(ns("notif_ui")))
    )
  )
  )
}

# ---------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------
admin_server <- function(id, user, demo, shared) {
  moduleServer(id, function(input, output, session) {

    setup_navigation(input, session, vapply(admin_nav_items(), function(x) x$id, character(1)))

    # ---- Populate selects ----
    observe({
      docs <- demo$doctors
      ch <- setNames(docs$doctor_id, paste0(docs$name, " (", docs$department, ")"))
      updateSelectInput(session, "pm_doctor", choices = ch)
      updateSelectInput(session, "rm_doctor", choices = ch)
    })
    observe({
      p <- shared$patients
      updateSelectInput(session, "rm_patient",
                        choices = setNames(p$patient_id, paste0(p$name, " (", p$patient_id, ")")))
    })

    # ---- Patient Management ----
    pm_msg <- reactiveVal(NULL)
    output$pm_msg <- renderUI(pm_msg())

    output$pm_tbl <- DT::renderDT({
      p <- shared$patients
      ph <- if ("phone" %in% names(p)) p$phone else rep("", nrow(p))
      df <- data.frame(
        ID = p$patient_id, Name = p$name, Age = p$age, Gender = p$gender,
        `Blood Group` = p$blood_group, Phone = ph,
        Doctor = demo$doctors$name[match(p$doctor_id, demo$doctors$doctor_id)],
        Status = p$status, check.names = FALSE, stringsAsFactors = FALSE
      )
      hiq_table(df, page_length = 8, selection = "single")
    })

    observeEvent(input$pm_tbl_rows_selected, {
      sel <- input$pm_tbl_rows_selected
      if (length(sel) == 0) return()
      p <- shared$patients[sel, , drop = FALSE]
      updateTextInput(session, "pm_id", value = p$patient_id)
      updateTextInput(session, "pm_name", value = p$name)
      updateNumericInput(session, "pm_age", value = p$age)
      updateSelectInput(session, "pm_gender", selected = p$gender)
      updateSelectInput(session, "pm_bg", selected = p$blood_group)
      if ("phone" %in% names(p)) updateTextInput(session, "pm_phone", value = p$phone)
      updateSelectInput(session, "pm_doctor", selected = p$doctor_id)
      updateTextInput(session, "pm_diag", value = p$conditions)
      updateSelectInput(session, "pm_status", selected = p$status)
    })

    observeEvent(input$pm_add, {
      req(input$pm_add > 0)
      problems <- character(0)
      pid <- trimws(if (is.null(input$pm_id)) "" else input$pm_id)
      nm  <- trimws(if (is.null(input$pm_name)) "" else input$pm_name)
      if (pid == "") problems <- c(problems, "Patient ID is required.")
      if (nm == "")  problems <- c(problems, "Name is required.")
      if (pid != "" && pid %in% shared$patients$patient_id)
        problems <- c(problems, "Patient ID already exists.")
      if (length(problems) > 0) {
        pm_msg(div(class = "hiq-form-msg msg-error", lapply(problems, tags$div)))
        return()
      }
      new_pat <- data.frame(
        patient_id = pid, name = nm, age = as.integer(if (is.null(input$pm_age)) 0 else input$pm_age),
        gender = input$pm_gender, blood_group = input$pm_bg,
        phone = trimws(if (is.null(input$pm_phone)) "" else input$pm_phone),
        doctor_id = input$pm_doctor, status = input$pm_status,
        allergies = "None known",
        emergency_contact = paste0("Contact - ", trimws(if (is.null(input$pm_phone)) "N/A" else input$pm_phone)),
        conditions = trimws(if (is.null(input$pm_diag)) "" else input$pm_diag),
        stringsAsFactors = FALSE
      )
      missing_cols <- setdiff(names(shared$patients), names(new_pat))
      for (mc in missing_cols) new_pat[[mc]] <- NA
      shared$patients <- rbind(shared$patients, new_pat[, names(shared$patients), drop = FALSE])

      push_notification(shared, "Administrator", "ADMIN",
                        "New Patient Added", paste(nm, "(", pid, ") registered."), "info")
      push_notification(shared, "Doctor", input$pm_doctor,
                        "New Assigned Patient", paste(nm, "assigned to you."), "info")
      shared$activity <- rbind(shared$activity, data.frame(
        time = Sys.time(), actor = user$display_name,
        action = paste0("New patient added (", pid, " - ", nm, ")"),
        icon = "user-plus", stringsAsFactors = FALSE))
      pm_msg(div(class = "hiq-form-msg msg-ok", "Patient ", nm, " added successfully."))
    })

    observeEvent(input$pm_update, {
      req(input$pm_update > 0)
      sel <- input$pm_tbl_rows_selected
      if (length(sel) == 0) {
        pm_msg(div(class = "hiq-form-msg msg-error", "Select a patient row first."))
        return()
      }
      shared$patients$name[sel]        <- trimws(if (is.null(input$pm_name)) "" else input$pm_name)
      shared$patients$age[sel]         <- as.integer(if (is.null(input$pm_age)) 0 else input$pm_age)
      shared$patients$gender[sel]      <- input$pm_gender
      shared$patients$blood_group[sel] <- input$pm_bg
      if ("phone" %in% names(shared$patients))
        shared$patients$phone[sel]     <- trimws(if (is.null(input$pm_phone)) "" else input$pm_phone)
      shared$patients$doctor_id[sel]   <- input$pm_doctor
      shared$patients$status[sel]      <- input$pm_status
      shared$patients$conditions[sel]  <- trimws(if (is.null(input$pm_diag)) "" else input$pm_diag)
      pm_msg(div(class = "hiq-form-msg msg-ok", "Patient updated."))
    })

    observeEvent(input$pm_delete, {
      req(input$pm_delete > 0)
      sel <- input$pm_tbl_rows_selected
      if (length(sel) == 0) {
        pm_msg(div(class = "hiq-form-msg msg-error", "Select a patient row first."))
        return()
      }
      removed <- shared$patients$patient_id[sel]
      shared$patients <- shared$patients[-sel, , drop = FALSE]
      pm_msg(div(class = "hiq-form-msg msg-ok", "Patient ", removed, " deleted."))
    })

    # ---- Reports Management ----
    rm_msg <- reactiveVal(NULL)
    output$rm_msg <- renderUI(rm_msg())

    observeEvent(input$rm_submit, {
      req(input$rm_submit > 0)
      if (is.null(input$rm_patient) || input$rm_patient == "") {
        rm_msg(div(class = "hiq-form-msg msg-error", "Select a patient."))
        return()
      }
      rname <- trimws(if (is.null(input$rm_name)) "" else input$rm_name)
      if (rname == "") {
        rm_msg(div(class = "hiq-form-msg msg-error", "Report name is required."))
        return()
      }
      # Ensure summary / pdf_file columns exist
      if (!"summary" %in% names(shared$reports)) shared$reports$summary <- ""
      if (!"pdf_file" %in% names(shared$reports)) shared$reports$pdf_file <- ""

      pdf_name <- ""
      f <- input$rm_pdf
      if (!is.null(f) && !is.null(f$datapath) && file.exists(f$datapath)) {
        pdf_name <- paste0("R", nrow(shared$reports) + 200, "_", gsub("[^A-Za-z0-9._-]", "_", f$name))
        dir.create(file.path("www", "reports"), showWarnings = FALSE, recursive = TRUE)
        file.copy(f$datapath, file.path("www", "reports", pdf_name), overwrite = TRUE)
      }

      new_r <- data.frame(
        report_id = paste0("R", nrow(shared$reports) + 200),
        patient_id = input$rm_patient, report_name = rname,
        type = input$rm_type, date = as.Date(input$rm_date),
        doctor_id = input$rm_doctor, status = input$rm_status,
        summary = trimws(if (is.null(input$rm_summary)) "" else input$rm_summary),
        pdf_file = pdf_name,
        stringsAsFactors = FALSE
      )
      # Align columns
      for (mc in setdiff(names(shared$reports), names(new_r))) new_r[[mc]] <- NA
      shared$reports <- rbind(shared$reports, new_r[, names(shared$reports), drop = FALSE])
      push_notification(shared, "Patient", input$rm_patient,
                        "New Report", paste(rname, "(", input$rm_type, ") is available."), "info")
      push_notification(shared, "Doctor", input$rm_doctor,
                        "Report Uploaded", paste("Report for", input$rm_patient), "info")
      push_notification(shared, "Administrator", "ADMIN",
                        "New Report Uploaded", paste(rname, "for", input$rm_patient), "info")
      shared$activity <- rbind(shared$activity, data.frame(
        time = Sys.time(), actor = user$display_name,
        action = paste0("Report uploaded (", input$rm_patient, " - ", rname, ")"),
        icon = "file-medical", stringsAsFactors = FALSE))
      rm_msg(div(class = "hiq-form-msg msg-ok", "Report added."))
    })

    output$rm_tbl <- DT::renderDT({
      r <- shared$reports
      df <- data.frame(
        ID = r$report_id, Patient = r$patient_id, Name = r$report_name,
        Type = r$type, Date = fmt_date(r$date),
        Doctor = demo$doctors$name[match(r$doctor_id, demo$doctors$doctor_id)],
        Status = r$status, stringsAsFactors = FALSE
      )
      style_status(hiq_table(df, page_length = 8), "Status")
    })

    # ---- Appointment Management ----
    am_msg <- reactiveVal(NULL)
    output$am_msg <- renderUI(am_msg())

    combined_appts <- reactive({
      a <- shared$appointments
      reqs <- shared$requests
      if (nrow(reqs) > 0) {
        extra <- data.frame(
          appt_id = reqs$request_id, patient_id = reqs$patient_id,
          doctor_id = reqs$doctor_id, date = reqs$date, time = reqs$time,
          reason = reqs$reason, status = reqs$status,
          department = reqs$department, token = NA_integer_,
          stringsAsFactors = FALSE
        )
        a <- rbind(a, extra)
      }
      a[order(a$date, a$time), ]
    })

    output$am_tbl <- DT::renderDT({
      a <- combined_appts()
      df <- data.frame(
        ID = a$appt_id, Patient = a$patient_id,
        Doctor = demo$doctors$name[match(a$doctor_id, demo$doctors$doctor_id)],
        Dept = a$department, Date = fmt_date(a$date), Time = fmt_time12(a$time),
        Reason = a$reason, Status = a$status, stringsAsFactors = FALSE
      )
      style_status(hiq_table(df, page_length = 10, selection = "single"), "Status")
    })

    observeEvent(input$am_update, {
      req(input$am_update > 0)
      sel <- input$am_tbl_rows_selected
      if (length(sel) == 0) {
        am_msg(div(class = "hiq-form-msg msg-error", "Select a row first."))
        return()
      }
      a <- combined_appts()
      row <- a[sel, , drop = FALSE]
      new_status <- input$am_status

      if (row$appt_id %in% shared$appointments$appt_id) {
        idx <- which(shared$appointments$appt_id == row$appt_id)
        shared$appointments$status[idx] <- new_status
        if (new_status == "Rescheduled") {
          shared$appointments$date[idx] <- as.Date(input$am_date)
          shared$appointments$time[idx] <- input$am_time
        }
      } else {
        idx <- which(shared$requests$request_id == row$appt_id)
        if (length(idx) > 0) {
          shared$requests$status[idx] <- new_status
          if (new_status == "Rescheduled") {
            shared$requests$date[idx] <- as.Date(input$am_date)
            shared$requests$time[idx] <- input$am_time
          }
          if (new_status %in% c("Approved", "Scheduled")) {
            new_a <- data.frame(
              appt_id = paste0("A", nrow(shared$appointments) + 50),
              patient_id = row$patient_id, doctor_id = row$doctor_id,
              date = if (new_status == "Rescheduled") as.Date(input$am_date) else row$date,
              time = if (new_status == "Rescheduled") input$am_time else row$time,
              reason = row$reason, status = "Scheduled",
              department = row$department, token = 1L,
              stringsAsFactors = FALSE
            )
            shared$appointments <- rbind(shared$appointments, new_a)
          }
        }
      }
      push_notification(shared, "Patient", row$patient_id,
                        paste("Appointment", new_status),
                        paste("Your appointment is now", new_status), "info")
      am_msg(div(class = "hiq-form-msg msg-ok", "Status set to ", new_status, "."))
    })


    today <- demo$today
    dept_names <- c("Emergency", "Cardiology", "Neurology", "Orthopedics", "Pediatrics")
    defaults <- admin_defaults()

    clock <- reactive({
      invalidateLater(60000, session)
      Sys.time()
    })

    # A plain "not yet loaded" input (NULL) falls back to its default.
    # An empty box (NA) is NOT replaced, so validation can report it.
    get_val <- function(name) {
      v <- input[[name]]
      if (is.null(v)) defaults[[name]] else v
    }

    # ================================================================
    # CORE REACTIVE: runs once at start and again whenever the
    # "Update Dashboard" button is clicked (same logic as the
    # original project).
    # ================================================================
    metrics <- eventReactive(input$update, {

      inputs <- list(
        total_beds = get_val("total_beds"), occupied_beds = get_val("occupied_beds"),
        icu_beds = get_val("icu_beds"), occupied_icu = get_val("occupied_icu"),
        total_patients = get_val("total_patients"), doctors = get_val("doctors"),
        nurses = get_val("nurses"), emergency = get_val("emergency"),
        cardiology = get_val("cardiology"), neurology = get_val("neurology"),
        orthopedics = get_val("orthopedics"), pediatrics = get_val("pediatrics"),
        revenue_pp = get_val("revenue_pp")
      )

      msgs <- check_inputs(inputs)
      if (length(msgs) > 0) {
        return(list(valid = FALSE, messages = msgs))
      }

      # ---- KPI calculations (unchanged from the original project) ----
      bed_occupancy  <- inputs$occupied_beds / inputs$total_beds * 100
      icu_occupancy  <- inputs$occupied_icu / inputs$icu_beds * 100
      revenue        <- inputs$total_patients * inputs$revenue_pp
      available_beds <- inputs$total_beds - inputs$occupied_beds

      # ---- Hospital Health Score (unchanged formula) ----------------
      bed_score   <- 100 - bed_occupancy
      icu_score   <- 100 - icu_occupancy
      staff_score <- min(100, ((inputs$doctors + inputs$nurses) / inputs$total_patients) * 100)
      health_score <- round(0.4 * bed_score + 0.4 * icu_score + 0.2 * staff_score)
      cls <- classify_score(health_score)

      dept <- data.frame(
        Department = dept_names,
        Patients = c(inputs$emergency, inputs$cardiology, inputs$neurology,
                     inputs$orthopedics, inputs$pediatrics),
        stringsAsFactors = FALSE
      )

      list(
        valid = TRUE, i = inputs, dept = dept,
        bed_occupancy = bed_occupancy, icu_occupancy = icu_occupancy,
        revenue = revenue, available_beds = available_beds,
        bed_score = bed_score, icu_score = icu_score, staff_score = staff_score,
        health_score = health_score, status = cls$status, hex = cls$hex
      )
    }, ignoreNULL = FALSE)

    # Shows a friendly message instead of a chart when inputs are invalid
    need_valid <- function(m) {
      validate(need(m$valid, "Please correct the input warnings to view this chart."))
    }

    # ---- Warnings (shown above every admin page) ----------------------
    output$warnings <- renderUI({
      m <- metrics()
      if (m$valid) return(NULL)
      div(class = "hiq-form-msg msg-error",
          strong(icon("exclamation-triangle"), " Please fix the following:"),
          tags$ul(lapply(m$messages, tags$li)))
    })

    # ---- Demo-roster helpers (surgeries and queues) -------------------
    surg_now <- reactive(surgery_status_now(demo$surgeries, clock()))

    roster <- reactive({
      s <- surg_now()
      q <- queue_status_now(demo$appointments[demo$appointments$date == today, , drop = FALSE], clock())
      d <- demo$doctors
      avail <- vapply(d$doctor_id, function(x) {
        if (any(s$doctor_id == x & s$status == "In Surgery")) "In Surgery"
        else if (nrow(q) > 0 && any(q$doctor_id == x & q$queue_status == "In Consultation")) "In Consultation"
        else "Available"
      }, character(1))
      data.frame(
        Doctor = d$name, Specialization = d$specialization, Department = d$department,
        Availability = unname(avail),
        `Today's Appointments` = vapply(d$doctor_id, function(x) sum(demo$appointments$doctor_id == x & demo$appointments$date == today), numeric(1)),
        `Today's Surgeries` = vapply(d$doctor_id, function(x) sum(demo$surgeries$doctor_id == x & demo$surgeries$date == today), numeric(1)),
        check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
      )
    })

    appts_today   <- sum(demo$appointments$date == today & demo$appointments$status != "Cancelled")
    surgeries_today <- sum(demo$surgeries$date == today)

    # ---- Smart alerts --------------------------------------------------
    alerts <- reactive({
      m <- metrics()
      out <- list()
      if (!m$valid) {
        return(list(make_alert("danger", "Hospital data needs correction",
                               "Fix the input warnings to refresh capacity and staffing alerts.")))
      }

      if (m$icu_occupancy >= 90) {
        out[[length(out) + 1]] <- make_alert("danger", "Critical ICU Capacity",
          paste0("ICU occupancy is ", round(m$icu_occupancy, 1), "% (", m$i$occupied_icu, " of ", m$i$icu_beds, " beds)."))
      }
      if (m$bed_occupancy >= 90) {
        out[[length(out) + 1]] <- make_alert("warning", "High Bed Occupancy",
          paste0("Bed occupancy is ", round(m$bed_occupancy, 1), "% (", m$i$occupied_beds, " of ", m$i$total_beds, " beds)."))
      }
      if (m$staff_score < 60) {
        out[[length(out) + 1]] <- make_alert("warning", "Low staff availability",
          paste0("Doctors + nurses cover ", round(m$staff_score), "% of the current patient load."))
      }
      if (m$icu_occupancy < 90 && m$bed_occupancy < 90) {
        out[[length(out) + 1]] <- make_alert("success", "Capacity within normal range",
          paste0("Bed ", round(m$bed_occupancy, 1), "% and ICU ", round(m$icu_occupancy, 1), "% occupancy are below 90%."))
      }

      s <- surg_now()
      s <- s[s$date == today & s$status == "Scheduled", , drop = FALSE]
      if (nrow(s) > 0) {
        mins <- minutes_until(to_datetime(s$date, s$time), clock())
        soon <- which(mins <= 180)
        if (length(soon) > 0) {
          for (k in soon) {
            out[[length(out) + 1]] <- make_alert("warning", "Surgery starting soon",
              paste0(s$procedure[k], " - ", s$theatre[k], " at ", fmt_time12(s$time[k])))
          }
        } else {
          out[[length(out) + 1]] <- make_alert("info", "Surgeries later today",
            paste(nrow(s), "surgeries are scheduled for later today."))
        }
      }
      out
    })

    output$topbar <- renderUI(hiq_topbar(count_urgent(alerts())))
    output$exec_alerts <- renderUI(render_alerts(head(alerts(), 4)))
    output$alerts_ui <- renderUI(render_alerts(alerts()))

    # ================= EXECUTIVE KPI CARDS ============================
    output$kpi_ui <- renderUI({
      m <- metrics()
      v <- function(x) if (m$valid) x else "--"
      kpi_grid(
        kpi_card("Total Patients", v(format(m$i$total_patients, big.mark = ",")), "user-injured", NULL, "blue"),
        kpi_card("Available Rooms", v(format(m$available_beds, big.mark = ",")), "bed", NULL, "teal"),
        kpi_card("Bed Occupancy", v(paste0(round(m$bed_occupancy, 1), "%")), "procedures", NULL, "navy"),
        kpi_card("ICU Utilization", v(paste0(round(m$icu_occupancy, 1), "%")), "heartbeat", NULL, "purple"),
        kpi_card("Doctors Available", v(m$i$doctors), "user-md", NULL, "blue"),
        kpi_card("Nurses Available", v(m$i$nurses), "user-nurse", NULL, "teal"),
        kpi_card("Today's Appointments", appts_today, "calendar-check", NULL, "amber"),
        kpi_card("Today's Surgeries", surgeries_today, "procedures", NULL, "red"),
        kpi_card("Estimated Revenue", v(fmt_currency(m$revenue)), "dollar-sign", NULL, "green")
      )
    })

    # ================= HEALTH SCORE CARD (original) ===================
    output$health_card <- renderUI({
      m <- metrics()
      if (!m$valid) return(div(class = "hiq-empty", "Score unavailable until inputs are corrected."))
      div(class = "health-wrap",
          div(class = "health-score", style = paste0("color:", m$hex, ";"), m$health_score),
          div(class = "health-status", style = paste0("background:", m$hex, ";"), m$status),
          div(class = "health-note", "Score out of 100 based on bed capacity, ICU capacity and staff coverage. Project-specific operational score, not a validated healthcare quality metric."))
    })

    # ================= ORIGINAL CHARTS ================================
    # Resource Utilization (Bed % and ICU %)
    output$util_plot <- plotly::renderPlotly({
      m <- metrics(); need_valid(m)
      df <- data.frame(Resource = c("Bed Occupancy %", "ICU Occupancy %"),
                       Percent = round(c(m$bed_occupancy, m$icu_occupancy), 1))
      plotly::plot_ly(df, x = ~Resource, y = ~Percent, type = "bar",
                      text = ~paste0(Percent, "%"), textposition = "inside",
                      insidetextfont = list(color = "#FFFFFF", size = 16),
                      hovertemplate = "%{x}: %{y}%<extra></extra>",
                      marker = list(color = c(HIQ_COL$teal, HIQ_COL$amber))) %>%
        hiq_plot(ylab = "Utilization (%)") %>%
        plotly::layout(yaxis = list(title = "Utilization (%)", range = c(0, 100), gridcolor = HIQ_COL$grid))
    })

    # Bed Status (Occupied vs Available)
    bed_status_plot <- function(m) {
      df <- data.frame(Status = c("Occupied Beds", "Available Beds"),
                       Count = c(m$i$occupied_beds, m$available_beds))
      plotly::plot_ly(df, x = ~Status, y = ~Count, type = "bar",
                      text = ~Count, textposition = "outside",
                      hovertemplate = "%{x}: %{y} beds<extra></extra>",
                      marker = list(color = c(HIQ_COL$red, HIQ_COL$green))) %>%
        hiq_plot(ylab = "Number of Beds")
    }
    output$bed_plot <- plotly::renderPlotly({ m <- metrics(); need_valid(m); bed_status_plot(m) })
    output$capacity_plot <- plotly::renderPlotly({ m <- metrics(); need_valid(m); bed_status_plot(m) })

    # Department Distribution (pie)
    output$dept_pie <- plotly::renderPlotly({
      m <- metrics(); need_valid(m)
      validate(need(sum(m$dept$Patients) > 0, "Enter patients for at least one department."))
      plotly::plot_ly(m$dept, labels = ~Department, values = ~Patients, type = "pie",
                      textinfo = "label+percent",
                      hovertemplate = "%{label}<br>Patients: %{value} (%{percent})<extra></extra>",
                      marker = list(colors = c(HIQ_COL$red, HIQ_COL$blue, HIQ_COL$purple, HIQ_COL$amber, HIQ_COL$teal),
                                    line = list(color = "#FFFFFF", width = 2))) %>%
        plotly::layout(showlegend = TRUE, paper_bgcolor = "rgba(0,0,0,0)", margin = list(t = 20, b = 20)) %>%
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    })

    # Staff Availability (Doctors vs Nurses)
    output$staff_plot <- plotly::renderPlotly({
      m <- metrics(); need_valid(m)
      df <- data.frame(Role = c("Doctors", "Nurses"), Count = c(m$i$doctors, m$i$nurses))
      plotly::plot_ly(df, x = ~Role, y = ~Count, type = "bar",
                      text = ~Count, textposition = "outside",
                      hovertemplate = "%{x}: %{y}<extra></extra>",
                      marker = list(color = c(HIQ_COL$blue, HIQ_COL$purple))) %>%
        hiq_plot(ylab = "Staff Available")
    })

    # Revenue Analysis (ggplot2 column chart converted to Plotly)
    output$rev_plot <- plotly::renderPlotly({
      m <- metrics(); need_valid(m)
      df <- data.frame(Category = "Total Revenue", Revenue = m$revenue, Label = fmt_currency(m$revenue))
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Category, y = Revenue, text = paste0("Total Revenue: ", Label))) +
        ggplot2::geom_col(fill = HIQ_COL$teal, width = 0.4) +
        ggplot2::geom_text(ggplot2::aes(label = Label), vjust = -0.5, fontface = "bold", size = 5) +
        ggplot2::scale_y_continuous(labels = function(x) fmt_currency(x),
                                    expand = ggplot2::expansion(mult = c(0, 0.15))) +
        ggplot2::labs(x = NULL, y = "Revenue") +
        ggplot2::theme_minimal(base_size = 13)
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE) %>%
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    })

    # Summary Table (original DT table)
    output$summary_tbl <- DT::renderDT({
      m <- metrics(); need_valid(m)
      i <- m$i
      df <- data.frame(
        Metric = c("Total Patients", "Total Beds", "Occupied Beds", "Bed Occupancy %",
                   "ICU Utilization %", "Doctors", "Nurses", "Revenue", "Health Score"),
        Value = c(format(i$total_patients, big.mark = ","), format(i$total_beds, big.mark = ","),
                  format(i$occupied_beds, big.mark = ","), paste0(round(m$bed_occupancy, 1), "%"),
                  paste0(round(m$icu_occupancy, 1), "%"), format(i$doctors, big.mark = ","),
                  format(i$nurses, big.mark = ","), fmt_currency(m$revenue),
                  paste0(m$health_score, " (", m$status, ")")),
        stringsAsFactors = FALSE
      )
      hiq_table(df, page_length = 10)
    })

    # ================= PATIENT ANALYTICS ==============================
    output$patient_kpi <- renderUI({
      m <- metrics()
      v <- function(x) if (m$valid) x else "--"
      p <- shared$patients
      ip_status <- c("Under Treatment", "Post-operative", "Pre-procedure", "Under Observation", "Pre-operative")
      n_ip <- sum(p$status %in% ip_status)
      n_op <- nrow(p) - n_ip
      kpi_grid(
        kpi_card("Total Patients", v(format(m$i$total_patients, big.mark = ",")), "user-injured", NULL, "blue"),
        kpi_card("Inpatients", n_ip, "procedures", "Admitted / under care", "red"),
        kpi_card("Outpatients", n_op, "walking", "Ambulatory / follow-up", "teal"),
        kpi_card("Registered Records", nrow(p), "id-card", "Live patient table", "navy")
      )
    })


    # ---- Inpatients / Outpatients (demo classification by status) ----
    output$inpatient_tbl <- DT::renderDT({
      p <- shared$patients
      ip_status <- c("Under Treatment", "Post-operative", "Pre-procedure", "Under Observation", "Pre-operative")
      ip <- p[p$status %in% ip_status, , drop = FALSE]
      df <- data.frame(
        ID = ip$patient_id, Name = ip$name, Age = ip$age, Gender = ip$gender,
        Status = ip$status,
        Doctor = demo$doctors$name[match(ip$doctor_id, demo$doctors$doctor_id)],
        stringsAsFactors = FALSE
      )
      hiq_table(df, page_length = 6)
    })
    output$outpatient_tbl <- DT::renderDT({
      p <- shared$patients
      ip_status <- c("Under Treatment", "Post-operative", "Pre-procedure", "Under Observation", "Pre-operative")
      op <- p[!p$status %in% ip_status, , drop = FALSE]
      df <- data.frame(
        ID = op$patient_id, Name = op$name, Age = op$age, Gender = op$gender,
        Status = op$status,
        Doctor = demo$doctors$name[match(op$doctor_id, demo$doctors$doctor_id)],
        stringsAsFactors = FALSE
      )
      hiq_table(df, page_length = 6)
    })

    # ================= BED MANAGEMENT =================================
    output$capacity_ui <- renderUI({
      m <- metrics()
      if (!m$valid) return(div(class = "hiq-empty", "Hospital capacity unavailable until inputs are corrected."))
      i <- m$i
      bed_tone <- if (m$bed_occupancy >= 90) "red" else if (m$bed_occupancy >= 75) "amber" else "teal"
      icu_tone <- if (m$icu_occupancy >= 90) "red" else if (m$icu_occupancy >= 75) "amber" else "teal"
      tagList(
        if (m$icu_occupancy >= 90) div(class = "hiq-banner banner-danger pulse", icon("exclamation-circle"), "Critical ICU Capacity"),
        if (m$bed_occupancy >= 90) div(class = "hiq-banner banner-warning", icon("exclamation-triangle"), "High Bed Occupancy"),
        h4(class = "hiq-section-title", "Room & Bed Capacity"),
        kpi_grid(
          kpi_card("Total Rooms / Beds", i$total_beds, "bed", NULL, "navy"),
          kpi_card("Occupied Rooms", i$occupied_beds, "procedures", NULL, "red"),
          kpi_card("Available Rooms", m$available_beds, "check-circle", NULL, "green"),
          kpi_card("ICU Beds", i$icu_beds, "heartbeat", NULL, "purple"),
          kpi_card("Occupied ICU Beds", i$occupied_icu, "user-injured", NULL, "amber")
        ),
        hiq_card("Utilization",
                 progress_row("Bed occupancy", m$bed_occupancy, bed_tone),
                 progress_row("ICU utilization", m$icu_occupancy, icu_tone),
                 p(class = "hiq-muted", "Alerts appear when occupancy reaches 90% or more. They are operational indicators only."))
      )
    })

    output$icu_plot <- plotly::renderPlotly({
      m <- metrics(); need_valid(m)
      df <- data.frame(Status = c("Occupied ICU Beds", "Available ICU Beds"),
                       Count = c(m$i$occupied_icu, m$i$icu_beds - m$i$occupied_icu))
      plotly::plot_ly(df, x = ~Status, y = ~Count, type = "bar",
                      text = ~Count, textposition = "outside",
                      hovertemplate = "%{x}: %{y} beds<extra></extra>",
                      marker = list(color = c(HIQ_COL$purple, HIQ_COL$teal))) %>%
        hiq_plot(ylab = "Number of ICU Beds")
    })

    # ================= DEPARTMENTS ====================================
    dept_table <- reactive({
      m <- metrics()
      if (!m$valid) return(NULL)
      total <- sum(m$dept$Patients)
      docs_per_dept <- vapply(dept_names, function(d) sum(demo$doctors$department == d), numeric(1))
      appts <- vapply(dept_names, function(d) sum(demo$appointments$department == d & demo$appointments$date == today), numeric(1))
      surgs <- vapply(dept_names, function(d) {
        ids <- demo$doctors$doctor_id[demo$doctors$department == d]
        sum(demo$surgeries$doctor_id %in% ids & demo$surgeries$date == today)
      }, numeric(1))
      data.frame(
        Department = dept_names, Patients = m$dept$Patients,
        `Share (%)` = if (total > 0) round(m$dept$Patients / total * 100, 1) else 0,
        `Demo Doctors` = unname(docs_per_dept),
        `Patients per Doctor` = unname(round(m$dept$Patients / pmax(docs_per_dept, 1))),
        `Today's Appointments` = unname(appts), `Today's Surgeries` = unname(surgs),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    })

    output$dept_bar <- plotly::renderPlotly({
      m <- metrics(); need_valid(m)
      df <- dept_table()
      plotly::plot_ly(df, x = ~Department, y = ~Patients, type = "bar",
                      marker = list(color = HIQ_COL$blue),
                      text = ~paste0(Patients, " (", `Share (%)`, "%)"), textposition = "outside",
                      hovertemplate = "%{x}<br>Patients: %{y}<extra></extra>") %>%
        hiq_plot(ylab = "Patients")
    })

    output$dept_work <- plotly::renderPlotly({
      m <- metrics(); need_valid(m)
      df <- dept_table()
      plotly::plot_ly(df, x = ~Department) %>%
        plotly::add_bars(y = ~`Today's Appointments`, name = "Appointments", marker = list(color = HIQ_COL$teal),
                         hovertemplate = "%{x}<br>Appointments: %{y}<extra></extra>") %>%
        plotly::add_bars(y = ~`Today's Surgeries`, name = "Surgeries", marker = list(color = HIQ_COL$red),
                         hovertemplate = "%{x}<br>Surgeries: %{y}<extra></extra>") %>%
        hiq_plot(ylab = "Count today", legend = TRUE) %>%
        plotly::layout(barmode = "stack")
    })

    output$dept_tbl <- DT::renderDT({
      m <- metrics(); need_valid(m)
      hiq_table(dept_table(), page_length = 10)
    })

    # ================= DOCTORS & STAFF ================================
    output$staff_kpi <- renderUI({
      m <- metrics()
      r <- roster()
      in_surgery <- sum(r$Availability == "In Surgery")
      v <- function(x) if (m$valid) x else "--"
      kpi_grid(
        kpi_card("Total Doctors", v(m$i$doctors), "user-md", NULL, "blue"),
        kpi_card("Total Nurses", v(m$i$nurses), "user-nurse", NULL, "teal"),
        kpi_card("Available Doctors", v(max(m$i$doctors - in_surgery, 0)), "check-circle", "Total minus doctors in surgery", "green"),
        kpi_card("Doctors in Surgery Now", in_surgery, "procedures", "From the demo roster", "red")
      )
    })

    output$roster_tbl <- DT::renderDT({
      r <- roster()
      DT::formatStyle(hiq_table(r, page_length = 8), "Availability", fontWeight = "600",
                      color = DT::styleEqual(c("Available", "In Consultation", "In Surgery"),
                                             c("#1F7A52", "#0E8C88", "#C53030")))
    })

    # ================= HOSPITAL SCHEDULE ==============================
    events <- reactive(build_hospital_events(demo))

    schedule_ok <- reactive({
      d <- input$sch_dates
      if (is.null(d) || length(d) < 2 || any(is.na(d))) return("Please select both a start date and an end date.")
      if (d[1] > d[2]) return("The start date must be on or before the end date.")
      if (length(input$sch_type) == 0) return("Please select at least one event type.")
      NULL
    })

    output$sch_msg <- renderUI({
      msg <- schedule_ok()
      if (is.null(msg)) NULL else div(class = "hiq-form-msg msg-error", msg)
    })

    output$sch_tbl <- DT::renderDT({
      validate(need(is.null(schedule_ok()), "Adjust the filters above to see the schedule."))
      e <- events()
      d <- input$sch_dates
      e <- e[e$date >= d[1] & e$date <= d[2], , drop = FALSE]
      if (!is.null(input$sch_dept) && input$sch_dept != "All") e <- e[e$department == input$sch_dept, , drop = FALSE]
      e <- e[e$type %in% input$sch_type, , drop = FALSE]
      df <- data.frame(Date = fmt_date(e$date), Time = fmt_time12(e$time), Type = e$type, Event = e$title,
                       Department = e$department, Doctor = e$doctor, Location = e$location,
                       stringsAsFactors = FALSE)
      hiq_table(df, page_length = 10)
    })

    # ================= FINANCIAL ANALYTICS ============================
    output$fin_kpi <- renderUI({
      m <- metrics()
      v <- function(x) if (m$valid) x else "--"
      kpi_grid(
        kpi_card("Estimated Revenue", v(fmt_currency(m$revenue)), "dollar-sign", "Patients x revenue per patient", "green"),
        kpi_card("Total Patients", v(format(m$i$total_patients, big.mark = ",")), "users", NULL, "teal"),
        kpi_card("Departments", "5", "building", "Revenue split below", "blue")
      )
    })

    output$daily_rev <- plotly::renderPlotly({
      m <- metrics(); need_valid(m)
      days <- seq(today - 29, today, by = "day")
      k <- seq_along(days)
      raw <- 1 + 0.12 * sin(k * 0.8) + ifelse(format(days, "%u") %in% c("6", "7"), -0.15, 0)
      daily <- raw / sum(raw) * m$revenue
      df <- data.frame(Date = days, Revenue = daily)
      plotly::plot_ly(df, x = ~Date, y = ~Revenue, type = "scatter", mode = "lines+markers",
                      line = list(color = HIQ_COL$teal, width = 3), marker = list(color = HIQ_COL$teal, size = 6),
                      text = ~fmt_currency(Revenue),
                      hovertemplate = "%{x|%d %b %Y}<br>Revenue: %{text}<extra></extra>") %>%
        hiq_plot(ylab = "Daily revenue") %>%
        plotly::layout(yaxis = list(tickprefix = CURRENCY, tickformat = ",", gridcolor = HIQ_COL$grid))
    })

    output$monthly_rev <- plotly::renderPlotly({
      m <- metrics(); need_valid(m)
      months <- rev(seq(as.Date(format(today, "%Y-%m-01")), by = "-1 month", length.out = 12))
      k <- 1:12
      monthly <- m$revenue * (0.72 + 0.28 * (k - 1) / 11) * (1 + 0.04 * sin(k * 1.5))
      monthly[12] <- m$revenue
      df <- data.frame(Month = factor(format(months, "%b %Y"), levels = format(months, "%b %Y")), Revenue = monthly)
      plotly::plot_ly(df, x = ~Month, y = ~Revenue, type = "bar", marker = list(color = HIQ_COL$blue),
                      text = ~fmt_currency(Revenue),
                      hovertemplate = "%{x}<br>Revenue: %{text}<extra></extra>") %>%
        hiq_plot(ylab = "Monthly revenue") %>%
        plotly::layout(yaxis = list(tickprefix = CURRENCY, tickformat = ",", gridcolor = HIQ_COL$grid))
    })

    output$dept_rev <- plotly::renderPlotly({
      m <- metrics(); need_valid(m)
      # Simple demo model: each department earns a different amount per patient
      weights <- c(Emergency = 1.0, Cardiology = 1.8, Neurology = 1.5, Orthopedics = 1.6, Pediatrics = 0.8)
      score <- m$dept$Patients * weights[m$dept$Department]
      validate(need(sum(score) > 0, "Enter patients for at least one department."))
      df <- data.frame(Department = m$dept$Department, Revenue = score / sum(score) * m$revenue)
      plotly::plot_ly(df, x = ~Department, y = ~Revenue, type = "bar", marker = list(color = HIQ_COL$teal),
                      text = ~fmt_currency(Revenue),
                      hovertemplate = "%{x}<br>Revenue: %{text}<extra></extra>") %>%
        hiq_plot(ylab = "Revenue") %>%
        plotly::layout(yaxis = list(tickprefix = CURRENCY, tickformat = ",", gridcolor = HIQ_COL$grid))
    })

    # ================= HOSPITAL HEALTH ================================
    output$gauge_plot <- plotly::renderPlotly({
      m <- metrics(); need_valid(m)
      plotly::plot_ly(
        type = "indicator", mode = "gauge+number", value = m$health_score,
        title = list(text = m$status),
        gauge = list(
          axis = list(range = list(0, 100)),
          bar = list(color = m$hex),
          steps = list(
            list(range = c(0, 40), color = "#FBE3E4"),
            list(range = c(40, 60), color = "#FCEFD6"),
            list(range = c(60, 80), color = "#DCEBFA"),
            list(range = c(80, 100), color = "#D8F1E4")))
      ) %>%
        plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", margin = list(t = 60, b = 10, l = 30, r = 30)) %>%
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    })

    output$health_break <- renderUI({
      m <- metrics()
      if (!m$valid) return(div(class = "hiq-empty", "Score unavailable until inputs are corrected."))
      tagList(
        progress_row("Bed score (weight 40%)", m$bed_score, "teal", paste0(round(m$bed_score, 1), " / 100")),
        progress_row("ICU score (weight 40%)", m$icu_score, "purple", paste0(round(m$icu_score, 1), " / 100")),
        progress_row("Staff score (weight 20%)", m$staff_score, "blue", paste0(round(m$staff_score, 1), " / 100")),
        p(class = "hiq-muted", "Health Score = 0.4 x Bed score + 0.4 x ICU score + 0.2 x Staff score, rounded."),
        p(class = "hiq-muted", "80-100 Excellent | 60-79 Good | 40-59 Needs Attention | below 40 Critical")
      )
    })

    # ================= ACTIVITY LOG ===================================
    output$notif_ui <- renderUI({
      render_notifications(shared, "Administrator", "ADMIN")
    })


    # ---- OT Room Management ----
    # Auto-set OT Available when surgery is Completed
    observe({
      s <- shared$surgeries
      if (is.null(s) || nrow(s) == 0) return()
      completed <- s[s$status == "Completed", , drop = FALSE]
      if (nrow(completed) == 0) return()
      for (i in seq_len(nrow(completed))) {
        th <- completed$theatre[i]
        if (is.null(th) || is.na(th)) next
        # Map OT-1 style to OT 1
        ot_label <- gsub("OT-", "OT ", as.character(th))
        hit <- which(shared$ot_rooms$ot_id == ot_label)
        if (length(hit) > 0 && shared$ot_rooms$status[hit[1]] != "Available") {
          shared$ot_rooms$status[hit[1]] <- "Available"
          shared$ot_rooms$current_surgery[hit[1]] <- ""
        }
      }
    })

    output$ot_kpi <- renderUI({
      o <- shared$ot_rooms
      kpi_grid(
        kpi_card("Total OTs", nrow(o), "procedures", NULL, "navy"),
        kpi_card("Available", sum(o$status == "Available"), "check-circle", NULL, "green"),
        kpi_card("Occupied", sum(o$status == "Occupied"), "user-injured", NULL, "red"),
        kpi_card("Scheduled", sum(o$status == "Scheduled"), "calendar", NULL, "amber")
      )
    })

    output$ot_tbl <- DT::renderDT({
      o <- shared$ot_rooms
      df <- data.frame(OT = o$ot_id, Status = o$status,
                       `Current / Scheduled` = o$current_surgery,
                       check.names = FALSE, stringsAsFactors = FALSE)
      style_status(hiq_table(df, page_length = 5, selection = "single"), "Status")
    })

    ot_msg <- reactiveVal(NULL)
    output$ot_msg <- renderUI(ot_msg())

    observeEvent(input$ot_edit, {
      req(input$ot_edit > 0)
      ot <- input$ot_select
      idx <- which(shared$ot_rooms$ot_id == ot)
      if (length(idx) == 0) return()
      shared$ot_rooms$status[idx] <- input$ot_status
      shared$ot_rooms$current_surgery[idx] <- trimws(if (is.null(input$ot_surgery)) "" else input$ot_surgery)
      ot_msg(div(class = "hiq-form-msg msg-ok", ot, " set to ", input$ot_status, "."))
    })


    output$activity_ui <- renderUI({
      a <- shared$activity
      a <- a[order(a$time, decreasing = TRUE), , drop = FALSE]
      a <- head(a, 12)
      div(class = "hiq-timeline",
          lapply(seq_len(nrow(a)), function(i) {
            timeline_item(format(a$time[i], "%I:%M %p"), a$action[i], a$actor[i], a$icon[i], "teal", "later")
          }))
    })
  })
}
