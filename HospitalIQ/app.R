# =====================================================================
# Hospital IQ
# Intelligent Hospital Management, Analytics & Patient Care Platform
# ---------------------------------------------------------------------
# Academic (Course Based Project) demonstration built with R Shiny.
#
#   * Uses SYNTHETIC demo data only - no real patient information.
#   * NOT production-ready and NOT suitable for real medical records.
#
# Run with:   shiny::runApp()
#
# All files inside the R/ folder (helpers.R, demo_data.R and the three
# role modules) are loaded automatically by Shiny when the app starts.
# =====================================================================

library(shiny)
library(bslib)
library(plotly)
library(ggplot2)
library(dplyr)
library(DT)

# ---------------------------------------------------------------------
# DEMONSTRATION-ONLY LOGIN
# ---------------------------------------------------------------------
# The login below compares what the user types with a small in-memory
# table of demo users (see R/demo_data.R). It is FOR DEMONSTRATION ONLY
# and must NOT be used to protect real medical records.
#
# For a production hospital system, authentication should use hashed
# passwords, secure databases, encryption, HTTPS, authorization
# policies, audit logging and applicable healthcare privacy/security
# requirements.
# ---------------------------------------------------------------------

# Login / landing page ------------------------------------------------
login_ui <- function(users) {
  div(class = "login-page",
      div(class = "login-wrap",

          # Left: login form
          div(class = "login-card",
              div(class = "login-logo", icon("hospital")),
              h1(class = "login-title", "Hospital IQ"),
              p(class = "login-sub", "Intelligent Hospital Management, Analytics & Patient Care Platform"),
              textInput("username", "Username", placeholder = "Enter your username", width = "100%"),
              passwordInput("password", "Password", placeholder = "Enter your password", width = "100%"),
              actionButton("login_btn", label = tagList(icon("sign-in-alt"), "Log in"), class = "login-btn"),
              uiOutput("login_msg")),

          # Right: demo credentials
          div(class = "login-demo",
              h4(icon("key"), " Demo credentials"),
              p("Choose any account below to explore a role-based dashboard."),
              tags$table(
                tags$thead(tags$tr(tags$th("Role"), tags$th("Username"), tags$th("Password"))),
                tags$tbody(lapply(seq_len(nrow(users)), function(i) {
                  tags$tr(tags$td(users$role[i]),
                          tags$td(tags$code(users$username[i])),
                          tags$td(tags$code(users$password[i])))
                }))),
              div(class = "login-note",
                  "Academic demonstration with synthetic data only. Plain-text demo passwords are not secure ",
                  "and this login must not be used to protect real medical records."))
      ))
}

# ---------------------------------------------------------------------
# USER INTERFACE
# The whole page is a single placeholder. After login, the server
# fills it with the dashboard that belongs to the user's role.
# ---------------------------------------------------------------------
ui <- fluidPage(
  title = "Hospital IQ",
  theme = bslib::bs_theme(version = 5, primary = "#12A8A3"),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(HTML("
      // Highlight the clicked navigation item and scroll back to the top
      $(document).on('click', '.hiq-nav-link', function () {
        $('.hiq-nav-item').removeClass('active');
        $(this).closest('.hiq-nav-item').addClass('active');
        window.scrollTo(0, 0);
      });
      // Pressing Enter in the password box clicks the Log in button
      $(document).on('keypress', '#password', function (e) {
        if (e.which === 13) { $('#login_btn').click(); }
      });
    "))
  ),
  uiOutput("page")
)

# ---------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  # Demo data is created fresh for every browser session
  demo <- load_demo_data()
  if (!"summary" %in% names(demo$reports)) demo$reports$summary <- rep("", nrow(demo$reports))
  if (!"pdf_file" %in% names(demo$reports)) demo$reports$pdf_file <- rep("", nrow(demo$reports))
  if (!"payment_status" %in% names(demo$requests)) {
    demo$requests$payment_status <- rep("Pending", nrow(demo$requests))
  }

  # Information shared between roles during THIS session only:
  #   - the activity log (shown to the Administrator)
  #   - appointment requests made by patients
  # Centralized reactive data shared by every role in THIS session
  shared <- reactiveValues(
    activity      = demo$activity,
    requests      = demo$requests,
    patients      = demo$patients,
    appointments  = demo$appointments,
    reports       = demo$reports,
    prescriptions = data.frame(
      prescription_id = character(0), patient_id = character(0),
      doctor_id = character(0), medicine = character(0), dosage = character(0),
      morning = logical(0), afternoon = logical(0), night = logical(0),
      duration_days = integer(0), start_date = as.Date(character(0)),
      end_date = as.Date(character(0)), created_at = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    ),
    notifications = data.frame(
      notif_id = character(0), role = character(0), user_id = character(0),
      title = character(0), message = character(0), level = character(0),
      time = as.POSIXct(character(0)), stringsAsFactors = FALSE
    ),
    # OT rooms for Room Management
    ot_rooms = data.frame(
      ot_id = c("OT 1", "OT 2", "OT 3", "OT 4"),
      status = c("Occupied", "Available", "Scheduled", "Available"),
      current_surgery = c("Coronary Angioplasty", "", "ACL Reconstruction", ""),
      stringsAsFactors = FALSE
    ),
    # Doctor schedule edits (cancellations / time changes) for this session
    schedule_edits = data.frame(
      doctor_id = character(0), time = character(0), title = character(0),
      type = character(0), detail = character(0), cancelled = logical(0),
      stringsAsFactors = FALSE
    ),
    meetings = demo$meetings,
    surgeries = demo$surgeries,
    schedule = demo$schedule
  )

  # Who is logged in?
  # NULL means nobody is logged in, so the login page is shown.
  auth <- reactiveValues(user = NULL, mod_id = NULL)
  login_msg <- reactiveVal(NULL)
  counter <- 0   # gives every login its own module id

  # ---- Decide which page to display ---------------------------------
  # Login -> role detection -> Patient / Doctor / Administrator
  output$page <- renderUI({
    u <- auth$user
    if (is.null(u)) {
      return(login_ui(demo$users))
    }
    switch(u$role,
           "Patient"       = patient_ui(auth$mod_id, u, demo),
           "Doctor"        = doctor_ui(auth$mod_id, u),
           "Administrator" = admin_ui(auth$mod_id, u))
  })

  output$login_msg <- renderUI({
    msg <- login_msg()
    if (is.null(msg)) NULL else div(class = "login-error", icon("exclamation-circle"), msg)
  })

  # ---- Login button ---------------------------------------------------
  observeEvent(input$login_btn, {
    # Prevent double-submit while a login is already in progress
    req(is.null(auth$user))
    req(input$login_btn > 0)

    uname <- tolower(trimws(if (is.null(input$username)) "" else input$username))
    pw    <- if (is.null(input$password)) "" else input$password

    if (uname == "" && pw == "") {
      login_msg("Please enter your username and password.")
      return()
    }
    if (uname == "") {
      login_msg("Please enter your username.")
      return()
    }
    if (pw == "") {
      login_msg("Please enter your password.")
      return()
    }

    idx <- which(demo$users$username == uname & demo$users$password == pw)
    if (length(idx) == 0) {
      login_msg("Invalid username or password. Please check the demo credentials.")
      return()
    }

    user <- as.list(demo$users[idx[1], , drop = FALSE])

    counter <<- counter + 1
    mod_id <- paste0("session", counter)

    # Set auth FIRST so the UI is rendered, then start the module
    login_msg(NULL)
    auth$mod_id <- mod_id
    auth$user   <- user

    if (user$role == "Patient") {
      patient_server(mod_id, user, demo, shared)
    } else if (user$role == "Doctor") {
      doctor_server(mod_id, user, demo, shared)
    } else {
      admin_server(mod_id, user, demo, shared)
    }

    shared$activity <- rbind(shared$activity, data.frame(
      time = Sys.time(), actor = user$display_name,
      action = paste0("Signed in (", user$role, " demo account)"),
      icon = "sign-in-alt", stringsAsFactors = FALSE))
  })

  # ---- Logout button (defined inside every role's sidebar) -----------
  observeEvent(input$logout, {
    req(input$logout > 0)
    if (!is.null(auth$user)) {
      shared$activity <- rbind(shared$activity, data.frame(
        time = Sys.time(), actor = auth$user$display_name,
        action = "Signed out", icon = "sign-out-alt", stringsAsFactors = FALSE))
    }
    auth$user <- NULL
    auth$mod_id <- NULL
    login_msg(NULL)
    # Clear login fields for next user
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
  })
}

shinyApp(ui = ui, server = server)
