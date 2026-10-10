# Hospital IQ

**Intelligent Hospital Management, Analytics & Patient Care Platform**

A role-based hospital dashboard built with **R Shiny**. One app shows three different workspaces (Patient, Doctor and Administrator) depending on who logs in. Built as a Course Based Project.

> **Academic demonstration only.** All data is synthetic. This app is not production-ready and must not be used with real patient records.



## Features

| Role | What they can do |
|------|------------------|
| **Patient** | Overview cards, medical history, reports with demo summaries, medicine schedule with today's timeline, book appointments and view their status, health trend charts, emergency information, notifications |
| **Doctor** | Daily timeline, live patient queue, approve / reject / reschedule own appointments, assigned patients, create prescriptions, view assigned patients' reports, surgeries, meetings, research and seminars, workload analytics, notifications |
| **Administrator** | Executive KPIs, Hospital Health Score, bed and ICU management, department analytics, doctor roster, combined hospital schedule, financial analytics, add patients, manage all appointments, add medical reports, alert centre, activity log |

Highlights:

- Role-based access: every table is filtered by the logged-in user's ID.
- One shared `reactiveValues()` store, so a change by one role (new patient, booking, prescription, report) shows up in the other dashboards immediately.
- Appointment workflow: `Requested`, `Approved`, `Rejected`, `Rescheduled`. Only doctors and administrators can change the status.
- Notification Center for every role, plus live alerts generated from data and the clock.
- Interactive Plotly charts, responsive layout, subtle animations.

## Demo Credentials

| Role | Username | Password |
|------|----------|----------|
| Administrator | `admin` | `admin123` |
| Doctor (Cardiology) | `doctor1` | `doctor123` |
| Doctor (Neurology) | `doctor2` | `doctor123` |
| Patient | `patient1` | `patient123` |
| Patient | `patient2` | `patient123` |

Patients added by the administrator get a login automatically: username is the patient ID in lowercase (for example `p013`), password `patient123`.

## Tech Stack

R, shiny, bslib, plotly, ggplot2, dplyr, DT, custom CSS. No database and no machine learning.

## Project Structure

```text
HospitalIQ/
├── app.R                  # UI shell, demo login, role routing, logout
├── www/
│   └── custom.css         # Theme, cards, animations, responsive layout
├── R/                     # Loaded automatically by Shiny
│   ├── helpers.R          # Formatting, validation, UI blocks, shared-store helpers
│   ├── demo_data.R        # Synthetic demo data and the central data store
│   ├── patient_module.R   # Patient portal
│   ├── doctor_module.R    # Doctor workspace
│   └── admin_module.R     # Administrator dashboard
└── README.md
```

## Run Locally

1. Install R 4.1 or newer.
2. Install the packages (shiny 1.7 or newer is required):

```r
install.packages(c("shiny", "bslib", "plotly", "ggplot2", "dplyr", "DT"))
```

3. Clone the repository and start the app from the project folder:

(https://github.com/deekshitha2706/hospital-iq-ai-healthcare-intelligence.git)
```

```r
shiny::runApp()
```

## Deployment

The app uses only relative paths, needs no database and loads its demo data automatically, so it can be deployed as it is.

> GitHub itself cannot run a Shiny app (GitHub Pages only hosts static files). Use one of the options below.

### Option 1: shinyapps.io (easiest)

1. Create a free account at <https://www.shinyapps.io>, then open **Account > Tokens** and copy the token command.
2. In R:

```r
install.packages("rsconnect")

# Paste the command from the Tokens page (do this in the console, never commit it)
rsconnect::setAccountInfo(name = "<ACCOUNT>", token = "<TOKEN>", secret = "<SECRET>")

# Run from the project folder
rsconnect::deployApp(appName = "hospital-iq")
```

3. Add the live link to the top of this README.

### Option 2: Posit Connect Cloud

Connect your GitHub repository at <https://connect.posit.cloud> and choose `app.R` as the primary file.

### Option 3: Docker (any server)

```dockerfile
FROM rocker/shiny:latest
RUN R -e "install.packages(c('bslib','plotly','ggplot2','dplyr','DT'))"
COPY . /srv/shiny-server/hospital-iq
```

Never put tokens or secrets in the code or in this repository.

## Data and Limitations

- Data lives in memory for the current browser session only. Refreshing the page resets it to the demo data.
- Passwords are plain-text demo values. For a real hospital system you would need hashed passwords, a secure database, encryption, HTTPS, authorization policies, audit logging and applicable healthcare privacy rules.
- Report summaries and health charts are placeholders, not diagnoses or predictions.
- The Hospital Health Score is a project-specific operational score, not a validated healthcare quality metric.

## Future Enhancements

- Persistent database and hashed authentication.
- Edit and delete patients from the admin panel.
- File upload for scans and reports.
- PDF export for administrators.
- Accessibility audit and multi-language support.

## License

Add a license of your choice (for example MIT) before sharing publicly.
