```r
# Install every required package with ONE command
install.packages(c("shiny", "bslib", "plotly", "ggplot2", "dplyr", "DT"))
```

# Hospital IQ
### Intelligent Hospital Management, Analytics & Patient Care Platform

An academic Course Based Project built with **R Shiny**. It shows how one application can present three different dashboards (Patient, Doctor and Hospital Administrator) depending on who logs in.

> **Academic demonstration only.** All data is synthetic. This application is **not production-ready** and must **not** be used with real patient records.

---

## Problem Statement

Hospitals manage patients, doctors, schedules, beds and finances in separate places. Patients struggle to see their own medicines and appointments, doctors lose time juggling consultations, surgeries and meetings, and administrators need a quick view of capacity, staffing and revenue.

## Objective

Demonstrate a single, role-based web application that:

- gives each user only the information that belongs to their role,
- turns hospital figures into interactive charts and an operational health score,
- generates smart alerts from the data and the clock,
- uses a modern, responsive and animated interface.

## Features

| Area | Highlights |
|------|-----------|
| Login | Glassmorphism landing page, demo credentials, validation, logout |
| Patient portal | Overview cards, medical history, reports with demo summaries, medicine schedule and today's timeline, appointment requests, health trend charts, emergency information |
| Doctor workspace | Daily timeline with icons, live patient queue, surgery table, meetings, research and seminars, workload analytics |
| Administrator | Executive KPIs, bed and ICU management, department analytics, doctor roster, combined hospital schedule with filters, financial analytics, Hospital Health Score, alert centre, activity log |
| Smart alerts | Generated from inputs and the current time for every role |
| Design | Navy/teal healthcare theme, rounded cards, subtle animations, responsive grid |

## User Roles

- **Patient** sees only their own profile, history, reports, medicines and appointments.
- **Doctor** sees only their own schedule, patients in their queue, surgeries, meetings and research.
- **Administrator** sees hospital-wide information, including finances. Patients and doctors never see these pages.

Role separation is done by filtering every table with the logged-in user's ID (`patient_id` or `doctor_id`).

## Technologies

R, shiny, bslib, plotly, ggplot2, dplyr, DT, custom CSS. No database and no machine learning.

## Folder Structure

```text
HospitalIQ/
├── app.R                 # UI shell, demo login, role routing, logout
├── www/
│   └── custom.css        # Theme, cards, animations, responsive layout
├── R/                    # Loaded automatically by Shiny
│   ├── helpers.R         # Formatting, validation, charts, UI building blocks
│   ├── demo_data.R       # Synthetic demo data linked through IDs
│   ├── patient_module.R  # Patient portal
│   ├── doctor_module.R   # Doctor workspace
│   └── admin_module.R    # Administrator dashboard
└── README.md
```

## Installation

1. Install R (4.1 or newer) and, optionally, VS Code with the R extension.
2. Install the packages with the command at the top of this file.
3. Use shiny **1.7 or newer** (the hidden tab panels need it).

## Run Locally

Open the `HospitalIQ` folder in VS Code (or set it as your working directory), then run:

```r
shiny::runApp()
```

In a VS Code terminal you can also run:

```bash
Rscript -e "shiny::runApp(launch.browser = TRUE)"
```

## Demo Credentials

| Role | Username | Password | ID |
|------|----------|----------|----|
| Administrator | `admin` | `admin123` | - |
| Doctor | `doctor1` | `doctor123` | D001 (Dr. Meera Nair, Cardiology) |
| Doctor | `doctor2` | `doctor123` | D002 (Dr. Arjun Menon, Neurology) |
| Patient | `patient1` | `patient123` | P001 (Ananya Rao, assigned to D001) |
| Patient | `patient2` | `patient123` | P002 (Rohan Mehta, assigned to D002) |

## Dashboard Modules

- **Patient**: Overview, Medical History, Reports, Medicines, Appointments, Health Trends, Profile.
- **Doctor**: Overview, Schedule, Appointments, Surgeries, Meetings, Research & Seminars, Analytics, Profile.
- **Administrator**: Executive Dashboard, Patient Analytics, Bed Management, Departments, Doctors & Staff, Hospital Schedule, Financial Analytics, Hospital Health, Alerts.

The Administrator data panel keeps the original project's inputs, calculations, validation and charts. Click **Update Dashboard** to refresh everything.

### Hospital Health Score

```r
bed_score   <- 100 - bed_occupancy
icu_score   <- 100 - icu_occupancy
staff_score <- min(100, ((doctors + nurses) / total_patients) * 100)
health_score <- round(0.4 * bed_score + 0.4 * icu_score + 0.2 * staff_score)
```

80-100 Excellent, 60-79 Good, 40-59 Needs Attention, below 40 Critical.
This is a **project-specific operational score**, not a validated healthcare quality metric.

### Data consistency

Demo tables are linked by IDs. For example, patient P001 is assigned to doctor D001, so P001's appointment appears in her portal, in Dr. Nair's schedule and in the Administrator's hospital schedule. Dates are generated relative to today so alerts such as "appointment tomorrow" always make sense.

## Deployment Overview (shinyapps.io)

Everything uses relative paths, needs no database and loads its demo data automatically, so the folder can be deployed as it is.

1. Create a free account at <https://www.shinyapps.io> and open **Account > Tokens** to copy your token command.
2. In R, install the deployment package: `install.packages("rsconnect")`.
3. Register your account **in the R console** (never inside source code). Paste the command shown on the token page:

```r
rsconnect::setAccountInfo(name = "<ACCOUNT>", token = "<TOKEN>", secret = "<SECRET>")
```

4. From the project folder, deploy:

```r
rsconnect::deployApp(appName = "hospital-iq")
```

Do not commit tokens or secrets to Git.

## Security Disclaimer

- All patients, doctors and records are **fictional**.
- Login uses **plain-text demo passwords stored in memory**. This is **not secure** and exists only to demonstrate role-based dashboards.
- Report summaries and health charts are placeholders, not diagnoses or predictions.
- The activity log is a demonstration timeline, not a security audit trail.
- For a production hospital system, authentication should use hashed passwords, secure databases, encryption, HTTPS, authorization policies, audit logging and applicable healthcare privacy/security requirements.

## Future Enhancements

- Hashed passwords and a real authentication provider.
- A secure database for persistent records and appointment approval workflow.
- Doctor-side approval of patient appointment requests.
- File upload for reports and scans.
- Exportable PDF reports for the Administrator.
- Multi-language support and accessibility audit.
