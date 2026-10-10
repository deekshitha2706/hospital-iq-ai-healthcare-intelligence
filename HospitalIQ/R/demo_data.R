# =====================================================================
# demo_data.R
# ---------------------------------------------------------------------
# ALL data in this file is SYNTHETIC (made up) and is used for
# demonstration only. No real patient information is included.
#
# Every table is linked through IDs so that the Patient portal, the
# Doctor workspace and the Admin dashboard always agree:
#
#   Patient P001 (Ananya Rao)  --assigned to-->  Doctor D001
#   Patient P002 (Rohan Mehta) --assigned to-->  Doctor D002
#
# Dates are generated RELATIVE to today's date, so "today's
# appointments", "tomorrow's appointment" and similar alerts always
# make sense whenever the app is opened.
# =====================================================================

load_demo_data <- function(today = Sys.Date()) {

  # -------------------------------------------------------------------
  # 1. USERS  (DEMO AUTHENTICATION ONLY)
  # -------------------------------------------------------------------
  # WARNING: Plain-text passwords are used here ONLY because this is a
  # classroom demonstration. Never store passwords like this in a real
  # system. For a production hospital system, authentication should use
  # hashed passwords, secure databases, encryption, HTTPS, authorization
  # policies, audit logging and applicable healthcare privacy/security
  # requirements.
  users <- data.frame(
    username     = c("admin", "doctor1", "doctor2", "patient1", "patient2"),
    password     = c("admin123", "doctor123", "doctor123", "patient123", "patient123"),
    role         = c("Administrator", "Doctor", "Doctor", "Patient", "Patient"),
    user_id      = c("ADMIN", "D001", "D002", "P001", "P002"),
    display_name = c("Hospital Administrator", "Dr. Meera Nair", "Dr. Arjun Menon",
                     "Ananya Rao", "Rohan Mehta"),
    stringsAsFactors = FALSE
  )

  # -------------------------------------------------------------------
  # 2. DOCTORS
  # -------------------------------------------------------------------
  doctors <- data.frame(
    doctor_id      = c("D001", "D002", "D003", "D004", "D005"),
    name           = c("Dr. Meera Nair", "Dr. Arjun Menon", "Dr. Kavita Sharma",
                       "Dr. Sameer Khan", "Dr. Priya Iyer"),
    specialization = c("Interventional Cardiology", "Neurology", "Orthopedic Surgery",
                       "Pediatrics", "Emergency Medicine"),
    department     = c("Cardiology", "Neurology", "Orthopedics", "Pediatrics", "Emergency"),
    experience     = c(14, 11, 16, 9, 8),
    stringsAsFactors = FALSE
  )

  # -------------------------------------------------------------------
  # 3. PATIENTS  (P001 and P002 can log in, the rest only appear in
  #    doctor queues, surgery lists and admin schedules)
  # -------------------------------------------------------------------
  patients <- data.frame(
    patient_id = sprintf("P%03d", 1:12),
    name = c("Ananya Rao", "Rohan Mehta", "Kiran Patel", "Sneha Joshi",
             "Vivek Kulkarni", "Lakshmi Pillai", "Aarav Singh", "Farhan Ali",
             "Meenakshi Reddy", "Joseph Thomas", "Isha Bansal", "Dev Malhotra"),
    age = c(34, 45, 58, 29, 63, 41, 7, 36, 52, 67, 38, 49),
    gender = c("Female", "Male", "Male", "Female", "Male", "Female",
               "Male", "Male", "Female", "Male", "Female", "Male"),
    blood_group = c("B+", "O+", "A+", "AB+", "B-", "O-", "A+", "B+", "O+", "A-", "AB-", "B+"),
    phone = c("90000 10001", "90000 10002", "90000 10003", "90000 10004",
              "90000 10005", "90000 10006", "90000 10007", "90000 10008",
              "90000 10009", "90000 10010", "90000 10011", "90000 10012"),
    doctor_id = c("D001", "D002", "D001", "D001", "D002", "D002",
                  "D004", "D003", "D001", "D001", "D002", "D002"),
    status = c("Under Treatment", "Follow-up Care", "Pre-procedure", "Under Observation",
               "Post-operative", "Under Observation", "Routine Care", "Pre-operative",
               "Routine Care", "Under Treatment", "Routine Care", "Under Observation"),
    allergies = c("Penicillin", "None known", "Sulfa drugs", "None known",
                  "Latex", "None known", "Peanuts", "None known",
                  "None known", "Aspirin", "None known", "Dust"),
    emergency_contact = c("Suresh Rao (Father) - 00000 10001", "Neha Mehta (Spouse) - 00000 10002",
                          "Demo Contact A - 00000 10003", "Demo Contact B - 00000 10004",
                          "Demo Contact C - 00000 10005", "Demo Contact D - 00000 10006",
                          "Demo Contact E - 00000 10007", "Demo Contact F - 00000 10008",
                          "Demo Contact G - 00000 10009", "Demo Contact H - 00000 10010",
                          "Demo Contact I - 00000 10011", "Demo Contact J - 00000 10012"),
    conditions = c("Hypertension, Type 2 Diabetes, High cholesterol",
                   "Migraine, Vitamin B12 deficiency",
                   "Coronary artery disease", "Palpitations (under observation)",
                   "Epilepsy", "Chronic headache",
                   "Asthma", "Ligament injury",
                   "Mitral valve disorder", "Heart rhythm disorder",
                   "Neuropathy", "Spinal disc problem"),
    stringsAsFactors = FALSE
  )

  # -------------------------------------------------------------------
  # 4. APPOINTMENTS
  # -------------------------------------------------------------------
  appt <- function(id, pid, did, offset, time, reason, status = "Scheduled") {
    data.frame(appt_id = id, patient_id = pid, doctor_id = did,
               date = today + offset, time = time, reason = reason,
               status = status, stringsAsFactors = FALSE)
  }

  appointments <- do.call(rbind, list(
    # --- TODAY: Dr. Meera Nair (D001) ---
    appt("A001", "P003", "D001", 0, "09:00", "Pre-procedure review"),
    appt("A002", "P004", "D001", 0, "09:30", "Palpitations follow-up"),
    appt("A003", "P009", "D001", 0, "10:00", "Valve assessment"),
    appt("A004", "P010", "D001", 0, "15:00", "Rhythm monitoring review"),
    # --- TODAY: Dr. Arjun Menon (D002) ---
    appt("A005", "P005", "D002", 0, "09:00", "Post-operative check"),
    appt("A006", "P011", "D002", 0, "09:30", "Nerve conduction review"),
    appt("A007", "P012", "D002", 0, "10:00", "Back pain evaluation"),
    appt("A008", "P006", "D002", 0, "15:30", "Pre-procedure counselling"),
    # --- TODAY: other doctors ---
    appt("A009", "P007", "D004", 0, "11:00", "Asthma review"),
    appt("A010", "P008", "D003", 0, "12:00", "Pre-operative assessment"),
    # --- UPCOMING for the two demo patients ---
    appt("A011", "P001", "D001", 1, "11:00", "Cardiology follow-up"),
    appt("A012", "P002", "D002", 5, "10:00", "Neurology review"),
    # --- PAST appointments for the two demo patients ---
    appt("A013", "P001", "D001", -30, "10:30", "Blood pressure review", "Completed"),
    appt("A014", "P001", "D001", -90, "09:30", "Annual cardiac check-up", "Completed"),
    appt("A015", "P001", "D001", -210, "11:30", "Hypertension consultation", "Completed"),
    appt("A016", "P001", "D001", -150, "10:00", "Diet consultation", "Cancelled"),
    appt("A017", "P002", "D002", -21, "14:30", "Medication dose review", "Completed"),
    appt("A018", "P002", "D002", -75, "10:30", "MRI consultation", "Completed"),
    appt("A019", "P002", "D002", -200, "12:00", "Migraine consultation", "Completed")
  ))
  appointments$department <- doctors$department[match(appointments$doctor_id, doctors$doctor_id)]
  appointments <- appointments[order(appointments$date, appointments$doctor_id, appointments$time), ]
  # Token numbers restart for every doctor and every day
  appointments$token <- ave(seq_len(nrow(appointments)),
                            appointments$date, appointments$doctor_id, FUN = seq_along)
  rownames(appointments) <- NULL

  # -------------------------------------------------------------------
  # 5. SURGERIES  (each surgery belongs to one doctor and one patient)
  # -------------------------------------------------------------------
  surg <- function(id, did, pid, procedure, offset, time, theatre, assistant, status = "Scheduled") {
    data.frame(surgery_id = id, doctor_id = did, patient_id = pid, procedure = procedure,
               date = today + offset, time = time, theatre = theatre,
               assistant = assistant, status = status, stringsAsFactors = FALSE)
  }

  surgeries <- do.call(rbind, list(
    surg("S001", "D001", "P003", "Coronary Angioplasty", 0, "10:30", "OT-2", "Dr. Priya Iyer"),
    surg("S002", "D002", "P006", "Diagnostic Brain Biopsy", 0, "14:00", "OT-3", "Dr. Kavita Sharma"),
    surg("S003", "D003", "P008", "ACL Reconstruction", 0, "08:00", "OT-1", "Dr. Priya Iyer"),
    surg("S004", "D001", "P010", "Pacemaker Implantation", 2, "09:00", "OT-2", "Dr. Priya Iyer"),
    surg("S005", "D002", "P012", "Lumbar Decompression", 3, "11:00", "OT-3", "Dr. Kavita Sharma"),
    surg("S006", "D003", "P008", "Knee Arthroscopy (follow-up)", 4, "13:00", "OT-1", "Dr. Sameer Khan"),
    surg("S007", "D001", "P009", "Mitral Valve Repair", 6, "08:30", "OT-2", "Dr. Priya Iyer"),
    surg("S008", "D001", "P003", "Coronary Angiography", -20, "09:00", "OT-2", "Dr. Priya Iyer", "Completed"),
    surg("S009", "D002", "P005", "Shunt Revision", -12, "10:00", "OT-3", "Dr. Kavita Sharma", "Completed")
  ))

  # -------------------------------------------------------------------
  # 6. MEETINGS  (one row per attending doctor)
  # -------------------------------------------------------------------
  meet <- function(id, dids, name, offset, time, location, priority) {
    data.frame(meeting_id = id, doctor_id = dids, meeting = name, date = today + offset,
               time = time, location = location, priority = priority,
               stringsAsFactors = FALSE)
  }

  meetings <- do.call(rbind, list(
    meet("M001", "D001", "Cardiology Department Meeting", 0, "14:00", "Conference Room B", "Medium"),
    meet("M002", "D002", "Neurology Research Meeting", 0, "16:30", "Research Wing, Lab 2", "Medium"),
    meet("M003", c("D001", "D002", "D003"), "Hospital Administration Meeting", 2, "11:00", "Board Room", "High"),
    meet("M004", c("D001", "D002"), "Medical Committee Meeting", 4, "15:00", "Committee Hall", "High"),
    meet("M005", "D001", "Cardiology Research Meeting", 7, "16:00", "Research Wing, Lab 1", "Low"),
    meet("M006", "D002", "Neurology Department Meeting", 3, "14:00", "Conference Room C", "Medium"),
    meet("M007", "D003", "Orthopedics Department Meeting", 1, "14:30", "Conference Room A", "Medium"),
    meet("M008", "D004", "Pediatrics Department Meeting", 2, "15:00", "Conference Room D", "Low")
  ))
  meetings$department <- doctors$department[match(meetings$doctor_id, doctors$doctor_id)]

  # -------------------------------------------------------------------
  # 7. DOCTOR DAILY SCHEDULE (today). The surgery and meeting rows use
  #    the SAME times as the surgeries/meetings tables above.
  # -------------------------------------------------------------------
  sched <- function(did, time, title, type, detail) {
    data.frame(doctor_id = did, time = time, title = title, type = type,
               detail = detail, stringsAsFactors = FALSE)
  }

  schedule <- do.call(rbind, list(
    sched("D001", "09:00", "OP Consultation", "Consultation", "Outpatient clinic - Token 1 to 3"),
    sched("D001", "10:30", "Surgery: Coronary Angioplasty", "Surgery", "OT-2 - Patient P003"),
    sched("D001", "13:00", "Lunch Break", "Break", "Doctors' lounge"),
    sched("D001", "14:00", "Cardiology Department Meeting", "Meeting", "Conference Room B"),
    sched("D001", "15:00", "Patient Consultation", "Consultation", "Rhythm monitoring review - Token 4"),
    sched("D001", "17:00", "Medical Research Review", "Research", "Wearable ECG pilot study data"),
    sched("D002", "09:00", "OP Consultation", "Consultation", "Outpatient clinic - Token 1 to 3"),
    sched("D002", "11:00", "Seminar: Stroke Care Update", "Seminar", "Auditorium"),
    sched("D002", "13:00", "Lunch Break", "Break", "Doctors' lounge"),
    sched("D002", "14:00", "Surgery: Diagnostic Brain Biopsy", "Surgery", "OT-3 - Patient P006"),
    sched("D002", "15:30", "Patient Consultation", "Consultation", "Pre-procedure counselling - Token 4"),
    sched("D002", "16:30", "Neurology Research Meeting", "Meeting", "Research Wing, Lab 2"),
    sched("D003", "08:00", "Surgery: ACL Reconstruction", "Surgery", "OT-1 - Patient P008"),
    sched("D003", "12:00", "Pre-operative Assessment", "Consultation", "Outpatient clinic"),
    sched("D004", "11:00", "OP Consultation", "Consultation", "Pediatrics clinic"),
    sched("D005", "09:00", "Emergency Duty", "Consultation", "Emergency department")
  ))

  # -------------------------------------------------------------------
  # 8. RESEARCH & ACADEMIC ACTIVITY
  # -------------------------------------------------------------------
  res <- function(did, title, type, offset, status) {
    data.frame(doctor_id = did, title = title, type = type, date = today + offset,
               status = status, stringsAsFactors = FALSE)
  }

  research <- do.call(rbind, list(
    res("D001", "Early Intervention Outcomes in Acute Coronary Syndrome", "Research Paper", -45, "Under Review"),
    res("D001", "Radial vs Femoral Access: A Single-Centre Review", "Publication", -160, "Published"),
    res("D001", "Wearable ECG Monitoring Pilot Study", "Current Research", -80, "In Progress"),
    res("D001", "National Cardiology Conference", "Conference", 14, "Registered"),
    res("D001", "Cardiac Imaging Update", "Seminar", 3, "Attending"),
    res("D001", "Hands-on Angioplasty Workshop", "Workshop", -25, "Completed"),
    res("D002", "Stroke Rehabilitation Outcomes in Urban Patients", "Research Paper", -60, "Submitted"),
    res("D002", "Migraine Management: A Retrospective Study", "Publication", -200, "Published"),
    res("D002", "Sleep Patterns and Neurological Health", "Current Research", -50, "In Progress"),
    res("D002", "Annual Neuroscience Conference", "Conference", 21, "Registered"),
    res("D002", "Stroke Care Update", "Seminar", 0, "Attending"),
    res("D002", "EEG Interpretation Workshop", "Workshop", -35, "Completed"),
    res("D003", "Minimally Invasive Joint Repair Techniques", "Publication", -120, "Published"),
    res("D004", "Childhood Asthma Care Pathways", "Current Research", -30, "In Progress"),
    res("D005", "Emergency Triage Efficiency Study", "Current Research", -20, "In Progress")
  ))

  acad <- function(did, event, type, offset, time, location) {
    data.frame(doctor_id = did, event = event, type = type, date = today + offset,
               time = time, location = location, stringsAsFactors = FALSE)
  }

  academic_events <- do.call(rbind, list(
    acad("D001", "Cardiac Imaging Update", "Seminar", 3, "16:00", "Auditorium"),
    acad("D001", "Research Presentation: Wearable ECG Findings", "Research Presentation", 9, "11:00", "Seminar Hall 2"),
    acad("D001", "National Cardiology Conference", "Medical Conference", 14, "09:00", "Convention Centre"),
    acad("D002", "Stroke Care Update", "Seminar", 0, "11:00", "Auditorium"),
    acad("D002", "Research Presentation: Sleep & Neurology", "Research Presentation", 10, "14:00", "Seminar Hall 1"),
    acad("D002", "Annual Neuroscience Conference", "Medical Conference", 21, "09:00", "Convention Centre"),
    acad("D003", "Orthopedic Techniques Workshop", "Workshop", 8, "10:00", "Skills Lab"),
    acad("D004", "Pediatric Care Seminar", "Seminar", 5, "15:00", "Auditorium"),
    acad("D005", "Emergency Medicine Conference", "Medical Conference", 18, "09:00", "Convention Centre")
  ))
  academic_events$department <- doctors$department[match(academic_events$doctor_id, doctors$doctor_id)]

  # -------------------------------------------------------------------
  # 9. PATIENT MEDICAL HISTORY
  # -------------------------------------------------------------------
  hst <- function(pid, offset, diagnosis, did, treatment, status) {
    data.frame(patient_id = pid, date = today + offset, diagnosis = diagnosis, doctor_id = did,
               treatment = treatment, status = status, stringsAsFactors = FALSE)
  }

  history <- do.call(rbind, list(
    hst("P001", -400, "Type 2 Diabetes Mellitus", "D001", "Metformin; diet counselling", "Ongoing"),
    hst("P001", -300, "Hypertension (Stage 1)", "D001", "Amlodipine; lifestyle changes", "Ongoing"),
    hst("P001", -180, "High Cholesterol", "D001", "Atorvastatin; dietary plan", "Ongoing"),
    hst("P001", -90, "Annual Cardiac Check-up", "D001", "ECG and lipid profile review", "Completed"),
    hst("P001", -30, "Blood Pressure Review", "D001", "Medication schedule reviewed", "Follow-up"),
    hst("P002", -400, "Acute Viral Fever", "D005", "Paracetamol; rest and fluids", "Resolved"),
    hst("P002", -300, "Migraine with Aura", "D002", "Gabapentin; trigger diary", "Ongoing"),
    hst("P002", -200, "Vitamin B12 Deficiency", "D002", "B12 supplements", "Ongoing"),
    hst("P002", -75, "Neurology Consultation (MRI Brain)", "D002", "Imaging and specialist review", "Completed"),
    hst("P002", -21, "Medication Dose Review", "D002", "Dose adjusted by doctor", "Follow-up")
  ))

  # -------------------------------------------------------------------
  # 10. PATIENT REPORTS & SCANS
  # -------------------------------------------------------------------
  rpt <- function(id, pid, name, type, offset, did, status) {
    data.frame(report_id = id, patient_id = pid, report_name = name, type = type,
               date = today + offset, doctor_id = did, status = status,
               stringsAsFactors = FALSE)
  }

  reports <- do.call(rbind, list(
    rpt("R001", "P001", "Complete Blood Count", "Blood Test", -35, "D001", "Reviewed"),
    rpt("R002", "P001", "Lipid Profile", "Blood Test", -35, "D001", "Reviewed"),
    rpt("R003", "P001", "Chest X-Ray", "X-Ray", -90, "D001", "Reviewed"),
    rpt("R004", "P001", "12-Lead ECG", "ECG", -30, "D001", "Reviewed"),
    rpt("R005", "P001", "CT Calcium Score Scan", "CT Scan", -60, "D001", "Reviewed"),
    rpt("R006", "P001", "Cardiac MRI", "MRI", -120, "D001", "Reviewed"),
    rpt("R007", "P001", "HbA1c Laboratory Test", "Laboratory Report", 4, "D001", "Scheduled"),
    rpt("R101", "P002", "MRI Brain", "MRI", -75, "D002", "Reviewed"),
    rpt("R102", "P002", "CT Head", "CT Scan", -74, "D002", "Reviewed"),
    rpt("R103", "P002", "Vitamin B12 Level", "Laboratory Report", -40, "D002", "Reviewed"),
    rpt("R104", "P002", "Complete Blood Count", "Blood Test", -40, "D002", "Reviewed"),
    rpt("R105", "P002", "EEG", "Other", -10, "D002", "Pending Review"),
    rpt("R106", "P002", "Vitamin B12 Level (repeat)", "Laboratory Report", 12, "D002", "Scheduled")
  ))

  # -------------------------------------------------------------------
  # 11. MEDICINES  (morning = 08:00, afternoon = 14:00, night = 21:00)
  # -------------------------------------------------------------------
  med <- function(pid, name, dose, m, a, n, food, start, end) {
    data.frame(patient_id = pid, medicine = name, dosage = dose,
               morning = m, afternoon = a, night = n, food = food,
               start_date = today + start, end_date = today + end,
               stringsAsFactors = FALSE)
  }

  medicines <- do.call(rbind, list(
    med("P001", "Metformin", "500 mg", TRUE, FALSE, TRUE, "After Food", -400, 120),
    med("P001", "Amlodipine", "5 mg", TRUE, FALSE, FALSE, "After Food", -300, 90),
    med("P001", "Vitamin D3", "1000 IU", FALSE, TRUE, FALSE, "After Food", -60, 60),
    med("P001", "Atorvastatin", "10 mg", FALSE, FALSE, TRUE, "After Food", -180, 150),
    med("P002", "Gabapentin", "300 mg", TRUE, FALSE, TRUE, "After Food", -300, 60),
    med("P002", "Vitamin B12", "1500 mcg", FALSE, TRUE, FALSE, "After Food", -200, 45),
    med("P002", "Propranolol", "20 mg", TRUE, FALSE, FALSE, "Before Food", -21, 70),
    med("P002", "Paracetamol", "500 mg", TRUE, FALSE, TRUE, "After Food", -400, -385)
  ))

  # -------------------------------------------------------------------
  # 12. HEALTH TRENDS  (12 monthly recorded values per patient)
  #     Generated with simple formulas (no random numbers) so the
  #     charts look identical every time the app starts.
  # -------------------------------------------------------------------
  health_dates <- seq(as.Date(format(today, "%Y-%m-01")), by = "-1 month", length.out = 12)
  health_dates <- rev(health_dates)
  idx <- 1:12

  trend <- function(pid, w0, wslope, s0, d0, h0) {
    data.frame(
      patient_id = pid,
      date       = health_dates,
      weight     = round(w0 + wslope * idx + 0.6 * sin(idx * 1.3), 1),
      systolic   = round(s0 + 3 * sin(idx * 0.9) - 0.4 * idx),
      diastolic  = round(d0 + 2 * cos(idx * 0.7) - 0.2 * idx),
      heart_rate = round(h0 + 3 * sin(idx * 1.1)),
      stringsAsFactors = FALSE
    )
  }

  health <- rbind(
    trend("P001", 70, -0.35, 138, 88, 78),
    trend("P002", 82, 0.10, 126, 82, 72)
  )

  # -------------------------------------------------------------------
  # 13. DOCTOR WORKLOAD STATISTICS (synthetic history for analytics)
  # -------------------------------------------------------------------
  doc_ids   <- doctors$doctor_id
  scale_app <- c(1.00, 0.90, 0.75, 0.85, 1.10)
  scale_sur <- c(1.00, 0.70, 1.40, 0.05, 0.20)

  weekday_levels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  weekday_base   <- c(18, 22, 20, 24, 19, 12)
  weekday_stats <- do.call(rbind, lapply(seq_along(doc_ids), function(i) {
    data.frame(doctor_id = doc_ids[i],
               weekday = factor(weekday_levels, levels = weekday_levels),
               appointments = round(weekday_base * scale_app[i]),
               stringsAsFactors = FALSE)
  }))

  month_dates  <- rev(seq(as.Date(format(today, "%Y-%m-01")), by = "-1 month", length.out = 6))
  month_labels <- format(month_dates, "%b %Y")
  surgery_base <- c(8, 10, 9, 12, 11, 10)
  month_stats <- do.call(rbind, lapply(seq_along(doc_ids), function(i) {
    data.frame(doctor_id = doc_ids[i], month = month_labels,
               month_order = seq_along(month_labels),
               surgeries = round(surgery_base * scale_sur[i]),
               stringsAsFactors = FALSE)
  }))

  consult_types <- c("New Patient", "Follow-up", "Emergency Review", "Pre/Post-op Review")
  consult_base  <- c(40, 85, 15, 25)
  consult_stats <- do.call(rbind, lapply(seq_along(doc_ids), function(i) {
    data.frame(doctor_id = doc_ids[i], type = consult_types,
               count = round(consult_base * scale_app[i]),
               stringsAsFactors = FALSE)
  }))

  work_cats <- c("Consultations", "Surgeries", "Meetings", "Research & Academic", "Administration")
  work_hours <- rbind(
    c(22, 10, 5, 4, 3),
    c(20, 7, 4, 7, 3),
    c(14, 18, 4, 3, 3),
    c(30, 1, 4, 3, 4),
    c(34, 2, 3, 2, 4)
  )
  workload_stats <- do.call(rbind, lapply(seq_along(doc_ids), function(i) {
    data.frame(doctor_id = doc_ids[i], category = work_cats,
               hours = work_hours[i, ], stringsAsFactors = FALSE)
  }))

  # -------------------------------------------------------------------
  # 14. ACTIVITY LOG  (synthetic demonstration timeline - NOT a real
  #     security audit trail)
  # -------------------------------------------------------------------
  activity <- data.frame(
    time = Sys.time() - c(20, 55, 110, 190, 320, 700) * 60,
    actor = c("Front Desk", "Ward Manager", "Scheduling Office", "OT Coordinator",
              "Laboratory", "Front Desk"),
    action = c("Patient appointment requested (P002 - Neurology review)",
               "Bed allocation updated (Ward 3B, 2 beds released)",
               "Doctor schedule updated (Dr. Meera Nair, department meeting)",
               "Surgery scheduled (S004 - Pacemaker Implantation)",
               "Patient report added (P002 - EEG)",
               "Patient registered (P012 - Dev Malhotra)"),
    icon = c("calendar-plus", "bed", "user-md", "procedures", "file-medical", "user-plus"),
    stringsAsFactors = FALSE
  )

  # Empty table that stores appointment requests made during a session
  requests <- data.frame(
    request_id = character(0), patient_id = character(0), patient_name = character(0),
    doctor_id = character(0), department = character(0), date = as.Date(character(0)),
    time = character(0), reason = character(0), status = character(0),
    stringsAsFactors = FALSE
  )

  list(
    today = today,
    users = users, doctors = doctors, patients = patients,
    appointments = appointments, surgeries = surgeries, meetings = meetings,
    schedule = schedule, research = research, academic_events = academic_events,
    history = history, reports = reports, medicines = medicines, health = health,
    weekday_stats = weekday_stats, month_stats = month_stats,
    consult_stats = consult_stats, workload_stats = workload_stats,
    activity = activity, requests = requests
  )
}

# Combine surgeries, meetings, academic events and appointments into
# ONE table used by the Administrator's Hospital Schedule page.
build_hospital_events <- function(demo) {
  doc_name <- function(ids) demo$doctors$name[match(ids, demo$doctors$doctor_id)]
  doc_dept <- function(ids) demo$doctors$department[match(ids, demo$doctors$doctor_id)]
  pat_name <- function(ids) demo$patients$name[match(ids, demo$patients$patient_id)]

  s <- demo$surgeries
  ev_surgery <- data.frame(
    date = s$date, time = s$time, type = "Surgery",
    title = paste0(s$procedure, " - ", pat_name(s$patient_id), " (", s$patient_id, ")"),
    department = doc_dept(s$doctor_id), doctor = doc_name(s$doctor_id),
    location = s$theatre, stringsAsFactors = FALSE
  )

  m <- demo$meetings
  ev_meeting <- data.frame(
    date = m$date, time = m$time, type = "Meeting", title = m$meeting,
    department = m$department, doctor = doc_name(m$doctor_id),
    location = m$location, stringsAsFactors = FALSE
  )

  a <- demo$academic_events
  ev_academic <- data.frame(
    date = a$date, time = a$time,
    type = ifelse(a$type == "Medical Conference", "Conference",
                  ifelse(a$type == "Seminar", "Seminar", "Seminar")),
    title = a$event, department = a$department, doctor = doc_name(a$doctor_id),
    location = a$location, stringsAsFactors = FALSE
  )
  # Workshops are listed as conferences/seminars for the filter
  ev_academic$type[a$type == "Workshop"] <- "Seminar"

  p <- demo$appointments
  p <- p[p$status == "Scheduled" & p$date >= demo$today, , drop = FALSE]
  ev_appt <- data.frame(
    date = p$date, time = p$time, type = "Major Appointment",
    title = paste0(p$reason, " - ", pat_name(p$patient_id), " (", p$patient_id, ")"),
    department = p$department, doctor = doc_name(p$doctor_id),
    location = "Outpatient Wing", stringsAsFactors = FALSE
  )

  ev <- rbind(ev_surgery, ev_meeting, ev_academic, ev_appt)
  ev <- ev[order(ev$date, ev$time), ]
  rownames(ev) <- NULL
  ev
}
