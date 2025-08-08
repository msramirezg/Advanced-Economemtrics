# ECON 4301 — Advanced Econometrics (2025-II)

**Course repo description (README)**

## What this repo is for

Materials for **Advanced Econometrics** focused on modern micro-econometric methods for **causal inference**—what to use, when, and why, with applications across labor, IO, health, and development.&#x20;

---

## How the course runs (quick facts)

* **Lecture:** Tue/Thu 8:00–9:20 (all sections). **Instructor:** Manuel Fernández. Office hours Tue/Thu 9:30–10:30 (W-902).
  **Complementary class:** Fri 8:00–9:20 by section (S1: J.F. Mora / SD-302; S2: D. Vlasak / SD-304; S3: D.E. Aristizábal / SD-303). TA: Rafael A. Sánchez.&#x20;
  For course/evaluation questions, contact **J.F. Mora**.&#x20;
* **Method:** Lectures + weekly complementary sessions; **read at least one listed reference before class**; attendance recommended; schedule may change.  &#x20;
* **Mandatory readings:** Marked with a **blue star** in the syllabus; they **are examined** (e.g., in quizzes).&#x20;
* **Biweekly group office hours (monitoría):** optional, scheduled in week 1.&#x20;

---

## Repo structure 

```
/syllabus/              # PDF + this README
/lectures/              # slides, notes
/complementary/         # Friday materials & practice
/assignments/           # Taller_01, Taller_02, Taller_03
/code/                  # src/ with master script run_all.(R|py|do)
/data/                  # raw/ processed/ (include readme on provenance)
/output/                # tables/ figures/ logs/
/reading-notes/         # 1-pagers on mandatory papers
/admin/                 # rubrics, policies, templates
```

**Replicability requirement:** every submission with code must be fully reproducible via a **.zip or GitHub repo**, with (i) data, (ii) **well-commented, organized code**, and (iii) a **master program** that runs the full pipeline without errors. Non-replicable work is **not graded**. &#x20;
**AI tools:** Allowed for coding **only if you disclose use**—add a brief comment next to each AI-assisted line/block.&#x20;

---

## Assessment & key dates

* **Exam 1 — 20%:** **Fri, Sept 26** (during complementary-class slot). &#x20;
* **Exam 2 — 20%:** Date set by university (may fall on last final-exam day).&#x20;
* **Workshops (Talleres) — 30% (3×10%):** Work **in pairs**, submit via Bloque Neón by **11:59 pm** on due dates. Late work not accepted. Schedule:
  T1 due **Sept 12**; T2 due **Oct 24**; T3 due **Nov 21**. &#x20;
* **Quizzes — 10% (2×5%):** Based on **mandatory readings**; prompt list released one week before; 1 question answered in class.&#x20;
* **Practical component — 20%:** choose **(A)** Individual **final paper** (proposal due **Oct 31, 11:59 pm**) or **(B)** Individual **data-handling practical** (brief issued **Nov 24**; due with final paper date).  &#x20;
  **More key dates:** Recess **Sept 29–Oct 4**; upload 30% grades by **Oct 10**; withdraw deadline **Oct 24**; final grades **Dec 12**.&#x20;

---

## Class-by-class outline & what to read

Below each topic you’ll see **Core chapters** (read before class) and the **Mandatory paper** (starred) that feeds the quiz pool.

1. **Introduction, counterfactuals & Rubin causal model**
   **Core:** IR ch.1–2; SC ch.1 & 4.1; Ge ch.3.&#x20;
   **Mandatory:** Angrist & Pischke (2010), *The Credibility Revolution*.&#x20;

2–4) **Linear regression model: estimators & properties**
**Core:** AP §3.1–3.2; Gr ch.2–3 & §4.1–4.4; BH ch.2–3, §4.1–4.9, §4.12–4.17, ch.6, §7.1–7.8.&#x20;
**Mandatory:** *(none specified; focus on core chapters and problem-solving).*

4–5) **Statistical inference: hypothesis tests, linear restrictions, delta method**
**Core:** Gr §5.1–5.7; BH ch.9.&#x20;
**Mandatory:** Ziliak & McCloskey (2008), “A Significant Problem.”&#x20;

6. **RCTs I**
   **Core:** BP ch.1; AP ch.1; Ge ch.1 & 11.&#x20;
   **Mandatory:** Banerjee et al. (2015), *The Miracle of Microfinance?*&#x20;

7. **RCTs II**
   **Core:** BP ch.2–4; Ge ch.2, 4, 15; AP ch.2.&#x20;
   **Mandatory:** *(same RCT mandatory paper applies for the module.)*

8. **Matching & propensity score matching (PSM)**
   **Core:** BP ch.6; Ge ch.8; CS ch.5.&#x20;
   **Mandatory:** Imbens (2015), “Matching Methods in Practice.”&#x20;

9–10) **Sharp Regression Discontinuity (RDD)**
**Core:** BP ch.8; AP ch.6; Ge ch.6; SC ch.6.&#x20;
**Mandatory:** Dell (2010), “The Persistent Effects of Peru’s Mining Mita.”&#x20;

11–15) **IV, 2SLS, Wald, LATE; fuzzy RD; RKD**
**Core:** BP ch.7; AP ch.4; Ge ch.5; SC ch.7; W1 ch.15.&#x20;
**Mandatory:** Londoño-Vélez & Saravia (2025), *Impact of being denied a wanted abortion*.

16–17) **Panels: fixed effects; dynamic panels; Anderson–Hsiao**
**Core:** BH §17.1–17.17, §17.26–17.27, §17.36–17.40; W2 ch.10, §11.4, §11.6.&#x20;
**Mandatory:** *(none specified; focus on core chapters.)*

17–20) **Difference-in-Differences & event studies**
**Core:** SC ch.9; AP ch.4; plus Roth et al. (2023) survey.&#x20;
**Mandatory:** Britto–Pinotti–Sampaio (2022); Kleven–Landais–Søgaard (2019).

21–23) **Synthetic Control & Synthetic DiD**
**Core:** SC ch.10; Abadie (2021); Arkhangelsky et al. (2021).&#x20;
**Mandatory:** Funke–Schularick–Trebesch (2023), *Populist leaders and the economy*.

24–25) **Discrete-choice models**
**Core:** W2 chs.13,15; Maddala pp.22–27, 41–45, 59–64; W1 §17.1.&#x20;
**Mandatory:** Attanasio–Meghir–Santiago (2012).

26–29) **Selection: incidental truncation, Heckman selection, Roy model**
**Core:** Gr §§19.1–19.3, 19.5; BH ch.27.&#x20;
**Mandatory:** Heckman (1979), “Sample Selection Bias as Specification Error.”

> **Quizzes use the mandatory papers**; a question list is shared one week before each quiz, and one question is drawn on quiz day.&#x20;

---

## Contributing & submission norms

* Follow the **replicable repo** pattern above; include a top-level `run_all.(R|py|do)` that builds tables/figures in `/output`.&#x20;
* If you used AI to help code, **annotate each assisted line/block** with a short comment.&#x20;

---

## Policies you should know

* **No attendance taken,** but you’re responsible for everything discussed in class (even if not in slides/syllabus).&#x20;
* **Grading/curve:** numeric 1.5–5.0; weighted average; no rounding; a curve may be applied without harming anyone or changing ranks. &#x20;
* **Academic integrity:** Fraud in **any** assessment (workshops, quizzes, exams, final work) is prohibited and can receive a grade of **0** plus disciplinary action.&#x20;


