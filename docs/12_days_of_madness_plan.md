# 12 Days of Madness — Twitter Campaign Plan

Analytics insights posted once per day in the 12 days leading up to the First Round of March Madness.

**Assumption:** First Round typically starts on a Thursday (First Four on Tue/Wed). So Day 1 = 12 days before that Thursday, and Day 12 = 1 day before (Wednesday before First Round).

---

## Your Topics (7)

| # | Topic | Data hints |
|---|-------|------------|
| 1 | **Strength of Schedule** | ELITESOS, Bart Torvik SOS, conference strength |
| 2 | **Size** | EFFHGT, AVGHGT (effective/avg height), frontcourt depth |
| 3 | **Experience** | EXP. (experience), veteran-heavy rosters |
| 4 | **Turnovers** | TOR, TORD, TOV%, TOV%D — low TO% on offense, high TO%D on defense |
| 5 | **3PT Shooting** | 3P%, 3P%D, 3PRATE, 3PRATED — volume + accuracy + defense |
| 6 | **Rebounding** | ORB, DRB, OREB%, OPOREB% — second-chance points |
| 7 | **Coaching Experience** | Years at school, NCAA Tournament record, Final Fours |

---

## Suggested Additions (5) — to reach 12 days

| # | Topic | Why it fits | Data hints |
|---|-------|-------------|------------|
| 8 | **Tempo (Adj T.)** | Pace matters vs slow vs fast teams; bracket matchups | Adj T., RAWT from Torvik |
| 9 | **Defense (AdjDE / EFGD%)** | "Defense travels" — tourney truism | AdjDE, EFGD%, 2P%D, BLKED% |
| 10 | **Free Throw Rate** | Close games, end-of-game FTs | FTR, FTRD, FT% — get to the line, make them |
| 11 | **Seeds & Upsets** | Historical seed advancement rates, Cinderellas | seed, predround, WAB, Barthag |
| 12 | **My Favorite Teams** | Your picks / dark horses / bracket darlings | — |

---

## Proposed 12-Day Order

Build from foundations → specific skills → big picture → finale.

| Day | Topic | Rationale |
|-----|-------|-----------|
| **1** | Strength of Schedule | Set context: who played whom? Start with schedule quality |
| **2** | Size | Physical foundation — height and length |
| **3** | Experience | Roster maturity — veterans vs youth |
| **4** | Turnovers | Ball security — tournament pressure, careless mistakes |
| **5** | 3PT Shooting | Shot profile — variance, hot/cold shooting |
| **6** | Rebounding | Glass — second chances and limiting opponents |
| **7** | Coaching Experience | Sideline factor — tourney résumés |
| **8** | Tempo | Pace — style matchups, who controls rhythm |
| **9** | Defense | "Defense travels" — AdjDE, EFGD%, blocks |
| **10** | Free Throw Rate | End-game factor — getting to the line, making FTs |
| **11** | Seeds & Upsets | Historical seed performance, upset predictors |
| **12** | My Favorite Teams | Your picks, dark horses, bracket darlings — one day before tip-off |

---

## Post Ideas by Topic

| Topic | Angle ideas |
|-------|-------------|
| **SOS** | "Teams that played a top-50 SOS and still won 25+ games" / "Soft schedules that could bite in March" |
| **Size** | "Tallest backcourts" / "Frontcourt length vs typical tourney opponents" |
| **Experience** | "Most/least experienced rosters in the field" / "Experience vs seed — who over- or under-indexes?" |
| **Turnovers** | "Lowest TO% in the field" / "Defenses that force the most turnovers" |
| **3PT** | "Highest 3P% + volume combos" / "3PT variance: who can win/lose on one night?" |
| **Rebounding** | "Best offensive rebounding teams" / "ORB% as second-chance insurance" |
| **Coaching** | "Coaches with the most NCAA Tournament wins" / "First-timers vs veterans" |
| **Tempo** | "Slowest vs fastest teams" / "Matchup alert: tempo clash in [region]" |
| **Defense** | "Top 10 AdjDE in the field" / "Elite defense + mediocre offense — can they survive?" |
| **FT Rate** | "Get to the line: highest FTR teams" / "End-game FT% — who do you want shooting?" |
| **Seeds** | "Historical advancement by seed" / "Seed lines most likely to get upset" |
| **My Favorite Teams** | Your top teams, sleeper picks, bracket favorites — personal call before tip-off |

---

## Data Sources (Your Pipeline)

- **Bart Torvik**: `data/external/trank_team_table_data*.csv`, team tables
- **Training data**: `data/processed/cbbtrainingdata_YYYYMMDD.csv`
- **Column reference**: `docs/data_sources.md`, `src/pipelines/madness.py` (column mapping)

---

## Logistics

- **Posting time:** Pick a consistent slot (e.g. 9am ET).
- **Format:** 1–2 charts or tables + 1–2 sentences per tweet; thread if needed.
- **Prep:** Run pipeline close to Selection Sunday to get current-year features.
- **Coach data:** May need manual or external source (Wikipedia, sports-reference, etc.) — not in Torvik.

---

*Edit this doc as you lock in topics and dates. Good luck with the run!*
