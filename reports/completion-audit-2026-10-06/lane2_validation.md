# Lane 2: source time, leakage and validation

Lead auditor, 2026-10-06. All checks read-only; scanner output in `scratch/leakage_scan.json`.

## Verdict

No leakage found in the frozen model (log5) or its seed-pair baseline. Two hindsight channels exist
in the candidate menu and the data vintage; both are disclosed or bounded, neither touches the
headline comparison in a way that favours log5.

## Checks

| # | Check | Result |
|---|---|---|
| V1 | **Do team inputs contain tournament games?** Champion WINS/GAMES for all 18 seasons equal the pre-tournament record (e.g. 2008 Kansas 30-3 in table vs 37-3 final; 2024 UConn 31-3 vs 37-3; 2025 Florida 30-4 vs 36-4). | PASS, type=R excludes NCAA games |
| V2 | **Does a Selection Sunday pull reproduce the stored rows?** 2025 pull through 2025-03-16, 68/68 tournament teams: GAMES identical, Barthag max abs diff 0.0017 (median 0.0002), AdjOE max 0.05. | PASS, rows are a Selection Sunday snapshot |
| V3 | **2026 staleness.** Stored 2026 row is the 2026-03-05 export: vs the 03-15 pull, GAMES short by median 3 (max 5), Barthag diff median 0.008, max 0.044. Disclosed in the report; refreshed-input sensitivity -0.0054 [-0.0113, +0.0004] vs -0.0059 as scored. | DISCLOSED, conclusion unchanged |
| V4 | **Vintage drift 2008-2025.** Pre-2026 staging file vs live file: 5,971/5,971 rows match on year+Team; AdjOE, AdjDE, Barthag, WINS, GAMES, seed, EXP., TALENT identical (max diff 0). | PASS, only 2026 rows were added |
| V5 | **Outcome columns.** `OUTCOME_COLUMNS` (round, finish, PASE, PAKE, ov_rec, postseason strings) dropped before scoring (`run_powered_comparison.py:329`, `:521`) and asserted disjoint from `TEAM_COLS` (`:1157`). Leakage scanner: forbidden-name, target-identity, duplicate and near-perfect-correlation checks PASS. | PASS |
| V6 | **Walk-forward splits.** Every fit for year Y uses `year < Y` (`:287-288`, `:347`, `:352`, `:506`); seed history refused if it overlaps Torvik games (`:479`). Poisoning test passes. log5 has no fitted parameter, so its 2021-2026 predictions cannot be fit to outcomes. | PASS |
| V7 | **Games data integrity.** 1,134 games = 18 x 63; 0 team-name join misses; 0 seed mismatches vs training rows; one no-contest (2021 Oregon-VCU) excluded by `oriented_games`; 2020 excluded. Seed history 1985-2007 = 23 x 63. | PASS |
| V8 | **Bracket-time availability.** Inputs are Barthag (Selection Sunday table) and committee seed, both public before tip. Later-round game rows use the matchups that occurred; the path-aware advancement probabilities are the true pre-tournament forecast and are scored separately. | PASS |

## Hindsight channels (not leakage, but must be stated)

| # | Channel | Bound |
|---|---|---|
| H1 | **Algorithm hindsight.** Historical rows are Torvik's *current* algorithm applied to old seasons, not what Torvik published at the time. Unavoidable without archived tables. | Affects every candidate and T-Rank equally; disclose |
| H2 | **Menu hindsight (already disclosed in the report Limits).** The nine topic metrics in `logit_topics` variants were chosen on 2008-2025/2026 outcomes (`scripts/topic_inference.py`), which include the 2021-2026 test years. The 28-variant menu was written in September 2026 by someone who had watched those tournaments. | log5 is a textbook formula not drawn from the topic work; topic variants lost on 2010-2019; White's Reality Check 0.099 accounts for the search |
| H3 | **Selection-window reuse.** 2010-2019 picked the model and also appears in the "existing model 2010-2026" row. | Report labels 2021-2026 as the only untouched test; fine |

## Remaining work from this lane

None blocking. Should-fix: one sentence in the report naming H1 (algorithm hindsight); H2 is already in its Limits section.
