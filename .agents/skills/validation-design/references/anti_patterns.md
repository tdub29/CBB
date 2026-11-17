# Validation Anti-Patterns

1. Random K-fold on games/seasons
2. Fitting preprocessors on train+test
3. Early stopping on the true test fold
4. Dropping ugly seasons after metrics
5. Hyperparameter search on the final holdout
6. Mean-only reporting without per-fold table
7. No baseline comparison
8. Claiming game accuracy from team-game double rows without care
9. Reusing a small holdout with full readouts. When the sample is too small to keep a final holdout untouched (a few seasons, a handful of tracking weeks), consult it the Thresholdout way: use the training-fold score while it agrees with the holdout within a threshold (the paper's example T = 0.01, plus Laplace noise), reveal a noisy holdout score only when they disagree, and stop after a fixed budget of disagreements. Source: Dwork, Feldman, Hardt, Pitassi, Reingold and Roth, "Generalization in Adaptive Data Analysis and Holdout Reuse", arXiv:1506.02629, Sec. 4.1; short version "The reusable holdout", Science 349:636 (2015).
10. Calling a seen window pre-registered, untouched, blind or scored once. Score-once guards protect a window's bytes, not its information: if any model, analysis, diagnostic, reviewer rerun or published report already scored the window, it is development. Keep the window ledger and name the first window nobody has scored as the confirmation.
11. Setting a promotion bar by judgment (raising it "because several candidates were tried") instead of from a null simulated over all registered families. The bar for the n-th family comes from that null, and every pass reports n (method: `experiment-log`).
12. A code constant that differs from the charter bar, for example a builder that still applies a 0.80 bootstrap-share bar after the charter raised it. The charter sets every threshold before results and the code asserts it at a named file and line, so a mismatch fails the run instead of shipping.
13. Adopting a fragile pass with a "known risk" addendum. Disclosure is not a gate: a pass whose resampling support is below the replacement bar is fragile and stays research-only until prospective scoring.
