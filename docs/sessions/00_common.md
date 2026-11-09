# Brief shared by all sessions

**Project.** A survey of about 2,500 heating experts (energy advisers, HVAC installers, chimney
sweeps). Each sees six vignettes of a household and recommends a heating technology. Max
Stellbrink, an economics PhD student at ZEW, is rebuilding the pipeline for a paper. He and his
coauthors read and check every line, so the code has to be easy to audit.

**You are one of five sessions working in parallel.** Read, in this order:
1. Your own brief in `docs/sessions/`.
2. `docs/data_contract.md`, and `docs/results_contract.md` if your brief names it.
3. The standards: `C:/Users/msk/.claude/skills/economics-r/SKILL.md`, then
   `references/style.md` and the references your task needs (`tables.md`, `figures.md`,
   `data-cleaning.md`, `numerical-methods.md`, `econometrics.md`) next to it. They are
   binding.
4. The profile, for facts: `C:/Users/msk/OneDrive - ZEW/projects/heating_transition/energy_expert_survey/.claude/rules/economics-r-project.md`.
   It is git-ignored, so your worktree does not have it. Its layout, output and colour sections
   describe the old project: this folder's `docs/` and `config.R` win.

**Max's explicit style wishes for this rebuild** (Max, 2026-11-09):
- Few, large functions that do a whole job, written as plain numbered steps in one file, rather
  than many small functions calling each other. Helpers only where they save real repetition.
- Only the checks that protect a result: unique keys, join row counts, unknown answers, the
  minimum cell. Anything else goes to `tests/` or nowhere. Too many checks lower readability.
- His vocabulary: `%>%`, `<-`, plain `for` loops, `tribble`, `case_when`, `if_else`. No `purrr`,
  no `mapply`/`sapply` with anonymous functions, no tidy-eval (`{{ }}`, `!!`, `.data[[ ]]`). A
  function outside the vocabulary gets a one-line comment at its first use.
- Comments explain why, not what. Lines up to about 80 characters; tribble rows may run longer.
- Script header in the skill's format, numbered sections, `file.path()` for paths.

**Safety.**
- You may not read or run anything on real survey data, and `I:` is off limits. Everything runs
  on synthetic data. Non-sensitive inputs in Dropbox `public_data_dropbox/` are fine to read.
- Do not touch `C:/Users/msk/Dropbox/Heating Transition/analysis_survey/output_dropbox/` except to
  read `reports_cleaning/codebook.csv`.
- Outputs: aggregates only, minimum cell as in `docs/data_contract.md`.
- Do not push. Do not commit `.claude/`. Do not edit `renv.lock`, `renv/` or `.Rprofile`.

**How to run R.** From PowerShell, in your worktree, with your session's letter:

```powershell
$env:RENV_CONFIG_SANDBOX_ENABLED = 'FALSE'
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = 'FALSE'
$env:RENV_PATHS_LIBRARY = 'C:/Users/msk/AppData/Local/renv/library/energy_expert_survey-692a0e44'
$env:CLAUDECODE = '1'
$env:EES_SYNTHETIC_DIR = 'C:/Users/msk/AppData/Local/Temp/ees/<letter>'
New-Item -ItemType Directory -Force -Path $env:EES_SYNTHETIC_DIR | Out-Null
& 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe' tests/simulate_data.R
& 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe' R/run_cleaning.R
& 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe' <your script>
```

Rscript from Git Bash does not work on this machine. Every script starts with
`source(file.path("R", "setup.R"))` and runs from the project root. Output goes to
`EES_SYNTHETIC_DIR/output`, never into the project or OneDrive. Do not install packages. The
machine has 8 cores and 31 GB; other sessions run at the same time, so keep test runs small and
pass `n_workers = 4` in tests of anything parallel.
LaTeX: `pdflatex` and `pdftoppm` are on PATH (MiKTeX, packages install on demand).

**The old code,** read-only, for reuse: `C:/Users/msk/AppData/Local/Temp/ees/old_code/` (git tag
`pre-restructure`). Port what is good, in the new style; never source it. It holds years of small
decisions: if you change a rule, say so.

**Ownership.** Edit only the files your brief lists. If you need something in a shared file
(`config.R`, `R/setup.R`, `R/functions/labels.R`, `exhibits.R`, `questionnaire.R`, `docs/`), do not
edit it: define it in your own file, and name it in your final report so that it can move.

**When you are done:**
1. Run your script top to bottom on synthetic data, with no errors and no unexplained warnings,
   and read the output: do counts, signs and magnitudes make sense?
2. Commit on your branch, one or a few commits, each message ending with
   `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.
3. Report back: what you wrote; what you ran and the key results; anything outside your ownership
   you need changed; and, as four separate lists, programming problems, statistical issues,
   identification issues, alternative specifications. Flag research decisions, do not make them.

**Your branch.** You work in a git worktree of its own. Before anything else, run `git log -1
--oneline` and make sure your branch starts at the local branch `restructure`: `git reset --hard
restructure` if it does not (your worktree is fresh, so nothing is lost). Five sessions share the
repository's history but not their files. Do not merge other sessions' branches.
