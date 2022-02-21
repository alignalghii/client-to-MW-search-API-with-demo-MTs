# Auxiliary scripts

Auxiliary scripts (Perl, bash, AWK or sed programs) for developers:

- **`fix-Dia-output-SVG-for-automata.sed`**: this sed-script can fix the finite-state-machine diagrams of the Dia program (only when exported to SVG).
  The Dia program uses a non-standard notation for the final states of a state machine (automata). This sed-script makes the inner circle of the double circle thicker in its contour.
- **`rename-modules.bash`**: an auxiliary bash script to help the developer to keep Haskell module imports consistent. In other words: it helps the developer to rename Haskell module path names and import paths in the whole Haskell sourcecode accordingly if a module is renamed or relocated.
- **`lift-READMEs-asset-sources-and-local-links.sed`**: an auxiliary sed script to help the developer to keep the  README file's asset sources ands repo-local links (image sources and linkified Haskell module names) consistent with the renamings/relocations of image asset files or Haskell sourcecode folder relocations.
- Automatic update of the name of the executable in `.gitignore`: You can set the name of the executable in `src/Makefile`. The name of the executable is automatically updated (rewritten) in `.gitignore` at each `make` call.
    - **`rewrite-execname-in.bash`**:  this is an auxiliary bash script, used by `Makefile` for this automatic update
    - **`rewrite-execname-in.awk`**:  this is an auxiliary AWK script, used by `rewrite-execname-in.bash`! Thus, the AWK file serves as a „*second-degree*” auxiliary script (i.e. an aux script of another aux script).
