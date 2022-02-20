# Auxiliary scripts

Auxiliary scripts (Perl, bash, AWK or sed programs) for developers:

- `fix-the-final-state-notation-of-finite-automata-diagrams-of-Dia-program.sed`: this sed-script can fix the finite-state-machine diagrams of the Dia program (only when exported to SVG).
  The Dia program uses a non-standard notation for the final states of a state machine (automata). This sed-script makes the inner circle of the double circle thicker in its contour.
  - `rename-modules.bash`: an auxiliary bash script to help the developer to keep Haskell module imports consistent. In other words: it helps the developer to rename Haskell module path names and import paths in the whole Haskell sourcecode accordingly if a module is renamed or relocated.
  - `image-and-haskell-link-lifting.sed`: an auxiliary sed script to help the developer to keep the  README file's asset sources ands repo-local links (image sources and linkified Haskell module names) consistent with the renamings/relocations of image asset files or Haskell sourcecode folder relocations.
