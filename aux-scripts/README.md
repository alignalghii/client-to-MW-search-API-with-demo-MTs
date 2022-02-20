# Auxiliary scripts

Auxiliary scripts (Perl, bash, AWK or sed programs) for developers:

- `fix-the-final-state-notation-of-finite-automata-diagrams-of-Dia-program.sed`: this sed-script can fix the finite-state-machine diagrams of the Dia program (only when exported to SVG).
  The Dia program uses a non-standard notation for the final states of a state machine (automata). This sed-script makes the inner circle of the double circle thicker in its contour.