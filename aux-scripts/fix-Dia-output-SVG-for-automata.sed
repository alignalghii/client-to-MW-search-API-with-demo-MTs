#!/bin/sed -f

s/\(ellipse\>.*\<stroke-width:\s*\)0\.2\>/\12/;

###################################################
# The `Dia` program is a Diagram drawing program for vector graphics).
# It has several drawing elements in various topics.
# The elements for the „Automata” topic is rather unorthodox.
# Notion of the final states is hard to distinguish from normal states.
#
# This sed script is able do a hackfix in the SVG output of a Dia program.
# It converts SVG-outputs of diagram of automata:
# in the „double circle” of a final state notation the inner circle gets a thicker inner contour line.
###################################################
