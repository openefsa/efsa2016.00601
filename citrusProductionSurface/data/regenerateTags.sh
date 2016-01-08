#!/bin/bash
find . -name "*.R" | xargs etags --language=none --regex='/\([^ \t]+\)[ \t]*<-[ \t]*function/\1/' -
