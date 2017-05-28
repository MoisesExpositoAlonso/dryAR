#!/bin/bash

find . -name 'Fig*pdf' -exec convert -density 300  {} "{}.png" \;
