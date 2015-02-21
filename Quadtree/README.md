# Quadtree
Basically, this is my solution to a programming assignment given in a 
textbook a GD co-worker of mine keeps on his shelf. The textbook in
question is something like "Algorithms and Data Structures in C++".

## Problem Description
A point quadtree is a 4-way tree used to represent points on a plane.
A node contains a city and pointers to four children that represent the
four quadrants NW, NE, SW, and SE. Write a program that takes the names
and locations of cities and inserts them into a quadtree. Then, the 
program should give the names of all cities located within distance r 
from a location or alternatively within distance r from a city c.  

The great circle formula is d = R * arccos(sin(lat1) * sin(lat2) + 
cos(lat1) * cos(lat2) * cos(lon2 - lon1)), where R is the Earth's radius.

## How to run
Type make and then ./Quadtree_Main

## Dependencies
g++
