:- module('io', []).
:- use_module(library(shlib)).
:- ensure_loaded(utility).
:- fix_file_path('c/io.so', Path), 
    initialization(use_foreign_library(foreign(Path))).
:- module(io).
