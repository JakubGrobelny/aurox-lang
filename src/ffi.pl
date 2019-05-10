:- module('io', []).
:- use_module(library(shlib)).
:-  fix_file_path('c/io.so', Path),
    use_foreign_library(foreign(Path)).
