# Haskell QIF Library

This library is designed to do simple parsing and rendering of QIF financial
documents. The goal is to support transmission of financial information between
programs, not to serve as the back end to any particular financial software.

There does not appear to be a specification of QIF by any standards body, so
this parser attempts to be as general as possible given the limited
documentation I've seen. I have tested it against the output of at least one
financial program; I don't know that it will work with others, though.

If you find a QIF file for which this parser does not work, please report the
problem and, if possible, provide the file.
