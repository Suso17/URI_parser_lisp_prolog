A simple URI parser that can recognize some basic URIs from the standard, mailto, news, tel, fax and zos schemes. 

The program is written twice in two different languages: SWI-Prolog and Common Lisp to show how differently the two languages work.

The Prolog program recognizes URIs using base cases and recursion to parse the given string, whereas the Lisp one utilizes a series of functions.

Both programs have display functions to generate a more user-friendly output after the parsing.

In both programs the starting point is the uri_parse or uri-parse function, which recognizes the scheme and proceeds to parse the remaining part of the string accordingly.

The uri_parse/2 predicate in Prolog accepts a string (the URI) and a uri object which should be composed like shown in the first lines of the file.

The uri-parse function in Lisp accepts a string (again the URI).

Both functions generate a uri object (or a structure in Lisp) which then can be put in the uri_display function. 