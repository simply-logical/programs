WHAT CAN I FIND HERE?	

This directory contains Prolog programs from the book

			SIMPLY LOGICAL
		Intelligent Reasoning by Example
			Peter A. Flach
		    John Wiley & Sons, 1994
		   ISBN 0471 94152 2 (book)
		   ISBN 0471 94153 0 (disk)
		   ISBN 0471 94215 4 (book + disk)

Disks containing the same set of programs are available for 
Apple Macintosh, IBM PC, and Sun workstation.


The programs are grouped in the following files, reflecting the
structure of the book:

	chapter1
	chapter3
	hanoi
	chapter4
	chapter5
	chapter6
	chapter7
	chapter8
	chapter9
	section92
	section93
	library
	appendixB
	builtins

"hanoi" is the small program given at the start of Part II of the book.
"library" contains the utility predicates listed in Appendix A.2. Predicates
from this file are needed for programs in most of the other files. 
"builtins" contains the definition of some predicates that are available
in some Prologs (e.g. LPA Prolog), but not in others (e.g. Quintus
Prolog). If you have a Prolog of the second type, you need this file
whenever you need the file "library".
NB. Note that there is no file "chapter2", since this chapter does not
contain Prolog programs!


WHAT PROLOG DO I NEED TO RUN THESE PROGRAMS?	

The programs follow Edinburgh syntax, and the files are ordinary text
files. The programs have been tested in Quintus Prolog. They should run under
Sicstus Prolog as well. For other Prologs, some minor adjustments may be
necessary.
NB. Quintus Prolog refuses to redefine built-in predicates like
copy_term/2, and generates an ERROR message. These can safely be
ignored, as well as the WARNINGs about singleton variables.


ARE THE PROGRAMS EXACTLY AS IN THE BOOK?	

In principle, they are. If the book gives various versions of a
predicate, or describes several examples, all but one of these are
commented out. You will need to edit the file if you want to run the
other version or example.
Also, some of the meta-interpreters use a new predicate cl/2 instead of
clause/2. This is done because a call like
		?-clause(not(flies(tweety)),B)
where not/1 is a built-in predicate, generates an error-message in
several Prologs, instead of failing.


BUG OR FEATURE?	

There is no technical support for these programs. However, if you
discover what you think is a serious error in one of the programs,
please send a detailed report by email to

		Peter.Flach@kub.nl


DISCLAIMER	

These programs were written for educational purposes only. No claim is
made regarding their correctness, and no responsibility is accepted by
either the author or John Wiley & Sons for any damage resulting from the
use of these programs.



