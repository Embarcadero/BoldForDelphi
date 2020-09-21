@ECHO OFF
set GREP=grep
set OPT=-din
set DOGREP=%GREP% %OPT%
set FILESET=*.pas
set DFMFILES=*.dfm
set REMOVEOK=%GREP% -v "//ISOK"
ECHO ON


REM ***
REM *** Scan for D6isms in DFM-files
REM ***
%DOGREP% "designsize" %DFMFILES% 

REM ***
REM *** Scan for unraised exceptions
REM ***
%DOGREP% "EBold.*\.create" %FILESET% | %GREP% -vi "raise" | %GREP% -v ":=" | %GREP% -iv "constructor"

REM ***
REM *** Scan wrongly defined BoldGuards
REM ***
%DOGREP% ":[^=].*TBoldGuard" %FILESET% 

REM ***
REM *** Scan for exceptions with formatted message created without CreateFmt
REM ***
REM * could be extended to scan for string concatenation, but no rush with that.
%DOGREP% "raise.*Format\(" %FILESET% | %GREP% -vi "createfmt" | %REMOVEOK%

REM ***
REM *** Scan looping from 0 but not to xxx-1
REM ***
%DOGREP% "0 to" %FILESET% | %GREP% -v [^-] | %REMOVEOK%


REM ***
REM *** Scan for classes inheriting from TComponent instead of TBoldSubscribableComponent
REM ***
%DOGREP% "class\(TComponent\)" %FILESET%

REM ***
REM *** Scan for FIXME, CHECKME etc
REM ***
%GREP% -d "//.*ME" %FILESET% 

REM ***
REM *** Scan for Continue statements
REM ***
%DOGREP% continue; %FILESET% | %REMOVEOK%

REM ***
REM *** Scan for classes with implicit ancestor
REM ***
REM * Still finds too much!
%DOGREP% " "T.*=.*class[^;(]" %FILESET% | %GREP% -iv ":" | %GREP% -iv "'"|%GREP% -iv "class of"|%GREP% -iv " if "

REM ***
REM *** Scan for Exit statements
REM ***
%DOGREP% exit; %FILESET% | %REMOVEOK%

REM ***
REM *** Scan for Break statements
REM ***
%DOGREP% break; %FILESET% | %REMOVEOK%

REM ***
REM *** Scan for one line control statements
REM ***
%DOGREP% "if .*;" %FILESET% | %REMOVEOK%
%DOGREP% "while .*;" %FILESET% | %REMOVEOK%

REM ***
REM *** Scan for begin on line of control statement 
REM ***
%DOGREP% "while .*begin" %FILESET% | %REMOVEOK%
%DOGREP% "if .*begin" %FILESET% | %REMOVEOK%

REM ***
REM *** Scan for using wrongly named constant
REM ***
%DOGREP% CRLF %FILESET% | %GREP% -v BOLDCRLF

REM ***
REM *** Grep for using #13#10 istead of named constant
REM ***
REM BOLDCRLF
%DOGREP% "#13#10" %FILESET% | %REMOVEOK%
REM BOLDLF
%DOGREP% "#10" %FILESET% | %REMOVEOK%
REM BOLDCR
%DOGREP% "#13" %FILESET% | %REMOVEOK%
REM BOLDESC
%DOGREP% "#27" %FILESET% | %REMOVEOK%
REM BOLDNULL
%DOGREP% "#0" %FILESET% | %REMOVEOK%

REM ***
REM *** Scan for erronous spacing
REM ***
REM Space after left bracket
%DOGREP% "\[ " %FILESET%

REM ***
REM *** Space before right bracket
REM ***
%DOGREP% " \]" %FILESET% | %REMOVEOK%

REM ***
REM *** Space before left parenthesis
REM ***
%DOGREP% " \)" %FILESET%

REM ***
REM *** Space after right parenthesis
REM ***
%DOGREP% "\( " %FILESET%

REM ***
REM *** No space after comma
REM ***
%DOGREP% ",[^ ]" %FILESET%

REM ***
REM *** No space before equal sign
REM ***
%DOGREP% "[^ :\<\>=/]=" %FILESET%

REM ***
REM *** No space efter equal sign
REM ***
%DOGREP% "=[^ =']" %FILESET%

REM ***
REM *** No space efter assignment
REM ***
%DOGREP% ":=[^ ]" %FILESET%

REM ***
REM *** No space before assignment
REM ***
%DOGREP% "[^ ]:=" %FILESET%

REM ***
REM *** No space around binary operators 
REM ***
%DOGREP% "count-1" %FILESET%
%DOGREP% "Length\(.*-1" %FILESET%

REM *** END SCAN SPACING ***

REM ***
REM *** Scan for non-const string parameters
REM ***
%DOGREP% "[fp][ur][no]c.*\(.*:.*string[\) ]" %FILESET% | %GREP% -v const | %REMOVEOK%

REM ***
REM *** Scan for comparing to nil
REM ***
%DOGREP% "[^:]=.*nil" %FILESET% | %REMOVEOK%
%DOGREP% "<>.*nil" %FILESET% | %REMOVEOK%

REM ***
REM *** Scan for using one letter loop variables
REM ***
%DOGREP% "for [abcdefghjklmnopqrstuvxyz] := " %FILESET%

REM ***
REM *** REM Scan for using i as loop variable
REM ***
REM %DOGREP% "for i := " %FILESET% | %REMOVEOK%


REM ***
REM *** Scan for checking equal to true (xxx = True)
REM ***
%DOGREP% "if .*[^:]=.*[^'_]true[^']" %FILESET%

REM ***
REM *** Scan for checking equal to false (xxx = False)
REM ***
%DOGREP% "if .*[^:]=.*[^'_]false[^']" %FILESET%





