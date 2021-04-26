
{ Global compiler directives }
{$include bold.inc}
unit BoldDefsDT;

interface
uses
  BoldDefs;

const
{$IFDEF BOLD_DELPHI6}
  HelpFile = 'boldd6.hlp';
{$ENDIF}
{$IFDEF BOLD_DELPHI7}
  HelpFile = 'boldd7.hlp';
{$ENDIF}

  ATTRIBUTEWIZARDHELPFILE = 'boldattributewizard.hlp';
  MODELEDITORHELPFILE = 'boldmodeleditor.hlp';

  URLBoldSoft = 'http://www.boldsoft.com';
  URLBoldForDelphi = 'http://www.boldsoft.com';
  URLSupport = 'http://www.boldsoft.com';

  GETTINGSTARTEDPATH = '\doc\frames\';
  GETTINGSTARTEDDOCNAME = 'getting_started_f.html';

  regPath = 'Path';
  regPathRoot = 'RootDir';

  {Delphi Registry keys}
  regLibrary = 'Library';
  regSearchPath = 'Search Path';

  { Component editor verbs }
  BOLD_OPENUMLEDITOR = 'Open Bold UML Editor';

{$IFDEF BOLD_DELPHI}
  BOLDSYM_ASSIGNMENT = ':=';
  BOLDSYM_BEGIN = 'begin';
  BOLDSYM_END = 'end;';
  BOLDSYM_THEN = 'then';
  BOLDSYM_ASSIGNED = 'Assigned';
  BOLDSYM_AND = 'and';
  BOLDSYM_TYPEINTEGER = '';
  BOLDSYM_RETURNRESULT = '';
  BOLDSYM_POINTERDEREFERENCE = '.';
  BOLDSYM_QUOTECHAR = '''';
{$ENDIF}

{$IFDEF BOLD_BCB}
  BOLDSYM_ASSIGNMENT = '=';
  BOLDSYM_BEGIN = '{';
  BOLDSYM_END = '}';
  BOLDSYM_THEN = '';
  BOLDSYM_ASSIGNED = '';
  BOLDSYM_AND = '&&';
  BOLDSYM_TYPEINTEGER = 'int ';
  BOLDSYM_RETURNRESULT = BOLDCRLF + BOLDCRLF + '  return result;';
  BOLDSYM_POINTERDEREFERENCE = '->';
  BOLDSYM_QUOTECHAR = '"';
{$ENDIF}

implementation

uses
  BoldRev;

initialization

end.
