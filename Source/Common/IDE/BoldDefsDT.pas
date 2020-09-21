unit BoldDefsDT;

interface

uses
  BoldDefs; // BOLDCRLF for BCB

const
	HelpFile = 'bold90.hlp';

  ATTRIBUTEWIZARDHELPFILE = 'boldattributewizard.hlp';
  MODELEDITORHELPFILE = 'boldmodeleditor.hlp';

  URLBoldSoft = 'http://www.borland.com';
  URLBoldForDelphi = 'http://www.borland.com';
  URLSupport = 'http://www.borland.com';

  GETTINGSTARTEDPATH = '\doc\frames\';  //NOTE: Must end with backslash!
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

end.
