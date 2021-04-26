
{ Global compiler directives }
{$include bold.inc}
unit BoldUndoInterfaces;

interface

uses
  BoldValueSpaceInterfaces;

type
  { forward declarations }
  IBoldUndoBlock = interface;
  IBoldUndoList = interface;
  IBoldUndoHandler = interface;

  TBoldUndoState = (busNormal, busUndoing, busRedoing);  

  IBoldUndoBlock = interface
  ['{F56612C3-7D33-4D60-9737-71ECE8E54D56}']
    function GetName: string;
    function GetValueSpace: IBoldValueSpace;
    function GetContainsChanges: Boolean;
    property Name: string read GetName;
    property ValueSpace: IBoldValueSpace read GetValueSpace;
    property ContainsChanges: Boolean read GetContainsChanges;
  end;

  IBoldUndoList = interface
  ['{C4FF7FBB-C14A-4B2D-82C5-BED76B80D81B}']
    function GetCount: Integer;
    function GetItem(Index: integer): IBoldUndoBlock;
    function GetItemByName(const Name: string): IBoldUndoBlock;
    function GetTopBlock: IBoldUndoBlock;
    function IndexOf(const BlockName: string): integer;
    function CanMoveBlock(CurIndex, NewIndex: integer): Boolean;
    procedure MergeBlocks(const DestinationBlockName, SourceBlockName: string);
    procedure MoveToTop(const BlockName: string);
    procedure MoveBlock(CurIndex, NewIndex: integer);
    procedure RenameBlock(const OldName, NewName: string);
    function RemoveBlock(const BlockName: string): Boolean;
    property Count: Integer read GetCount;
    property Items[Index: integer]: IBoldUndoBlock read GetItem; default;
    property ItemByName[const Name: string]: IBoldUndoBlock read GetItemByName;
    property TopBlock: IBoldUndoBlock read GetTopBlock;
    function GetContainsChanges: Boolean;
    property ContainsChanges: Boolean read GetContainsChanges;
  end;

  IBoldUndoHandler = interface
  ['{FCAC02E0-0067-41F1-B755-C6F162EA40E7}']
    function GetUndoList: IBoldUndoList;
    function GetRedoList: IBoldUndoList;
    function GetUniqueBlockName(const SuggestedName: string): string;
    function CanUndoBlock(const BlockName: string): Boolean;
    function CanRedoBlock(const BlockName: string):Boolean;
    function GetEnabled: Boolean;
    procedure SetEnabled(value: Boolean);
    procedure UndoLatest;
    procedure RedoLatest;
    procedure UndoBlock(const BlockName: string);
    procedure RedoBlock(const BlockName: string);
    procedure SetNamedCheckPoint(const CheckPointName: string);
    procedure SetCheckPoint;
    procedure ClearAllUndoBlocks;
    property UndoList: IBoldUndoList read GetUndoList;
    property RedoList: IBoldUndoList read GetRedoList;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;


implementation

uses
  BoldRev;

initialization
end.
