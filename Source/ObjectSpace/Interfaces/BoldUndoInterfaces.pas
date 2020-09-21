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
    function GetItems(Index: integer): IBoldUndoBlock;
    function GetItemsByName(Name: string): IBoldUndoBlock;
    function GetTopBlock: IBoldUndoBlock;
    function IndexOf(BlockName: string): integer;
    function CanMoveBlock(CurIndex, NewIndex: integer): Boolean;
    procedure MergeBlocks(DestinationBlockName, SourceBlockName: string);
    procedure MoveToTop(BlockName: string);
    procedure MoveBlock(CurIndex, NewIndex: integer);
    procedure RenameBlock(OldName, NewName: string);
    function RemoveBlock(BlockName: string): Boolean;
    property Count: Integer read GetCount;
    property Items[Index: integer]: IBoldUndoBlock read GetItems; default;
    property ItemsByName[Name: string]: IBoldUndoBlock read GetItemsByName;
    property TopBlock: IBoldUndoBlock read GetTopBlock;
  end;

  IBoldUndoHandler = interface
  ['{FCAC02E0-0067-41F1-B755-C6F162EA40E7}']
    function GetUndoList: IBoldUndoList;
    function GetRedoList: IBoldUndoList;
    function GetUniqueBlockName(SuggestedName: string): string;
    function CanUndoBlock(BlockName: string): Boolean;
    function CanRedoBlock(BlockName: string):Boolean;
    function GetEnabled: Boolean;
    procedure SetEnabled(value: Boolean);
    procedure UndoLatest;
    procedure RedoLatest;
    procedure UndoBlock(BlockName: string);
    procedure RedoBlock(BlockName: string);
    procedure SetNamedCheckPoint(CheckPointName: string);
    procedure SetCheckPoint;
    procedure ClearAllUndoBlocks;
    property UndoList: IBoldUndoList read GetUndoList;
    property RedoList: IBoldUndoList read GetRedoList;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    // add state here
  end;


implementation

end.
