
{ Global compiler directives }
{$include bold.inc}
unit BoldUndoInterfaces;

interface

uses
  BoldValueSpaceInterfaces;

const
// from BoldSubscription
  beUndoBlock = 60;
  beRedoBlock = 61;
  beUndoSetCheckpoint = 62;
  beUndoChanged = 63;

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
    function GetContent: String;
    function GetCaption: String;
    function GetCreated: TDateTime;
    function GetObjectCount: integer;
    function GetIndex: integer;
    property Name: string read GetName;
    property ValueSpace: IBoldValueSpace read GetValueSpace;
    property ContainsChanges: Boolean read GetContainsChanges;
    property Content: String read GetContent;
    property Created: TDateTime read GetCreated;
    property Caption: String read GetCaption;
    property ObjectCount: integer read GetObjectCount;
    property Index: integer read GetIndex;
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
    procedure Clear;
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
    function GetUniqueBlockName: string;
    function CanUndoBlock(const BlockName: string): Boolean;
    function CanRedoBlock(const BlockName: string):Boolean;
    function GetEnabled: Boolean;
    procedure SetEnabled(value: Boolean);
    procedure UndoLatest;
    procedure RedoLatest;
    procedure UndoBlock(const BlockName: string);
    procedure RedoBlock(const BlockName: string);
    function SetCheckPoint(const ACaption: string = ''): string; overload;
    procedure ClearAllUndoBlocks;
    function GetCurrentUndoBlockHasChanges: boolean;
    procedure ClearCurrentUndoBlock;
    function GetIsEmpty: boolean;
    property UndoList: IBoldUndoList read GetUndoList;
    property RedoList: IBoldUndoList read GetRedoList;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property CurrentUndoBlockHasChanges: boolean read GetCurrentUndoBlockHasChanges;
  end;

implementation

end.
