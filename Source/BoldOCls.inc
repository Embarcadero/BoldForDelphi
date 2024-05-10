type
  ALexClass = class(SSLex)
  public
  function TokenToString( TheToken : Longint) : string; override;
end;

type
   AYaccStackElement = class(SSYaccStackElement)
   public
      start, stop,                         // Used for enumerations;
      Node : TBoldOclNode;
      Name : String;
      SimpleTypeSpecifierIsEnum : Boolean; // Otherwise it is a pathname. Data ís stored in the DeclaratorTypelist
      DeclaratorTypeList,
      List : TList;                        // Of AYaccStackElement
      Offset : integer;
      CollectionKind : TBOCollectionKind;
      nameIstype : Boolean;
      function Clone : AYaccStackElement;
      procedure FreeAllOwnedElements;
      constructor Create;
      constructor CreateOffs( offs : integer );
   end;

type
  AYaccClass = class(SSYacc)
  public
  finalValue : AYaccStackElement;
  function StackElement : SSYaccStackElement; override;
  function Reduce( TheProduction, TheSize : Longint) : SSYaccStackElement; override;
end;


