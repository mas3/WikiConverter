unit MyWiki.Footnote;

interface

uses
  System.SysUtils, System.Generics.Collections;

type
  TFootnoteItem = class(TObject)
  private
    FCaption: String; // footnote caption
    FCount: Integer; // number of use
    FId: String; // footnote ID
    FIndex: Integer; // order of appearance
  public
    /// <summary>
    /// Footnote caption
    /// </summary>
    property Caption: String read FCaption write FCaption;
    /// <summary>
    /// Number of use
    /// </summary>
    /// <remarks>
    /// 0: not used
    /// </remarks>
    property Count: Integer read FCount write FCount;
    /// <summary>
    /// Footnote ID
    /// </summary>
    property Id: String read FId write FId;
    /// <summary>
    /// Order of appearance
    /// </summary>
    /// <remarks>
    /// 0: not used
    /// </remarks>
    property Index: Integer read FIndex write FIndex;
  end;

  TFootnote = class(TObject)
  private
    FItems: TObjectDictionary<String, TFootnoteItem>;
    FOrderOfAppearance: TList<TFootnoteItem>;

    function GetFootnoteItem(Index: Integer): TFootnoteItem;
  public
    class function AnchorMark: String;
    class function ClassAnchor: String;
    class function ClassRefAnchor: String;
    class function ClassSection: String;
    class function IdPrefix: String;
    class function RefIdPrex: String;

    constructor Create;
    destructor Destroy; override;

    procedure Add(const Id: String; const Caption: String);
    function FindByIdAndMarkUse(const Id: String): TFootnoteItem;
    function UseIndex: Integer;

    property Items[Index: Integer]: TFootnoteItem read GetFootnoteItem;
  end;

implementation

{ TFootnote }

/// <summary>
/// Add footnote item
/// </summary>
/// <param name="Id">Footnote ID</param>
/// <param name="Caption">Footnote caption</param>
/// <remarks>
/// If the ID already exists, it will be ignored.
/// </remarks>
procedure TFootnote.Add(const Id, Caption: String);
var
  Key: String;
begin
  Key := LowerCase(Id);
  if FItems.ContainsKey(Key) then
    Exit;

  var
    Item: TFootnoteItem := TFootnoteItem.Create;
  Item.Id := Key;
  Item.Caption := Caption;
  Item.Count := 0;
  Item.Index := 0;

  FItems.Add(Key, Item);
end;

class function TFootnote.AnchorMark: String;
begin
  Result := '↩';
end;

class function TFootnote.ClassAnchor: String;
begin
  Result := 'fnarrow';
end;

class function TFootnote.ClassRefAnchor: String;
begin
  Result := 'fnref';
end;

class function TFootnote.ClassSection: String;
begin
  Result := 'footnotes';
end;

constructor TFootnote.Create;
begin
  FItems := TObjectDictionary<String, TFootnoteItem>.Create([doOwnsValues]);
  FOrderOfAppearance := TList<TFootnoteItem>.Create;
end;

destructor TFootnote.Destroy;
begin
  FItems.Free;
  FOrderOfAppearance.Free;

  inherited;
end;

/// <summary>
/// Find footnote item by ID and mark use
/// </summary>
/// <param name="Id">Footnote ID</param>
/// <returns>Footnote item</returns>
/// <remarks>
/// If the ID does not exist, it will return nil.
/// </remarks>
function TFootnote.FindByIdAndMarkUse(const Id: String): TFootnoteItem;
var
  Key: String;
  Item: TFootnoteItem;
begin
  Key := LowerCase(Id);
  if not FItems.ContainsKey(Key) then
    Exit(nil);

  Item := FItems[Key];
  if Item.Index = 0 then
  begin
    // first use
    FOrderOfAppearance.Add(Item);
    Item.Index := FOrderOfAppearance.Count;
  end;
  Item.Count := Item.Count + 1;

  Result := Item;
end;

/// <summary>
/// Get footnote item by index
/// </summary>
/// <param name="Index">Index</param>
/// <returns>Footnote item</returns>
/// <remarks>
/// Index is order of appearance.
/// </remarks>
function TFootnote.GetFootnoteItem(Index: Integer): TFootnoteItem;
begin
  Result := FOrderOfAppearance[Index];
end;

class function TFootnote.IdPrefix: String;
begin
  Result := 'fn-';
end;

class function TFootnote.RefIdPrex: String;
begin
  Result := 'fnref-';
end;

/// <summary>
/// Get the index of the next use
/// </summary>
/// <returns>Index of the next use</returns>
function TFootnote.UseIndex: Integer;
begin
  Result := FOrderOfAppearance.Count;
end;

end.
