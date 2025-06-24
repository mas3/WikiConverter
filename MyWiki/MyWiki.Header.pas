unit MyWiki.Header;

interface

uses
  System.SysUtils, System.Generics.Collections, System.RegularExpressions,
  System.StrUtils;

type
  THeaderItem = class(TObject)
  private
    FCaption: String;
    FId: String;
    FLevel: Integer;
    FLine: Integer;
  public
    property Caption: String read FCaption write FCaption;
    property Id: String read FId write FId;
    property Level: Integer read FLevel write FLevel;
    property Line: Integer read FLine write FLine;
  end;

  THeader = class(TObject)
  private
    FHeaderList: TObjectList<THeaderItem>;
    FIds: TDictionary<String, THeaderItem>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(HeaderItem: THeaderItem);
    procedure Clear;

    property HeaderList: TObjectList<THeaderItem> read FHeaderList write FHeaderList;
  end;

implementation

var
  RegexDeleteUnusableCharacters: TRegEx;

{ THeader }

/// <summary>
/// Add header and Give ID to the header
/// </summary>
/// <param name="HeaderItem">Header item</param>
/// <remarks>
/// ID is created from the caption of the header.
/// Remove characters that cannot be used in IDs.
/// </remarks>
procedure THeader.Add(HeaderItem: THeaderItem);
var
  Id: String;
  BranchNumber: Integer;
begin
  Id := ReplaceStr(HeaderItem.Caption, ' ', '-');
  Id := RegexDeleteUnusableCharacters.Replace(Id, '');
  BranchNumber := 0;
  while FIds.ContainsKey(Id) do
  begin
    Inc(BranchNumber);
    Id := Format('%s-%d', [Id, BranchNumber]);
  end;

  HeaderItem.Id := Id;
  FHeaderList.Add(HeaderItem);
  FIds.Add(Id, HeaderItem);
end;

procedure THeader.Clear;
begin
  FHeaderList.Clear;
  FIds.Clear;
end;

constructor THeader.Create;
begin
  FHeaderList := TObjectList<THeaderItem>.Create;
  FIds := TDictionary<String, THeaderItem>.Create;
end;

destructor THeader.Destroy;
begin
  FHeaderList.Free;
  FIds.Free;

  inherited;
end;

initialization

RegexDeleteUnusableCharacters := TRegEx.Create('[\x00-\x2c./\x3a-\x40\x5b-\x5e`\x7b-\x7e]', [roCompiled]);

end.
