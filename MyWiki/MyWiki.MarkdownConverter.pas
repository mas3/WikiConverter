unit MyWiki.MarkdownConverter;

interface

uses
  System.SysUtils, System.RegularExpressions, System.Classes, System.Math,
  System.Generics.Collections, System.StrUtils, System.NetEncoding,
  System.Character,
  MyWiki.WikiConverter, MyWiki.WikiNode, MyWiki.HtmlEntities, MyWiki.Footnote,
  MyWiki.Header;

type
  TTableAligns = array of String;
  TTableCells = array of String;

  TDelimiter = class(TObject)
  private
    FCanClose: Boolean;
    FCanOpen: Boolean;
    FLength: Integer;
    FMark: Char;
    FPrevLength: Integer;
  public
    property CanClose: Boolean read FCanClose write FCanClose;
    property CanOpen: Boolean read FCanOpen write FCanOpen;
    property Length: Integer read FLength write FLength;
    property Mark: Char read FMark write FMark;
    property PrevLength: Integer read FPrevLength write FPrevLength;
  end;

  TCloser = record
    Length: Integer;
    Position: Integer;
  end;

  TMarkdownConverter = class(TWikiConverter)
  private
    FBlockTags: TDictionary<String, Integer>;
    FLinkDefinitions: TLinkDefinitions;
    FHtmlEntities: TDictionary<String, String>;

    procedure AddBeforeTextNode(const Node: TWikiNode; const Text: String);
    procedure AddOrderdList(const ParentNode: TWikiNode;
      const Lines: TStringList);
    procedure AddTableRow(const Line: String; const CellCount: Integer;
      const TableAligns: TTableAligns; const ParentNode: TWikiNode);
    procedure AddUnorderdList(const ParentNode: TWikiNode;
      const Lines: TStringList);
    function CanBeginEmphasis(const Text: String; const BPos, EPos: Integer;
      const Mark: String): Boolean;
    function CanEndEmphasis(const Text: String; const BPos, EPos: Integer;
      const Mark: String): Boolean;
    function CountChar(const Text: String; const Ch: Char): Integer;
    function CountLine(const Text: String): Integer;
    function Encode(const Text: String; const WithHtmlEncode: Boolean): String;
    function ExistsElement(const Node: TWikiNode;
      const TargetType: TNodeType): Boolean;
    function ExistsLink(const Text: String): Boolean;
    function GetAtSkipEscapedChar(const Text: String; var CPos: Integer): Char;
    function GetAutoLinkEndPosition(const Text: String;
      const Offset: Integer): Integer;
    function GetCodeCloseMarkStartingPosition(const Text, Mark: string;
      Offset: Integer): Integer;
    function GetCodeSpanEndPosition(const Text: String; const Mark: Char;
      Offset: Integer): Integer;
    function GetDeleteCloseMarkStartingPostion(const Text, Mark: string;
      Offset: Integer): Integer;
    function GetIndentLength(const Line: String): Integer;
    function GetLinkDestinationAndTitle(const Text: String;
      const Position: Integer): TLinkDefinition;
    function GetLinkId(const Id: String): String;
    function GetLinkInfo(const Text: String; const Position: Integer;
      const AllowOtherLink: Boolean): TLinkDefinition;
    function GetHeaderType(const Match: String): TNodeType;
    function GetRawHtmlEndPosition(const Text: String; Pos: Integer): Integer;
    function GetTableAligns(const Line: String; const CellCount: Integer)
      : TTableAligns;
    function GetTableCellCount(const Line: String): Integer;
    function GetTableCells(const Line: String; const Count: Integer)
      : TTableCells;
    function GetTableHeader(const Line: String; const Count: Integer;
      const TableAligns: TTableAligns): TWikiNode;
    function GetTextValue(const Node: TWikiNode): String;
    function GetUrlAutoLinkEndPosition(const Text: String;
      const Offset: Integer): Integer;
    function IndentTabToSpace(const Line: String): String;
    function IsBlockTag(const Tag: String): Boolean;
    function IsCloseEmphasis(const Opener: TDelimiter; const CanOpen: Boolean;
      const Len: Integer): Boolean;
    function IsEmailAddress(const Text: String): Boolean;
    function IsLink(const Text: String): Boolean;
    function IsListBreakLine(const Line: String): Boolean;
    function IsPunctuation(const Text: String; const Pos: Integer;
      const ToRight: Boolean): Boolean;
    function IsThematicBreak(const Line: String): Boolean;
    function IsUnicodeWhitespaceChar(const Ch: Char): Boolean;
    function IsWhitespaceChar(const Ch: Char): Boolean;
    procedure OutputBuffer(const ParentNode: TWikiNode; const Node: TWikiNode);
    procedure ParseBlockNode(const ParentNode: TWikiNode);
    function ProcessFootnote(const CurrentNode: TWikiNode; const Text: String;
      const Offset: Integer; const Copied: Integer): Integer;
    function ProcessFootnoteDefinition(const Index: Integer;
      const Lines: TStringList): Integer;
    function ProcessLinkDefinition(const Text: String): Integer;
    procedure ProcessUntilEndMark(var Index: Integer; const Lines: TStringList;
      const ParentNode: TWikiNode; const Condition: String);
    function ProcessWikiLink(const CurrentNode: TWikiNode; const Text: String;
      const Offset: Integer; const Copied: Integer): Integer;
    function SearchCloser(const Text: String; const BeginPos: Integer;
      const Mark: Char): TCloser;
  protected
    procedure Parse(const WikiText: String); override;
    procedure ParseHeader(const WikiText: String); override;
    procedure ProcessInline(const Node: TWikiNode); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

const
  BlockTags: array of String = ['address', 'article', 'aside', 'base',
    'basefont', 'blockquote', 'body', 'caption', 'center', 'col', 'colgroup',
    'dd', 'details', 'dialog', 'dir', 'div', 'dl', 'dt', 'fieldset',
    'figcaption', 'figure', 'footer', 'form', 'frame', 'frameset', 'h1', 'h2',
    'h3', 'h4', 'h5', 'h6', 'head', 'header', 'hr', 'html', 'iframe', 'legend',
    'li', 'link', 'main', 'menu', 'menuitem', 'nav', 'noframes', 'ol',
    'optgroup', 'option', 'p', 'param', 'section', 'source', 'summary', 'table',
    'tbody', 'td', 'tfoot', 'th', 'thead', 'title', 'tr', 'track', 'ul'];

  { TMarkdownConverter }

procedure TMarkdownConverter.AddBeforeTextNode(const Node: TWikiNode;
  const Text: String);
begin
  if Text = '' then
    Exit;

  Node.AddChild(TWikiNode.Create(TNodeType.Text, Encode(Text, True), True));
end;

procedure TMarkdownConverter.AddOrderdList(const ParentNode: TWikiNode;
  const Lines: TStringList);
var
  Node: TWikiNode;
  Regex: String;
  Ret: TMatch;
  Line: String;
  Index: Integer;
  ListItem: String;
  ListItemPositionFromNumberEnd: Integer;
  IndentLength: Integer;
  FrontBlankLineIndex: Integer;
  LastIndentLength: Integer;
  NumberLength: Integer;
begin
  Lines.Text := TRegEx.Replace(Lines.Text, '(\r?\n)+$', '', [roCompiled]);

  Index := 0;
  Regex := '^ {0,3}([0-9]{1,9})[.)] ?(.*)$';
  Line := Lines[Index];

  Ret := TRegEx.Match(Line, Regex, [roCompiled]);
  if not Ret.Success then
  begin
    raise EArgumentException.CreateFmt('Invalid orderd list. [%s]', [Line]);
  end;

  ListItem := IndentTabToSpace(Ret.Groups[2].Value);
  ListItemPositionFromNumberEnd := GetIndentLength(ListItem) + 2;
  if ListItemPositionFromNumberEnd >= 6 then
  begin
    // indented code
    ListItemPositionFromNumberEnd := 2;
  end;
  NumberLength := Length(Ret.Groups[1].Value);
  Inc(Index);
  FrontBlankLineIndex := -1;
  LastIndentLength := GetIndentLength(Line);

  while Index < Lines.Count do
  begin
    Line := Lines[Index];
    if Line <> '' then
    begin
      Break;
    end;

    FrontBlankLineIndex := Index;
    Inc(Index);
  end;

  while Index < Lines.Count do
  begin
    Line := Lines[Index];

    if Line = '' then
    begin
      ListItem := AddLine(ListItem, Line);
      Inc(Index);
      Continue;
    end;

    IndentLength := GetIndentLength(Line);
    Ret := TRegEx.Match(Line, Regex, [roCompiled]);
    if Ret.Success then
    begin
      NumberLength := Length(Ret.Groups[1].Value);
      if IndentLength >= (LastIndentLength + NumberLength +
        ListItemPositionFromNumberEnd) then
      begin
        // nested list
        Delete(Line, 1, LastIndentLength + NumberLength +
          ListItemPositionFromNumberEnd);
        ListItem := AddLine(ListItem, Line);
        Inc(Index);
        Continue;
      end;

      // list item
      Node := TWikiNode.Create(TNodeType.ListItem);
      Node.Text := ListItem;
      if TRegEx.IsMatch(ListItem, '\n$', [roCompiled]) then
      begin
        // Reflect the last blank line when parsing
        Node.Text := Node.Text + WikiLB;
      end;
      if (Index > 0) and (Lines[Index - 1] = '') then
      begin
        ParentNode.IsLooseList := True;
      end;
      ParentNode.AddChild(Node);
      ParseBlockNode(Node);
      LastIndentLength := IndentLength;

      ListItem := Ret.Groups[2].Value;
      Inc(Index);
      Continue;
    end;

    // paragraph continuation line
    Delete(Line, 1, Min(LastIndentLength + NumberLength +
      ListItemPositionFromNumberEnd, IndentLength));

    if (Index - 1) = FrontBlankLineIndex then
    begin
      // Reflect skipped blank lines
      ListItem := AddLine(ListItem, '');
    end;

    ListItem := AddLine(ListItem, Line);
    Inc(Index);
  end;

  if ListItem <> '' then
  begin
    Node := TWikiNode.Create(TNodeType.ListItem);
    Node.Text := ListItem;
    if TRegEx.IsMatch(ListItem, '\n$', [roCompiled]) then
    begin
      // Reflect the last blank line when parsing
      Node.Text := Node.Text + WikiLB;
    end;
    ParentNode.AddChild(Node);
    ParseBlockNode(Node);
  end;
end;

procedure TMarkdownConverter.AddTableRow(const Line: String;
  const CellCount: Integer; const TableAligns: TTableAligns;
  const ParentNode: TWikiNode);
var
  Cell: TWikiNode;
  Cells: TTableCells;
  Row: TWikiNode;
begin
  Row := TWikiNode.Create(TNodeType.TableRow);
  Cells := GetTableCells(Line, CellCount);

  for var I := 0 to CellCount - 1 do
  begin
    Cell := TWikiNode.Create(TNodeType.TableDataCell);

    if I < Length(Cells) then
    begin
      Cell.Text := Cells[I];
    end;

    if TableAligns[I] <> '' then
    begin
      Cell.SetAttribute('align', TableAligns[I]);
    end;

    Row.AddChild(Cell);
  end;

  ParentNode.AddChild(Row);
end;

procedure TMarkdownConverter.AddUnorderdList(const ParentNode: TWikiNode;
  const Lines: TStringList);
var
  Node: TWikiNode;
  Regex: String;
  Ret: TMatch;
  Line: String;
  Index: Integer;
  ListItem: String;
  ListItemPositionFromMark: Integer;
  IndentLength: Integer;
  FrontBlankLineIndex: Integer;
  LastIndentLength: Integer;
begin
  Lines.Text := TRegEx.Replace(Lines.Text, '(\r?\n)+$', '', [roCompiled]);

  Index := 0;
  Regex := '^( {0,3})[-+*] ?(.*)$';
  Line := Lines[Index];

  Ret := TRegEx.Match(Line, Regex, [roCompiled]);
  if not Ret.Success then
  begin
    raise EArgumentException.CreateFmt('Invalid unorderd list. [%s]', [Line]);
  end;

  ListItem := IndentTabToSpace(Ret.Groups[2].Value);
  ListItemPositionFromMark := GetIndentLength(ListItem) + 2;
  if ListItemPositionFromMark >= 6 then
  begin
    // indented code
    ListItemPositionFromMark := 2;
  end;
  Inc(Index);
  FrontBlankLineIndex := -1;
  LastIndentLength := GetIndentLength(Line);

  while Index < Lines.Count do
  begin
    Line := Lines[Index];
    if Line <> '' then
    begin
      Break;
    end;

    FrontBlankLineIndex := Index;
    Inc(Index);
  end;

  while Index < Lines.Count do
  begin
    Line := Lines[Index];

    if Line = '' then
    begin
      ListItem := AddLine(ListItem, Line);
      Inc(Index);
      Continue;
    end;

    IndentLength := GetIndentLength(Line);
    Ret := TRegEx.Match(Line, Regex, [roCompiled]);
    if Ret.Success then
    begin
      if IndentLength >= (LastIndentLength + ListItemPositionFromMark) then
      begin
        // nested list
        Delete(Line, 1, LastIndentLength + ListItemPositionFromMark);
        ListItem := AddLine(ListItem, Line);
        Inc(Index);
        Continue;
      end;

      // list item
      Node := TWikiNode.Create(TNodeType.ListItem);
      Node.Text := ListItem;
      if TRegEx.IsMatch(ListItem, '\n$', [roCompiled]) then
      begin
        // Reflect the last blank line when parsing
        Node.Text := Node.Text + WikiLB;
      end;
      if (Index > 0) and (Lines[Index - 1] = '') then
      begin
        ParentNode.IsLooseList := True;
      end;
      ParentNode.AddChild(Node);
      ParseBlockNode(Node);
      LastIndentLength := IndentLength;

      ListItem := Ret.Groups[2].Value;
      Inc(Index);
      Continue;
    end;

    // paragraph continuation line
    Delete(Line, 1, Min(LastIndentLength + ListItemPositionFromMark,
      IndentLength));

    if (Index - 1) = FrontBlankLineIndex then
    begin
      // Reflect skipped blank lines
      ListItem := AddLine(ListItem, '');
    end;

    ListItem := AddLine(ListItem, Line);
    Inc(Index);
  end;

  if ListItem <> '' then
  begin
    Node := TWikiNode.Create(TNodeType.ListItem);
    Node.Text := ListItem;
    if TRegEx.IsMatch(ListItem, '\n$', [roCompiled]) then
    begin
      // Reflect the last blank line when parsing
      Node.Text := Node.Text + WikiLB;
    end;
    ParentNode.AddChild(Node);
    ParseBlockNode(Node);
  end;
end;

constructor TMarkdownConverter.Create;
begin
  inherited;

  FLinkDefinitions := TLinkDefinitions.Create([doOwnsValues]);
  FBlockTags := TDictionary<String, Integer>.Create;
  for var Tag in BlockTags do
  begin
    FBlockTags.Add(Tag, 0);
  end;
  // 2200 is about html entities count
  // ref. https://html.spec.whatwg.org/entities.json
  FHtmlEntities := TDictionary<String, String>.Create(2200);
  SetEntities(FHtmlEntities);
end;

destructor TMarkdownConverter.Destroy;
begin
  FBlockTags.Free;
  FLinkDefinitions.Free;
  FHtmlEntities.Free;

  inherited;
end;

function TMarkdownConverter.CanBeginEmphasis(const Text: String;
  const BPos, EPos: Integer; const Mark: String): Boolean;
begin
  if Mark = '_' then
    if (BPos <> 1) and not IsUnicodeWhitespaceChar(GetAt(Text, BPos - 1)) and
      not IsPunctuation(Text, BPos - 1, False) then
      Exit(False);

  if IsUnicodeWhitespaceChar(GetAt(Text, EPos + 1)) then
    Exit(False);

  if Length(Text) = EPos then
    Exit(False);

  if not IsPunctuation(Text, EPos + 1, True) then
    Exit(True);

  if IsUnicodeWhitespaceChar(GetAt(Text, BPos - 1)) or
    IsPunctuation(Text, BPos - 1, False) then
    Exit(True);

  if BPos = 1 then
    Exit(True);

  Result := False;
end;

function TMarkdownConverter.CanEndEmphasis(const Text: String;
  const BPos, EPos: Integer; const Mark: String): Boolean;
begin
  if Mark = '_' then
    if (EPos <> Length(Text)) and not IsUnicodeWhitespaceChar
      (GetAt(Text, EPos + 1)) and not IsPunctuation(Text, EPos + 1, True) then
      Exit(False);

  if IsUnicodeWhitespaceChar(GetAt(Text, BPos - 1)) then
    Exit(False);

  if not IsPunctuation(Text, BPos - 1, False) then
    Exit(True);

  if IsUnicodeWhitespaceChar(GetAt(Text, EPos + 1)) or
    IsPunctuation(Text, EPos + 1, True) then
    Exit(True);

  if EPos = Length(Text) then
    Exit(True);

  Result := False;
end;

function TMarkdownConverter.CountChar(const Text: String;
  const Ch: Char): Integer;
var
  Count: Integer;
begin
  Count := 0;
  for var I := 1 to Length(Text) do
  begin
    if Text[I] = Ch then
      Inc(Count);
  end;
  Result := Count;
end;

function TMarkdownConverter.CountLine(const Text: String): Integer;
var
  Count: Integer;
begin
  Count := 1;
  for var I := 1 to Length(Text) do
  begin
    if Text[I] = #10 then
      Inc(Count);
  end;
  Result := Count;
end;

function TMarkdownConverter.Encode(const Text: String;
  const WithHtmlEncode: Boolean): String;
var
  Pos, Pos2, Copied: Integer;
  Ch, Ch2: Char;
  Size: Integer;
  Encoded: String;
  Entity: String;
  Ret: TMatch;

  function CharHtmlEncode(const Ch: Char): String;
  begin
    if not WithHtmlEncode then
      Exit(Ch);

    if Ch = '<' then
      Exit('&lt;');

    if Ch = '>' then
      Exit('&gt;');

    if Ch = '"' then
      Exit('&quot;');

    if Ch = '&' then
      Exit('&amp;');

    Result := Ch;
  end;

  function NumberToChar(CharNumber: Integer): String;
  var
    Ch: Char;
  begin
    if CharNumber = 0 then
    begin
      CharNumber := $FFFD;
    end;
    Ch := Chr(CharNumber);
    Result := CharHtmlEncode(Ch);
  end;

begin
  Encoded := '';
  Size := Length(Text);
  Copied := 0;
  Pos := 1;
  while Pos <= Size do
  begin
    Ch := GetAt(Text, Pos);

    if Ch = '\' then
    begin
      // unescape
      Ch2 := GetAt(Text, Pos + 1);
      if CharInSet(Ch2, ['!', '"', '#', '$', '%', '&', '''', '(', ')', '*', '+',
        ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', ']', '^',
        '_', '`', '{', '|', '}', '~', '\']) then
      begin
        Encoded := Encoded + Copy(Text, Copied + 1, Pos - 1 - Copied) +
          CharHtmlEncode(Ch2);
        Copied := Pos + 1;
        Inc(Pos, 2);
        Continue;
      end;
    end;

    if Ch = '&' then
    begin
      Pos2 := Pos + 1;
      var
        Reference: String := '';

      while Pos2 <= Size do
      begin
        Ch2 := GetAt(Text, Pos2);
        if Ch2 = ';' then
        begin
          Reference := Copy(Text, Pos, Pos2 - Pos + 1);

          if FHtmlEntities.ContainsKey(Reference) then
          begin
            Entity := FHtmlEntities.Items[Reference];
            if Length(Entity) = 1 then
            begin
              Entity := CharHtmlEncode(Entity[1]);
            end;
            Encoded := Encoded + Copy(Text, Copied + 1,
              Pos - 1 - Copied) + Entity;
            Copied := Pos2;
            Pos := Pos2 + 1;
            Break;
          end;

          Ret := TRegEx.Match(Reference, '^&#x([0-9A-F]{1,6});$',
            [roCompiled, roIgnoreCase]);
          if Ret.Success then
          begin
            Entity := NumberToChar(('$' + Ret.Groups[1].Value).ToInteger);
            Encoded := Encoded + Copy(Text, Copied + 1,
              Pos - 1 - Copied) + Entity;
            Copied := Pos2;
            Pos := Pos2 + 1;
            Break;
          end;

          Ret := TRegEx.Match(Reference, '^&#([0-9]{1,7});$', [roCompiled]);
          if Ret.Success then
          begin
            Entity := NumberToChar(Ret.Groups[1].Value.ToInteger);
            Encoded := Encoded + Copy(Text, Copied + 1,
              Pos - 1 - Copied) + Entity;
            Copied := Pos2;
            Pos := Pos2 + 1;
            Break;
          end;
        end;
        Pos2 := Pos2 + 1;
      end;
      if Pos2 <= Size then
      begin
        Continue;
      end;
    end;

    if CharInSet(Ch, ['<', '>', '"', '&']) then
    begin
      Encoded := Encoded + Copy(Text, Copied + 1, Pos - 1 - Copied) +
        CharHtmlEncode(Ch);
      Copied := Pos;
      Pos := Pos + 1;
      Continue;
    end;

    Inc(Pos);
  end;
  Encoded := Encoded + Copy(Text, Copied + 1, Size - Copied);

  Result := Encoded;
end;

function TMarkdownConverter.ExistsElement(const Node: TWikiNode;
  const TargetType: TNodeType): Boolean;
begin
  if Node.NodeType = TargetType then
    Exit(True);

  if Node.ChildNodes = nil then
    Exit(False);

  for var Child in Node.ChildNodes do
  begin
    if ExistsElement(Child, TargetType) then
      Exit(True);
  end;

  Result := False;
end;

function TMarkdownConverter.ExistsLink(const Text: String): Boolean;
var
  Node: TWikiNode;
begin
  Node := TWikiNode.Create(TNodeType.Text);
  try
    Node.Text := Text;
    Node.ParentNode := nil;
    ProcessInline(Node);

    Result := ExistsElement(Node, TNodeType.Anchor);
  finally
    Node.Free;
  end;
end;

function TMarkdownConverter.GetAtSkipEscapedChar(const Text: String;
  var CPos: Integer): Char;
var
  Ch: Char;
  Escaped: Boolean;
begin
  Escaped := False;
  while True do
  begin
    Ch := GetAt(Text, CPos);
    if Ch = '\' then
    begin
      Escaped := not Escaped;
      Inc(CPos);
      Continue;
    end;

    if CharInSet(Ch, [#0, #10, #13]) then
      Exit(Ch);

    if Escaped then
    begin
      Inc(CPos);
      Escaped := False;
      Continue;
    end;

    Exit(Ch);
  end;
end;

function TMarkdownConverter.GetAutoLinkEndPosition(const Text: String;
  const Offset: Integer): Integer;
var
  EPos: Integer;
  Uri: String;
begin
  if GetAt(Text, Offset) <> '<' then
    Exit(0);

  EPos := Pos('>', Text, Offset + 1);
  if EPos = 0 then
    Exit(0);

  Uri := Copy(Text, Offset + 1, EPos - Offset - 1);

  if IsLink(Uri) then
    Exit(EPos);

  if IsEmailAddress(Uri) then
    Exit(EPos);

  Result := 0;
end;

function TMarkdownConverter.GetCodeCloseMarkStartingPosition(const Text,
  Mark: string; Offset: Integer): Integer;
var
  Position: Integer;
  Len, Len2: Integer;
begin
  Len := Length(Mark);
  while True do
  begin
    Position := Pos(Mark, Text, Offset);

    if Position = 0 then
      Break;

    Len2 := GetSameCharLength(Text, Mark[1], Position);
    if Len = Len2 then
      Break;

    Offset := Position + Len2;
    if Offset >= Length(Text) then
    begin
      Position := 0;
      Break;
    end;
  end;
  Result := Position;
end;

function TMarkdownConverter.GetCodeSpanEndPosition(const Text: String;
  const Mark: Char; Offset: Integer): Integer;
var
  Len: Integer;
  SearchPos: Integer;
  CloseMark: String;
begin
  Len := GetSameCharLength(Text, Mark, Offset);
  SearchPos := Offset + Len;
  CloseMark := StringOfChar(Mark, Len);
  Result := GetCodeCloseMarkStartingPosition(Text, CloseMark, SearchPos);
end;

function TMarkdownConverter.GetDeleteCloseMarkStartingPostion(const Text,
  Mark: string; Offset: Integer): Integer;
var
  Len, Len2: Integer;
  Ch: Char;
begin
  Len := Length(Mark);
  while True do
  begin
    Inc(Offset);
    if Offset > Length(Text) then
      Break;

    Ch := GetAt(Text, Offset);
    if Ch = Mark[1] then
    begin
      Len2 := GetSameCharLength(Text, Ch, Offset);
      if Len = Len2 then
      begin
        Exit(Offset);
      end;

      Offset := Offset + Len2;
      Continue;
    end;

    if Ch = #10 then
      Break;
  end;
  Result := 0;
end;

function TMarkdownConverter.GetHeaderType(const Match: String): TNodeType;
begin
  case Length(Match) of
    1:
      Result := TNodeType.Heading1;
    2:
      Result := TNodeType.Heading2;
    3:
      Result := TNodeType.Heading3;
    4:
      Result := TNodeType.Heading4;
    5:
      Result := TNodeType.Heading5;
    6:
      Result := TNodeType.Heading6;
  else
    raise Exception.CreateFmt('Invalid # count. cuont[%d}', [Length(Match)]);
  end;
end;

function TMarkdownConverter.GetIndentLength(const Line: String): Integer;
var
  Ret: TMatch;
begin
  Result := 0;

  Ret := TRegEx.Match(Line, '(^ *)', [roCompiled]);
  if Ret.Success then
  begin
    Result := Length(Ret.Groups[1].Value);
  end;
end;

function TMarkdownConverter.GetLinkDestinationAndTitle(const Text: String;
  const Position: Integer): TLinkDefinition;
var
  Ch: Char;
  CPos: Integer;
  Link: TLinkDefinition;
  TextLength: Integer;
  Uri, Title: String;
  Escaped: Boolean;
  LinkCovered: Boolean;
  ParenthesesLevel: Integer;
  LineBreaked: Boolean;

  function SkipBlank(const Text: String; CPos: Integer;
    LineBreaked: Boolean): Integer;
  var
    Ch: Char;
  begin
    Ch := GetAt(Text, CPos);
    while IsWhitespaceChar(Ch) do
    begin
      if Ch = #13 then
      begin
        if LineBreaked then
          Exit(0);

        LineBreaked := True;
      end;

      Inc(CPos);
      Ch := GetAt(Text, CPos);
    end;

    Result := CPos;
  end;

begin
  CPos := Position + 1;
  Uri := '';
  Title := '';
  Escaped := False;
  LinkCovered := False;
  LineBreaked := False;
  ParenthesesLevel := 1;
  TextLength := Length(Text);

  CPos := SkipBlank(Text, CPos, False);
  if CPos = 0 then
    Exit(nil);

  if Text[CPos] = '<' then
  begin
    LinkCovered := True;
    Inc(CPos);
  end;

  while CPos <= TextLength do
  begin
    Ch := GetAt(Text, CPos);

    if Ch = '\' then
    begin
      Escaped := not Escaped;
      Uri := Uri + Ch;
      Inc(CPos);
      Continue;
    end;

    if (Ch = ' ') and (not LinkCovered) then
      Break;

    if Escaped then
    begin
      Escaped := False;
      Uri := Uri + Ch;
      Inc(CPos);
      Continue;
    end;

    if (Ch = '>') and LinkCovered then
    begin
      Inc(CPos);
      Break;
    end;

    if (Ch = #13) then
    begin
      if LinkCovered then
        Exit(nil)
      else
      begin
        LineBreaked := True;
        Inc(CPos);
        Break;
      end;
    end;

    if (Ch = '(') and (not LinkCovered) then
    begin
      Inc(ParenthesesLevel);
      Uri := Uri + Ch;
      CPos := CPos + 1;
      Continue;
    end;

    if (Ch = ')') and (not LinkCovered) then
    begin
      if ParenthesesLevel = 1 then
        Break;

      Dec(ParenthesesLevel);
      Uri := Uri + Ch;
      Inc(CPos);
      Continue;
    end;

    Uri := Uri + Ch;
    Inc(CPos);
  end;

  CPos := SkipBlank(Text, CPos, LineBreaked);
  if CPos = 0 then
    Exit(nil);

  var
    Mark: Char := GetAt(Text, CPos);

  if Mark = ')' then
  begin
    // uri only
    Link := TLinkDefinition.Create;
    Link.Destination := Uri;
    Link.Title := '';
    Link.EndPosition := CPos;
    Exit(Link);
  end;

  if CPos <= TextLength then
  begin
    if not CharInSet(Mark, ['''', '"', '(']) then
      Exit(nil);

    if Mark = '(' then
      Mark := ')';

    var
      Ended: Boolean := False;

    Escaped := False;
    Inc(CPos);
    while CPos <= TextLength do
    begin
      Ch := GetAt(Text, CPos);

      if (not Escaped) and (Ch = Mark) then
      begin
        Ended := True;
        Inc(CPos);
        Break;
      end;

      Title := Title + Ch;

      if Ch = '\' then
        Escaped := not Escaped
      else
        Escaped := False;

      Inc(CPos);
    end;

    if not Ended then
      Exit(nil);
  end;

  CPos := SkipBlank(Text, CPos, LineBreaked);
  if CPos = 0 then
    Exit(nil);

  Link := nil;

  if GetAt(Text, CPos) = ')' then
  begin
    Link := TLinkDefinition.Create;
    Link.Destination := Uri;
    Link.Title := Title;
    Link.EndPosition := CPos;
  end;

  Result := Link;
end;

function TMarkdownConverter.GetLinkId(const Id: String): String;
begin
  Result := TRegEx.Replace(AnsiUpperCase(Id), '\s+', ' ');
end;

function TMarkdownConverter.GetLinkInfo(const Text: String;
  const Position: Integer; const AllowOtherLink: Boolean): TLinkDefinition;
var
  CPos: Integer;
  Ch: Char;
  NestLevel: Integer;
  TextLength: Integer;
begin
  if GetAt(Text, Position) <> '[' then
    Exit(nil);

  CPos := Position + 1;
  NestLevel := 1;
  TextLength := Length(Text);

  while CPos <= TextLength do
  begin
    Ch := GetAtSkipEscapedChar(Text, CPos);

    // maybe code span
    if Ch = '`' then
    begin
      var
        EndPos: Integer := GetCodeSpanEndPosition(Text, '`', CPos);
      if EndPos > 0 then
      begin
        // skip code span
        CPos := EndPos + 1;
        Continue;
      end;
    end;

    // maybe raw HTML or auto link
    if Ch = '<' then
    begin
      var
        EndPos: Integer := GetRawHtmlEndPosition(Text, CPos);
      if EndPos > 0 then
      begin
        // skip raw HTML
        CPos := EndPos + 1;
        Continue;
      end;

      EndPos := GetAutoLinkEndPosition(Text, CPos);
      if EndPos > 0 then
      begin
        // skip auto link
        CPos := EndPos + 1;
        Continue;
      end;
    end;

    if Ch = '[' then
    begin
      Inc(NestLevel);
      Inc(CPos);
      Continue;
    end;

    if Ch = ']' then
    begin
      Dec(NestLevel);
      if NestLevel > 0 then
      begin
        Inc(CPos);
        Continue;
      end;

      if NestLevel < 0 then
      begin
        Break;
      end;

      var
        LinkName: String := Copy(Text, Position + 1, CPos - Position - 1);

      if (not AllowOtherLink) and ExistsLink(LinkName) then
      begin
        // may not contain other links
        Exit(nil);
      end;

      var
        LinkId: String := GetLinkId(LinkName);
      var
        Ch2: String := GetAt(Text, CPos + 1);

      if Ch2 = '(' then
      begin
        var
          LinkInfo: TLinkDefinition := GetLinkDestinationAndTitle(Text,
            CPos + 1);
        if LinkInfo <> nil then
        begin
          // inline link;
          if (not AllowOtherLink) and ExistsLink(LinkName) then
          begin
            // may not contain other links
            LinkInfo.Free;
            Exit(nil);
          end;

          LinkInfo.Name := LinkName;
          Exit(LinkInfo);
        end;
      end;

      if Ch2 = '[' then
      begin
        if GetAt(Text, CPos + 2) = ']' then
        begin
          if FLinkDefinitions.ContainsKey(LinkId) then
          begin
            // [foo][] is equivalent to [foo][foo]
            var
              LinkDef: TLinkDefinition := TLinkDefinition.Create;
            LinkDef.Assign(FLinkDefinitions.Items[LinkId]);

            LinkDef.EndPosition := CPos + 2;
            LinkDef.Name := LinkName;
            Exit(LinkDef);
          end;

          Exit(nil);
        end;

        var
          Ch3: Char;
        var
          CPos3: Integer := CPos + 2;
        while CPos3 <= TextLength do
        begin
          Ch3 := GetAtSkipEscapedChar(Text, CPos3);
          // maybe code span
          if Ch3 = '`' then
          begin
            var
              EndPos: Integer := GetCodeSpanEndPosition(Text, '`', CPos3);
            if EndPos > 0 then
            begin
              // skip code span
              CPos3 := EndPos + 1;
              Continue;
            end;
          end;

          // maybe raw HTML or auto link
          if Ch3 = '<' then
          begin
            var
              EndPos: Integer := GetRawHtmlEndPosition(Text, CPos3);
            if EndPos > 0 then
            begin
              // skip raw HTML
              CPos3 := EndPos + 1;
              Continue;
            end;

            EndPos := GetAutoLinkEndPosition(Text, CPos3);
            if EndPos > 0 then
            begin
              // skip auto link
              CPos3 := EndPos + 1;
              Continue;
            end;
          end;

          if Ch3 = ']' then
          begin
            var
              Id: String := GetLinkId(Copy(Text, CPos + 2, CPos3 - CPos - 2));
            if FLinkDefinitions.ContainsKey(Id) then
            begin
              // Reference link
              var
                LinkDef: TLinkDefinition := TLinkDefinition.Create;
              LinkDef.Assign(FLinkDefinitions.Items[Id]);

              LinkDef.EndPosition := CPos3;
              LinkDef.Name := LinkName;
              Exit(LinkDef);
            end;

            Exit(nil);
          end;

          Inc(CPos3);
        end;
      end;

      if FLinkDefinitions.ContainsKey(LinkId) then
      begin
        // Reference link
        var
          LinkDef: TLinkDefinition := TLinkDefinition.Create;
        LinkDef.Assign(FLinkDefinitions.Items[LinkId]);

        LinkDef.EndPosition := CPos;
        LinkDef.Name := LinkName;
        Exit(LinkDef);
      end
      else
      begin
        // not link
        Exit(nil);
      end;
    end;

    Inc(CPos);
  end;

  Result := nil;
end;

function TMarkdownConverter.GetRawHtmlEndPosition(const Text: String;
  Pos: Integer): Integer;
var
  CPos: Integer;
  Ch: Char;
  TextLength: Integer;
  IsCloseing: Boolean;
begin
  CPos := Pos;
  Ch := GetAt(Text, CPos);
  TextLength := Length(Text);
  IsCloseing := False;

  if Ch <> '<' then
    Exit(0);

  Inc(CPos);
  Ch := GetAt(Text, CPos);

  // maybe comment, declaration or CDATA
  if Ch = '!' then
  begin
    if Copy(Text, CPos + 1, 2) = '--' then
    begin
      var
        EPos: Integer := System.Pos('-->', Text, CPos + 3);
      if EPos > 0 then
      begin
        // comment
        Exit(EPos + 2);
      end;
    end
    else if CharInSet(GetAt(Text, CPos + 1), ['A' .. 'Z']) then
    begin
      var
        EPos: Integer := System.Pos('>', Text, CPos + 2);
      if EPos > 0 then
      begin
        // declaration
        Exit(EPos + 1);
      end;
    end
    else if Copy(Text, CPos + 1, 7) = '[CDATA[' then
    begin
      var
        EPos: Integer := System.Pos(']]>', Text, CPos + 8);
      if EPos > 0 then
      begin
        // CDATA
        Exit(EPos + 2);
      end;
    end;
  end;

  // maybe processing instruction
  if Ch = '?' then
  begin
    var
      EPos: Integer := System.Pos('?>', Text, CPos + 2);
    if EPos > 0 then
    begin
      // processing instruction
      Exit(EPos + 1);
    end;
  end;

  // maybe tag name
  if Ch = '/' then
  begin
    Inc(CPos);
    Ch := GetAt(Text, CPos);
    IsCloseing := True;
  end;

  if not CharInSet(Ch, ['a' .. 'z', 'A' .. 'Z']) then
    Exit(0);

  Inc(CPos);
  while CPos <= TextLength do
  begin
    Ch := GetAt(Text, CPos);
    if not CharInSet(Ch, ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '-']) then
      Break;

    Inc(CPos);
  end;

  if IsWhitespaceChar(Ch) then
  begin
    while CPos <= TextLength do
    begin
      // skip white spaces
      while IsWhitespaceChar(Ch) do
      begin
        Inc(CPos);
        Ch := GetAt(Text, CPos);
      end;

      // attribute name
      if not CharInSet(Ch, ['a' .. 'z', 'A' .. 'Z', '_', ':']) then
        Break;

      if IsCloseing then
        // closing tag can not have attributes
        Break;

      while CharInSet(Ch, ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_', '.',
        ':', '-']) do
      begin
        Inc(CPos);
        Ch := GetAt(Text, CPos);
      end;

      // skip white spaces
      while IsWhitespaceChar(Ch) do
      begin
        Inc(CPos);
        Ch := GetAt(Text, CPos);
      end;

      if Ch = '=' then
      begin
        // attribute value
        Inc(CPos);
        Ch := GetAt(Text, CPos);

        // skip white spaces
        while IsWhitespaceChar(Ch) do
        begin
          Inc(CPos);
          Ch := GetAt(Text, CPos);
        end;

        var
          Quote: String := '';
        if CharInSet(Ch, ['''', '"']) then
        begin
          Quote := Ch;
          Inc(CPos);
          Ch := GetAt(Text, CPos);
        end;

        while CPos <= TextLength do
        begin
          if Quote = '' then
          begin
            if CharInSet(Ch, [' ', #9, #10, #13, '"', '''', '=', '<', '>', '`'])
            then
              Break;
          end
          else
          begin
            if Ch = Quote then
            begin
              Inc(CPos);
              Ch := GetAt(Text, CPos);
              Break;
            end;
          end;

          Inc(CPos);
          Ch := GetAt(Text, CPos);
        end;

        // need white space between attributes
        if not IsWhitespaceChar(Ch) then
          Break;
      end;
    end;
  end;

  // skip white spaces
  Ch := GetAt(Text, CPos);
  while IsWhitespaceChar(Ch) do
  begin
    Inc(CPos);
    Ch := GetAt(Text, CPos);
  end;

  // tag end (/>)
  if (Ch = '/') and (GetAt(Text, CPos + 1) = '>') then
    Exit(CPos + 1);

  // tag end (>)
  if Ch = '>' then
    Exit(CPos);

  Result := 0;
end;

function TMarkdownConverter.GetTableAligns(const Line: String;
  const CellCount: Integer): TTableAligns;
var
  Buff: String;
  Cells: TTableAligns;
  I: Integer;
  Ret: TMatchCollection;
  Item: String;
begin
  SetLength(Cells, CellCount);
  Buff := TRegEx.Replace(Line, '^\s*\|', '', [roCompiled]);
  Buff := TRegEx.Replace(Buff, '\|\s*$', '', [roCompiled]);
  Buff := Buff + '|';

  Ret := TRegEx.Matches(Buff, '\s*(.+?)\s*(?!\\)\|', [roCompiled]);
  I := 0;
  while (I < CellCount) and (I < Ret.Count) do
  begin
    Item := Ret.Item[I].Groups[1].Value;
    if TRegEx.IsMatch(Item, '^:-+:$', [roCompiled]) then
      Cells[I] := 'center'
    else if TRegEx.IsMatch(Item, ':$', [roCompiled]) then
      Cells[I] := 'right'
    else
      Cells[I] := '';

    Inc(I);
  end;

  Result := Cells;
end;

function TMarkdownConverter.GetTableCellCount(const Line: String): Integer;
var
  Buff: String;
  Count: Integer;
begin
  Buff := TRegEx.Replace(Line, '^\s*\|', '', [roCompiled]);
  Buff := TRegEx.Replace(Buff, '\|\s*$', '', [roCompiled]);
  Buff := TRegEx.Replace(Buff, '\\\|', '', [roCompiled]);

  Count := 0;
  for var I := 1 to Length(Buff) do
  begin
    if Buff[I] = '|' then
    begin
      Count := Count + 1;
    end;
  end;

  Result := Count + 1;
end;

function TMarkdownConverter.GetTableCells(const Line: String;
  const Count: Integer): TTableCells;
var
  Buff: String;
  Cells: TTableCells;
  I: Integer;
  Ret: TMatchCollection;

  function EscapePipe(const Text: String): String;
  var
    Escaped: Boolean;
    Item: Char;
  begin
    Result := '';
    Escaped := False;
    for var I := 1 to Length(Text) do
    begin
      Item := GetAt(Text, I);
      if Item = '\' then
      begin
        Escaped := not Escaped;
        if GetAt(Text, I + 1) = '|' then
          Continue;
      end
      else
      begin
        Escaped := False;
      end;

      Result := Result + Item;
    end;
  end;

begin
  SetLength(Cells, Count);
  Buff := TRegEx.Replace(Line, '^\s*\|', '', [roCompiled]);
  Buff := TRegEx.Replace(Buff, '\|\s*$', '', [roCompiled]);
  Buff := Buff + '|';

  Ret := TRegEx.Matches(Buff, '\s*(.+?)\s*(?<!\\)\|', [roCompiled]);
  I := 0;
  while (I < Count) and (I < Ret.Count) do
  begin
    Cells[I] := EscapePipe(Ret.Item[I].Groups[1].Value);
    Inc(I);
  end;

  SetLength(Cells, Min(Count, Ret.Count));
  Result := Cells;
end;

function TMarkdownConverter.GetTableHeader(const Line: String;
  const Count: Integer; const TableAligns: TTableAligns): TWikiNode;
var
  Cell: TWikiNode;
  Cells: TTableCells;
  Header: TWikiNode;
  Row: TWikiNode;
begin
  Header := TWikiNode.Create(TNodeType.TableHeader);
  Row := TWikiNode.Create(TNodeType.TableRow);
  Header.AddChild(Row);
  Cells := GetTableCells(Line, Count);

  for var I := 0 to High(Cells) do
  begin
    Cell := TWikiNode.Create(TNodeType.TableHeaderCell);
    Cell.Text := Cells[I];
    if TableAligns[I] <> '' then
    begin
      Cell.SetAttribute('align', TableAligns[I]);
    end;

    Row.AddChild(Cell);
  end;

  Result := Header;
end;

function TMarkdownConverter.GetTextValue(const Node: TWikiNode): String;
begin
  Result := '';
  if Node.NodeType = TNodeType.Image then
    Node.Attributes.TryGetValue('alt', Result)
  else
    Result := Node.Value;

  if Node.ChildNodes <> nil then
  begin
    for var Child in Node.ChildNodes do
    begin
      Result := Result + GetTextValue(Child);
    end;
  end;
end;

function TMarkdownConverter.GetUrlAutoLinkEndPosition(const Text: String;
  const Offset: Integer): Integer;
var
  Regex: TRegEx;
  Ret: TMatch;
  Pos: Integer;
begin
  Regex := TRegEx.Create
    ('https?://(([a-z0-9][a-z0-9-]*[a-z0-9]\.)+[a-z]{2,})?[^\x00-\x20<>]*',
    [roCompiled, roIgnoreCase]);
  Ret := Regex.Match(Text, Offset);
  if (not Ret.Success) or (Ret.Index <> Offset) then
    Exit(0);

  Pos := Offset + Ret.Length - 1;
  if GetAt(Text, Pos) = ')' then
  begin
    var
      Open: Integer := CountChar(Ret.Groups[0].Value, '(');
    var
      Close: Integer := CountChar(Ret.Groups[0].Value, ')');
    while Open < Close do
    begin
      Dec(Pos);
      Dec(Close);
    end;
  end;

  Result := Pos;
end;

function TMarkdownConverter.IndentTabToSpace(const Line: String): String;
var
  Indent: String;
  Ret: TMatch;
begin
  Result := Line;

  if Line = '' then
    Exit;

  Ret := TRegEx.Match(Line, '^([\t ]+)(.+)', [roCompiled]);
  if not Ret.Success then
    Exit;

  Indent := Ret.Groups[1].Value;
  Indent := TRegEx.Replace(Indent, '\t| \t|  \t', '    ', [roCompiled]);

  Result := Indent + Ret.Groups[2].Value;
end;

function TMarkdownConverter.IsBlockTag(const Tag: String): Boolean;
begin
  Result := FBlockTags.ContainsKey(LowerCase(Tag));
end;

function TMarkdownConverter.IsCloseEmphasis(const Opener: TDelimiter;
  const CanOpen: Boolean; const Len: Integer): Boolean;
var
  CalcLength: Integer;
begin
  if not Opener.CanOpen then
    Exit(False);

  if CanOpen or Opener.CanClose then
  begin
    if Len mod 3 = 0 then
      Exit(True);

    CalcLength := Opener.Length + Opener.PrevLength;
    if CalcLength mod 3 = 0 then
      Exit(True);

    if Len mod 3 = CalcLength mod 3 then
      Exit(True);

    Exit(False);
  end;

  Result := True;
end;

function TMarkdownConverter.IsEmailAddress(const Text: String): Boolean;
begin
  Result := TRegEx.IsMatch(Text,
    '^[a-zA-Z0-9.!#$%&''*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$',
    [roCompiled]);
end;

function TMarkdownConverter.IsLink(const Text: String): Boolean;
begin
  Result := TRegEx.IsMatch(Text,
    '^[A-Za-z][A-Za-z0-9+.-]{1,31}:[^\x00-\x20<>]*$', [roCompiled]);
end;

function TMarkdownConverter.IsListBreakLine(const Line: String): Boolean;
begin
  // other unorderd list
  if TRegEx.IsMatch(Line, '^ *[-+*] ', [roCompiled]) then
    Exit(True);

  // other orderd list
  if TRegEx.IsMatch(Line, '^ *[0-9]{1,9}\. ', [roCompiled]) then
    Exit(True);

  Result := False;
end;

function TMarkdownConverter.IsPunctuation(const Text: String;
  const Pos: Integer; const ToRight: Boolean): Boolean;
var
  Next: Integer;
  Ch: Char;
  Key: String;
  CPos: Integer;
  Category: TUnicodeCategory;
begin
  if CharInSet(GetAt(Text, Pos), ['!', '"', '#', '$', '%', '&', '''', '(', ')',
    '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', '\',
    ']', '^', '_', '`', '{', '|', '}', '~']) then
    Exit(True);

  if ToRight then
    Next := 1
  else
    Next := -1;

  CPos := Pos;

  Ch := GetAt(Text, CPos);
  Key := Ch;
  while Ch.IsSurrogate do
  begin
    Ch := GetAt(Text, CPos);
    CPos := CPos + Next;
    Key := Key + Ch;
  end;

  // at Delphi 11
  // if write "TCharHelper.GetUnicodeCategory(Str, Index)" then E2018 error
  Category := 'A'.GetUnicodeCategory(Key, 0);

  Result := Category in [TUnicodeCategory.ucConnectPunctuation,
    TUnicodeCategory.ucDashPunctuation, TUnicodeCategory.ucClosePunctuation,
    TUnicodeCategory.ucFinalPunctuation, TUnicodeCategory.ucInitialPunctuation,
    TUnicodeCategory.ucOtherPunctuation, TUnicodeCategory.ucOpenPunctuation];
end;

function TMarkdownConverter.IsThematicBreak(const Line: String): Boolean;
begin
  Result := TRegEx.IsMatch(Line, '^ {0,3}([-*_])[ \t]*\1[ \t]*\1([ \t]|\1)*$',
    [roCompiled]);
end;

function TMarkdownConverter.IsUnicodeWhitespaceChar(const Ch: Char): Boolean;
begin
  if CharInSet(Ch, [#$20, #$09, #$0A, #$0B, #$0C, #$0D]) then
    Exit(True);

  Result := Ch.GetUnicodeCategory = TUnicodeCategory.ucSpaceSeparator;
end;

function TMarkdownConverter.IsWhitespaceChar(const Ch: Char): Boolean;
begin
  Result := CharInSet(Ch, [#$20, #$09, #$0A, #$0B, #$0C, #$0D]);
end;

procedure TMarkdownConverter.OutputBuffer(const ParentNode: TWikiNode;
  const Node: TWikiNode);
var
  ChildNode: TWikiNode;
begin
  if Node = nil then
    Exit;

  if Node.NodeType = TNodeType.Paragraph then
  begin
    if TRegEx.IsMatch(Node.Text, '^[-+*]$', [roCompiled]) then
    begin
      // only unorder list mark
      Node.Text := '';
      Node.NodeType := TNodeType.UnorderedList;
      ChildNode := TWikiNode.Create(TNodeType.ListItem);
      Node.AddChild(ChildNode);
      ParentNode.AddChild(Node);
      Exit;
    end;

    if TRegEx.IsMatch(Node.Text, '^[0-9]{1,9}\.$', [roCompiled]) then
    begin
      // only order list mark
      var
      Number := LeftStr(Node.Text, Length(Node.Text) - 1).ToInteger;

      Node.Text := '';
      Node.NodeType := TNodeType.OrderedList;
      if Number <> 1 then
      begin
        Node.SetAttribute('start', Number.ToString);
      end;
      ChildNode := TWikiNode.Create(TNodeType.ListItem);
      Node.AddChild(ChildNode);
      ParentNode.AddChild(Node);
      Exit;
    end;
  end;

  if Node.NodeType = TNodeType.Blockquote then
  begin
    ParseBlockNode(Node);
    Node.Text := '';
    ParentNode.AddChild(Node);
    Exit;
  end;

  if Node.NodeType = TNodeType.PreformattedText then
  begin
    ChildNode := TWikiNode.Create(TNodeType.Code);
    if Node.CodeLanguage <> '' then
    begin
      ChildNode.SetAttribute('class', Format('language-%s',
        [Encode(Node.CodeLanguage, True)]));
    end;

    if (Node.Text <> '') and not TRegEx.IsMatch(Node.Text, '\n$', [roCompiled])
    then
    begin
      Node.Text := Node.Text + WikiLB;
    end;

    ChildNode.Text := Node.Text;
    Node.AddChild(ChildNode);
    ParentNode.AddChild(Node);
    Node.Text := '';
    Exit;
  end;

  if (Node.NodeType in [TNodeType.OrderedList, TNodeType.UnorderedList]) and
    (Node.ChildNodes <> nil) then
  begin
    for var Child in Node.ChildNodes do
    begin
      if Child.NodeType = TNodeType.ListItem then
      begin
        if (Child.ChildNodes <> nil) and
          (Child.ChildNodes[0].NodeType = TNodeType.Text) then
        begin
          var
            GrandChild: TWikiNode := Child.ChildNodes[0];
          var
            Ret: TMatch := TRegEx.Match(GrandChild.Text,
              '^( {0,3}\[([Xx ])\] )', [roCompiled]);

          if Ret.Success then
          begin
            // Task list item
            var
              Input: TWikiNode := TWikiNode.Create(TNodeType.Input);

              // Input.SetAttribute('disabled', '');
            Input.SetAttribute('type', 'checkbox');
            if Ret.Groups[2].Value <> ' ' then
            begin
              Input.SetAttribute('checked', '');
            end;
            Input.Value := ' ';
            GrandChild.AddChild(Input);
            GrandChild.Text := RightStr(GrandChild.Text, Length(GrandChild.Text)
              - Length(Ret.Groups[1].Value));
            Child.SetAttribute('class', 'task-list-item');
          end;
        end;
      end;
    end;

    if Node.IsLooseList then
    begin
      for var Child in Node.ChildNodes do
      begin
        if Child.NodeType = TNodeType.ListItem then
        begin
          // For loose lists, the text becomes a paragraph
          if Child.ChildNodes <> nil then
          begin
            for var GrandChild in Child.ChildNodes do
            begin
              if GrandChild.NodeType = TNodeType.Text then
              begin
                GrandChild.NodeType := TNodeType.Paragraph;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  ParentNode.AddChild(Node);
end;

procedure TMarkdownConverter.Parse(const WikiText: String);
begin
  FRoot := TWikiNode.Create(TNodeType.Text, WikiText, False);
  FHeader.Clear;
  FLinkDefinitions.Clear;

  FFootnote := TFootnote.Create;
  try
    ParseBlockNode(FRoot);
    ProcessInline(FRoot);

    if FFootnote.UseIndex > 0 then
      AddFootnote(FRoot);
  finally
    FFootnote.Free;
  end;
end;

procedure TMarkdownConverter.ParseBlockNode(const ParentNode: TWikiNode);
var
  Line: String;
  Lines: TStringList;
  LineCount: Integer;
  Index: Integer;
  Ret: TMatch;
  After, Match: String;
  Node: TWikiNode;
  ListMark: String;
  TableBody: TWikiNode;
  TableCellCount: Integer;
  TableAligns: TTableAligns;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := ParentNode.Text;

    ParentNode.Text := '';
    Node := nil;
    ListMark := '';
    Index := 0;
    TableBody := nil;
    TableCellCount := -1;
    LineCount := Lines.Count;

    while Index < LineCount do
    begin
      Line := Lines[Index];

      if Line = '' then
      begin
        if ParentNode.NodeType = TNodeType.ListItem then
        begin
          ParentNode.ParentNode.IsLooseList := True;
        end;

        OutputBuffer(ParentNode, Node);
        Node := nil;
        Inc(Index);
        Continue;
      end;

      Line := IndentTabToSpace(Line);

      // Setext headings
      if (Node <> nil) and (Node.NodeType in [TNodeType.Paragraph,
        TNodeType.Text]) then
      begin
        if TRegEx.IsMatch(Line, '^ {0,3}=+ *$', [roCompiled]) then
        begin
          Node.NodeType := TNodeType.Heading1;
          if (ParentNode = FRoot) and GiveIdToHeader then
            AddIdToHeader(Node, Index - CountLine(Node.Text));

          OutputBuffer(ParentNode, Node);
          Node := nil;

          Inc(Index);
          Continue;
        end;

        if TRegEx.IsMatch(Line, '^ {0,3}-+ *$', [roCompiled]) then
        begin
          Node.NodeType := TNodeType.Heading2;

          if (ParentNode = FRoot) and GiveIdToHeader then
            AddIdToHeader(Node, Index - CountLine(Node.Text));

          OutputBuffer(ParentNode, Node);
          Node := nil;

          Inc(Index);
          Continue;
        end;
      end;

      // preformatted text
      if (Node = nil) or (Node.NodeType in [TNodeType.Blockquote]) then
      begin
        Ret := TRegEx.Match(Line, '^ {4}(.*)', [roCompiled]);
        if Ret.Success then
        begin
          if Node <> nil then
          begin
            OutputBuffer(ParentNode, Node);
          end;

          Node := TWikiNode.Create(TNodeType.PreformattedText);

          Node.Text := AddLine(Node.Text, Ret.Groups[1].Value);
          var
            LocalIndex: Integer := Index + 1;

          while LocalIndex < LineCount do
          begin
            Line := IndentTabToSpace(Lines[LocalIndex]);

            Ret := TRegEx.Match(Line, '^ {4}(.*)', [roCompiled]);
            if Ret.Success then
            begin
              Node.Text := AddLine(Node.Text, Ret.Groups[1].Value);
              Inc(LocalIndex);
              Continue;
            end;

            if TRegEx.IsMatch(Line, '^ {0,3}$', [roCompiled]) then
            begin
              // only white space line
              Node.Text := AddLine(Node.Text, '');
              Inc(LocalIndex);
              Continue;
            end;

            Break;
          end;

          Node.Text := TRegEx.Replace(Node.Text, '(\r\n)+$', WikiLB,
            [roCompiled]);
          OutputBuffer(ParentNode, Node);
          Node := nil;
          Index := LocalIndex;
          Continue;
        end;
      end;

      // Fenced code blocks
      Ret := TRegEx.Match(Line, '^ {0,3}(([`~])\2{2,})(?: *)([^ ]*)(.*)$',
        [roCompiled]);
      if Ret.Success then
      begin
        Match := Ret.Groups[1].Value;
        var
          FenceMark: String := Ret.Groups[2].Value;
        var
          FenceLength: Integer := Length(Match);
        var
          IndentLength: Integer := GetIndentLength(Line);
        var
          IndentRemoveRegEx: String := Format('^ {0,%d}', [IndentLength]);
        var
          Language: String := Ret.Groups[3].Value;
        var
          AfterLanguage: String := Ret.Groups[4].Value;

        if (FenceMark[1] <> '`') or (Pos('`', Language) = 0) and
          (Pos('`', AfterLanguage) = 0) then
        begin
          if Node <> nil then
          begin
            OutputBuffer(ParentNode, Node);
          end;

          Node := TWikiNode.Create(TNodeType.PreformattedText);
          Node.CodeLanguage := Language;

          var
            LocalIndex: Integer := Index + 1;
          while LocalIndex < LineCount do
          begin
            Line := IndentTabToSpace(Lines[LocalIndex]);

            if TRegEx.IsMatch(Line, Format('^ {0,3}%s{%d,}$',
              [FenceMark, FenceLength])) then
            begin
              // end of block
              Inc(LocalIndex);
              Break;
            end;

            if IndentLength > 0 then
            begin
              // remove indent
              Line := TRegEx.Create(IndentRemoveRegEx).Replace(Line, '', 1);
            end;

            Node.Text := Node.Text + Line + WikiLB;
            Inc(LocalIndex);
          end;

          OutputBuffer(ParentNode, Node);
          Node := nil;
          Index := LocalIndex;
          Continue;
        end;
      end;

      // blockquote
      Ret := TRegEx.Match(Line, '^ {0,3}> ?(.*)$', [roCompiled]);
      if Ret.Success then
      begin
        if (Node = nil) or (Node.NodeType <> TNodeType.Blockquote) then
        begin
          OutputBuffer(ParentNode, Node);
          Node := TWikiNode.Create(TNodeType.Blockquote);
        end;

        Match := Ret.Groups[1].Value;
        Node.Text := AddLine(Node.Text, Match);
        Inc(Index);
        Continue;
      end;

      // horizontanl rule
      if IsThematicBreak(Line) then
      begin
        OutputBuffer(ParentNode, Node);

        Node := TWikiNode.Create(TNodeType.HorizontalRule);
        OutputBuffer(ParentNode, Node);

        Node := nil;
        Inc(Index);
        Continue;
      end;

      // empty unorderd list
      Ret := TRegEx.Match(Line, '^( {0,3})([-+*]) *$', [roCompiled]);
      if Ret.Success then
      begin
        if Index <= (LineCount - 2) then
        begin
          var
            Next: String := IndentTabToSpace(Lines[Index + 1]);
          var
            Indent: String := Ret.Groups[1].Value;
          var
            Mark: String := Ret.Groups[2].Value;
          var
            NeedIndent: Integer := Length(Indent) + 2;

          if NeedIndent <= GetIndentLength(Next) then
          begin
            Next := RightStr(Next, Length(Next) - NeedIndent);
            Line := Indent + Mark + ' ' + Next;
            Inc(Index);
          end;
        end;
      end;

      // unorderd list
      Ret := TRegEx.Match(Line, '^ {0,3}([-+*])( +).*$', [roCompiled]);
      if Ret.Success then
      begin
        OutputBuffer(ParentNode, Node);
        Node := TWikiNode.Create(TNodeType.UnorderedList);
        ListMark := Ret.Groups[1].Value;

        var
          ItemIndent: Integer := Length(Ret.Groups[2].Value) - 1;

        var
          ItemBeginPos: Integer;

        if ItemIndent < 4 then
          ItemBeginPos := GetIndentLength(Line) + Length(ListMark) + 1 +
            ItemIndent
        else
          // indented code
          ItemBeginPos := GetIndentLength(Line) + Length(ListMark) + 1;

        var
          LocalIndex: Integer := Index;
        var
          ListBlockLines: TStringList := TStringList.Create;
        try
          ListBlockLines.Add(Line);
          Inc(LocalIndex);

          while LocalIndex < LineCount do
          begin
            Line := IndentTabToSpace(Lines[LocalIndex]);

            if TRegEx.IsMatch(Line, '^ *$', [roCompiled]) then
            begin
              ListBlockLines.Add(Line);
              Inc(LocalIndex);
              Continue;
            end;

            if ItemBeginPos <= GetIndentLength(Line) then
            begin
              // paragraph continuation line or nested list item
              ListBlockLines.Add(Line);
              Inc(LocalIndex);
              Continue;
            end;

            Ret := TRegEx.Match(Line, '^ {0,3}([-+*])(.*)$');
            if not Ret.Success then
            begin
              if (ItemBeginPos > GetIndentLength(Line)) then
              begin
                if Lines[LocalIndex - 1] = '' then
                begin
                  Break;
                end;
              end;

              if IsListBreakLine(Line) then
              begin
                Break;
              end;

              // paragraph continuation line
              ListBlockLines.Add(Line);
              Inc(LocalIndex);
              Continue;
            end;

            if Ret.Groups[1].Value <> ListMark then
            begin
              Break;
            end;

            if IsThematicBreak(Line) then
            begin
              Break;
            end;

            var
              ListItem: String := Ret.Groups[2].Value;

            if (ListItem <> '') and (ListItem[1] <> ' ') then
            begin
              // No space after list symbol
              Break;
            end;

            // list item
            ListBlockLines.Add(Line);
            Inc(LocalIndex);
          end;

          Index := LocalIndex - 1;
          if Lines[Index] = '' then
          begin
            // If the line ends with a blank line, restart processing from the last blank line
            Index := Index - 1;
          end;
          AddUnorderdList(Node, ListBlockLines);
          OutputBuffer(ParentNode, Node);
          Node := nil;
          Inc(Index);
          Continue;
        finally
          ListBlockLines.Free;
        end;
      end;

      // empty orderd list
      Ret := TRegEx.Match(Line, '^( {0,3})([0-9]{1,9})([.)]) *$', [roCompiled]);
      if Ret.Success then
      begin
        if Index <= (LineCount - 2) then
        begin
          var
            Next: String := IndentTabToSpace(Lines[Index + 1]);
          var
            Indent: String := Ret.Groups[1].Value;
          var
            Mark: String := Ret.Groups[3].Value;
          var
            Number: String := Ret.Groups[2].Value;
          var
            NeedIndent: Integer := Length(Indent) + Length(Number) + 2;

          if NeedIndent <= GetIndentLength(Next) then
          begin
            Next := RightStr(Next, Length(Next) - NeedIndent);
            Line := Indent + Number + Mark + ' ' + Next;
            Inc(Index);
          end;
        end;
      end;

      // orderd list
      Ret := TRegEx.Match(Line, '^ {0,3}([0-9]{1,9})([.)])( +).*$',
        [roCompiled]);
      if Ret.Success then
      begin
        var
          StartNumber: Integer := Ret.Groups[1].Value.ToInteger;

        if (StartNumber = 1) or (Node = nil) or
          (Node.NodeType <> TNodeType.Paragraph) then
        begin
          OutputBuffer(ParentNode, Node);
          Node := TWikiNode.Create(TNodeType.OrderedList);
          ListMark := Ret.Groups[2].Value;

          var
            ItemIndent: Integer := Length(Ret.Groups[3].Value) - 1;

          var
            ItemBeginPos: Integer;

          if ItemIndent < 4 then
            ItemBeginPos := GetIndentLength(Line) + Length(ListMark) +
              Length(Ret.Groups[1].Value) + 1 + ItemIndent
          else
            // indented code
            ItemBeginPos := GetIndentLength(Line) + Length(ListMark) +
              Length(Ret.Groups[1].Value) + 1;

          var
            LocalIndex: Integer := Index;

          if StartNumber <> 1 then
          begin
            Node.SetAttribute('start', StartNumber.ToString);
          end;

          var
            ListBlockLines: TStringList := TStringList.Create;

          try
            ListBlockLines.Add(Line);
            Inc(LocalIndex);

            while LocalIndex < LineCount do
            begin
              Line := IndentTabToSpace(Lines[LocalIndex]);

              if TRegEx.IsMatch(Line, '^ *$', [roCompiled]) then
              begin
                ListBlockLines.Add(Line);
                Inc(LocalIndex);
                Continue;
              end;

              if ItemBeginPos <= GetIndentLength(Line) then
              begin
                // paragraph continuation line or nested list item
                ListBlockLines.Add(Line);
                Inc(LocalIndex);
                Continue;
              end;

              Ret := TRegEx.Match(Line, '^ {0,3}[0-9]{1,9}([.)])(.*)$');
              if not Ret.Success then
              begin
                if (ItemBeginPos > GetIndentLength(Line)) then
                begin
                  if Lines[LocalIndex - 1] = '' then
                  begin
                    Break;
                  end;
                end;

                if IsListBreakLine(Line) then
                begin
                  Break;
                end;

                // paragraph continuation line
                ListBlockLines.Add(Line);
                Inc(LocalIndex);
                Continue;
              end;

              if Ret.Groups[1].Value <> ListMark then
              begin
                Break;
              end;

              var
                ListItem: String := Ret.Groups[2].Value;

              if (ListItem <> '') and (ListItem[1] <> ' ') then
              begin
                // No space after list symbol
                Break;
              end;

              if IsThematicBreak(Line) then
              begin
                Break;
              end;

              // list item
              ListBlockLines.Add(Line);
              Inc(LocalIndex);
            end;

            Index := LocalIndex - 1;
            if Lines[Index] = '' then
            begin
              // If the line ends with a blank line, restart processing from the last blank line
              Index := Index - 1;
            end;
            AddOrderdList(Node, ListBlockLines);
            OutputBuffer(ParentNode, Node);
            Node := nil;
            Inc(Index);
            Continue;
          finally
            ListBlockLines.Free;
          end;
        end;
      end;

      // headers empty
      Ret := TRegEx.Match(Line, '^ {0,3}(#{1,6})(?:[ \t]+#*)?(?:[ \t]*)$',
        [roCompiled]);
      if Ret.Success then
      begin
        OutputBuffer(ParentNode, Node);

        Match := Ret.Groups[1].Value;

        Node := TWikiNode.Create(GetHeaderType(Match));
        ParentNode.AddChild(Node);

        if (ParentNode = FRoot) and GiveIdToHeader then
          AddIdToHeader(Node, Index);

        Node := nil;
        Inc(Index);
        Continue;
      end;

      // headers
      Ret := TRegEx.Match(Line,
        '^ {0,3}(#{1,6})[ \t]+(.+?)(?:[ \t]+#*)?(?:[ \t]*)$', [roCompiled]);
      if Ret.Success then
      begin
        OutputBuffer(ParentNode, Node);

        Match := Ret.Groups[1].Value;
        After := Ret.Groups[2].Value;

        Node := TWikiNode.Create(GetHeaderType(Match));
        ParentNode.AddChild(Node);
        Node.Text := After;

        if (ParentNode = FRoot) and GiveIdToHeader then
          AddIdToHeader(Node, Index);

        Node := nil;
        Inc(Index);
        Continue;
      end;

      // comment (html type 2)
      if TRegEx.IsMatch(Line, '^ {0,3}<!--', [roCompiled]) then
      begin
        OutputBuffer(ParentNode, Node);
        ProcessUntilEndMark(Index, Lines, ParentNode, '-->');
        Node := nil;
        Inc(Index);
        Continue;
      end;

      // processing instruction (html type 3)
      if TRegEx.IsMatch(Line, '^ {0,3}<\?', [roCompiled]) then
      begin
        OutputBuffer(ParentNode, Node);
        ProcessUntilEndMark(Index, Lines, ParentNode, '\?>');
        Node := nil;
        Inc(Index);
        Continue;
      end;

      // declaration (html type 4)
      if TRegEx.IsMatch(Line, '^ {0,3}<![A-Z]', [roCompiled]) then
      begin
        OutputBuffer(ParentNode, Node);
        ProcessUntilEndMark(Index, Lines, ParentNode, '>');
        Node := nil;
        Inc(Index);
        Continue;
      end;

      // CDATA (html type 5)
      if TRegEx.IsMatch(Line, '^ {0,3}<!\[CDATA\[', [roCompiled]) then
      begin
        OutputBuffer(ParentNode, Node);
        ProcessUntilEndMark(Index, Lines, ParentNode, '\]\]>');
        Node := nil;
        Inc(Index);
        Continue;
      end;

      // html
      Ret := TRegEx.Match(Line, '^ {0,3}</?([a-zA-Z]+)([ />]|$)', [roCompiled]);
      if Ret.Success then
      begin
        var
          TagName: String := LowerCase(Ret.Groups[1].Value);

          // type 1 tag
        if TRegEx.IsMatch(TagName, '^(script|style|pre)$', [roCompiled]) then
        begin
          OutputBuffer(ParentNode, Node);
          ProcessUntilEndMark(Index, Lines, ParentNode, '</' + TagName + '>');
          Node := nil;
          Inc(Index);
          Continue;
        end;

        // type 7 tag
        if not IsBlockTag(TagName) then
        begin
          if (Node <> nil) and (Node.NodeType = TNodeType.Paragraph) then
          begin
            Node.Text := AddLine(Node.Text, Line);
            Inc(Index);
            Continue;
          end;

          if (Node <> nil) and (Node.NodeType = TNodeType.Table) then
          begin
            if TableBody = nil then
            begin
              raise ERangeError.CreateFmt('Invalid table. [%s]', [Line]);
            end;
            AddTableRow(Line, TableCellCount, TableAligns, TableBody);
            Inc(Index);
            Continue;
          end;

          Ret := TRegEx.Match(Line, '^.*?<.*?>(.*)', [roCompiled]);
          if Ret.Success then
          begin
            var
              AfterText: String := Ret.Groups[1].Value;

            if AfterText <> '' then
            begin
              // Characters exist after the tag
              OutputBuffer(ParentNode, Node);
              Node := TWikiNode.Create(TNodeType.Paragraph, Line, False);
              Inc(Index);
              Continue;
            end;

            if GetRawHtmlEndPosition(TrimLeft(Line), 1) = 0 then
            begin
              // not tag
              OutputBuffer(ParentNode, Node);
              Node := TWikiNode.Create(TNodeType.Paragraph, Line, False);
              Inc(Index);
              Continue;
            end;
          end;
        end;

        OutputBuffer(ParentNode, Node);

        Node := TWikiNode.Create(TNodeType.Raw, Line, False);

        var
          LocalIndex: Integer := Index + 1;
        while LocalIndex < LineCount do
        begin
          Line := Lines[LocalIndex];
          if Line = '' then
          begin
            Node.Text := Node.Text + WikiLB;
            Inc(LocalIndex);
            Break;
          end;

          Node.Text := AddLine(Node.Text, Line);
          Inc(LocalIndex);
        end;

        if LocalIndex = LineCount then
        begin
          Node.Text := Node.Text + WikiLB;
        end;

        OutputBuffer(ParentNode, Node);
        Node := nil;
        Index := LocalIndex;
        Continue;
      end;

      // table body
      if (Node <> nil) and (Node.NodeType = TNodeType.Table) then
      begin
        if TableBody = nil then
        begin
          raise ERangeError.CreateFmt('Invalid table. [%s]', [Line]);
        end;
        AddTableRow(Line, TableCellCount, TableAligns, TableBody);
        Inc(Index);
        Continue;
      end;

      // table
      if TRegEx.IsMatch(Line, '^ {0,3}.+\|.+', [roCompiled]) then
      begin
        if Index < (LineCount - 1) then
        begin
          var
            Header: String := Lines[Index + 1];

          if TRegEx.IsMatch(Header, '^ {0,3}\|?(\s*:?-+:?\s*|)+', [roCompiled])
          then
          begin
            TableCellCount := GetTableCellCount(Header);

            if TableCellCount = GetTableCellCount(Line) then
            begin
              OutputBuffer(ParentNode, Node);
              TableAligns := GetTableAligns(Header, TableCellCount);
              Node := TWikiNode.Create(TNodeType.Table);
              Node.AddChild(GetTableHeader(Line, TableCellCount, TableAligns));
              TableBody := TWikiNode.Create(TNodeType.TableBody);
              Node.AddChild(TableBody);
              Inc(Index, 2);
              Continue;
            end;
          end;
        end;
      end;

      // link definition or footnote definition
      if TRegEx.IsMatch(Line, '^ {0,3}\[', [roCompiled]) then
      begin
        // cannot interrupt a paragraph
        if (Node = nil) or (Node.NodeType <> TNodeType.Paragraph) then
        begin
          if TRegEx.IsMatch(Line, '^ {0,3}\[\^[^\s]*\]', [roCompiled]) then
          begin
            // footnote definistion
            var
              LineNumber: Integer := ProcessFootnoteDefinition(Index, Lines);

            if LineNumber > 0 then
            begin
              Index := Index + LineNumber;
              Continue;
            end;
          end
          else
          begin
            // link definition
            var
              Text: String := Line;
            var
              LocalIndex: Integer := Index;

            while LocalIndex < (LineCount - 1) do
            begin
              LocalIndex := LocalIndex + 1;
              if Lines[LocalIndex] = '' then
              begin
                Break;
              end;

              Text := AddLine(Text, Lines[LocalIndex]);
            end;

            var
              LineNumber: Integer := ProcessLinkDefinition(Text + WikiLB);

            if LineNumber > 0 then
            begin
              Index := Index + LineNumber;
              Continue;
            end;
          end;
        end;
      end;

      if TRegEx.IsMatch(Line, '^\s*$', [roCompiled]) then
      begin
        // Lines containing only white space characters are considered blank lines
        OutputBuffer(ParentNode, Node);
        Node := nil;
        Inc(Index);
        Continue;
      end;

      // paragraph
      if Node = nil then
      begin
        if ParentNode.NodeType = TNodeType.ListItem then
          // List items are basically text.
          // If the list is loose, make changes to the paragraph elsewhere.
          Node := TWikiNode.Create(TNodeType.Text)
        else
          Node := TWikiNode.Create(TNodeType.Paragraph);
      end
      else
      begin
        if (Node.NodeType = TNodeType.Blockquote) and
          TRegEx.IsMatch(Lines[Index - 1], '^>[ \t]*$', [roCompiled]) then
        begin
          // Terminate block quote if the previous line is only '>'
          OutputBuffer(ParentNode, Node);
          Node := TWikiNode.Create(TNodeType.Paragraph);
        end;
      end;

      Node.Text := AddLine(Node.Text, Line);
      Inc(Index);
    end;

    OutputBuffer(ParentNode, Node);
  finally
    Lines.Free;
  end;
end;

procedure TMarkdownConverter.ParseHeader(const WikiText: String);
begin
  FRoot := TWikiNode.Create(TNodeType.Text, WikiText, False);
  FHeader.Clear;

  FFootnote := TFootnote.Create;
  try
    ParseBlockNode(FRoot);
  finally
    FFootnote.Free;
  end;
end;

function TMarkdownConverter.ProcessFootnote(const CurrentNode: TWikiNode;
  const Text: String; const Offset, Copied: Integer): Integer;
var
  BPos, CPos, EPos: Integer;
  Id: String;
  Item: TFootnoteItem;
  Ch: Char;
  TextLength: Integer;
begin
  if (GetAt(Text, Offset) <> '[') or (GetAt(Text, Offset + 1) <> '^') then
    Exit(0);

  BPos := Offset + 2;
  CPos := BPos;
  TextLength := Length(Text);
  EPos := 0;
  while CPos <= TextLength do
  begin
    Ch := GetAtSkipEscapedChar(Text, CPos);

    if Ch = ']' then
    begin
      EPos := CPos;
      Break;
    end;

    Inc(CPos);
  end;

  if EPos = 0 then
    Exit(0);

  Id := Encode(Copy(Text, BPos, EPos - BPos), False);

  Item := FFootnote.FindByIdAndMarkUse(Id);
  if Item = nil then
    Exit(0);

  AddBeforeTextNode(CurrentNode, Copy(Text, Copied + 1, Offset - Copied - 1));

  var
    RefId: String := Item.Id;
  if Item.Count > 1 then
    RefId := RefId + '-' + Item.Count.ToString;
  var
    Node: TWikiNode := TWikiNode.Create(TNodeType.Anchor);
  Node.SetAttribute('class', TFootnote.ClassRefAnchor);
  Node.SetAttribute('href', '#' + TFootnote.IdPrefix + Item.Id);
  Node.SetAttribute('id', TFootnote.RefIdPrex + RefId);
  Node.AddChild(TWikiNode.Create(TNodeType.Text, '[' + Item.Index.ToString +
    ']', True));
  CurrentNode.AddChild(Node);

  Result := EPos;
end;

function TMarkdownConverter.ProcessFootnoteDefinition(const Index: Integer;
  const Lines: TStringList): Integer;
var
  Id, Caption: String;
  LineCount: Integer;
  CurrentIndex: Integer;
  CPos: Integer;
  Ch: Char;
  Line: String;
  TextLength: Integer;
  Escaped: Boolean;
begin
  CPos := 1;
  Line := Lines[Index];
  TextLength := Length(Line);

  // skip space
  while (CPos <= TextLength) and (GetAt(Line, CPos) = ' ') do
    Inc(CPos);

  if (GetAt(Line, CPos) <> '[') or (GetAt(Line, CPos + 1) <> '^') then
    Exit(0);

  Inc(CPos, 2);
  Escaped := False;
  Id := '';
  while CPos <= TextLength do
  begin
    Ch := GetAt(Line, CPos);
    if Ch = '\' then
    begin
      Escaped := not Escaped;
      Inc(CPos);
      Continue;
    end;

    if Ch = ' ' then
      Exit(0);

    if Escaped then
    begin
      Escaped := False;
      Inc(CPos);
      Continue;
    end;

    if Ch = ']' then
    begin
      Break;
    end;

    Id := Id + Ch;
    Inc(CPos);
  end;

  if GetAt(Line, CPos + 1) <> ':' then
    Exit(0);

  Inc(CPos);

  if Id = '' then
    Exit(0);

  Id := Encode(LowerCase(Id), False);
  Caption := RightStr(Line, TextLength - CPos);

  LineCount := Lines.Count;
  CurrentIndex := Index + 1;
  while CurrentIndex < LineCount do
  begin
    Line := Lines[CurrentIndex];
    if Line = '' then
    begin
      Caption := AddLine(Caption, '');
      Inc(CurrentIndex);
      Continue;
    end;

    if GetIndentLength(Line) < 2 then
      Break;

    Caption := AddLine(Caption, Trim(Line));
    Inc(CurrentIndex);
    Continue;
  end;

  FFootnote.Add(Id, Caption);
  Result := CurrentIndex - Index;
end;

procedure TMarkdownConverter.ProcessInline(const Node: TWikiNode);
var
  ChildNode: TWikiNode;
  Pos, Copied: Integer;
  Text: String;
  Item: Char;
  Escaped: Boolean;
begin
  if Node.Processed then
    Exit;

  if Node.ChildNodes <> nil then
  begin
    for var Child in Node.ChildNodes do
    begin
      ProcessInline(Child);
    end;
  end;

  if Node.NodeType = TNodeType.Raw then
  begin
    Node.Value := Node.Text;
    Node.Text := '';
    Node.Processed := True;
    Exit;
  end;

  if Node.NodeType = TNodeType.Code then
  begin
    if Node.Text <> '' then
    begin
      Node.AddChild(TWikiNode.Create(TNodeType.Text,
        TNetEncoding.Html.Encode(Node.Text), True));
      Node.Text := '';
    end;
    Node.Processed := True;
    Exit;
  end;

  // Remove leading whitespace
  Node.Text := TRegEx.Replace(Node.Text, '^ +', '', [roCompiled, roMultiLine]);

  // Remove blank spaces at the end of paragraphs
  Node.Text := TRegEx.Replace(Node.Text, ' +$', '', [roCompiled]);

  if Node.Text <> '' then
  begin
    Text := Node.Text;
    Node.Text := '';
    Pos := 1;
    Copied := 0;
    Escaped := False;
    var
      TextLength: Integer := Length(Text);

    while Pos < TextLength do
    begin
      Item := GetAt(Text, Pos);

      if Item = #0 then
      begin
        // replace insecure character
        Text[Pos] := #$FFFD;
      end;

      if Item = '\' then
      begin
        if not Escaped then
        begin
          if GetAt(Text, Pos + 1) = #13 then
          begin
            // Hard line break (\ + line break)
            AddBeforeTextNode(Node,
              TrimRight(Copy(Text, Copied + 1, Pos - Copied - 1)));

            Node.AddChild(TWikiNode.Create(TNodeType.Break, '', True));

            Inc(Pos, 3);
            Copied := Pos - 1;
            Escaped := False;
            Continue;
          end;
        end;
        Escaped := not Escaped;
        Inc(Pos);
        Continue;
      end;

      if Item = ' ' then
      begin
        if (GetAt(Text, Pos + 1) = ' ') and (GetAt(Text, Pos + 2) = #13) then
        begin
          // Hard line break ('  ' + line break)
          AddBeforeTextNode(Node, TrimRight(Copy(Text, Copied + 1,
            Pos - Copied - 1)));

          Node.AddChild(TWikiNode.Create(TNodeType.Break, '', True));

          Inc(Pos, 4);
          Copied := Pos - 1;
          Escaped := False;
          Continue;
        end;
      end;

      if Escaped then
      begin
        Escaped := False;
        Inc(Pos);
        Continue;
      end;

      // maybe link or footnote referance
      if Item = '[' then
      begin
        if GetAt(Text, Pos + 1) = '^' then
        begin
          var
            Pos2: Integer := ProcessFootnote(Node, Text, Pos, Copied);
          if Pos2 > 0 then
          begin
            Pos := Pos2 + 1;
            Copied := Pos - 1;
            Continue;
          end;
        end;

        if GetAt(Text, Pos + 1) = '[' then
        begin
          var
            Pos2: Integer := ProcessWikiLink(Node, Text, Pos, Copied);
          if Pos2 > 0 then
          begin
            Pos := Pos2 + 1;
            Copied := Pos - 1;
            Continue;
          end;
        end;

        var
          LinkInfo: TLinkDefinition := GetLinkInfo(Text, Pos, False);
        if LinkInfo <> nil then
        begin
          // inline or reference link
          try
            AddBeforeTextNode(Node, Copy(Text, Copied + 1, Pos - Copied - 1));

            ChildNode := TWikiNode.Create(TNodeType.Anchor);
            ChildNode.SetAttribute('href', Encode(LinkInfo.Destination, False));
            if LinkInfo.Title <> '' then
            begin
              ChildNode.SetAttribute('title', Encode(LinkInfo.Title, True));
            end;
            ChildNode.AddChild(TWikiNode.Create(TNodeType.Text,
              LinkInfo.Name, False));
            ProcessInline(ChildNode);
            Node.AddChild(ChildNode);

            Pos := LinkInfo.EndPosition + 1;
            Copied := Pos - 1;
            Continue;
          finally
            LinkInfo.Free;
          end;
        end;
      end;

      // maybe image
      if Item = '!' then
      begin
        var
          LinkInfo: TLinkDefinition := GetLinkInfo(Text, Pos + 1, True);
        if LinkInfo <> nil then
        begin
          // image
          try
            AddBeforeTextNode(Node, Copy(Text, Copied + 1, Pos - Copied - 1));

            ChildNode := TWikiNode.Create(TNodeType.Image);
            var
              AltNode: TWikiNode := TWikiNode.Create(TNodeType.Text,
                LinkInfo.Name, False);
            try
              ProcessInline(AltNode);
              ChildNode.SetAttribute('alt', GetTextValue(AltNode));
            finally
              AltNode.Free;
            end;
            ChildNode.SetAttribute('src', Encode(LinkInfo.Destination, False));
            if LinkInfo.Title <> '' then
            begin
              ChildNode.SetAttribute('title', Encode(LinkInfo.Title, True));
            end;
            Node.AddChild(ChildNode);
            ProcessInline(ChildNode);

            Pos := LinkInfo.EndPosition + 1;
            Copied := Pos - 1;
            Continue;
          finally
            LinkInfo.Free;
          end;
        end;
      end;

      // maybe code span
      if (Item = '`') and (GetAt(Text, Pos - 1) <> '`') then
      begin
        var
          Len: Integer := GetSameCharLength(Text, '`', Pos);
        var
          Pos2: Integer := Pos + Len;
        var
          Mark: String := StringOfChar('`', Len);
        var
          Offset: Integer := Pos2;
        var
          Pos3: Integer := GetCodeCloseMarkStartingPosition(Text, Mark, Offset);

        if Pos3 > 0 then
        begin
          // Code span
          AddBeforeTextNode(Node, Copy(Text, Copied + 1, Pos - Copied - 1));
          var
            Code: String := Copy(Text, Pos2, Pos3 - Pos2);

          if (Code[1] = ' ') and (Code[Length(Code)] = ' ') and
            TRegEx.IsMatch(Code, '[^ ]', [roCompiled]) then
          begin
            // Strip single leading and trailing space
            Code := Copy(Code, 2, Length(Code) - 2);
          end;
          // Line ending to space
          Code := TRegEx.Replace(Code, '^\r\n', '', [roCompiled]);
          Code := TRegEx.Replace(Code, '\r\n$', '', [roCompiled]);
          Code := TRegEx.Replace(Code, '\r\n', ' ', [roCompiled]);

          ChildNode := TWikiNode.Create(TNodeType.Code,
            TNetEncoding.Html.Encode(Code), True);
          Node.AddChild(ChildNode);

          Pos := Pos3 + Len;
          Copied := Pos - 1;
          Continue;
        end;
      end;

      // maybe strikethrough
      if Item = '~' then
      begin
        var
          Len: Integer := GetSameCharLength(Text, '~', Pos);
        if Len > 2 then
        begin
          // too long
          Inc(Pos, Len);
          Continue;
        end;

        var
          Mark: String := StringOfChar(Item, Len);
        var
          Pos2: Integer := GetDeleteCloseMarkStartingPostion(Text, Mark,
            Pos + Len);

        if Pos2 > 0 then
        begin
          AddBeforeTextNode(Node, Copy(Text, Copied + 1, Pos - Copied - 1));
          var
            Target: String := Copy(Text, Pos + Len, Pos2 - Pos - Len);

          ChildNode := TWikiNode.Create(TNodeType.Delete, Target, False);
          ProcessInline(ChildNode);
          Node.AddChild(ChildNode);

          Pos := Pos2 + Len;
          Copied := Pos - 1;
        end;

        // not found close mark
        Inc(Pos, Len);
        Continue;
      end;

      // maybe emphasis
      if CharInSet(Item, ['*', '_']) then
      begin
        var
          Closer: TCloser := SearchCloser(Text, Pos, Item);

        if Closer.Position > 0 then
        begin
          // emphasis
          AddBeforeTextNode(Node, Copy(Text, Copied + 1, Pos - Copied - 1));
          var
            InnerStr: string := Copy(Text, Pos + Closer.Length,
              (Closer.Position - Closer.Length) - Pos - Closer.Length + 1);

          if Closer.Length = 1 then
            ChildNode := TWikiNode.Create(TNodeType.Emphasis)
          else
            ChildNode := TWikiNode.Create(TNodeType.Strong);

          ChildNode.Text := InnerStr;
          ProcessInline(ChildNode);
          Node.AddChild(ChildNode);

          Copied := Closer.Position;
          Pos := Copied + 1;
          Continue;
        end;

        // not emphasis
        Inc(Pos);
        Continue;
      end;

      // maybe autolink or raw html
      if Item = '<' then
      begin
        var
          Pos2: Integer := System.Pos('>', Text, Pos + 1);

        if Pos2 > 0 then
        begin
          var
            InnerValue: String := Copy(Text, Pos + 1, Pos2 - Pos - 1);

          if IsLink(InnerValue) then
          begin
            // InnerValue link
            AddBeforeTextNode(Node, Copy(Text, Copied + 1, Pos - Copied - 1));

            ChildNode := TWikiNode.Create(TNodeType.Anchor,
              TNetEncoding.Html.Encode(InnerValue), True);
            ChildNode.SetAttribute('href', InnerValue);
            Node.AddChild(ChildNode);

            Pos := Pos2 + 1;
            Copied := Pos - 1;
            Continue;
          end;

          if IsEmailAddress(InnerValue) then
          begin
            // email link
            AddBeforeTextNode(Node, Copy(Text, Copied + 1, Pos - Copied - 1));

            ChildNode := TWikiNode.Create(TNodeType.Anchor,
              TNetEncoding.Html.Encode(InnerValue), True);
            ChildNode.SetAttribute('href', 'mailto:' + InnerValue);
            Node.AddChild(ChildNode);

            Pos := Pos2 + 1;
            Copied := Pos - 1;
            Continue;
          end;

          var
            Pos3: Integer := GetRawHtmlEndPosition(Text, Pos);

          if Pos3 > 0 then
          begin
            // raw html
            AddBeforeTextNode(Node, Copy(Text, Copied + 1, Pos - Copied - 1));

            Node.AddChild(TWikiNode.Create(TNodeType.Raw,
              Copy(Text, Pos, Pos3 - Pos + 1), True));

            Pos := Pos3 + 1;
            Copied := Pos - 1;
            Continue;
          end;
        end;
      end;

      // maybe extended url autolink
      begin
        var
          Pos2: Integer := GetUrlAutoLinkEndPosition(Text, Pos);
        if Pos2 > 0 then
        begin
          AddBeforeTextNode(Node, Copy(Text, Copied + 1, Pos - Copied - 1));
          var
            InnerValue: String := Copy(Text, Pos, Pos2 - Pos + 1);

          ChildNode := TWikiNode.Create(TNodeType.Anchor,
            TNetEncoding.Html.Encode(InnerValue), True);
          ChildNode.SetAttribute('href', InnerValue);
          Node.AddChild(ChildNode);

          Pos := Pos2 + 1;
          Copied := Pos - 1;
          Continue;
        end;
      end;

      Pos := Pos + 1;
    end;

    Node.Text := Copy(Text, Copied + 1, Pos);
  end;

  // Remove trailing whitespace without Hard line breaks mark
  Node.Text := TRegEx.Replace(Node.Text, '[ \x09]+$', '',
    [roCompiled, roMultiLine]);

  if (Node.Text = '') and (Node.ChildNodes = nil) and
    (Node.NodeType = TNodeType.Paragraph) then
  begin
    // Don't turn empty paragraphs into paragraphs
    // Occurs on block quote lines with only blank spaces
    Node.NodeType := TNodeType.Text;
  end;

  if Node.Text <> '' then
  begin
    if Node.ChildNodes = nil then
      Node.Value := Encode(Node.Text, True)
    else
      Node.AddChild(TWikiNode.Create(TNodeType.Text, Encode(Node.Text,
        True), True));
    Node.Text := '';
  end;

  Node.Processed := True;
end;

function TMarkdownConverter.ProcessLinkDefinition(const Text: String): Integer;
var
  Destination: String;
  Ended: Boolean;
  Escaped: Boolean;
  Id: String;
  Item: Char;
  LineNumber: Integer;
  Title: String;
  Pos: Integer;
  TextLength: Integer;
begin
  Pos := 1;
  Destination := '';
  Id := '';
  Title := '';
  LineNumber := 0;
  TextLength := Length(Text);

  while GetAt(Text, Pos) <> '[' do
    Inc(Pos);

  // get id
  Escaped := False;
  Ended := False;
  while Pos <= TextLength do
  begin
    Inc(Pos);
    Item := GetAt(Text, Pos);

    if (Item = ']') and not Escaped then
    begin
      Ended := True;
      Break;
    end;

    if (Item = '[') and not Escaped then
    begin
      // cannot contain brackets
      Break;
    end;

    if Item = #10 then
    begin
      Inc(LineNumber);
    end;

    if Item = '\' then
      Escaped := not Escaped
    else
      Escaped := False;

    Id := Id + Item;
  end;

  if not Ended then
    Exit(0);

  Id := GetLinkId(Id);
  if (Id = '') or (Id = ' ') then
    // must contain at least one non-whitespace character
    Exit(0);

  // need ':'
  Pos := Pos + 1;
  if GetAt(Text, Pos) <> ':' then
    Exit(0);

  // skip blank
  repeat
    Inc(Pos);
    Item := GetAt(Text, Pos);
    if Item = #10 then
    begin
      Inc(LineNumber);
    end;
  until not IsWhitespaceChar(Item);

  // destination
  Escaped := False;
  Ended := False;
  var
    ExistsBlank: Boolean := False;
  if Item = '<' then
  begin
    while Pos <= TextLength do
    begin
      Inc(Pos);
      Item := GetAt(Text, Pos);

      if (Item = '>') and not Escaped then
      begin
        Ended := True;
        Break;
      end;

      if Item = #13 then
      begin
        Exit(0);
      end;

      if Item = '\' then
        Escaped := not Escaped
      else
        Escaped := False;

      Destination := Destination + Item;
    end;
  end
  else if Pos <= TextLength then
  begin
    while Pos <= TextLength do
    begin
      if IsWhitespaceChar(Item) then
      begin
        ExistsBlank := True;
        Break;
      end;

      if Item = '\' then
        Escaped := not Escaped
      else
        Escaped := False;

      Ended := True;
      Destination := Destination + Item;
      Inc(Pos);
      Item := GetAt(Text, Pos);
    end;
  end;

  if not Ended then
    Exit(0);

  // need blank
  if (not ExistsBlank) and (not IsWhitespaceChar(GetAt(Text, Pos + 1))) then
    Exit(0);

  // skip blank
  repeat
    Inc(Pos);
    Item := GetAt(Text, Pos);
    if Item = #10 then
    begin
      Inc(LineNumber);
    end;
  until not IsWhitespaceChar(Item);

  var
    LineNumberBeforTitle: Integer := LineNumber;

    // title
  if Pos <= TextLength then
  begin
    var
      Mark: Char := GetAt(Text, Pos);
    var
      ExistsTitle: Boolean := False;

    if CharInSet(Mark, ['"', '''', '(']) then
    begin
      ExistsTitle := True;
    end;

    var
      BreakLineAfterDest: Boolean := GetAt(Text, Pos - 1) = #10;

    if (not ExistsTitle) and (not BreakLineAfterDest) then
      Exit(0);

    if ExistsTitle then
    begin
      if Mark = '(' then
        Mark := ')';

      Escaped := False;
      Ended := False;

      while True do
      begin
        Inc(Pos);
        Item := GetAt(Text, Pos);

        if (Item = Mark) and (not Escaped) then
        begin
          Ended := True;
          Break;
        end;

        if Pos > TextLength then
          Break;

        if Item = #10 then
        begin
          Inc(LineNumber);
        end;

        if Item = '\' then
          Escaped := not Escaped
        else
          Escaped := False;

        Title := Title + Item;
      end;

      if not Ended then
      begin
        if BreakLineAfterDest then
          Title := ''
        else
          Exit(0);
      end;

      if Ended then
      begin
        // skip blank
        repeat
          Inc(Pos);
          Item := GetAt(Text, Pos);
          if Item = #10 then
          begin
            Inc(LineNumber);
          end;
        until not IsWhitespaceChar(Item);

        // text after title
        if Pos <= TextLength then
        begin
          if BreakLineAfterDest then
            Title := ''
          else if Text[Pos - 1] <> #10 then
            Exit(0);
        end;
      end;
    end;
  end;

  if not FLinkDefinitions.ContainsKey(Id) then
  begin
    var
      LinkDefinition: TLinkDefinition := TLinkDefinition.Create;

    LinkDefinition.Id := Id;
    LinkDefinition.Destination := Destination;
    LinkDefinition.Title := Title;

    FLinkDefinitions.Add(Id, LinkDefinition);
  end;

  if Title <> '' then
    Result := LineNumber
  else
    Result := LineNumberBeforTitle;
end;

procedure TMarkdownConverter.ProcessUntilEndMark(var Index: Integer;
  const Lines: TStringList; const ParentNode: TWikiNode;
  const Condition: String);
var
  Node: TWikiNode;
  LocalIndex: Integer;
  Line: String;
begin
  Node := TWikiNode.Create(TNodeType.Raw);

  LocalIndex := Index;
  while LocalIndex < Lines.Count do
  begin
    Line := Lines[LocalIndex];
    Node.Text := AddLine(Node.Text, Line);
    Inc(LocalIndex);
    if TRegEx.IsMatch(Line, Condition, [roIgnoreCase]) then
    begin
      Break;
    end;
  end;

  Index := LocalIndex - 1;

  Node.Text := Node.Text + WikiLB;
  OutputBuffer(ParentNode, Node);
end;

function TMarkdownConverter.ProcessWikiLink(const CurrentNode: TWikiNode;
  const Text: String; const Offset, Copied: Integer): Integer;
var
  CPos, EPos: Integer;
  Alias, Name: String;
  Ch: Char;
  TextLength: Integer;
begin
  if (GetAt(Text, Offset) <> '[') or (GetAt(Text, Offset + 1) <> '[') then
    Exit(0);

  CPos := Offset + 2;
  EPos := 0;
  TextLength := Length(Text);
  while CPos <= TextLength do
  begin
    Ch := GetAtSkipEscapedChar(Text, CPos);

    if (Ch = ']') and (GetAt(Text, CPos + 1) = ']') then
    begin
      EPos := CPos;
      Break;
    end;

    Inc(CPos);
  end;

  if EPos = 0 then
    Exit(0);

  Alias := '';
  Name := Encode(Copy(Text, Offset + 2, EPos - Offset - 2), False);

  // check [[page name]]
  if (FPageList = nil) or (not FPageList.ContainsKey(Name)) then
  begin
    var
      AliasPos: Integer := Pos('|', Name);
    if AliasPos = 0 then
      Exit(0);

    Alias := Copy(Name, 1, AliasPos - 1);
    Name := RightStr(Name, Length(Name) - AliasPos);

    // check [[alias|page name]]
    if (FPageList = nil) or (not FPageList.ContainsKey(Name)) then
    begin
      // check [[alias|uri]]
      if IsLink(Name) then
      begin
        AddBeforeTextNode(CurrentNode, Copy(Text, Copied + 1,
          Offset - Copied - 1));

        var
          Node: TWikiNode := TWikiNode.Create(TNodeType.Anchor);
        Node.SetAttribute('href', Name);
        Node.AddChild(TWikiNode.Create(TNodeType.Text, Alias, False));
        ProcessInline(Node);
        CurrentNode.AddChild(Node);

        Exit(EPos + 1);
      end;
      Exit(0);
    end;
  end;

  AddBeforeTextNode(CurrentNode, Copy(Text, Copied + 1, Offset - Copied - 1));

  var
    Node: TWikiNode := TWikiNode.Create(TNodeType.Anchor, '', True);
  Node.SetAttribute('href', FPageList[Name]);
  if Alias <> '' then
    Node.AddChild(TWikiNode.Create(TNodeType.Text, Alias, True))
  else
    Node.AddChild(TWikiNode.Create(TNodeType.Text, Name, True));
  CurrentNode.AddChild(Node);

  Result := EPos + 1;
end;

function TMarkdownConverter.SearchCloser(const Text: String;
  const BeginPos: Integer; const Mark: Char): TCloser;
var
  Closer: TCloser;
  DStack: TObjectList<TDelimiter>;
  CurrentPos: Integer;
  TextLength: Integer;
  Escaped: Boolean;
  Item: String;
  Len: Integer;
  CanOpen: Boolean;
  CanClose: Boolean;
  Delim: TDelimiter;
begin
  CurrentPos := BeginPos;
  Closer.Length := 0;
  Closer.Position := 0;

  Len := GetSameCharLength(Text, Mark, CurrentPos);
  var
    LeftPos: Integer := CurrentPos - 1;
  while GetAt(Text, LeftPos) = Mark do
  begin
    Dec(LeftPos);
  end;
  CanOpen := CanBeginEmphasis(Text, LeftPos + 1, CurrentPos + Len - 1, Mark);

  if not CanOpen then
    Exit(Closer);

  DStack := TObjectList<TDelimiter>.Create;
  try
    Delim := TDelimiter.Create;
    Delim.CanClose := False;
    Delim.CanOpen := CanOpen;
    Delim.Length := Len;
    Delim.Mark := Mark;
    Delim.FPrevLength := CurrentPos - LeftPos - 1;
    DStack.Add(Delim);

    CurrentPos := CurrentPos + Delim.Length;

    TextLength := Length(Text);
    Escaped := False;

    while CurrentPos <= TextLength do
    begin
      Item := GetAtSkipEscapedChar(Text, CurrentPos);

      // maybe code span
      if Item = '`' then
      begin
        var
          EndPos: Integer := GetCodeSpanEndPosition(Text, '`', CurrentPos);
        if EndPos > 0 then
        begin
          // skip code span
          CurrentPos := EndPos + 1;
          Continue;
        end;
      end;

      // maybe link
      if Item = '[' then
      begin
        var
          LinkInfo: TLinkDefinition := GetLinkInfo(Text, CurrentPos, False);
        if LinkInfo <> nil then
        begin
          // skip link
          CurrentPos := LinkInfo.EndPosition + 1;
          LinkInfo.Free;
          Continue;
        end;
      end;

      // maybe autolink or raw HTML
      if Item = '<' then
      begin
        var
          Pos2: Integer := GetRawHtmlEndPosition(Text, CurrentPos);
        if Pos2 > 0 then
        begin
          // skip raw HTML
          CurrentPos := Pos2 + 1;
          Continue;
        end;

        Pos2 := System.Pos('>', Text, CurrentPos + 1);
        if Pos2 > 0 then
        begin
          var
            InnerValue: String := Copy(Text, CurrentPos + 1,
              Pos2 - CurrentPos - 1);
          if IsEmailAddress(InnerValue) or IsLink(InnerValue) then
          begin
            // skip link
            CurrentPos := Pos2 + 1;
            Continue;
          end;
        end;
      end;

      if CharInSet(Item[1], ['*', '_']) then
      begin
        var
          CurrentMark: Char := Item[1];
        Len := GetSameCharLength(Text, CurrentMark, CurrentPos);
        var
          EndPos: Integer := CurrentPos + Len - 1;

        CanOpen := CanBeginEmphasis(Text, CurrentPos, EndPos, CurrentMark);
        CanClose := CanEndEmphasis(Text, CurrentPos, EndPos, CurrentMark);

        if CanClose then
        begin
          var
            CalcLen: Integer := Len;
          for var I := DStack.Count - 1 downto 0 do
          begin
            var
              Target: TDelimiter := DStack.Items[I];

            if CurrentMark <> Target.Mark then
              Continue;

            if IsCloseEmphasis(Target, CanOpen, CalcLen) then
            begin
              var
                MatchLength: Integer := 0;

              if (Target.Length mod 3 = 0) and (CalcLen mod 3 = 0) then
              begin
                // both are  (***)+
                // remove *** part
                var
                  MinLength: Integer := Min(Target.Length, CalcLen);
                var
                  DelLength: Integer := MinLength - (MinLength mod 3);

                Target.Length := Target.Length - DelLength;
                CalcLen := CalcLen - DelLength;

                if Target.Length = 0 then
                begin
                  // opener all match /(*)?(**)+/
                  MatchLength := 2 - (DelLength mod 2);
                end;
              end
              else if Target.Length = CalcLen then
              begin
                // all match /(*)?(**)+/
                MatchLength := 2 - (CalcLen mod 2);
                Target.Length := 0;
                CalcLen := 0;
              end
              else
              begin
                // remove * or ** part
                if (Target.Length mod 3 <> 0) and (CalcLen mod 3 <> 0) then
                  // both are not (***)+
                  MatchLength := 2 - Max(Target.Length mod 2, CalcLen mod 2)
                else
                begin
                  // one is (***)+
                  MatchLength := Max(Target.Length mod 3, CalcLen mod 3);
                end;

                Target.Length := Target.Length - MatchLength;
                Dec(CalcLen, MatchLength);
              end;

              if Target.Length = 0 then
              begin
                for var J := DStack.Count - 1 downto I do
                begin
                  DStack.Delete(J);
                end;

                if I = 0 then
                begin
                  Closer.Length := MatchLength;
                  Closer.Position := EndPos - CalcLen;
                  Exit(Closer);
                end;
              end;

              if CalcLen = 0 then
                Break;
            end;
          end;

          if CalcLen > 0 then
          begin
            Delim := TDelimiter.Create;
            Delim.CanClose := CanClose;
            Delim.CanOpen := CanOpen;
            Delim.Length := CalcLen;
            Delim.Mark := CurrentMark;
            Delim.FPrevLength := 0;
            DStack.Add(Delim);
          end;

          CurrentPos := CurrentPos + GetSameCharLength(Text, CurrentMark,
            CurrentPos);
          Continue;
        end;

        Delim := TDelimiter.Create;
        Delim.CanClose := CanClose;
        Delim.CanOpen := CanOpen;
        Delim.Length := Len;
        Delim.Mark := CurrentMark;
        Delim.FPrevLength := 0;
        DStack.Add(Delim);

        Inc(CurrentPos, Len);
        Continue;
      end;

      Inc(CurrentPos);
    end;
  finally
    DStack.Free;
  end;

  Result := Closer;
end;

end.
