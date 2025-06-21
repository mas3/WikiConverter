unit MyWiki.WikiConverter;

interface

uses
  Classes, System.SysUtils, System.Generics.Collections, System.StrUtils,
  MyWiki.WikiNode, MyWiki.WikiPlugin, MyWiki.Footnote, MyWiki.Header;

const
  WikiLB = #13#10;

type
  TWikiPlugins = TObjectDictionary<String, TWikiPlugin>;
  TWikiPages = TDictionary<String, String>; // page name and destination

  TLinkDefinition = class(TPersistent)
  private
    FDestination: String;
    FEndPosition: Integer; // for parsing inline link
    FId: String; // for picking up definition
    FName: String; // for creating a tag
    FTitle: String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Destination: String read FDestination write FDestination;
    property EndPosition: Integer read FEndPosition write FEndPosition;
    property Id: String read FId write FId;
    property Name: String read FName write FName;
    property Title: String read FTitle write FTitle;
  end;

  TLinkDefinitions = TObjectDictionary<String, TLinkDefinition>;

  TWikiConverter = class abstract(TObject)
  private
    function UriEncode(const Uri: String): String;
  protected
    FFootnote: TFootnote;
    FGiveIdToHeader: Boolean;
    FHeader: THeader;
    FPageList: TWikiPages; // reference of page names
    FRoot: TWikiNode;
    FWikiPlugins: TWikiPlugins; // reference of plugins

    procedure AddFootnote(const ParentNode: TWikiNode);
    procedure AddHeader(Header: THeaderItem);
    procedure AddIdToHeader(const Node: TWikiNode; const Index: Integer);
    function AddLine(const Prev, Line: String): String;
    function ExistsLineBreakAtEndOfLine(const Line: String): Boolean;
    function GetAt(const Text: String; const Pos: Integer): Char;
    function GetHeaderLevel(const NodeType: TNodeType): Integer;
    function GetHtml(const WikiNode: TWikiNode): String;
    function GetSameCharLength(const Text: String; const Ch: Char;
      Pos: Integer): Integer;
    function IsInListItem(const WikiNode: TWikiNode): Boolean;
    procedure Parse(const WikiText: String); virtual; abstract;
    procedure ParseHeader(const WikiText: String); virtual; abstract;
    // for SetHeader
    procedure ProcessInline(const Node: TWikiNode); virtual;
    function TrimRightLineBreak(const Line: String): String;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetPageList(const PageList: TWikiPages);
    procedure SetPlugins(const Plugins: TWikiPlugins);
    procedure SetHeader(const WikiText: String); // for code folding
    function WikiToHtml(const WikiText: String): String; virtual;

    property GiveIdToHeader: Boolean read FGiveIdToHeader write FGiveIdToHeader;
    property Header: THeader read FHeader;
  end;

implementation

{ TWikiConverter }

/// <summary>
/// Add footnote to the end of the document.
/// </summary>
/// <param name="ParentNode">Parent node to add footnote</param>
procedure TWikiConverter.AddFootnote(const ParentNode: TWikiNode);
var
  SectionNode: TWikiNode;
  ListNode: TWikiNode;
begin
  if FFootnote.UseIndex = 0 then
    Exit;

  SectionNode := TWikiNode.Create(TNodeType.Section);
  SectionNode.SetAttribute('class', TFootnote.ClassSection);
  ListNode := TWikiNode.Create(TNodeType.OrderedList);
  SectionNode.AddChild(ListNode);

  // If there is a footnote within a footnote, the UseIndex number increases.
  var
    I: Integer := 0;
  while I < FFootnote.UseIndex do
  begin
    var
      FootnoteItem: TFootnoteItem := FFootnote.Items[I];
    var
      ListItemNode: TWikiNode := TWikiNode.Create(TNodeType.ListItem);

    var
      Captions: TStringList := TStringList.Create;
    try
      Captions.Text := Trim(FootnoteItem.Caption);

      if Captions.Count > 1 then
      begin
        // multi line footnote
        for var J := 0 to Captions.Count - 1 do
        begin
          ListItemNode.AddChild(TWikiNode.Create(TNodeType.Text,
            Captions[J], False));

          if J < (Captions.Count - 1) then
            ListItemNode.AddChild(TWikiNode.Create(TNodeType.Break, '', True));
        end;
      end
      else
      begin
        ListItemNode.AddChild(TWikiNode.Create(TNodeType.Text,
          FootnoteItem.Caption, False));
      end;
    finally
      Captions.Free;
    end;

    ListItemNode.SetAttribute('id', TFootnote.IdPrefix + FootnoteItem.Id);

    for var J := 0 to FootnoteItem.Count - 1 do
    begin
      // add linebreak before back anchor
      ListItemNode.AddChild(TWikiNode.Create(TNodeType.Text, WikiLB, True));

      var
        BackAnchor: TWikiNode := TWikiNode.Create(TNodeType.Anchor);

        // add anchor mark
      BackAnchor.AddChild(TWikiNode.Create(TNodeType.Text,
        TFootnote.AnchorMark, True));
      var
        BackId: String := FootnoteItem.Id;
      if J > 0 then
      begin
        // There are multiple referrers
        BackId := BackId + '-' + (J + 1).ToString;
        // add superscript after anchor mark
        BackAnchor.AddChild(TWikiNode.Create(TNodeType.Superscript,
          BackAnchor.Value + (J + 1).ToString, True));
      end;
      BackAnchor.SetAttribute('class', TFootnote.ClassAnchor);
      BackAnchor.SetAttribute('href', '#' + TFootnote.RefIdPrex + BackId);

      ListItemNode.AddChild(BackAnchor);
    end;
    ProcessInline(ListItemNode);
    ListNode.AddChild(ListItemNode);

    Inc(I);
  end;

  ProcessInline(SectionNode);
  ParentNode.AddChild(SectionNode);
end;

/// <summary>
/// Add header to the list.
/// </summary>
/// <param name="Header">Header item</param>
procedure TWikiConverter.AddHeader(Header: THeaderItem);
begin
  FHeader.Add(Header);
end;

/// <summary>
/// Add id to the header and set the id attribute to the node.
/// </summary>
/// <param name="Node">Node to add id</param>
/// <param name="Index">Line number of the header</param>
procedure TWikiConverter.AddIdToHeader(const Node: TWikiNode;
  const Index: Integer);
var
  HeaderItem: THeaderItem;
begin
  HeaderItem := THeaderItem.Create;
  HeaderItem.Caption := Node.Text;
  HeaderItem.Level := GetHeaderLevel(Node.NodeType);
  HeaderItem.Line := Index;
  AddHeader(HeaderItem);
  Node.SetAttribute('id', HeaderItem.Id);
end;

/// <summary>
/// Add line to the previous line.
/// </summary>
/// <param name="Prev">Previous line</param>
/// <param name="Line">Current line</param>
/// <returns>Combined line</returns>
function TWikiConverter.AddLine(const Prev, Line: String): String;
var
  Added: String;
begin
  if Prev <> '' then
  begin
    Added := Prev + WikiLB;
  end;
  Added := Added + Line;

  Result := Added;
end;

constructor TWikiConverter.Create;
begin
  FHeader := THeader.Create;
end;

destructor TWikiConverter.Destroy;
begin
  FreeAndNil(FRoot);
  FreeAndNil(FHeader);

  inherited;
end;

function TWikiConverter.ExistsLineBreakAtEndOfLine(const Line: String): Boolean;
begin
  Result := (Line <> '') and CharInSet(Line[Length(Line)], [#10, #13]);
end;

/// <summary>
/// Get character at the specified position.
/// </summary>
/// <param name="Text">Text</param>
/// <param name="Pos">Position</param>
/// <returns>Character</returns>
/// <remarks>Return #0 if the position is out of range.</remarks>
function TWikiConverter.GetAt(const Text: String; const Pos: Integer): Char;
begin
  if (Pos > Length(Text)) or (Pos <= 0) then
    Exit(#0);

  Result := Text[Pos];
end;

/// <summary>
/// Get the header level from the node type.
/// </summary>
/// <param name="NodeType">Node type</param>
/// <returns>Header level</returns>
/// <remarks>Return 0 if the node type is not a header.</remarks>
function TWikiConverter.GetHeaderLevel(const NodeType: TNodeType): Integer;
begin
  case NodeType of
    TNodeType.Heading1:
      Result := 1;
    TNodeType.Heading2:
      Result := 2;
    TNodeType.Heading3:
      Result := 3;
    TNodeType.Heading4:
      Result := 4;
    TNodeType.Heading5:
      Result := 5;
    TNodeType.Heading6:
      Result := 6;
  else
    Result := 0;
  end;
end;

/// <summary>
/// Get HTML from the node.
/// </summary>
/// <param name="WikiNode">Node</param>
/// <returns>HTML</returns>
function TWikiConverter.GetHtml(const WikiNode: TWikiNode): String;
var
  Html: String;
  BeginTag: String;
  BeforeEnter: Boolean;
  BeginTagEnter: Boolean;
  EndTag: String;
begin
  BeforeEnter := False;
  BeginTagEnter := False;

  case WikiNode.NodeType of
    TNodeType.Raw:
      begin
        BeginTag := '';
        EndTag := '';
      end;
    TNodeType.Anchor:
      begin
        BeginTag := '<a';
        EndTag := '</a>';
      end;
    TNodeType.Blockquote:
      begin
        BeforeEnter := IsInListItem(WikiNode);
        BeginTag := '<blockquote';
        BeginTagEnter := True;
        EndTag := '</blockquote>' + WikiLB;
      end;
    TNodeType.Break:
      begin
        BeginTag := '';
        EndTag := '<br />' + WikiLB;
      end;
    TNodeType.Code:
      begin
        BeginTag := '<code';
        EndTag := '</code>';
      end;
    TNodeType.Delete:
      begin
        BeginTag := '<del';
        EndTag := '</del>';
      end;
    TNodeType.DescriptionDetail:
      begin
        if (WikiNode.ChildNodes = nil) and (WikiNode.Value = '') then
          Exit(''); // for Pukiwiki

        BeginTag := '<dd';
        EndTag := '</dd>' + WikiLB;
      end;
    TNodeType.DescriptionList:
      begin
        BeforeEnter := IsInListItem(WikiNode);
        BeginTag := '<dl';
        BeginTagEnter := True;
        EndTag := '</dl>' + WikiLB;
      end;
    TNodeType.DescriptionTerm:
      begin
        BeginTag := '<dt';
        EndTag := '</dt>' + WikiLB;
      end;
    TNodeType.Division:
      begin
        if (WikiNode.ChildNodes <> nil) and
          (WikiNode.ChildNodes[0].NodeType = TNodeType.Text) then
          BeginTagEnter := False
        else
          BeginTagEnter := True;
        BeginTag := '<div';
        EndTag := '</div>' + WikiLB;
      end;
    TNodeType.Emphasis:
      begin
        BeginTag := '<em';
        EndTag := '</em>';
      end;
    TNodeType.Heading1:
      begin
        BeginTag := '<h1';
        EndTag := '</h1>' + WikiLB;
      end;
    TNodeType.Heading2:
      begin
        BeginTag := '<h2';
        EndTag := '</h2>' + WikiLB;
      end;
    TNodeType.Heading3:
      begin
        BeginTag := '<h3';
        EndTag := '</h3>' + WikiLB;
      end;
    TNodeType.Heading4:
      begin
        BeginTag := '<h4';
        EndTag := '</h4>' + WikiLB;
      end;
    TNodeType.Heading5:
      begin
        BeginTag := '<h5';
        EndTag := '</h5>' + WikiLB;
      end;
    TNodeType.Heading6:
      begin
        BeginTag := '<h6';
        EndTag := '</h6>' + WikiLB;
      end;
    TNodeType.HorizontalRule:
      begin
        BeginTag := '';
        EndTag := '<hr />' + WikiLB;
      end;
    TNodeType.Image:
      begin
        BeginTag := '<img';
      end;
    TNodeType.Input:
      begin
        BeginTag := '<input';
        EndTag := '';
      end;
    TNodeType.Insert:
      begin
        BeginTag := '<ins';
        EndTag := '</ins>';
      end;
    TNodeType.ListItem:
      begin
        BeginTag := '<li';
        BeginTagEnter := WikiNode.ParentNode.IsLooseList and
          (WikiNode.ChildNodes <> nil); // for Markdown
        EndTag := '</li>' + WikiLB;
      end;
    TNodeType.OrderedList:
      begin
        BeforeEnter := IsInListItem(WikiNode);
        BeginTag := '<ol';
        BeginTagEnter := True;
        EndTag := '</ol>' + WikiLB;
      end;
    TNodeType.Paragraph:
      begin
        BeginTag := '<p';
        EndTag := '</p>' + WikiLB;
      end;
    TNodeType.PreformattedText:
      begin
        if (WikiNode.ChildNodes = nil) and (WikiNode.Value = '') then
          Exit(''); // form Pukiwiki

        BeforeEnter := IsInListItem(WikiNode);
        BeginTag := '<pre';
        EndTag := '</pre>' + WikiLB;
      end;
    TNodeType.Section:
      begin
        BeginTag := '<section';
        BeginTagEnter := True;
        EndTag := '</section>' + WikiLB;
      end;
    TNodeType.Strong:
      begin
        BeginTag := '<strong';
        EndTag := '</strong>';
      end;
    TNodeType.Superscript:
      begin
        BeginTag := '<sup';
        EndTag := '</sup>';
      end;
    TNodeType.Table:
      begin
        BeforeEnter := IsInListItem(WikiNode);
        BeginTag := '<table';
        BeginTagEnter := True;
        EndTag := '</table>' + WikiLB;
      end;
    TNodeType.TableBody:
      begin
        if WikiNode.ChildNodes = nil then
          Exit('');

        BeginTag := '<tbody';
        BeginTagEnter := True;
        EndTag := '</tbody>' + WikiLB;
      end;
    TNodeType.TableDataCell:
      begin
        BeginTag := '<td';
        EndTag := '</td>' + WikiLB;
      end;
    TNodeType.TableFooter:
      begin
        if WikiNode.ChildNodes = nil then
          Exit('');

        BeginTag := '<tfoot';
        BeginTagEnter := True;
        EndTag := '</tfoot>' + WikiLB;
      end;
    TNodeType.TableHeader:
      begin
        if WikiNode.ChildNodes = nil then
          Exit('');

        BeginTag := '<thead';
        BeginTagEnter := True;
        EndTag := '</thead>' + WikiLB;
      end;
    TNodeType.TableHeaderCell:
      begin
        BeginTag := '<th';
        EndTag := '</th>' + WikiLB;
      end;
    TNodeType.TableRow:
      begin
        BeginTag := '<tr';
        BeginTagEnter := True;
        EndTag := '</tr>' + WikiLB;
      end;
    TNodeType.Text:
      begin
        BeginTag := '';
        EndTag := '';
      end;
    TNodeType.UnorderedList:
      begin
        BeforeEnter := IsInListItem(WikiNode);
        BeginTag := '<ul';
        BeginTagEnter := True;
        EndTag := '</ul>' + WikiLB;
      end;
  else
    raise Exception.CreateFmt('Invalid node type. [%d]',
      [Ord(WikiNode.NodeType)]);
  end;

  if BeforeEnter then
  begin
    Html := WikiLB;
  end;

  Html := Html + BeginTag;

  if BeginTag <> '' then
  begin
    // output attributes
    var
      Encoded: String;
    var
      Comparekey: String;
    for var Key in WikiNode.Attributes.Keys do
    begin
      Comparekey := AnsiLowerCase(Key);
      if (Comparekey = 'href') or (Comparekey = 'src') then
        Encoded := UriEncode(WikiNode.Attributes[Key])
      else
        Encoded := StringReplace(WikiNode.Attributes[Key], '"', '&quot;',
          [rfReplaceAll]);

      Html := Html + Format(' %s="%s"', [Key, Encoded]);
    end;

    if WikiNode.NodeType = TNodeType.Image then
      Html := Html + ' />'
    else
      Html := Html + '>';

    if BeginTagEnter then
    begin
      Html := Html + WikiLB;
    end;
  end;

  if WikiNode.ChildNodes <> nil then
  begin
    for var Child in WikiNode.ChildNodes do
    begin
      var
        ChildHtml: String := GetHtml(Child);

      if (ChildHtml <> '') and (ChildHtml[1] = #13) and (Html <> '') and
        (Html[Length(Html)] = #10) then
      begin
        // Do not add line breaks if they are consecutive
        Html := Copy(Html, 1, Length(Html) - 2);
      end;
      Html := Html + GetHtml(Child);
    end;
  end;

  Html := Html + WikiNode.Value;
  Html := Html + EndTag;

  Result := Html;
end;

/// <summary>
/// Get the same character length from the specified position.
/// </summary>
/// <param name="Text">Text</param>
/// <param name="Ch">Character</param>
/// <param name="Pos">Position</param>
/// <returns>Length</returns>
/// <remarks>Return 0 if the character is not found.</remarks>
function TWikiConverter.GetSameCharLength(const Text: String; const Ch: Char;
  Pos: Integer): Integer;
var
  Count: Integer;
  Len: Integer;
begin
  Count := 0;
  Len := Length(Text);
  while Pos <= Len do
  begin
    if Ch <> GetAt(Text, Pos) then
      Break;

    Inc(Count);
    Inc(Pos);
  end;

  Result := Count;
end;

/// <summary>
/// Check if the node is in the list item.
/// </summary>
/// <param name="WikiNode">Node</param>
/// <returns>True if the node is in the list item</returns>
function TWikiConverter.IsInListItem(const WikiNode: TWikiNode): Boolean;
begin
  Result := (WikiNode.ParentNode <> nil) and
    (WikiNode.ParentNode.NodeType in [TNodeType.ListItem,
    TNodeType.DescriptionDetail]);
end;

/// <summary>
/// Processing inline elements.
/// </summary>
/// <param name="Node">Node</param>
procedure TWikiConverter.ProcessInline(const Node: TWikiNode);
begin
  // do nothing
end;

/// <summary>
/// Set header information.
/// </summary>
/// <param name="WikiText">Wiki text</param>
/// <remarks>Use when only header information is needed.</remarks>
procedure TWikiConverter.SetHeader(const WikiText: String);
begin
  ParseHeader(WikiText);
end;

/// <summary>
/// Set the list of pages.
/// </summary>
/// <param name="PageList">List of pages</param>
procedure TWikiConverter.SetPageList(const PageList: TWikiPages);
begin
  FPageList := PageList;
end;

/// <summary>
/// Set the list of plugins.
/// </summary>
/// <param name="Plugins">List of plugins</param>
procedure TWikiConverter.SetPlugins(const Plugins: TWikiPlugins);
begin
  FWikiPlugins := Plugins;
end;

/// <summary>
/// Trim right line breaks.
/// </summary>
/// <param name="Line">Text</param>
/// <returns>Trimmed string</returns>
function TWikiConverter.TrimRightLineBreak(const Line: String): String;
var
  EPos: Integer;
begin
  EPos := Length(Line);
  while EPos > 0 do
  begin
    if not CharInSet(Line[EPos], [#10, #13]) then
      Break;
    Dec(EPos);
  end;

  Result := LeftStr(Line, EPos);
end;

/// <summary>
/// Encode to URI encoded format.
/// </summary>
/// <param name="Uri">URI text</param>
/// <returns>URI encoded string</returns>
function TWikiConverter.UriEncode(const Uri: String): String;
var
  Utf8: TBytes;
  Num: UInt8;
  Item: String;
  Ch: AnsiChar;
begin
  Utf8 := TEncoding.Utf8.GetBytes(Uri);
  Result := '';

  for var C in Utf8 do
  begin
    Num := Ord(C);
    Ch := AnsiChar(C);

    if (Num <= 32) or (Num >= 128) then
      Item := '%' + Num.ToHexString
    else if Ch in ['0' .. '9', 'A' .. 'Z', 'a' .. 'z'] then
      Item := String(Ch)
    else if not(Ch in [';', ',', '/', '?', ':', '@', '&', '=', '+', '$', '-',
      '_', '.', '!', '~', '*', '''', '(', ')', '#', '%']) then
      Item := '%' + Num.ToHexString
    else if Ch = '&' then
      Item := '&amp;'
    else
      Item := String(Ch);

    Result := Result + Item;
  end;
end;

/// <summary>
/// Convert wiki text to HTML.
/// </summary>
/// <param name="WikiText">Wiki text</param>
/// <returns>HTML</returns>
function TWikiConverter.WikiToHtml(const WikiText: String): String;
begin
  FreeAndNil(FRoot);
  Parse(WikiText);
  Result := GetHtml(FRoot);
end;

{ TLinkDefinition }

procedure TLinkDefinition.AssignTo(Dest: TPersistent);
begin
  if Dest is TLinkDefinition then
  begin
    TLinkDefinition(Dest).FDestination := FDestination;
    TLinkDefinition(Dest).FEndPosition := FEndPosition;
    TLinkDefinition(Dest).FId := FId;
    TLinkDefinition(Dest).FName := FName;
    TLinkDefinition(Dest).FTitle := FTitle;
  end
  else
    inherited AssignTo(Dest);

end;

end.
