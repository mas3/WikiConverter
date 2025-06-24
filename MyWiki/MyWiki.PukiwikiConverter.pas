unit MyWiki.PukiwikiConverter;

interface

uses
  System.Classes, System.NetEncoding, System.RegularExpressions,
  System.SysUtils, System.Math, System.StrUtils, System.Generics.Collections,
  MyWiki.WikiConverter, MyWiki.WikiNode, MyWiki.HtmlEntities, MyWiki.Footnote;

type
  TNodeTypeSet = set of TNodeType;
  TRowSpans = array of array of Integer;
  TTableCells = array of array of TWikiNode;

  TPukiwikiConverter = class(TWikiConverter)
  private
    FHtmlEntities: TDictionary<String, String>;

    procedure AddBeforeTextNode(const Node: TWikiNode; const Text: String);
    procedure AddTable(var CurrentNode: TWikiNode;
      const TableLines: TStringList);
    function Encode(const Text: String): String;
    function GetBlockParentNode(const CurrentNode: TWikiNode): TWikiNode;
    function GetArgumentString(const Line: String): String;
    function GetTargetNode(const CurrentNode: TWikiNode;
      const LeveledTargetType: TNodeTypeSet; const MaxLevel: Integer;
      const AnyLevelTargetType: TNodeTypeSet): TWikiNode;
    function ProcessAutoLink(const CurrentNode: TWikiNode; const Text: String;
      const Offset: Integer; const Copied: Integer): Integer;
    procedure ProcessBlockQuote(var CurrentNode: TWikiNode; var Index: Integer;
      const Line: String);
    procedure ProcessBreakBlockQuote(var CurrentNode: TWikiNode;
      var Index: Integer; const Line: String);
    procedure ProcessCsvTable(var CurrentNode: TWikiNode; var Index: Integer;
      const Lines: TStringList);
    function ProcessDescriptionList(var CurrentNode: TWikiNode;
      var Index: Integer; const Line: String): Boolean;
    function ProcessEmphasis(const CurrentNode: TWikiNode; const Text: String;
      const Offset: Integer; const Copied: Integer; const Mark: Char): Integer;
    function ProcessEntity(const CurrentNode: TWikiNode; const Text: String;
      const Offset: Integer; const Copied: Integer): Integer;
    function ProcessFootnote(const CurrentNode: TWikiNode; const Text: String;
      const Offset: Integer; const Copied: Integer): Integer;
    function ProcessInlinePlugin(const CurrentNode: TWikiNode;
      const Text: String; const Offset: Integer; const Copied: Integer)
      : Integer;
    procedure ProcessHeader(var CurrentNode: TWikiNode; var Index: Integer;
      const Line: String);
    procedure ProcessList(var CurrentNode: TWikiNode; var Index: Integer;
      const Line: String);
    procedure ProcessParagraph(var CurrentNode: TWikiNode; var Index: Integer;
      const Line: String);
    procedure ProcessPreformattedText(var CurrentNode: TWikiNode;
      var Index: Integer; const Line: String);
    function ProcessWikiLink(const CurrentNode: TWikiNode; const Text: String;
      const Offset: Integer; const Copied: Integer): Integer;
    function ToHtml(const Text: String): String;
  protected
    procedure Parse(const WikiText: String); override;
    procedure ParseHeader(const WikiText: String); override;
    procedure ProcessInline(const Node: TWikiNode); override;
  public
    procedure ParseBlockNode(const ParentNode: TWikiNode);

    constructor Create; override;
    destructor Destroy; override;
  end;

  TTableCellDecoration = class(TPersistent)
  private
    FAlign: String;
    FBackgroundColor: String;
    FCellText: String;
    FColor: String;
    FFontSize: String;
    FFontWeight: String;
    FText: String;

    function GetStyle: String;
    procedure ReflectStyle(const Text: String);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure SetCellText(const Text: String);
    procedure SetCellTextToOverride(const Text: String);

    property Style: String read GetStyle;
    property Text: String read FText;
  end;

implementation

var
  RegexBlockPlugin: TRegEx;
  RegexBlockPluginMultiLineBegin: TRegEx;
  RegexCellStyle: TRegEx;
  RegexDecimalEntityReference: TRegEx;
  RegexHexadecimalEntityReference: TRegEx;
  RegexImageFile: TRegEx;
  RegexTable: TRegEx;
  RegexTextAlign: TRegEx;
  RegexTrimRightEachLine: TRegEx;
  RegexUri: TRegEx;

  { TPukiwikiConverter }

procedure TPukiwikiConverter.AddBeforeTextNode(const Node: TWikiNode;
  const Text: String);
var
  ChildNode: TWikiNode;
begin
  if Text = '' then
    Exit;

  ChildNode := TWikiNode.Create(TNodeType.Text, Encode(Text), True);
  Node.AddChild(ChildNode);
end;

procedure TPukiwikiConverter.AddTable(var CurrentNode: TWikiNode;
  const TableLines: TStringList);
var
  RowNum, ColNum: Integer;
  TargetNode: TWikiNode;
  TableNode, HeaderNode, FooterNode, BodyNode: TWikiNode;
  TableCells: TTableCells;
  RowSpans: TRowSpans;
  DefaultDecorations: TObjectList<TTableCellDecoration>;
  ExistsRow: Boolean;
begin
  TableNode := TWikiNode.Create(TNodeType.Table);
  HeaderNode := TWikiNode.Create(TNodeType.TableHeader);
  FooterNode := TWikiNode.Create(TNodeType.TableFooter);
  BodyNode := TWikiNode.Create(TNodeType.TableBody);

  TableNode.AddChild(HeaderNode);
  TableNode.AddChild(FooterNode);
  TableNode.AddChild(BodyNode);

  ExistsRow := False;
  RowNum := TableLines.Count;
  ColNum := TableLines[0].CountChar('|') - 1;

  SetLength(TableCells, RowNum, ColNum);
  SetLength(RowSpans, RowNum, ColNum);

  // set cell data
  for var Row := 0 to RowNum - 1 do
  begin
    var
      Line: String := TableLines[Row];
    var
      BPos: Integer := 2;
    var
      ColSpan: Integer := 0;
    for var Col := 0 to ColNum - 1 do
    begin
      var
        EPos: Integer := Pos('|', Line, BPos);
      var
        Cell: TWikiNode := TWikiNode.Create(TNodeType.TableDataCell);
      var
        Text: String := Copy(Line, BPos, EPos - BPos);
      BPos := EPos + 1;

      if Text = '>' then
      begin
        Inc(ColSpan);
        Continue;
      end;

      if Text = '~' then
      begin
        RowSpans[Row, Col] := 1;
        Continue;
      end;

      if GetAt(Text, 1) = '~' then
      begin
        Cell.NodeType := TNodeType.TableHeaderCell;
        Text := Copy(Text, 2, Length(Text) - 1);
      end;

      Cell.Text := Text;

      if ColSpan > 0 then
      begin
        Cell.SetAttribute('colspan', (ColSpan + 1).ToString);
        ColSpan := 0;
      end;

      TableCells[Row, Col] := Cell;
    end;
  end;

  // calc rowspan
  for var Col := 0 to ColNum - 1 do
  begin
    var
      RowSpan: Integer := 0;
    for var Row := RowNum - 1 downto 0 do
    begin
      if RowSpans[Row, Col] > 0 then
      begin
        Inc(RowSpan);
        RowSpans[Row, Col] := -1;
        Continue;
      end;

      if RowSpan > 0 then
      begin
        RowSpans[Row, Col] := RowSpan + 1;
        RowSpan := 0;
      end;
    end;
  end;

  var
    Deco: TTableCellDecoration := TTableCellDecoration.Create;
  try
    DefaultDecorations := TObjectList<TTableCellDecoration>.Create;
    try
      for var Col := 0 to ColNum - 1 do
      begin
        DefaultDecorations.Add(TTableCellDecoration.Create);
      end;

      var
        LastAddTo: TWikiNode := BodyNode;
      for var Row := 0 to RowNum - 1 do
      begin
        var
          RowNode: TWikiNode := TWikiNode.Create(TNodeType.TableRow);
        var
          ExistsRowSkip: Boolean := False;
        var
          Line: String := TableLines[Row];

        if CharInSet(Line[Length(Line)], ['c', 'C']) then
        begin
          // format indication row
          for var Col := 0 to ColNum - 1 do
          begin
            var
              Cell: TWikiNode := TableCells[Row, Col];
            if Cell = nil then
              DefaultDecorations.Items[Col].SetCellText('')
            else
              DefaultDecorations.Items[Col].SetCellText(Cell.Text);

            // do not use data
            FreeAndNil(Cell);
          end;
          Continue;
        end;

        for var Col := 0 to ColNum - 1 do
        begin
          var
            Cell: TWikiNode := TableCells[Row, Col];
          var
            RowSpan: Integer := RowSpans[Row, Col];

          if RowSpan = -1 then
            ExistsRowSkip := True;

          if Cell = nil then
            Continue;

          Deco.Assign(DefaultDecorations.Items[Col]);
          Deco.SetCellTextToOverride(Cell.Text);

          Cell.Text := Trim(Deco.Text);
          var
            Style: String := Deco.Style;
          if Style <> '' then
            Cell.SetAttribute('style', Style);
          if RowSpan > 0 then
            Cell.SetAttribute('rowspan', RowSpan.ToString);

          RowNode.AddChild(Cell);
        end;

        ExistsRow := True;
        if ExistsRowSkip then
        begin
          LastAddTo.AddChild(RowNode);
          Continue;
        end;

        case Line[Length(Line)] of
          'f', 'F':
            LastAddTo := FooterNode;
          'h', 'H':
            LastAddTo := HeaderNode;
        else
          LastAddTo := BodyNode;
        end;

        LastAddTo.AddChild(RowNode);
      end;
    finally
      DefaultDecorations.Free;
    end;
  finally
    Deco.Free;
  end;

  if ExistsRow then
  begin
    // add table
    TargetNode := GetTargetNode(CurrentNode, [], 0,
      [TNodeType.BlockQuote, TNodeType.DescriptionDetail, TNodeType.ListItem]);
    TargetNode.AddChild(TableNode);
  end
  else
    TableNode.Free;
end;

constructor TPukiwikiConverter.Create;
begin
  inherited;

  // 2200 is about html entities count
  // ref. https://html.spec.whatwg.org/entities.json
  FHtmlEntities := TDictionary<String, String>.Create(2200);
  SetEntities(FHtmlEntities);

  // PukiWiki original
  FHtmlEntities.Add('&heart;', #$2764);
  FHtmlEntities['&smile;'] := #$1F60A;
  FHtmlEntities.Add('&bigsmile;', #$1F604);
  FHtmlEntities.Add('&huh;', #$1F914);
  FHtmlEntities.Add('&oh;', #$1F62E);
  FHtmlEntities.Add('&wink;', #$1F609);
  FHtmlEntities.Add('&sad;', #$1F622);
  FHtmlEntities.Add('&worried;', #$1F61F);
end;

destructor TPukiwikiConverter.Destroy;
begin
  FHtmlEntities.Free;

  inherited;
end;

function TPukiwikiConverter.Encode(const Text: String): String;
begin
  Result := ReplaceStr(Text, '&', '&amp;');
  Result := ReplaceStr(Result, '<', '&lt;');
  Result := ReplaceStr(Result, '>', '&gt;');
end;

function TPukiwikiConverter.GetArgumentString(const Line: String): String;
var
  BPos, EPos: Integer;
begin
  BPos := Pos('(', Line);
  if BPos = 0 then
    Exit('');

  EPos := Pos(')', Line, BPos + 1);
  if EPos = 0 then
    Exit('');

  Result := Copy(Line, BPos + 1, EPos - BPos - 1);
end;

function TPukiwikiConverter.GetBlockParentNode(const CurrentNode: TWikiNode)
  : TWikiNode;
begin
  Result := GetTargetNode(CurrentNode, [], 0,
    [TNodeType.BlockQuote, TNodeType.DescriptionDetail, TNodeType.ListItem]);
end;

function TPukiwikiConverter.GetTargetNode(const CurrentNode: TWikiNode;
  const LeveledTargetType: TNodeTypeSet; const MaxLevel: Integer;
  const AnyLevelTargetType: TNodeTypeSet): TWikiNode;
var
  Node: TWikiNode;
begin
  Node := CurrentNode;
  while Node.ParentNode <> nil do
  begin
    if (Node.NodeType in LeveledTargetType) and (Node.NestLevel <= MaxLevel)
    then
      Break;

    if Node.NodeType in AnyLevelTargetType then
      Break;

    Node := Node.ParentNode;
  end;

  Result := Node;
end;

procedure TPukiwikiConverter.Parse(const WikiText: String);
begin
  FRoot := TWikiNode.Create(TNodeType.Text, WikiText, False);
  FHeader.Clear;

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

procedure TPukiwikiConverter.ParseBlockNode(const ParentNode: TWikiNode);
var
  Line: String;
  Lines: TStringList;
  LineCount: Integer;
  Index: Integer;
  Node: TWikiNode;
  CurrentNode: TWikiNode;
  FirstChar: Char;
  Ret: TMatch;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := ParentNode.Text;
    ParentNode.Text := '';
    LineCount := Lines.Count;
    Index := 0;
    CurrentNode := ParentNode;

    while Index < LineCount do
    begin
      Line := Lines[Index];

      if Line = '' then
      begin
        CurrentNode := ParentNode;
        Inc(Index);
        Continue;
      end;

      // comment
      if LeftStr(Line, 2) = '//' then
      begin
        Inc(Index);
        Continue;
      end;

      FirstChar := Line[1];

      // block quote
      if FirstChar = '>' then
      begin
        ProcessBlockQuote(CurrentNode, Index, Line);
        Continue;
      end;

      // break block quote
      // Unlike the original, it is only works within block quotes
      if (FirstChar = '<') and
        (GetTargetNode(CurrentNode, [TNodeType.BlockQuote], 1, []) <> ParentNode)
      then
      begin
        ProcessBreakBlockQuote(CurrentNode, Index, Line);
        Continue;
      end;

      // paragraph
      // (only '~' is end of line newline)
      if (FirstChar = '~') and (Length(Line) > 1) then
      begin
        ProcessParagraph(CurrentNode, Index, Line);
        Continue;
      end;

      // horizontal rule
      if Copy(Line, 1, 4) = '----' then
      begin
        Node := TWikiNode.Create(TNodeType.HorizontalRule);

        ParentNode.AddChild(Node);
        CurrentNode := ParentNode;
        Inc(Index);
        Continue;
      end;

      // unorderd or orderd list
      if CharInSet(FirstChar, ['-', '+']) then
      begin
        ProcessList(CurrentNode, Index, Line);
        Continue;
      end;

      // description list
      if (FirstChar = ':') then
      begin
        if ProcessDescriptionList(CurrentNode, Index, Line) then
          Continue;
      end;

      // preformatted text
      if (FirstChar = ' ') or (FirstChar = #9) then
      begin
        ProcessPreformattedText(CurrentNode, Index, Line);
        Continue;
      end;

      // header
      if FirstChar = '*' then
      begin
        ProcessHeader(CurrentNode, Index, Line);
        Continue;
      end;

      // CSV table
      if FirstChar = ',' then
      begin
        ProcessCsvTable(CurrentNode, Index, Lines);
        Continue;
      end;

      // table
      if RegexTable.IsMatch(Line) then
      begin
        var
          RowCount: Integer := Line.CountChar('|');
        var
          TableLines: TStringList := TStringList.Create;

        try
          while Index < LineCount do
          begin
            Line := Lines[Index];

            // comment
            if LeftStr(Line, 2) = '//' then
            begin
              Inc(Index);
              Continue;
            end;

            if RowCount <> Line.CountChar('|') then
              Break;

            if not RegexTable.IsMatch(Line) then
              Break;

            TableLines.Add(Line);
            Inc(Index);
          end;

          AddTable(CurrentNode, TableLines);
        finally
          TableLines.Free;
        end;
        Continue;
      end;

      // text align
      Ret := RegexTextAlign.Match(Line);
      if Ret.Success then
      begin
        var
          Align: String := Ret.Groups[1].Value;
        var
          Text: String := TrimLeft(Ret.Groups[2].Value);

        if Text <> '' then
        begin
          CurrentNode := GetBlockParentNode(CurrentNode);
          Node := TWikiNode.Create(TNodeType.Division, Text, False);
          Node.SetAttribute('style', Format('text-align:%s',
            [LowerCase(Align)]));
          CurrentNode.AddChild(Node);
          CurrentNode := Node;
        end;

        Inc(Index);
        Continue;
      end;

      // block plugin
      Ret := RegexBlockPlugin.Match(Line);
      if (FWikiPlugins <> nil) and Ret.Success then
      begin
        var
          PluginName: String := LowerCase(Ret.Groups[1].Value);
        var
          RestString: String := '';

        if Ret.Groups.Count > 2 then
        begin
          RestString := TrimRight(Ret.Groups[2].Value);
        end;

        if FWikiPlugins.ContainsKey(PluginName) then
        begin
          var
            ValidFormat: Boolean := RestString = '';
          var
            MultiLine: String := '';

          if not ValidFormat then
          begin
            Ret := RegexBlockPluginMultiLineBegin.Match(Line);
            if Ret.Success then
            begin
              var
                LocalIndex: Integer := Index + 1;
              var
                LocalLine: String;
              while LocalIndex < LineCount do
              begin
                LocalLine := Lines[LocalIndex];
                if Pos('}}', LocalLine) = 1 then
                begin
                  ValidFormat := True;
                  Index := LocalIndex;
                  Break;
                end;
                MultiLine := AddLine(MultiLine, LocalLine);
                Inc(LocalIndex);
              end;
            end
            else
            begin
              ValidFormat := True;
            end;
          end;

          if ValidFormat then
          begin
            var
              Argument: String := GetArgumentString(RestString);
            CurrentNode := GetBlockParentNode(CurrentNode);
            Node := TWikiNode.Create(TNodeType.Raw);
            if FWikiPlugins[PluginName].ParseMultiline then
              MultiLine := ToHtml(MultiLine);

            Node.Text := FWikiPlugins[PluginName].ExecuteMultiline(Argument,
              MultiLine) + WikiLB;
            CurrentNode.AddChild(Node);
            CurrentNode := Node;

            Inc(Index);
            Continue;
          end;
        end;
      end;

      // paragraph
      if CurrentNode.NodeType in [TNodeType.PreformattedText, TNodeType.Table,
        TNodeType.Raw] then
      begin
        CurrentNode := GetBlockParentNode(CurrentNode);
      end;

      if (CurrentNode = ParentNode) or
        (CurrentNode.NodeType = TNodeType.BlockQuote) then
      begin
        Node := TWikiNode.Create(TNodeType.Paragraph);
        CurrentNode.AddChild(Node);
        CurrentNode := Node;
      end;

      CurrentNode.Text := AddLine(CurrentNode.Text, TrimLeft(Line));
      Inc(Index);
    end;
  finally
    Lines.Free;
  end;
end;

procedure TPukiwikiConverter.ParseHeader(const WikiText: String);
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

function TPukiwikiConverter.ProcessAutoLink(const CurrentNode: TWikiNode;
  const Text: String; const Offset, Copied: Integer): Integer;
var
  Str: String;
  Ret: TMatch;
begin
  Str := RightStr(Text, Length(Text) - Offset + 1);

  Ret := RegexUri.Match(Str);
  if not Ret.Success then
    Exit(0);

  var
    Uri: String := Ret.Value;

  AddBeforeTextNode(CurrentNode, Copy(Text, Copied + 1, Offset - Copied - 1));
  var
    Node: TWikiNode := TWikiNode.Create(TNodeType.Anchor);
  Node.SetAttribute('href', Uri);

  if RegexImageFile.IsMatch(Uri) then
  begin
    var
      Image: TWikiNode := TWikiNode.Create(TNodeType.Image);
    Image.SetAttribute('alt', Uri);
    Image.SetAttribute('src', Uri);
    Image.Processed := True;
    Node.AddChild(Image);
  end
  else
  begin
    Node.Value := Encode(Uri);
  end;
  Node.Processed := True;
  CurrentNode.AddChild(Node);

  Exit(Offset + Length(Uri) - 1);
end;

procedure TPukiwikiConverter.ProcessBlockQuote(var CurrentNode: TWikiNode;
  var Index: Integer; const Line: String);
var
  Level: Integer;
  Node: TWikiNode;
begin
  Level := Min(GetSameCharLength(Line, '>', 1), 3);

  Node := nil;
  CurrentNode := GetTargetNode(CurrentNode, [TNodeType.BlockQuote], Level,
    [TNodeType.ListItem, TNodeType.DescriptionDetail]);
  case CurrentNode.NodeType of
    TNodeType.BlockQuote:
      begin
        if CurrentNode.NestLevel < Level then
        begin
          Node := TWikiNode.Create(TNodeType.BlockQuote);
        end;
      end;
    TNodeType.DescriptionDetail:
      begin
        Node := TWikiNode.Create(TNodeType.BlockQuote);
      end
  else
    begin
      Node := TWikiNode.Create(TNodeType.BlockQuote);
    end;
  end;

  if Node <> nil then
  begin
    Node.NestLevel := Level;
    CurrentNode.AddChild(Node);
    CurrentNode := Node;
  end;

  Node := TWikiNode.Create(TNodeType.Paragraph);
  var
    Text: String := TrimLeft(Copy(Line, Level + 1, Length(Line) - Level));

  if GetAt(Text, 1) = '~' then
  begin
    Text := TrimLeft(Copy(Text, 2, Length(Text) - 1));
  end;

  Node.Text := Text;

  CurrentNode.AddChild(Node);
  CurrentNode := Node;
  Inc(Index);
end;

procedure TPukiwikiConverter.ProcessBreakBlockQuote(var CurrentNode: TWikiNode;
  var Index: Integer; const Line: String);
var
  Level: Integer;
  Node: TWikiNode;
  Text: String;
begin
  Level := Min(GetSameCharLength(Line, '<', 1), 3);
  CurrentNode := GetTargetNode(CurrentNode, [TNodeType.BlockQuote], Level, [])
    .ParentNode;

  Text := TrimLeft(Copy(Line, Level + 1, Length(Line) - Level));
  if Text <> '' then
  begin
    Node := TWikiNode.Create(TNodeType.Paragraph);

    if CurrentNode.NodeType = TNodeType.ListItem then
      Node.NodeType := TNodeType.Text;

    if GetAt(Text, 1) = '~' then
    begin
      Node.NodeType := TNodeType.Paragraph;
      Text := TrimLeft(Copy(Text, 2, Length(Text) - 1));
    end;

    Node.Text := Text;
    CurrentNode.AddChild(Node);
    CurrentNode := Node;
  end;
  Inc(Index);
end;

procedure TPukiwikiConverter.ProcessCsvTable(var CurrentNode: TWikiNode;
  var Index: Integer; const Lines: TStringList);
var
  Line: String;
  LineCount: Integer;
  ParentNode: TWikiNode;
  ColNum: Integer;
  TableNode, BodyNode, RowNode, CellNode: TWikiNode;
  Csv: TStringList;
  ColSpan: Integer;
begin
  CurrentNode := GetBlockParentNode(CurrentNode);
  ParentNode := CurrentNode;
  BodyNode := nil;
  ColNum := 0;
  LineCount := Lines.Count;
  Csv := TStringList.Create;
  try
    while Index < LineCount do
    begin
      Line := Lines[Index];

      // comment
      if LeftStr(Line, 2) = '//' then
      begin
        Inc(Index);
        Continue;
      end;

      if GetAt(Line, 1) <> ',' then
        Exit;

      Csv.StrictDelimiter := True;
      Csv.DelimitedText := Copy(Line, 2, Length(Line) - 1);

      if ColNum <> Csv.Count then
      begin
        // begin new table
        TableNode := TWikiNode.Create(TNodeType.Table);
        BodyNode := TWikiNode.Create(TNodeType.TableBody);
        TableNode.AddChild(BodyNode);
        CurrentNode := TableNode;

        ParentNode.AddChild(TableNode);
        ColNum := Csv.Count;
      end;

      // add row
      RowNode := TWikiNode.Create(TNodeType.TableRow);
      BodyNode.AddChild(RowNode);

      // add data
      ColSpan := 1;
      CellNode := nil;
      for var Col := 0 to ColNum - 1 do
      begin
        var
          Item: String := Csv[Col];

        if Trim(Item) = '==' then
        begin
          Inc(ColSpan);
          Continue;
        end;

        if ColSpan > 1 then
        begin
          if CellNode <> nil then
            CellNode.SetAttribute('colspan', ColSpan.ToString);
          ColSpan := 1;
        end;

        CellNode := TWikiNode.Create(TNodeType.TableDataCell);
        if CharInSet(GetAt(Item, 1), [' ', #9]) then
        begin
          if CharInSet(GetAt(Item, Length(Item)), [' ', #9]) then
            CellNode.SetAttribute('style', 'text-align:center')
          else
            CellNode.SetAttribute('style', 'text-align:right');
        end;

        CellNode.Text := Trim(Item);
        RowNode.AddChild(CellNode);
      end;

      Inc(Index);
    end;

  finally
    Csv.Free;
  end;
end;

function TPukiwikiConverter.ProcessDescriptionList(var CurrentNode: TWikiNode;
  var Index: Integer; const Line: String): Boolean;
var
  Node: TWikiNode;
  SepPos: Integer;
  Level: Integer;
  Term: String;
  Detail: String;
begin
  SepPos := System.Pos('|', Line);

  if SepPos = 0 then
    Exit(False);

  Level := Min(GetSameCharLength(Line, ':', 1), 3);
  Term := Copy(Line, Level + 1, SepPos - Level - 1);
  Detail := Copy(Line, SepPos + 1, MaxInt);

  CurrentNode := GetTargetNode(CurrentNode, [TNodeType.DescriptionDetail],
    Level, [TNodeType.BlockQuote, TNodeType.ListItem]);

  Node := nil;
  case CurrentNode.NodeType of
    TNodeType.DescriptionDetail:
      begin
        if CurrentNode.NestLevel = Level then
        begin
          // sibling list item
          CurrentNode := CurrentNode.ParentNode;
        end
        else
        begin
          // child list item
          Node := TWikiNode.Create(TNodeType.DescriptionList);
        end;
      end;
    TNodeType.ListItem:
      begin
        if CurrentNode.NestLevel = Level then
        begin
          // start of same lebel list
          CurrentNode := CurrentNode.ParentNode.ParentNode;
          Node := TWikiNode.Create(TNodeType.DescriptionList);
        end
        else
        begin
          // start of child list
          Node := TWikiNode.Create(TNodeType.DescriptionList);
        end;
      end
  else
    begin
      // start of list
      Node := TWikiNode.Create(TNodeType.DescriptionList);
    end;
  end;

  if Node <> nil then
  begin
    Node.NestLevel := Level;
    Node.SetAttribute('class', 'list' + Level.ToString);
    CurrentNode.AddChild(Node);
    CurrentNode := Node;
  end;

  Term := Trim(Term);
  if Term <> '' then
  begin
    // add term
    Node := TWikiNode.Create(TNodeType.DescriptionTerm);
    Node.NestLevel := Level;
    CurrentNode.AddChild(Node);
    var
      ChildNode: TWikiNode := TWikiNode.Create(TNodeType.Text);
    if GetAt(Term, 1) = '~' then
    begin
      ChildNode.NodeType := TNodeType.Paragraph;
      Term := TrimLeft(Copy(Term, 2, Length(Term) - 1));
    end;
    ChildNode.Text := Term;
    Node.AddChild(ChildNode);
  end;

  // add detail
  Node := TWikiNode.Create(TNodeType.DescriptionDetail);
  Node.NestLevel := Level;
  CurrentNode.AddChild(Node);
  CurrentNode := Node;

  if TrimLeft(Detail) <> '' then
  begin
    var
      ChildNode: TWikiNode := TWikiNode.Create(TNodeType.Text);
      // "|~" -> paragraph
      // "| ~" -> not paragraph
    if GetAt(Detail, 1) = '~' then
    begin
      ChildNode.NodeType := TNodeType.Paragraph;
      Detail := Copy(Detail, 2, Length(Detail) - 1);
    end;
    ChildNode.Text := TrimLeft(Detail);
    Node.AddChild(ChildNode);
    CurrentNode := ChildNode;
  end;

  Inc(Index);
  Result := True;
end;

function TPukiwikiConverter.ProcessEmphasis(const CurrentNode: TWikiNode;
  const Text: String; const Offset, Copied: Integer; const Mark: Char): Integer;
var
  OpenCount: Integer;
  Count: Integer;
  CPos: Integer;
  TextLength: Integer;
  Ch: Char;
  Mark2Pos, Mark3Pos: Integer;
  Node2Type, Node3Type: TNodeType;
begin
  CPos := Offset;
  OpenCount := GetSameCharLength(Text, Mark, CPos);
  if (OpenCount <= 1) or (OpenCount = 4) or (OpenCount >= 6) then
    Exit(0);

  case Mark of
    '''':
      begin
        Node2Type := TNodeType.Strong;
        Node3Type := TNodeType.Emphasis;
      end;
    '%':
      begin
        Node2Type := TNodeType.Delete;
        Node3Type := TNodeType.Insert;
      end;
  else
    Exit(0);
  end;

  Inc(CPos, OpenCount);

  Mark2Pos := 0;
  Mark3Pos := 0;
  TextLength := Length(Text);
  while CPos <= TextLength do
  begin
    Ch := GetAt(Text, CPos);

    if CharInSet(Ch, [#10, #13]) then
      Exit(0);

    if Ch <> Mark then
    begin
      Inc(CPos);
      Continue;
    end;

    Count := GetSameCharLength(Text, Mark, CPos);

    if Count = 2 then
    begin
      if Mark2Pos = 0 then
        Mark2Pos := CPos
      else
        // <em>..<strong>..</strong>.. or <ins>..<del>..</del>..
        Mark2Pos := 0;
    end;

    if (Count = 3) or (Count = 4) then
    begin
      if (OpenCount = 2) and (Count = 4) then
        // <strong>..</strong>'' or <del>..</del>%%
        Mark2Pos := CPos
      else if Mark3Pos = 0 then
        Mark3Pos := CPos
      else
        // <strong>..<em>..</em>.. or <del>..<ins>..</ins>
        Mark3Pos := 0;
    end;

    if (Count >= 5) then
    begin
      if Mark3Pos = 0 then
      begin
        if Mark2Pos = 0 then
        begin
          case OpenCount of
            2:
              // <strong>..</strong>''' or <del>..</del>%%%
              Mark2Pos := CPos;
            3:
              // <em>..</em>''
              Mark3Pos := CPos;
          else
            begin
              // <strong><em>..</em></strong> or <del><ins>..</ins></strong>
              Mark3Pos := CPos;
              Mark2Pos := CPos + 3;
            end;
          end;
        end
        else
        begin
          // <em>..<strong>..</strong></em> or <ins>..<del>..</del></ins>
          Mark3Pos := CPos + 2;
        end;
      end
      else
      begin
        // <strong>..<em>..</em></strong> or <del>..<ins>..</ins></del>
        Mark2Pos := CPos + 3;
      end;
    end;

    var
      NodeType: TNodeType := TNodeType.Text;
    var
      MatchCount: Integer := 0;
    var
      ClosePosition: Integer := 0;
    case OpenCount of
      2:
        begin
          if Mark2Pos > 0 then
          begin
            NodeType := Node2Type;
            MatchCount := OpenCount;
            ClosePosition := Mark2Pos;
          end;
        end;
      3:
        begin
          if Mark3Pos > 0 then
          begin
            NodeType := Node3Type;
            MatchCount := OpenCount;
            ClosePosition := Mark3Pos;
          end;
        end;
      5:
        begin
          if (Mark2Pos > 0) and (Mark3Pos > 0) then
          begin
            if Mark2Pos > Mark3Pos then
            begin
              // <strong><em>..</em></storng> or <del><ins>..</ins></del>
              NodeType := Node2Type;
              MatchCount := 2;
              ClosePosition := Mark2Pos;
            end
            else
            begin
              // <em><strong>..</strong></em> or <ins><del>..</del></ins>
              NodeType := Node3Type;
              MatchCount := 3;
              ClosePosition := Mark3Pos;
            end;
          end
          else
          begin
            if Mark2Pos > 0 then
            begin
              // <em><strong>..</strong>.. or <ins><del>..</del>..
              Dec(OpenCount, 2);
              Mark2Pos := 0;
            end
            else
            begin
              // <strong><em>..</em>.. <del><ins>..</ins>..
              Dec(OpenCount, 3);
              Mark3Pos := 0;
            end;
          end;
        end
    else
      Exit(0);
    end;

    if ClosePosition > 0 then
    begin
      AddBeforeTextNode(CurrentNode, Copy(Text, Copied + 1,
        Offset - Copied - 1));

      var
        Node: TWikiNode := TWikiNode.Create(NodeType);
      Node.Text := Copy(Text, Offset + MatchCount, ClosePosition - Offset -
        MatchCount);
      ProcessInline(Node);
      CurrentNode.AddChild(Node);

      Exit(ClosePosition + MatchCount - 1);
    end;

    Inc(CPos, Count);
  end;

  if Mark3Pos > 0 then
  begin
    // ''..''' -> <strong>..</strong>'
    var
      NodeType: TNodeType := Node2Type;
    var
      MatchCount: Integer := 2;
    var
      ClosePosition: Integer := Mark3Pos;

    AddBeforeTextNode(CurrentNode, Copy(Text, Copied + 1, Offset - Copied - 1));

    var
      Node: TWikiNode := TWikiNode.Create(NodeType);
    Node.Text := Copy(Text, Offset + MatchCount, ClosePosition - Offset -
      MatchCount);
    ProcessInline(Node);
    CurrentNode.AddChild(Node);

    Exit(ClosePosition + MatchCount - 1);
  end;

  Result := 0;
end;

function TPukiwikiConverter.ProcessEntity(const CurrentNode: TWikiNode;
  const Text: String; const Offset, Copied: Integer): Integer;
var
  EPos: Integer;
  Ret: TMatch;
begin
  if GetAt(Text, Offset) <> '&' then
    Exit(0);

  EPos := Pos(';', Text, Offset + 1);
  if EPos = 0 then
    Exit(0);

  var
    Reference: String := Copy(Text, Offset, EPos - Offset + 1);
  if FHtmlEntities.ContainsKey(Reference) then
  begin
    AddBeforeTextNode(CurrentNode, Copy(Text, Copied + 1, Offset - Copied - 1) +
      FHtmlEntities.Items[Reference]);
    Exit(EPos);
  end;

  Ret := RegexHexadecimalEntityReference.Match(Reference);
  if Ret.Success then
  begin
    var
      Entity: Char := Chr(('$' + Ret.Groups[1].Value).ToInteger);
    AddBeforeTextNode(CurrentNode, Copy(Text, Copied + 1, Offset - Copied - 1)
      + Entity);
    Exit(EPos);
  end;

  Ret := RegexDecimalEntityReference.Match(Reference);
  if Ret.Success then
  begin
    var
      Entity: Char := Chr(Ret.Groups[1].Value.ToInteger);
    AddBeforeTextNode(CurrentNode, Copy(Text, Copied + 1, Offset - Copied - 1)
      + Entity);
    Exit(EPos);
  end;

  Result := 0;
end;

function TPukiwikiConverter.ProcessFootnote(const CurrentNode: TWikiNode;
  const Text: String; const Offset, Copied: Integer): Integer;
var
  Ch: Char;
  CPos, EPos: Integer;
  TextLength: Integer;
  Note: String;
  Level: Integer;
begin
  if (GetAt(Text, Offset) <> '(') or (GetAt(Text, Offset + 1) <> '(') then
    Exit(0);

  Note := '';
  EPos := 0;
  CPos := Offset + 2;
  Level := 1;
  TextLength := Length(Text);
  while CPos <= TextLength do
  begin
    Ch := GetAt(Text, CPos);
    if Ch = #13 then
      Exit(0);

    if (Ch = '(') and (GetAt(Text, CPos + 1) = '(') then
    begin
      // nested footnote
      Inc(Level);
      Inc(CPos, 2);
      Continue;
    end;

    if Ch <> ')' then
    begin
      Inc(CPos);
      Continue;
    end;

    if GetAt(Text, CPos + 1) <> ')' then
    begin
      Inc(CPos, 2);
      Continue;
    end;

    // close mark
    Dec(Level);
    Note := Copy(Text, Offset + 2, CPos - Offset - 2);
    EPos := CPos + 1;
    if Level = 0 then
      Break;

    Inc(CPos, 2);
  end;

  if EPos = 0 then
    Exit(0);

  AddBeforeTextNode(CurrentNode, Copy(Text, Copied + 1, Offset - Copied - 1));

  var
    Id: String := (FFootnote.UseIndex + 1).ToString;
  FFootnote.Add(Id, Note);
  FFootnote.FindByIdAndMarkUse(Id); // record usage

  var
    Anchor: TWikiNode := TWikiNode.Create(TNodeType.Anchor);
  Anchor.SetAttribute('class', TFootnote.ClassRefAnchor);
  Anchor.SetAttribute('href', '#' + TFootnote.IdPrefix + Id);
  Anchor.SetAttribute('id', TFootnote.RefIdPrex + Id);
  Anchor.Value := '*' + Id;
  Anchor.Processed := True;
  CurrentNode.AddChild(Anchor);

  Result := EPos;
end;

procedure TPukiwikiConverter.ProcessHeader(var CurrentNode: TWikiNode;
  var Index: Integer; const Line: String);
var
  Level: Integer;
  HeaderNode: TWikiNode;
  Text: String;
begin
  Level := Min(GetSameCharLength(Line, '*', 1), 3);

  HeaderNode := nil;
  while CurrentNode.ParentNode <> nil do
  begin
    CurrentNode := CurrentNode.ParentNode;
  end;

  case Level of
    1:
      HeaderNode := TWikiNode.Create(TNodeType.Heading2);
    2:
      HeaderNode := TWikiNode.Create(TNodeType.Heading3);
    3:
      HeaderNode := TWikiNode.Create(TNodeType.Heading4);
  end;

  Text := TrimLeft(Copy(Line, Level + 1, Length(Line) - Level));

  if GetAt(Text, 1) = '~' then
  begin
    CurrentNode.AddChild(HeaderNode);
    var
      ParagraphNode: TWikiNode := TWikiNode.Create(TNodeType.Paragraph);
    ParagraphNode.Text := TrimLeft(Copy(Text, 2, Length(Text) - 1));
    HeaderNode.AddChild(ParagraphNode);
    Inc(Index);
    Exit;
  end;

  HeaderNode.Text := Text;
  CurrentNode.AddChild(HeaderNode);

  if (CurrentNode = FRoot) and FGiveIdToHeader then
    AddIdToHeader(HeaderNode, Index);

  Inc(Index);
end;

procedure TPukiwikiConverter.ProcessInline(const Node: TWikiNode);
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

  if Node.NodeType = TNodeType.PreformattedText then
  begin
    if Node.Text <> '' then
    begin
      var
        Code: string := Node.Text;

      if RightStr(Code, 2) = WikiLB then
        SetLength(Code, Length(Code) - 2);

      Node.AddChild(TWikiNode.Create(TNodeType.Text, Encode(Code), True));
      Node.Text := '';
      Exit;
    end;
  end;

  if Node.NodeType = TNodeType.Raw then
  begin
    Node.Value := Node.Text;
    Node.Text := '';
    Node.Processed := True;
    Exit;
  end;

  var
    Text: String := Node.Text;
  var
    TextLength: Integer := Length(Text);
  var
    Ch: Char;
  var
    CPos: Integer := 1;
  var
    Copied: Integer := 0;

  while CPos <= TextLength do
  begin
    Ch := GetAt(Text, CPos);

    if Ch = '~' then
    begin
      // maybe end of line newline
      if (GetAt(Text, CPos + 1) = #13) or (CPos = TextLength) then
      begin
        if not(Node.NodeType in [TNodeType.TableDataCell,
          TNodeType.TableHeaderCell]) and
          (GetTargetNode(Node, [], 0, [TNodeType.DescriptionTerm]).NodeType <>
          TNodeType.DescriptionTerm) then
        begin
          // end of line newline
          AddBeforeTextNode(Node,
            TrimLeft(Copy(Text, Copied + 1, CPos - Copied - 1)));
          Node.AddChild(TWikiNode.Create(TNodeType.Break));
          Copied := CPos;
          Inc(CPos);
          Continue;
        end;
      end;
    end;

    if Ch = '&' then
    begin
      // maybe inline plugin
      var
        NewPos: Integer := ProcessInlinePlugin(Node, Text, CPos, Copied);
      if NewPos > 0 then
      begin
        Copied := NewPos;
        CPos := NewPos + 1;
        Continue;
      end;

      NewPos := ProcessEntity(Node, Text, CPos, Copied);
      if NewPos > 0 then
      begin
        Copied := NewPos;
        CPos := NewPos + 1;
        Continue;
      end;
    end;

    if CharInSet(Ch, ['''', '%']) then
    begin
      // maybe emphasis
      var
        NewPos: Integer := ProcessEmphasis(Node, Text, CPos, Copied, Ch);
      if NewPos > 0 then
      begin
        Copied := NewPos;
        CPos := NewPos + 1;
        Continue;
      end;
    end;

    if Ch = '[' then
    begin
      // maybe wiki link
      var
        NewPos: Integer := ProcessWikiLink(Node, Text, CPos, Copied);
      if NewPos > 0 then
      begin
        Copied := NewPos;
        CPos := NewPos + 1;
        Continue;
      end;
    end;

    if Ch = '(' then
    begin
      // maybe footnote
      var
        NewPos: Integer := ProcessFootnote(Node, Text, CPos, Copied);
      if NewPos > 0 then
      begin
        Copied := NewPos;
        CPos := NewPos + 1;
        Continue;
      end;
    end;

    if CharInSet(Ch, ['a' .. 'z', 'A' .. 'Z']) then
    begin
      // maybe link
      var
        NewPos: Integer := ProcessAutoLink(Node, Text, CPos, Copied);
      if NewPos > 0 then
      begin
        Copied := NewPos;
        CPos := NewPos + 1;
        Continue;
      end;
    end;

    Inc(CPos);
  end;

  Text := Copy(Text, Copied + 1, CPos - Copied);
  Text := RegexTrimRightEachLine.Replace(Text, '');
  AddBeforeTextNode(Node, Text);
  Node.Text := '';
  Node.Processed := True;
end;

function TPukiwikiConverter.ProcessInlinePlugin(const CurrentNode: TWikiNode;
  const Text: String; const Offset: Integer; const Copied: Integer): Integer;
var
  Ch: Char;
  CPos: Integer;
  TextLength: Integer;
begin
  if FWikiPlugins = nil then
    Exit(0);

  CPos := Offset;
  Ch := GetAt(Text, CPos);
  if Ch <> '&' then
    Exit(0);

  TextLength := Length(Text);
  Inc(CPos);

  var
    PluginName: String := '';
  while CPos <= TextLength do
  begin
    Ch := GetAt(Text, CPos);
    if CharInSet(Ch, ['0' .. '9', 'a' .. 'z', 'A' .. 'Z', '_']) then
    begin
      PluginName := PluginName + Ch;
      Inc(CPos);
      Continue;
    end;

    Break;
  end;

  var
    Argument: String := '';
  if Ch = '(' then
  begin
    Inc(CPos);
    while CPos <= TextLength do
    begin
      Ch := GetAt(Text, CPos);
      if Ch = ')' then
        Break;

      Argument := Argument + Ch;
      Inc(CPos);
    end;

    if Ch <> ')' then
      Exit(0);

    Inc(CPos);
    Ch := GetAt(Text, CPos);
  end;

  var
    Body: String := '';
  if Ch = '{' then
  begin
    Inc(CPos);
    while CPos <= TextLength do
    begin
      Ch := GetAt(Text, CPos);
      if Ch = '}' then
        Break;

      Body := Body + Ch;
      Inc(CPos);
    end;

    if Ch <> '}' then
      Exit(0);

    Inc(CPos);
    Ch := GetAt(Text, CPos);
  end;

  if Ch <> ';' then
    Exit(0);

  PluginName := LowerCase(PluginName);
  if not FWikiPlugins.ContainsKey(PluginName) then
    Exit(0);

  AddBeforeTextNode(CurrentNode, Copy(Text, Copied + 1, Offset - Copied - 1));

  var
    BodyPart: TWikiNode := TWikiNode.Create(TNodeType.Text, Body, False);
  try
    ProcessInline(BodyPart);

    CurrentNode.AddChild(TWikiNode.Create(TNodeType.Raw,
      FWikiPlugins[PluginName].Execute(Argument, GetHtml(BodyPart)), True));
  finally
    BodyPart.Free;
  end;

  Result := CPos;
end;

procedure TPukiwikiConverter.ProcessList(var CurrentNode: TWikiNode;
  var Index: Integer; const Line: String);
var
  Node: TWikiNode;
  ListType: TNodeType;
  Mark: Char;
  Level: Integer;
begin
  Mark := Line[1];
  Level := Min(GetSameCharLength(Line, Mark, 1), 3);

  if Mark = '-' then
    ListType := TNodeType.UnorderedList
  else
    ListType := TNodeType.OrderedList;

  CurrentNode := GetTargetNode(CurrentNode, [TNodeType.ListItem], Level,
    [TNodeType.BlockQuote, TNodeType.DescriptionDetail]);

  Node := nil;

  case CurrentNode.NodeType of
    TNodeType.ListItem:
      begin
        if CurrentNode.NestLevel = Level then
        begin
          if CurrentNode.ParentNode.NodeType = ListType then
          begin
            // sibling list item
            CurrentNode := CurrentNode.ParentNode;
          end
          else
          begin
            // another type list
            Node := TWikiNode.Create(ListType);
            CurrentNode := CurrentNode.ParentNode.ParentNode
          end;
        end
        else
        begin
          // chlid list item
          Node := TWikiNode.Create(ListType);
        end;
      end;
    TNodeType.DescriptionDetail:
      begin
        if CurrentNode.NestLevel = Level then
        begin
          // start of same lebel list
          CurrentNode := CurrentNode.ParentNode.ParentNode;
          Node := TWikiNode.Create(ListType);
        end
        else
        begin
          // start of child list
          Node := TWikiNode.Create(ListType);
        end;
      end
  else
    begin
      // start of list
      Node := TWikiNode.Create(ListType);
    end;
  end;

  if Node <> nil then
  begin
    Node.NestLevel := Level;
    Node.SetAttribute('class', 'list' + Level.ToString);
    CurrentNode.AddChild(Node);
    CurrentNode := Node;
  end;

  Node := TWikiNode.Create(TNodeType.ListItem);
  Node.NestLevel := Level;
  CurrentNode.AddChild(Node);

  var
    ChildNode: TWikiNode := TWikiNode.Create(TNodeType.Text);
  var
    Text: String := TrimLeft(Copy(Line, Level + 1, Length(Line) - Level));

  if GetAt(Text, 1) = '~' then
  begin
    ChildNode.NodeType := TNodeType.Paragraph;
    Text := Copy(Text, 2, Length(Text) - 1);
  end;

  ChildNode.Text := Text;
  Node.AddChild(ChildNode);

  CurrentNode := ChildNode;
  Inc(Index);
end;

procedure TPukiwikiConverter.ProcessParagraph(var CurrentNode: TWikiNode;
  var Index: Integer; const Line: String);
var
  Node: TWikiNode;
begin
  // remove head '~'
  Node := TWikiNode.Create(TNodeType.Paragraph,
    Copy(Line, 2, Length(Line) - 1), False);

  CurrentNode := GetBlockParentNode(CurrentNode);
  CurrentNode.AddChild(Node);
  CurrentNode := Node;
  Inc(Index);
end;

procedure TPukiwikiConverter.ProcessPreformattedText(var CurrentNode: TWikiNode;
  var Index: Integer; const Line: String);
var
  Code: String;
  Node: TWikiNode;
begin
  if Line[1] = ' ' then
    Code := Copy(Line, 2, Length(Line) - 1)
  else
    Code := Line;

  if CurrentNode.NodeType = TNodeType.PreformattedText then
  begin
    CurrentNode.Text := CurrentNode.Text + WikiLB + Code;
  end
  else
  begin
    CurrentNode := GetBlockParentNode(CurrentNode);
    Node := TWikiNode.Create(TNodeType.PreformattedText, Code, False);
    CurrentNode.AddChild(Node);
    CurrentNode := Node;
  end;

  Inc(Index);
end;

function TPukiwikiConverter.ProcessWikiLink(const CurrentNode: TWikiNode;
  const Text: String; const Offset, Copied: Integer): Integer;
var
  CPos, EPos: Integer;
  Alias, Name: String;
begin
  if (GetAt(Text, Offset) <> '[') or (GetAt(Text, Offset + 1) <> '[') then
    Exit(0);

  CPos := Offset + 2;
  EPos := Pos(']]', Text, CPos);
  if EPos = 0 then
    Exit(0);

  Alias := '';
  Name := Copy(Text, CPos, EPos - CPos);

  // check [[page name]]
  if (FPageList = nil) or (not FPageList.ContainsKey(Name)) then
  begin
    var
      AliasPos: Integer := Pos('>', Name);
    if AliasPos = 0 then
      Exit(0);

    Alias := Copy(Name, 1, AliasPos - 1);
    Name := RightStr(Name, Length(Name) - AliasPos);

    // check [[alias>page name]]
    if (FPageList = nil) or (not FPageList.ContainsKey(Name)) then
    begin
      // check [[alias>uri]]
      var
        Ret: TMatch := RegexUri.Match(Name);
      if (Ret.Success) and (Length(Name) = Ret.Length) then
      begin
        AddBeforeTextNode(CurrentNode, Copy(Text, Copied + 1,
          Offset - Copied - 1));

        var
          Node: TWikiNode := TWikiNode.Create(TNodeType.Anchor);
        Node.AddChild(TWikiNode.Create(TNodeType.Text, Alias, False));
        Node.SetAttribute('href', Name);
        ProcessInline(Node);
        CurrentNode.AddChild(Node);

        Exit(EPos + 1);
      end;
      Exit(0);
    end;
  end;

  AddBeforeTextNode(CurrentNode, Copy(Text, Copied + 1, Offset - Copied - 1));

  var
    Node: TWikiNode := TWikiNode.Create(TNodeType.Anchor);
  if Alias <> '' then
    Node.AddChild(TWikiNode.Create(TNodeType.Text, Alias, False))
  else
    Node.AddChild(TWikiNode.Create(TNodeType.Text, Name, False));
  Node.SetAttribute('href', FPageList[Name]);
  ProcessInline(Node);
  CurrentNode.AddChild(Node);

  Result := EPos + 1;
end;

function TPukiwikiConverter.ToHtml(const Text: String): String;
var
  Node: TWikiNode;
begin
  Node := TWikiNode.Create(TNodeType.Text, Text, False);
  try
    ParseBlockNode(Node);
    ProcessInline(Node);

    Result := GetHtml(Node);
  finally
    Node.Free;
  end;
end;

{ TTableCellDecoration }

procedure TTableCellDecoration.AssignTo(Dest: TPersistent);
begin
  if Dest is TTableCellDecoration then
  begin
    TTableCellDecoration(Dest).FAlign := FAlign;
    TTableCellDecoration(Dest).FBackgroundColor := FBackgroundColor;
    TTableCellDecoration(Dest).FCellText := FCellText;
    TTableCellDecoration(Dest).FColor := FColor;
    TTableCellDecoration(Dest).FFontSize := FFontSize;
    TTableCellDecoration(Dest).FFontWeight := FFontWeight;
    TTableCellDecoration(Dest).FText := FText;
  end
  else
    inherited;
end;

function TTableCellDecoration.GetStyle: String;
var
  Style: String;
begin
  Style := '';

  if FBackgroundColor <> '' then
    Style := Style + ' ' + Format('background-color:%s;', [FBackgroundColor]);

  if FColor <> '' then
    Style := Style + ' ' + Format('color:%s;', [FColor]);

  if FFontSize <> '' then
    Style := Style + ' ' + Format('font-size:%spx;', [FFontSize]);

  if FFontWeight <> '' then
    Style := Style + ' ' + Format('font-weight:%s;', [FFontWeight]);

  if FAlign <> '' then
    Style := Style + ' ' + Format('text-align:%s;', [FAlign]);

  Result := TrimLeft(Style);
end;

procedure TTableCellDecoration.ReflectStyle(const Text: String);
var
  Ret: TMatch;
  Style: String;
begin
  FCellText := Text;
  FText := Text;

  while True do
  begin
    Ret := RegexCellStyle.Match(FText);
    if not Ret.Success then
      Break;

    Style := Ret.Groups[1].Value;
    FText := Ret.Groups[2].Value;

    if (Style = 'LEFT') or (Style = 'CENTER') or (Style = 'RIGHT') then
    begin
      FAlign := LowerCase(Style);
      Continue;
    end;

    if Copy(Style, 1, 7) = 'BGCOLOR' then
    begin
      FBackgroundColor := Copy(Style, 9, Length(Style) - 9);
      Continue;
    end;

    if Copy(Style, 1, 5) = 'COLOR' then
    begin
      FColor := Copy(Style, 7, Length(Style) - 7);
      Continue;
    end;

    if Copy(Style, 1, 4) = 'SIZE' then
    begin
      FFontSize := Copy(Style, 6, Length(Style) - 6);
      Continue;
    end;

    if Style = 'BOLD' then
    begin
      FFontWeight := 'bold';
      Continue;
    end;
  end;
end;

procedure TTableCellDecoration.SetCellText(const Text: String);
begin
  FAlign := '';
  FBackgroundColor := '';
  FColor := '';
  FFontSize := '';
  FFontWeight := '';

  ReflectStyle(Text);
end;

procedure TTableCellDecoration.SetCellTextToOverride(const Text: String);
begin
  ReflectStyle(Text);
end;

initialization

RegexBlockPlugin := TRegEx.Create('^#([a-zA-Z0-9_]+)(({{.*)|(\((.*)\).*))?$',
  [roCompiled]);
RegexBlockPluginMultiLineBegin := TRegEx.Create('{{\s*$', [roCompiled]);
RegexCellStyle := TRegEx.Create
  ('^(LEFT|CENTER|RIGHT|COLOR\(#?[A-Za-z0-9]+\)|BGCOLOR\(#?[A-Za-z0-9]+\)|SIZE\([0-9]+\)|BOLD):(.*)$',
  [roCompiled]);
RegexDecimalEntityReference := TRegEx.Create('^&#([0-9]{1,7});$', [roCompiled]);
RegexHexadecimalEntityReference := TRegEx.Create('^&#x([0-9a-f]{1,6});$',
  [roCompiled]);
RegexImageFile := TRegEx.Create('(\.gif|\.jpg|\.jpeg|\.png)$',
  [roCompiled, roIgnoreCase]);
RegexTable := TRegEx.Create('^\|.+\|[cCfFhH]?$', [roCompiled]);
RegexTextAlign := TRegEx.Create('^(LEFT|CENTER|RIGHT):(.*)$', [roCompiled]);
RegexTrimRightEachLine := TRegEx.Create('[ \t]+$', [roCompiled, roMultiLine]);
RegexUri := TRegEx.Create('^https?://[\w!?/+\-_~=;.,*&@#$%()'']+',
  [roCompiled, roIgnoreCase]);

end.
