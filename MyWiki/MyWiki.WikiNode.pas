unit MyWiki.WikiNode;

interface

uses
  System.Generics.Collections, System.SysUtils;

type
{$SCOPEDENUMS ON}
  TNodeType = (Raw, Anchor, BlockQuote, Break, Code, Delete, DescriptionDetail,
    DescriptionList, DescriptionTerm, Division, Emphasis, Heading1, Heading2,
    Heading3, Heading4, Heading5, Heading6, HorizontalRule, Image, Input,
    Insert, ListItem, OrderedList, Paragraph, PreformattedText, Section, Strong,
    Superscript, Table, TableBody, TableDataCell, TableFooter, TableHeader,
    TableHeaderCell, TableRow, Text, UnorderedList);
{$SCOPEDENUMS OFF}
  TAttributes = TDictionary<String, String>;

  TWikiNode = class(TObject)
  private
    FNodeType: TNodeType;
    FValue: String; // processed value
    FText: String; // value before processing
    FAttributes: TAttributes;
    FChildNodes: TObjectList<TWikiNode>;
    FCodeLanguage: String; // for Markdown code block
    FIsLooseList: Boolean; // for Markdown list
    FNestLevel: Integer; // for PukiWiki
    FParentNode: TWikiNode;
    FProcessed: Boolean;
  protected
  public
    constructor Create(NodeType: TNodeType); overload;
    constructor Create(NodeType: TNodeType; InitialValue: String; Processed: Boolean); overload;
    destructor Destroy; override;

    procedure AddChild(const Child: TWikiNode);
    procedure SetAttribute(const Key, Value: String);

    property Attributes: TAttributes read FAttributes;
    property ChildNodes: TObjectList<TWikiNode> read FChildNodes
      write FChildNodes;
    property CodeLanguage: String read FCodeLanguage write FCodeLanguage;
    property IsLooseList: Boolean read FIsLooseList write FIsLooseList;
    property NestLevel: Integer read FNestLevel write FNestLevel;
    property NodeType: TNodeType read FNodeType write FNodeType;
    property ParentNode: TWikiNode read FParentNode write FParentNode;
    property Processed: Boolean read FProcessed write FProcessed;
    property Text: String read FText write FText;
    property Value: String read FValue write FValue;
  end;

implementation

{ TWikiNode }

/// <summary>
/// Add child node
/// </summary>
/// <param name="Child">Child node</param>
procedure TWikiNode.AddChild(const Child: TWikiNode);
begin
  if FChildNodes = nil then
  begin
    FChildNodes := TObjectList<TWikiNode>.Create;
  end;

  FChildNodes.Add(Child);
  Child.ParentNode := Self;
end;

constructor TWikiNode.Create(NodeType: TNodeType);
begin
  FChildNodes := nil;
  FCodeLanguage := '';
  FIsLooseList := False;
  FParentNode := nil;
  FProcessed := False;
  FNestLevel := 0;

  FNodeType := NodeType;
  FAttributes := TAttributes.Create;
end;

constructor TWikiNode.Create(NodeType: TNodeType; InitialValue: String;
  Processed: Boolean);
begin
  Create(NodeType);

  FProcessed := Processed;
  if Processed then
    FValue := InitialValue
  else
    FText := InitialValue;
end;

destructor TWikiNode.Destroy;
begin
  FreeAndNil(FChildNodes);
  FreeAndNil(FAttributes);

  inherited;
end;

procedure TWikiNode.SetAttribute(const Key, Value: String);
begin
  FAttributes.AddOrSetValue(Key, Value);
end;

end.
