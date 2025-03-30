unit MyWiki.TextConverter;

interface

uses
  System.SysUtils, System.RegularExpressions, System.NetEncoding,
  MyWiki.WikiConverter, MyWiki.WikiNode;

type
  TTextConverter = class(TWikiConverter)
  protected
    procedure Parse(const WikiText: String); override;
    procedure ParseHeader(const WikiText: String); override;
  end;

implementation

{ TTextConverter }

const
  RegexUri = '(https?://[\w!?/+\-_~=;.,*&@#$%()'']+)';

procedure TTextConverter.Parse(const WikiText: String);
var
  Ret: TMatch;
  Before, After, Match: String;
begin
  FreeAndNil(FRoot);
  FRoot := TWikiNode.Create(TNodeType.PreformattedText);

  Before := '';
  After := WikiText;
  Ret := TRegEx.Match(After, RegexUri, [roIgnoreCase, roCompiled]);
  while Ret.Success do
  begin
    Before := Copy(After, 1, Ret.Index - 1);
    Match := Ret.Groups[1].Value;
    Delete(After, 1, Length(Before) + Ret.Length);

    if Before <> '' then
      FRoot.AddChild(TWikiNode.Create(TNodeType.text,
        TNetEncoding.HTML.Encode(Before), True));

    if Match <> '' then
    begin
      var
        Anchor: TWikiNode := TWikiNode.Create(TNodeType.Anchor);
      Anchor.SetAttribute('href', Match);
      Anchor.AddChild(TWikiNode.Create(TNodeType.text, Match, True));
      FRoot.AddChild(Anchor);
    end;

    Ret := TRegEx.Match(After, RegexUri, [roIgnoreCase, roCompiled]);
  end;

  if After <> '' then
    FRoot.AddChild(TWikiNode.Create(TNodeType.text,
      TNetEncoding.HTML.Encode(After), True));

  FRoot.SetAttribute('class', 'text');
end;

procedure TTextConverter.ParseHeader(const WikiText: String);
begin
  // header does not exist
end;

end.
