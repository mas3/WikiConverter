unit WikiPluginTest;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  MyWiki.WikiConverter, MyWiki.WikiPlugin;

type
  [TestFixture]
  TextWikiPluginTest = class
  private
    FPlugin: TWikiPlugin;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestFixedPlugin;
    [Test]
    procedure TestFixedPlugin2;
    [Test]
    procedure TestFixedPlugin3;
    [Test]
    procedure TestFixedPlugin4;

    [Test]
    procedure TestEmbeddedPlugin;
    [Test]
    procedure TestEmbeddedPlugin2;
    [Test]
    procedure TestEmbeddedPlugin3;
    [Test]
    procedure TestEmbeddedPlugin4;
    [Test]
    procedure TestEmbeddedPlugin5;
    [Test]
    procedure TestEmbeddedPlugin6;
    [Test]
    procedure TestEmbeddedPlugin7;
    [Test]
    procedure TestEmbeddedPlugin8;
    [Test]
    procedure TestEmbeddedPlugin9;
    [Test]
    procedure TestEmbeddedPlugin10;
  end;

implementation

procedure TextWikiPluginTest.Setup;
begin
  FPlugin := TWikiPlugin.Create;
end;

procedure TextWikiPluginTest.TearDown;
begin
  FPlugin.Free;
end;

procedure TextWikiPluginTest.TestEmbeddedPlugin;
var
  Expected: String;
  Json: String;
begin
  Json := '{"FormatVersion":1,"Plugin":{"Name":"embedded","Version":"1.0.0","Usage":"&embedded(arg1,arg2,arg3);","Note":"note","Example":"&embedded(1,2,3);","Author":"author name"},"Process":{"Kind":"Embedded","Format":["<plugin />","<plugin>[{1}]</plugin>","<plugin>[{1}][{2}]</plugin>","<plugin>[{1}][{2}][{3}]</plugin>"]}}';
  FPlugin.SetJsonString(Json);

  Expected := '<plugin />';
  Assert.AreEqual(Expected, FPlugin.ExecuteMultiline('', ''));
end;

procedure TextWikiPluginTest.TestEmbeddedPlugin10;
var
  Expected: String;
  Json: String;
begin
  Json := '{"FormatVersion":1,"Plugin":{"Name":"embedded","Version":"1.0.0","Usage":"&embedded(arg1,arg2){data};","Note":"note","Example":"&embedded(1,2){text};","Author":"author name"},"Process":{"Kind":"Embedded","Format":["[{0}]","[{0}][{1}]","[{0}][{1}][{2}]"]}}';
  FPlugin.SetJsonString(Json);

  Expected := '[text][arg1][arg2]';
  Assert.AreEqual(Expected, FPlugin.Execute('arg1,arg2', 'text'));
end;

procedure TextWikiPluginTest.TestEmbeddedPlugin2;
var
  Expected: String;
  Json: String;
begin
  Json := '{"FormatVersion":1,"Plugin":{"Name":"embedded","Version":"1.0.0","Usage":"&embedded(arg1,arg2,arg3);","Note":"note","Example":"&embedded(1,2,3);","Author":"author name"},"Process":{"Kind":"Embedded","Format":["<plugin />","<plugin>[{1}]</plugin>","<plugin>[{1}][{2}]</plugin>","<plugin>[{1}][{2}][{3}]</plugin>"]}}';
  FPlugin.SetJsonString(Json);

  Expected := '<plugin>[arg1]</plugin>';
  Assert.AreEqual(Expected, FPlugin.ExecuteMultiline('arg1', ''));
end;

procedure TextWikiPluginTest.TestEmbeddedPlugin3;
var
  Expected: String;
  Json: String;
begin
  Json := '{"FormatVersion":1,"Plugin":{"Name":"embedded","Version":"1.0.0","Usage":"&embedded(arg1,arg2,arg3);","Note":"note","Example":"&embedded(1,2,3);","Author":"author name"},"Process":{"Kind":"Embedded","Format":["<plugin />","<plugin>[{1}]</plugin>","<plugin>[{1}][{2}]</plugin>","<plugin>[{1}][{2}][{3}]</plugin>"]}}';
  FPlugin.SetJsonString(Json);

  Expected := '<plugin>[arg1][arg2]</plugin>';
  Assert.AreEqual(Expected, FPlugin.ExecuteMultiline('arg1,arg2', ''));
end;

procedure TextWikiPluginTest.TestEmbeddedPlugin4;
var
  Expected: String;
  Json: String;
begin
  Json := '{"FormatVersion":1,"Plugin":{"Name":"embedded","Version":"1.0.0","Usage":"&embedded(arg1,arg2,arg3);","Note":"note","Example":"&embedded(1,2,3);","Author":"author name"},"Process":{"Kind":"Embedded","Format":["<plugin />","<plugin>[{1}]</plugin>","<plugin>[{1}][{2}]</plugin>","<plugin>[{1}][{2}][{3}]</plugin>"]}}';
  FPlugin.SetJsonString(Json);

  Expected := '<plugin>[arg1][arg2][arg3]</plugin>';
  Assert.AreEqual(Expected, FPlugin.ExecuteMultiline('arg1,arg2,arg3,arg4', ''));
end;

procedure TextWikiPluginTest.TestEmbeddedPlugin5;
var
  Expected: String;
  Json: String;
begin
  Json := '{"FormatVersion":1,"Plugin":{"Name":"embedded","Version":"1.0.0","Usage":"&embedded(arg1,arg2,arg3);","Note":"note","Example":"&embedded(1,2,3);","Author":"author name"},"Process":{"Kind":"Embedded","Format":["<plugin />","<plugin>[{1}]</plugin>","<plugin>[{1}][{2}]</plugin>","<plugin>[{1}][{2}][{3}]</plugin>"]}}';
  FPlugin.SetJsonString(Json);

  Expected := '<plugin>[arg1][body]</plugin>';
  Assert.AreEqual(Expected, FPlugin.ExecuteMultiline('arg1', 'body'));
end;

procedure TextWikiPluginTest.TestEmbeddedPlugin6;
var
  Expected: String;
  Json: String;
begin
  Json := '{"FormatVersion":1,"Plugin":{"Name":"embedded","Version":"1.0.0","Usage":"&embedded(arg1,arg2,arg3);","Note":"note","Example":"&embedded(1,2,3);","Author":"author name"},"Process":{"Kind":"Embedded","Format":["<plugin />","<plugin>[{1}]</plugin>","<plugin>[{1}][{2}]</plugin>","<plugin>[{1}][{2}][{3}]</plugin>"]}}';
  FPlugin.SetJsonString(Json);

  Expected := '<plugin>[arg1,arg2]</plugin>';
  Assert.AreEqual(Expected, FPlugin.ExecuteMultiline('"arg1,arg2"', ''));
end;

procedure TextWikiPluginTest.TestEmbeddedPlugin7;
var
  Expected: String;
  Json: String;
begin
  Json := '{"FormatVersion":1,"Plugin":{"Name":"embedded","Version":"1.0.0","Usage":"&embedded(arg1,arg2){data};","Note":"note","Example":"&embedded(1,2){text};","Author":"author name"},"Process":{"Kind":"Embedded","Format":["[{0}]","[{0}][{1}]","[{0}][{1}][{2}]"]}}';
  FPlugin.SetJsonString(Json);

  Expected := '[]';
  Assert.AreEqual(Expected, FPlugin.Execute('', ''));
end;

procedure TextWikiPluginTest.TestEmbeddedPlugin8;
var
  Expected: String;
  Json: String;
begin
  Json := '{"FormatVersion":1,"Plugin":{"Name":"embedded","Version":"1.0.0","Usage":"&embedded(arg1,arg2){data};","Note":"note","Example":"&embedded(1,2){text};","Author":"author name"},"Process":{"Kind":"Embedded","Format":["[{0}]","[{0}][{1}]","[{0}][{1}][{2}]"]}}';
  FPlugin.SetJsonString(Json);

  Expected := '[text]';
  Assert.AreEqual(Expected, FPlugin.Execute('', 'text'));
end;

procedure TextWikiPluginTest.TestEmbeddedPlugin9;
var
  Expected: String;
  Json: String;
begin
  Json := '{"FormatVersion":1,"Plugin":{"Name":"embedded","Version":"1.0.0","Usage":"&embedded(arg1,arg2){data};","Note":"note","Example":"&embedded(1,2){text};","Author":"author name"},"Process":{"Kind":"Embedded","Format":["[{0}]","[{0}][{1}]","[{0}][{1}][{2}]"]}}';
  FPlugin.SetJsonString(Json);

  Expected := '[text][arg1]';
  Assert.AreEqual(Expected, FPlugin.Execute('arg1', 'text'));
end;

procedure TextWikiPluginTest.TestFixedPlugin;
var
  Expected: String;
  Json: String;
begin
  Json := '{"FormatVersion":1,"Plugin":{"Name":"hr","Version":"1.0.0","Usage":"&hr();","Note":"short horizontal rule.","Example":"&hr();","Author":"author name"},"Process":{"Kind":"Fixed","Value":"<hr class=\"short\">"}}';
  FPlugin.SetJsonString(Json);

  Expected := '<hr class="short">';
  Assert.AreEqual(Expected, FPlugin.ExecuteMultiline('', ''));
end;

procedure TextWikiPluginTest.TestFixedPlugin2;
var
  Expected: String;
  Json: String;
begin
  Json := '{"FormatVersion":99,"Plugin":{"Name":"hr","Version":"1.0.0","Usage":"&hr();","Note":"short horizontal rule.","Example":"&hr();","Author":"author name"},"Process":{"Kind":"Fixed","Value":"<hr class=\"short\">"}}';
  FPlugin.SetJsonString(Json);

  Expected := 'FormatVersion invalid.[99]';
  Assert.AreEqual(Expected, FPlugin.ExecuteMultiline('', ''));
end;

procedure TextWikiPluginTest.TestFixedPlugin3;
var
  Json: String;
begin
  Json := '{"FormatVersion":1a,"Plugin":{"Name":"hr","Version":"1.0.0","Usage":"&hr();","Note":"short horizontal rule.","Example":"&hr();","Author":"author name"},"Process":{"Kind":"Fixed","Value":"<hr class=\"short\">"}}';
  FPlugin.SetJsonString(Json);

  Assert.Contains(FPlugin.ExecuteMultiline('', ''), '1a');
  Assert.Contains(FPlugin.ExecuteMultiline('', ''), 'FormatVersion');
end;

procedure TextWikiPluginTest.TestFixedPlugin4;
var
  Expected: String;
  Json: String;
begin
  Json := '{"Plugin":{"Name":"hr","Version":"1.0.0","Usage":"&hr();","Note":"short horizontal rule.","Example":"&hr();","Author":"author name"},"Process":{"Kind":"Fixed","Value":"<hr class=\"short\">"}}';
  FPlugin.SetJsonString(Json);

  Expected := 'FormatVersion invalid.[0]';
  Assert.AreEqual(Expected, FPlugin.ExecuteMultiline('', ''));
end;

initialization
  TDUnitX.RegisterTestFixture(TextWikiPluginTest);

end.
