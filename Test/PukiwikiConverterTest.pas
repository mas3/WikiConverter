unit PukiwikiConverterTest;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  DUnitX.TestFramework,
  MyWiki.WikiConverter, MyWiki.PukiwikiConverter, MyWiki.WikiPlugin;

type

  [TestFixture]
  TPukiwikiConverterTest = class
  private
    FConverter: TWikiConverter;
    FPageList: TWikiPages;
    FPlugins: TWikiPlugins;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // encode
    [Test]
    procedure TestEncode;
    [Test]
    procedure TestEncode2;

    // paragraph
    [Test]
    procedure TestParagraph;
    [Test]
    procedure TestParagraph2;
    [Test]
    procedure TestParagraph3;
    [Test]
    procedure TestParagraph4;
    [Test]
    procedure TestParagraph5;
    [Test]
    procedure TestParagraph6;

    // block quote
    [Test]
    procedure TestQuote;
    [Test]
    procedure TestQuote2;
    [Test]
    procedure TestQuote3;
    [Test]
    procedure TestQuote4;
    [Test]
    procedure TestQuote5;
    [Test]
    procedure TestQuote6;
    [Test]
    procedure TestQuote7;
    [Test]
    procedure TestQuote8;
    [Test]
    procedure TestQuote9;
    [Test]
    procedure TestQuote10;
    [Test]
    procedure TestQuote11;
    [Test]
    procedure TestQuote12;
    [Test]
    procedure TestQuote13;
    [Test]
    procedure TestQuote14;
    [Test]
    procedure TestQuote15;
    [Test]
    procedure TestQuote16;
    [Test]
    procedure TestQuote17;
    [Test]
    procedure TestQuote18;

    // list
    [Test]
    procedure TestList;
    [Test]
    procedure TestList2;
    [Test]
    procedure TestList3;
    [Test]
    procedure TestList4;
    [Test]
    procedure TestList5;
    [Test]
    procedure TestList6;
    [Test]
    procedure TestList7;
    [Test]
    procedure TestList8;
    [Test]
    procedure TestList9;
    [Test]
    procedure TestList10;
    [Test]
    procedure TestList11;
    [Test]
    procedure TestList12;
    [Test]
    procedure TestList13;
    [Test]
    procedure TestList14;
    [Test]
    procedure TestList15;
    [Test]
    procedure TestList16;
    [Test]
    procedure TestList17;
    [Test]
    procedure TestList18;
    [Test]
    procedure TestList19;
    [Test]
    procedure TestList20;
    [Test]
    procedure TestList21;
    [Test]
    procedure TestList22;
    [Test]
    procedure TestList23;
    [Test]
    procedure TestList24;
    [Test]
    procedure TestList25;
    [Test]
    procedure TestList26;
    [Test]
    procedure TestList27;
    [Test]
    procedure TestList28;

    // description list
    [Test]
    procedure TestDescriptionList;
    [Test]
    procedure TestDescriptionList2;
    [Test]
    procedure TestDescriptionList3;
    [Test]
    procedure TestDescriptionList4;
    [Test]
    procedure TestDescriptionList5;
    [Test]
    procedure TestDescriptionList6;
    [Test]
    procedure TestDescriptionList7;
    [Test]
    procedure TestDescriptionList8;
    [Test]
    procedure TestDescriptionList9;
    [Test]
    procedure TestDescriptionList10;
    [Test]
    procedure TestDescriptionList11;
    [Test]
    procedure TestDescriptionList12;
    [Test]
    procedure TestDescriptionList13;
    [Test]
    procedure TestDescriptionList14;
    [Test]
    procedure TestDescriptionList15;
    [Test]
    procedure TestDescriptionList16;
    [Test]
    procedure TestDescriptionList17;
    [Test]
    procedure TestDescriptionList18;
    [Test]
    procedure TestDescriptionList19;
    [Test]
    procedure TestDescriptionList20;
    [Test]
    procedure TestDescriptionList21;
    [Test]
    procedure TestDescriptionList22;
    [Test]
    procedure TestDescriptionList23;
    [Test]
    procedure TestDescriptionList24;
    [Test]
    procedure TestDescriptionList25;
    [Test]
    procedure TestDescriptionList26;
    [Test]
    procedure TestDescriptionList27;
    [Test]
    procedure TestDescriptionList28;
    [Test]
    procedure TestDescriptionList29;
    [Test]
    procedure TestDescriptionList30;
    [Test]
    procedure TestDescriptionList31;
    [Test]
    procedure TestDescriptionList32;

    // preformatted text
    [Test]
    procedure TestFormattedText;
    [Test]
    procedure TestFormattedText2;
    [Test]
    procedure TestFormattedText3;
    [Test]
    procedure TestFormattedText4;
    [Test]
    procedure TestFormattedText5;
    [Test]
    procedure TestFormattedText6;
    [Test]
    procedure TestFormattedText7;
    [Test]
    procedure TestFormattedText8;
    [Test]
    procedure TestFormattedText9;
    [Test]
    procedure TestFormattedText10;
    [Test]
    procedure TestFormattedText11;
    [Test]
    procedure TestFormattedText12;
    [Test]
    procedure TestFormattedText13;
    [Test]
    procedure TestFormattedText14;
    [Test]
    procedure TestFormattedText15;
    [Test]
    procedure TestFormattedText16;
    [Test]
    procedure TestFormattedText17;
    [Test]
    procedure TestFormattedText18;
    [Test]
    procedure TestFormattedText19;
    [Test]
    procedure TestFormattedText20;
    [Test]
    procedure TestFormattedText21;
    [Test]
    procedure TestFormattedText22;
    [Test]
    procedure TestFormattedText23;
    [Test]
    procedure TestFormattedText24;
    [Test]
    procedure TestFormattedText25;

    // table
    [Test]
    procedure TestTable;
    [Test]
    procedure TestTable2;
    [Test]
    procedure TestTable3;
    [Test]
    procedure TestTable4;
    [Test]
    procedure TestTable5;
    [Test]
    procedure TestTable6;
    [Test]
    procedure TestTable7;
    [Test]
    procedure TestTable8;
    [Test]
    procedure TestTable9;
    [Test]
    procedure TestTable10;
    [Test]
    procedure TestTable11;
    [Test]
    procedure TestTable12;
    [Test]
    procedure TestTable13;
    [Test]
    procedure TestTable14;
    [Test]
    procedure TestTable15;
    [Test]
    procedure TestTable16;
    [Test]
    procedure TestTable17;
    [Test]
    procedure TestTable18;
    [Test]
    procedure TestTable19;
    [Test]
    procedure TestTable20;
    [Test]
    procedure TestTable21;
    [Test]
    procedure TestTable22;
    [Test]
    procedure TestTable23;
    [Test]
    procedure TestTable24;
    [Test]
    procedure TestTable25;
    [Test]
    procedure TestTable26;
    [Test]
    procedure TestTable27;
    [Test]
    procedure TestTable28;
    [Test]
    procedure TestTable29;
    [Test]
    procedure TestTable30;
    [Test]
    procedure TestTable31;
    [Test]
    procedure TestTable32;
    [Test]
    procedure TestTable33;
    [Test]
    procedure TestTable34;
    [Test]
    procedure TestTable35;
    [Test]
    procedure TestTable36;
    [Test]
    procedure TestTable37;
    [Test]
    procedure TestTable38;
    [Test]
    procedure TestTable39;
    [Test]
    procedure TestTable40;
    [Test]
    procedure TestTable41;
    [Test]
    procedure TestTable42;
    [Test]
    procedure TestTable43;
    [Test]
    procedure TestTable44;
    [Test]
    procedure TestTable45;
    [Test]
    procedure TestTable46;
    [Test]
    procedure TestTable47;
    [Test]
    procedure TestTable48;
    [Test]
    procedure TestTable49;
    [Test]
    procedure TestTable50;
    [Test]
    procedure TestTable51;
    [Test]
    procedure TestTable52;
    [Test]
    procedure TestTable53;
    [Test]
    procedure TestTable54;
    [Test]
    [Ignore('Not compatible')]
    procedure TestTable55;
    [Test]
    [Ignore('Not compatible')]
    procedure TestTable56;
    [Test]
    procedure TestTable57;

    [Test]
    procedure TestCsvTable;
    [Test]
    procedure TestCsvTable2;
    [Test]
    procedure TestCsvTable3;
    [Test]
    procedure TestCsvTable4;
    [Test]
    procedure TestCsvTable5;
    [Test]
    [Ignore('Not compatible')]
    procedure TestCsvTable6;
    [Test]
    [Ignore('Not compatible')]
    procedure TestCsvTable7;
    [Test]
    procedure TestCsvTable8;
    [Test]
    procedure TestCsvTable9;
    [Test]
    procedure TestCsvTable10;
    [Test]
    procedure TestCsvTable11;
    [Test]
    procedure TestCsvTable12;
    [Test]
    procedure TestCsvTable13;
    [Test]
    procedure TestCsvTable14;
    [Test]
    procedure TestCsvTable15;
    [Test]
    procedure TestCsvTable16;
    [Test]
    [Ignore('Not compatible')]
    procedure TestCsvTable17;
    [Test]
    procedure TestCsvTable18;
    [Test]
    procedure TestCsvTable19;
    [Test]
    procedure TestCsvTable20;
    [Test]
    procedure TestCsvTable21;

    // header
    [Test]
    procedure TestHeader;
    [Test]
    procedure TestHeader2;
    [Test]
    procedure TestHeader3;

    // text align
    [Test]
    procedure TestTextAlign;
    [Test]
    procedure TestTextAlign2;
    [Test]
    procedure TestTextAlign3;
    [Test]
    procedure TestTextAlign4;
    [Test]
    [Ignore('Not compatible')]
    procedure TestTextAlign5;
    [Test]
    [Ignore('Not compatible')]
    procedure TestTextAlign6;
    [Test]
    procedure TestTextAlign7;
    [Test]
    procedure TestTextAlign8;

    // horizontalrule
    [Test]
    procedure TestHorizontalrule;
    [Test]
    procedure TestHorizontalrule2;
    [Test]
    procedure TestHorizontalrule3;

    // block plugin
    [Test]
    procedure TestBlockPlugin;
    [Test]
    procedure TestBlockPlugin2;
    [Test]
    procedure TestBlockPlugin3;
    [Test]
    procedure TestBlockPlugin4;
    [Test]
    procedure TestBlockPlugin5;
    [Test]
    procedure TestBlockPlugin6;
    [Test]
    procedure TestBlockPlugin7;
    [Test]
    procedure TestBlockPlugin8;
    [Test]
    procedure TestBlockPlugin9;
    [Test]
    procedure TestBlockPlugin10;
    [Test]
    procedure TestBlockPlugin11;
    [Test]
    procedure TestBlockPlugin12;
    [Test]
    procedure TestBlockPlugin13;
    [Test]
    procedure TestBlockPlugin14;
    [Test]
    procedure TestBlockPlugin15;
    [Test]
    procedure TestBlockPlugin16;
    [Test]
    procedure TestBlockPlugin17;
    [Test]
    procedure TestBlockPlugin18;
    [Test]
    procedure TestBlockPlugin19;

    // inline plugin
    [Test]
    procedure TestInlinePlugin;
    [Test]
    procedure TestInlinePlugin2;
    [Test]
    procedure TestInlinePlugin3;
    [Test]
    procedure TestInlinePlugin4;
    [Test]
    procedure TestInlinePlugin5;
    [Test]
    procedure TestInlinePlugin6;
    [Test]
    procedure TestInlinePlugin7;
    [Test]
    procedure TestInlinePlugin8;
    [Test]
    procedure TestInlinePlugin9;
    [Test]
    procedure TestInlinePlugin10;
    [Test]
    procedure TestInlinePlugin11;
    [Test]
    procedure TestInlinePlugin12;
    [Test]
    procedure TestInlinePlugin13;

    // end of line newline
    [Test]
    procedure TestNewline;
    [Test]
    procedure TestNewline2;
    [Test]
    procedure TestNewline3;
    [Test]
    procedure TestNewline4;
    [Test]
    procedure TestNewline5;
    [Test]
    procedure TestNewline6;
    [Test]
    procedure TestNewline7;
    [Test]
    procedure TestNewline8;
    [Test]
    procedure TestNewline9;
    [Test]
    procedure TestNewline10;
    [Test]
    procedure TestNewline11;
    [Test]
    procedure TestNewline12;

    // emphasis
    [Test]
    procedure TestEmphasis;
    [Test]
    procedure TestEmphasis2;
    [Test]
    procedure TestEmphasis3;
    [Test]
    procedure TestEmphasis4;
    [Test]
    procedure TestEmphasis5;
    [Test]
    procedure TestEmphasis6;
    [Test]
    procedure TestEmphasis7;
    [Test]
    procedure TestEmphasis8;
    [Test]
    procedure TestEmphasis9;
    [Test]
    procedure TestEmphasis10;
    [Test]
    procedure TestEmphasis11;
    [Test]
    procedure TestEmphasis12;
    [Test]
    procedure TestEmphasis13;
    [Test]
    procedure TestEmphasis14;
    [Test]
    procedure TestEmphasis15;
    [Test]
    procedure TestEmphasis16;
    [Test]
    procedure TestEmphasis17;
    [Test]
    procedure TestEmphasis18;
    [Test]
    procedure TestEmphasis19;
    [Test]
    procedure TestEmphasis20;
    [Test]
    procedure TestEmphasis21;
    [Test]
    procedure TestEmphasis22;
    [Test]
    procedure TestEmphasis23;
    [Test]
    procedure TestEmphasis24;
    [Test]
    procedure TestEmphasis25;

    // delete
    [Test]
    procedure TestDelete;
    [Test]
    procedure TestDelete2;
    [Test]
    procedure TestDelete3;
    [Test]
    procedure TestDelete4;
    [Test]
    procedure TestDelete5;
    [Test]
    procedure TestDelete6;
    [Test]
    procedure TestDelete7;
    [Test]
    procedure TestDelete8;
    [Test]
    procedure TestDelete9;
    [Test]
    procedure TestDelete10;

    // insert
    [Test]
    procedure TestInsert;

    // page name
    [Test]
    procedure TestPageName;
    [Test]
    procedure TestPageName2;
    [Test]
    procedure TestPageName3;
    [Test]
    procedure TestPageName4;
    [Test]
    procedure TestPageName5;

    // auto link
    [Test]
    procedure TestAutoLink;
    [Test]
    procedure TestAutoLink2;
    [Test]
    procedure TestAutoLink3;

    // entitiy
    [Test]
    procedure TestEntity;
    [Test]
    procedure TestEntity2;
    [Test]
    procedure TestEntity3;

    // comment
    [Test]
    procedure TestComment;
    [Test]
    procedure TestComment2;
    [Test]
    procedure TestComment3;
    [Test]
    procedure TestComment4;
    [Test]
    procedure TestComment5;

    // footnote
    [Test]
    procedure TestFootnote;
    [Test]
    procedure TestFootnote2;
    [Test]
    procedure TestFootnote3;
    [Test]
    procedure TestFootnote4;
    [Test]
    procedure TestFootnote5;

    // add ID to Header
    [Test]
    procedure TestAddIdToHeader;
    [Test]
    procedure TestAddIdToHeader2;
    [Test]
    procedure TestAddIdToHeader3;
    [Test]
    procedure TestAddIdToHeader4;
    [Test]
    procedure TestAddIdToHeader5;
  end;

implementation

{ TPukiwikiConverterTest }

procedure TPukiwikiConverterTest.Setup;
var
  Plugin: TWikiPlugin;
begin
  FConverter := TPukiwikiConverter.Create;
  FPageList := TWikiPages.Create;
  FPlugins := TWikiPlugins.Create([doOwnsValues]);

  // set page
  FPageList.Add('Test Page', 'https://TestPage');

  FConverter.SetPageList(FPageList);

  // set plugin
  Plugin := TWikiPlugin.Create;
  Plugin.SetJsonString
    ('{"FormatVersion":1,"Plugin":{"Name":"fixed","Version":"1.0.0","Usage":"&fixed();","Note":"fixed plugin sample.","Example":"&fixed();","Author":"author name"},"Process":{"Kind":"Fixed","Value":"<fixed>"}}');
  FPlugins.Add(LowerCase(Plugin.Name), Plugin);

  Plugin := TWikiPlugin.Create;
  Plugin.SetJsonString
    ('{"FormatVersion":1,"Plugin":{"Name":"embedded","Version":"1.0.0","Usage":"&embedded(arg1,arg2,arg3);","Note":"note","Example":"&embedded(1,2,3);","Author":"author name"},"Process":{"Kind":"Embedded","Format":["<plugin />","<plugin>[{1}]</plugin>","<plugin>[{1}][{2}]</plugin>","<plugin>[{1}][{2}][{3}]</plugin>"]}}');
  FPlugins.Add(LowerCase(Plugin.Name), Plugin);

  Plugin := TWikiPlugin.Create;
  Plugin.SetJsonString
    ('{"FormatVersion":1,"Plugin":{"Name":"embedded2","Version":"1.0.0","Usage":"&embedded(arg1,arg2,arg3);","Note":"note","Example":"&embedded(1,2,3);","Author":"author name"},"Process":{"Kind":"Embedded","Format":["<plugin />","<plugin>[{1}]</plugin>","<plugin>[{1}][{2}]</plugin>","<plugin>[{1}][{2}][{3}]</plugin>"],"ParseMultiline":true}}');
  FPlugins.Add(LowerCase(Plugin.Name), Plugin);

  Plugin := TWikiPlugin.Create;
  Plugin.SetJsonString
    ('{"FormatVersion":1,"Plugin":{"Name":"inline","Version":"1.0.0","Usage":"&inline(arg1,arg2){data};","Note":"note","Example":"&inline(1,2){text};","Author":"author name"},"Process":{"Kind":"Embedded","Format":["[{0}]","[{0}][{1}]","[{0}][{1}][{2}]"]}}');
  FPlugins.Add(LowerCase(Plugin.Name), Plugin);

  FConverter.SetPlugins(FPlugins);
end;

procedure TPukiwikiConverterTest.TearDown;
begin
  FConverter.Free;
  FPageList.Free;
  FPlugins.Free;
end;

procedure TPukiwikiConverterTest.TestAddIdToHeader;
var
  Expected: String;
  Sentence: String;
begin
  FConverter.GiveIdToHeader := True;
  Sentence := String.Join(WikiLB, ['* foo', '* bar']);
  Expected := String.Join(WikiLB, ['<h2 id="foo">foo</h2>',
    '<h2 id="bar">bar</h2>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestAddIdToHeader2;
var
  Expected: String;
  Sentence: String;
begin
  FConverter.GiveIdToHeader := True;
  Sentence := String.Join(WikiLB, ['* foo', '* foo']);
  Expected := String.Join(WikiLB, ['<h2 id="foo">foo</h2>',
    '<h2 id="foo-1">foo</h2>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestAddIdToHeader3;
var
  Expected: String;
  Sentence: String;
begin
  FConverter.GiveIdToHeader := True;
  Sentence := String.Join(WikiLB, ['* foo-bar', '* foo_bar', '* foo+bar',
    '* foo/bar']);
  Expected := String.Join(WikiLB, ['<h2 id="foo-bar">foo-bar</h2>',
    '<h2 id="foo_bar">foo_bar</h2>', '<h2 id="foobar">foo+bar</h2>',
    '<h2 id="foobar-1">foo/bar</h2>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestAddIdToHeader4;
var
  Expected: String;
  Sentence: String;
begin
  FConverter.GiveIdToHeader := True;
  Sentence := String.Join(WikiLB, ['* foo', '* foo', '* foo-1', '* bar-1']);
  Expected := String.Join(WikiLB, ['<h2 id="foo">foo</h2>',
    '<h2 id="foo-1">foo</h2>', '<h2 id="foo-1-1">foo-1</h2>',
    '<h2 id="bar-1">bar-1</h2>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestAddIdToHeader5;
var
  Expected: String;
  Sentence: String;
begin
  FConverter.GiveIdToHeader := True;
  Sentence := String.Join(WikiLB, ['* foo bar', '* foo-bar']);
  Expected := String.Join(WikiLB, ['<h2 id="foo-bar">foo bar</h2>',
    '<h2 id="foo-bar-1">foo-bar</h2>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestAutoLink;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'https://www.example.com/';
  Expected :=
    '<p><a href="https://www.example.com/">https://www.example.com/</a></p>' +
    WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestAutoLink2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<https://www.example.com/index.html>';
  Expected :=
    '<p>&lt;<a href="https://www.example.com/index.html">https://www.example.com/index.html</a>&gt;</p>'
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestAutoLink3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<https://www.example.com/image.jpeg>';
  Expected :=
    '<p>&lt;<a href="https://www.example.com/image.jpeg"><img src="https://www.example.com/image.jpeg" alt="https://www.example.com/image.jpeg" /></a>&gt;</p>'
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['text', '#fixed', 'text']);
  Expected := String.Join(WikiLB, ['<p>text</p>', '<fixed>', '<p>text</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin10;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['#fixed(arg1,arg2) foo {{', 'arg3',
    'arg3}}foo']);
  Expected := String.Join(WikiLB, ['<p>#fixed(arg1,arg2) foo {{', 'arg3',
    'arg3}}foo</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin11;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '#embedded()';
  Expected := '<plugin />' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin12;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '#embedded(arg1)';
  Expected := '<plugin>[arg1]</plugin>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin13;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '#embedded(arg1,arg2)';
  Expected := '<plugin>[arg1][arg2]</plugin>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin14;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['#embedded(arg1){{', 'arg2', '}}']);
  Expected := '<plugin>[arg1][arg2]</plugin>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin15;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['#embedded(arg1){{', 'arg2', 'arg3', '}}']);
  Expected := String.Join(WikiLB, ['<plugin>[arg1][arg2', 'arg3]</plugin>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin16;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['#embedded{{', 'arg1', '}}']);
  Expected := '<plugin>[arg1]</plugin>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin17;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '#embedded';
  Expected := '<plugin />' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin18;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['#embedded{{', '}}']);
  Expected := '<plugin />' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin19;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['#embedded2(arg1){{', '%%arg2%%', '}}']);
  Expected := String.Join(WikiLB, ['<plugin>[arg1][<p><del>arg2</del></p>',
    ']</plugin>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['text', '#FIXED', 'text']);
  Expected := String.Join(WikiLB, ['<p>text</p>', '<fixed>', '<p>text</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['>quote', '#fixed', '>quote']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote</p>', '<fixed>',
    '<p>quote</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['-list', '#fixed', 'text']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list<fixed>',
    'text</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '#fixed;';
  Expected := '<p>#fixed;</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin6;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '#fixed ';
  Expected := '<p>#fixed</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin7;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '#fixed(arg1,arg2)foo';
  Expected := '<fixed>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin8;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '#fixed (arg1,arg2)';
  Expected := '<p>#fixed (arg1,arg2)</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestBlockPlugin9;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['#fixed(arg1,arg2) foo {{', 'arg3', '}}']);
  Expected := '<fixed>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestComment;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['foo', '// comment', 'bar']);
  Expected := String.Join(WikiLB, ['<p>foo', 'bar</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestComment2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['>quote', '// comment', '>quote']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote</p>',
    '<p>quote</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestComment3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['-list', '// comment', '-list']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list</li>',
    '<li>list</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestComment4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [',item', '// comment', ',item']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '</tr>', '<tr>', '<td>item</td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestComment5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|item|', '// comment', '|item|']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '</tr>', '<tr>', '<td>item</td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [',item,item', ',item,item']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '<td>item</td>', '</tr>', '<tr>', '<td>item</td>',
    '<td>item</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable10;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ',"item "," right"," center "';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '<td style="text-align:right">right</td>',
    '<td style="text-align:center">center</td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable11;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [',item,==,item', ',item, == ,item',
    ',item,"==",item', ',item," == ",item', ',item,===,item',
    ',item,item,item']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td colspan="2">item</td>', '<td>item</td>', '</tr>', '<tr>',
    '<td colspan="2">item</td>', '<td>item</td>', '</tr>', '<tr>',
    '<td colspan="2">item</td>', '<td>item</td>', '</tr>', '<tr>',
    '<td colspan="2">item</td>', '<td>item</td>', '</tr>', '<tr>',
    '<td>item</td>', '<td>===</td>', '<td>item</td>', '</tr>', '<tr>',
    '<td>item</td>', '<td>item</td>', '<td>item</td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable12;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ',item,==,==,item,==,item';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td colspan="3">item</td>', '<td colspan="2">item</td>', '<td>item</td>',
    '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable13;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ',item,&color(#000,#fff){text};,';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '<td>&amp;color(#000</td>', '<td>#fff){text};</td>',
    '<td></td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable14;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ',~item,item';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>~item</td>', '<td>item</td>', '</tr>', '</tbody>', '</table>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable15;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ',item'#9','#9'item'#9','#9'item';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '<td style="text-align:center">item</td>',
    '<td style="text-align:right">item</td>', '</tr>', '</tbody>', '</table>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable16;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ',item,,item';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '<td></td>', '<td>item</td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable17;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ',"item,item';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>"item</td>', '<td>item</td>', '</tr>', '</tbody>', '</table>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable18;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['-list', ',item', '-list']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list', '<table>',
    '<tbody>', '<tr>', '<td>item</td>', '</tr>', '</tbody>', '</table>',
    '</li>', '<li>list</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable19;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['>quote', ',item', '>quote']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote</p>', '<table>',
    '<tbody>', '<tr>', '<td>item</td>', '</tr>', '</tbody>', '</table>',
    '<p>quote</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [',item,item,', ',item,item,']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '<td>item</td>', '<td></td>', '</tr>', '<tr>',
    '<td>item</td>', '<td>item</td>', '<td></td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable20;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [':term|detail', ',item', ':term|detail']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term</dt>',
    '<dd>detail', '<table>', '<tbody>', '<tr>', '<td>item</td>', '</tr>',
    '</tbody>', '</table>', '</dd>', '<dt>term</dt>', '<dd>detail</dd>',
    '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable21;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['-list', ',item', ',item,item']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list', '<table>',
    '<tbody>', '<tr>', '<td>item</td>', '</tr>', '</tbody>', '</table>',
    '<table>', '<tbody>', '<tr>', '<td>item</td>', '<td>item</td>', '</tr>',
    '</tbody>', '</table>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [',item,item', ',item']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '<td>item</td>', '</tr>', '</tbody>', '</table>',
    '<table>', '<tbody>', '<tr>', '<td>item</td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ',"item,item",item';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item,item</td>', '<td>item</td>', '</tr>', '</tbody>', '</table>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ',"item""item",item';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item"item</td>', '<td>item</td>', '</tr>', '</tbody>', '</table>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable6;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ',"item"item",item';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item"item</td>', '<td>item</td>', '</tr>', '</tbody>', '</table>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable7;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ',"item,item" ,item';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>"item</td>', '<td>item"</td>', '<td>item</td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable8;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [',item , right, center ',
    ',item,item,item']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '<td style="text-align:right">right</td>',
    '<td style="text-align:center">center</td>', '</tr>', '<tr>',
    '<td>item</td>', '<td>item</td>', '<td>item</td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestCsvTable9;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ',item , "right", "center" ';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '<td style="text-align:right">"right"</td>',
    '<td style="text-align:center">"center"</td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDelete;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '%%text%%';
  Expected := '<p><del>text</del></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDelete10;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '&inline(arg1){%%body%%};';
  Expected := '<p>[<del>body</del>][arg1]</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDelete2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '%%%%text%%';
  Expected := '<p>%%<del>text</del></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDelete3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo%%text%%bar';
  Expected := '<p>foo<del>text</del>bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDelete4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '%%foo%%%%bar%%';
  Expected := '<p><del>foo</del><del>bar</del></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDelete5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '%%foo%%bar%%';
  Expected := '<p><del>foo</del>bar%%</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDelete6;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''%%text%%''''';
  Expected := '<p><strong><del>text</del></strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDelete7;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '-%%text%%';
  Expected := String.Join(WikiLB, ['<ul class="list1">',
    '<li><del>text</del></li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDelete8;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '|%%text%%|';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td><del>text</del></td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDelete9;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ',%%text%%';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td><del>text</del></td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ': term1 | detail1';
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>detail1</dd>', '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList10;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 |', ':: term2 | detail2',
    ':: term2 |', ': term1 | detail1']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>', '<dl class="list2">', '<dt>term2</dt>', '<dd>detail2</dd>',
    '<dt>term2</dt>', '</dl>', '</dd>', '<dt>term1</dt>', '<dd>detail1</dd>',
    '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList11;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': | ', ': | ']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList12;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | detail1', ': | ',
    ': term1 | detail1']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>detail1</dd>', '<dt>term1</dt>', '<dd>detail1</dd>', '</dl>'])
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList13;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | detail1', ': | ',
    ':: term2 | detail2']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>detail1</dd>', '<dd>', '<dl class="list2">', '<dt>term2</dt>',
    '<dd>detail2</dd>', '</dl>', '</dd>', '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList14;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | detail1', 'sentence',
    ': term1 | detail1']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>detail1', 'sentence</dd>', '<dt>term1</dt>', '<dd>detail1</dd>',
    '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList15;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | detail1', '~paragraph',
    ': term1 | detail1']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>detail1<p>paragraph</p>', '</dd>', '<dt>term1</dt>',
    '<dd>detail1</dd>', '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList16;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | ', '~paragraph',
    ': term1 | detail1']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd><p>paragraph</p>', '</dd>', '<dt>term1</dt>', '<dd>detail1</dd>',
    '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList17;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': ~term1 | detail1', ': term1 | detail1']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt><p>term1</p>',
    '</dt>', '<dd>detail1</dd>', '<dt>term1</dt>', '<dd>detail1</dd>', '</dl>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList18;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | ~detail1', ': term1 | detail1']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>~detail1</dd>', '<dt>term1</dt>', '<dd>detail1</dd>', '</dl>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList19;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 |~detail1', ': term1 | detail1']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd><p>detail1</p>', '</dd>', '<dt>term1</dt>', '<dd>detail1</dd>',
    '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ':| detail1';
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dd>detail1</dd>',
    '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList20;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | detail1', '- list1']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>detail1</dd>', '</dl>', '<ul class="list1">', '<li>list1</li>',
    '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList21;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | detail1', '-- list2']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>detail1', '<ul class="list2">', '<li>list2</li>', '</ul>', '</dd>',
    '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList22;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | ', '- list1']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '</dl>', '<ul class="list1">', '<li>list1</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList23;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | ', '-- list2']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>', '<ul class="list2">', '<li>list2</li>', '</ul>', '</dd>', '</dl>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList24;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | detail1', ':: term2 | detail2',
    '-- list2']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>detail1', '<dl class="list2">', '<dt>term2</dt>', '<dd>detail2</dd>',
    '</dl>', '<ul class="list2">', '<li>list2</li>', '</ul>', '</dd>', '</dl>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList25;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', ': term1 | detail1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1</li>',
    '</ul>', '<dl class="list1">', '<dt>term1</dt>', '<dd>detail1</dd>',
    '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList26;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', ':: term2 | detail2']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<dl class="list2">', '<dt>term2</dt>', '<dd>detail2</dd>', '</dl>',
    '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList27;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '-- list2',
    ':: term2 | detail2']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<ul class="list2">', '<li>list2</li>', '</ul>', '<dl class="list2">',
    '<dt>term2</dt>', '<dd>detail2</dd>', '</dl>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList28;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | detail1', '> quote']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>detail1', '<blockquote>', '<p>quote</p>', '</blockquote>', '</dd>',
    '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList29;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | ', '> quote']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>', '<blockquote>', '<p>quote</p>', '</blockquote>', '</dd>', '</dl>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [':term1 | detail1 ', ': term1 | detail1']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>detail1</dd>', '<dt>term1</dt>', '<dd>detail1</dd>', '</dl>'])
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList30;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term | ', ' code']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term</dt>',
    '<dd>', '<pre>code</pre>', '</dd>', '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList31;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term | detail', ' code',
    ': term | detail']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term</dt>',
    '<dd>detail', '<pre>code</pre>', '</dd>', '<dt>term</dt>',
    '<dd>detail</dd>', '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList32;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term | detail', ' code', 'text']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term</dt>',
    '<dd>detail', '<pre>code</pre>', 'text</dd>', '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | detail1', ':: term2 | detail2']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>detail1', '<dl class="list2">', '<dt>term2</dt>', '<dd>detail2</dd>',
    '</dl>', '</dd>', '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | detail1', ':: term2 | detail2',
    '::: term3 | detail3', ':: term2 | detail2', ': term1 | detail1']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>detail1', '<dl class="list2">', '<dt>term2</dt>', '<dd>detail2',
    '<dl class="list3">', '<dt>term3</dt>', '<dd>detail3</dd>', '</dl>',
    '</dd>', '<dt>term2</dt>', '<dd>detail2</dd>', '</dl>', '</dd>',
    '<dt>term1</dt>', '<dd>detail1</dd>', '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList6;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | detail1', ':: term2 | detail2',
    '::: term3 | detail3', ':::: term4 | detail4']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>detail1', '<dl class="list2">', '<dt>term2</dt>', '<dd>detail2',
    '<dl class="list3">', '<dt>term3</dt>', '<dd>detail3</dd>',
    '<dt>: term4</dt>', '<dd>detail4</dd>', '</dl>', '</dd>', '</dl>', '</dd>',
    '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList7;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 | detail1', '::: term3 | detail3',
    ':: term2 | detail2', ': term1 | detail1']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>detail1', '<dl class="list3">', '<dt>term3</dt>', '<dd>detail3</dd>',
    '</dl>', '<dl class="list2">', '<dt>term2</dt>', '<dd>detail2</dd>',
    '</dl>', '</dd>', '<dt>term1</dt>', '<dd>detail1</dd>', '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList8;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 |', ': term1 | detail1']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dt>term1</dt>', '<dd>detail1</dd>', '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestDescriptionList9;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term1 |', ':: term2 | detail2']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term1</dt>',
    '<dd>', '<dl class="list2">', '<dt>term2</dt>', '<dd>detail2</dd>', '</dl>',
    '</dd>', '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''''foo''''''';
  Expected := '<p><em>foo</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis10;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''foo''''''bar''''''''''''';
  Expected := '<p><strong>foo<em>bar</em></strong>''</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis11;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''foo''''''''''bar''''''''';
  Expected := '<p><strong>foo</strong><em>bar</em>''</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis12;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''''foo''''''''''bar''''''';
  Expected := '<p><em>foo</em><strong>bar</strong>''</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis13;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['''''foo', '''''']);
  Expected := String.Join(WikiLB, ['<p>''''foo', '''''</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis14;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''''foo''''';
  Expected := '<p>''<strong>foo</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis15;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''foo''''''';
  Expected := '<p><strong>foo</strong>''</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis16;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''''foo''''bar''''''';
  Expected := '<p><em>foo''''bar</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis17;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''foo''''''bar''''';
  // MEMO: not compatible
  // Expected := '<p><strong>foo</strong>''bar''''</p>' + WikiLB;
  Expected := '<p><strong>foo''''''bar</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis18;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''foo''''''bar''''''';
  Expected := '<p>''''foo<em>bar</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis19;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'text''''foo''''text';
  Expected := '<p>text<strong>foo</strong>text</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''foo''''';
  Expected := '<p><strong>foo</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis20;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''''''''''foo''''''bar''''''';
  Expected := '<p>''<strong><em>foo</em>bar</strong>''</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis21;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '-''''list''''';
  Expected := String.Join(WikiLB, ['<ul class="list1">',
    '<li><strong>list</strong></li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis22;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '|''''item''''|';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td><strong>item</strong></td>', '</tr>', '</tbody>', '</table>'])
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis23;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ',''''item''''';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td><strong>item</strong></td>', '</tr>', '</tbody>', '</table>'])
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis24;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '&inline(arg1){''''body''''};';
  Expected := '<p>[<strong>body</strong>][arg1]</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis25;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'text ''''foo'''' text';
  Expected := '<p>text <strong>foo</strong> text</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''''foo''''';
  Expected := '<p>''<strong>foo</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''''''foo''''''''';
  Expected := '<p>''<em>foo</em>''</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''''''''foo''''''''''';
  Expected := '<p><strong><em>foo</em></strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis6;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''''''''''foo''''''''''''';
  Expected := '<p>''<strong><em>foo</em></strong>''</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis7;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''''''''''foo''''''bar''''';
  Expected := '<p>''<strong><em>foo</em>bar</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis8;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''''''''''foo''''bar''''''';
  // NOTE: not compatible
  // Expected := '<p>''<strong><em>foo</em></strong><em>bar</em></p>' + WikiLB;
  Expected := '<p>''<em><strong>foo</strong>bar</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEmphasis9;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '''''''foo''''bar''''''''''''';
  Expected := '<p><em>foo<strong>bar</strong></em>''</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEncode;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<p class="article">text</p>';
  Expected := '<p>&lt;p class="article"&gt;text&lt;/p&gt;</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEncode2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '"hello" & "hi"';
  Expected := '<p>"hello" &amp; "hi"</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFootnote;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'test((footnote))';
  // does not support title attribute
  Expected := String.Join(WikiLB,
    ['<p>test<a id="fnref-1" class="fnref" href="#fn-1">*1</a></p>',
    '<section class="footnotes">', '<ol>', '<li id="fn-1">footnote',
    '<a class="fnarrow" href="#fnref-1">↩</a></li>', '</ol>', '</section>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFootnote2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['test((footnote))', 'foo((footnote2)) bar']);
  // does not support title attribute
  Expected := String.Join(WikiLB,
    ['<p>test<a id="fnref-1" class="fnref" href="#fn-1">*1</a>',
    'foo<a id="fnref-2" class="fnref" href="#fn-2">*2</a> bar</p>',
    '<section class="footnotes">', '<ol>', '<li id="fn-1">footnote',
    '<a class="fnarrow" href="#fnref-1">↩</a></li>', '<li id="fn-2">footnote2',
    '<a class="fnarrow" href="#fnref-2">↩</a></li>', '</ol>', '</section>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFootnote3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'test((%%footnote%%))';
  // does not support title attribute
  Expected := String.Join(WikiLB,
    ['<p>test<a id="fnref-1" class="fnref" href="#fn-1">*1</a></p>',
    '<section class="footnotes">', '<ol>', '<li id="fn-1"><del>footnote</del>',
    '<a class="fnarrow" href="#fnref-1">↩</a></li>', '</ol>', '</section>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFootnote4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'test((footnote((ftft))))';
  // does not support title attribute
  Expected := String.Join(WikiLB,
    ['<p>test<a id="fnref-1" class="fnref" href="#fn-1">*1</a></p>',
    '<section class="footnotes">', '<ol>',
    '<li id="fn-1">footnote<a id="fnref-2" class="fnref" href="#fn-2">*2</a>',
    '<a class="fnarrow" href="#fnref-1">↩</a></li>', '<li id="fn-2">ftft',
    '<a class="fnarrow" href="#fnref-2">↩</a></li>', '</ol>', '</section>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFootnote5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'test((footnote((ftft))';
  // does not support title attribute
  Expected := String.Join(WikiLB,
    ['<p>test<a id="fnref-1" class="fnref" href="#fn-1">*1</a></p>',
    '<section class="footnotes">', '<ol>', '<li id="fn-1">footnote((ftft',
    '<a class="fnarrow" href="#fnref-1">↩</a></li>', '</ol>', '</section>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ' code';
  Expected := '<pre>code</pre>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText10;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [' code', ' ~code']);
  Expected := String.Join(WikiLB, ['<pre>code', '~code</pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText11;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [' code', 'text']);
  Expected := String.Join(WikiLB, ['<pre>code</pre>', '<p>text</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText12;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [' code', '~paragraph']);
  Expected := String.Join(WikiLB, ['<pre>code</pre>', '<p>paragraph</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText13;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [' code', '- list1']);
  Expected := String.Join(WikiLB, ['<pre>code</pre>', '<ul class="list1">',
    '<li>list1</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText14;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', ' code']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<pre>code</pre>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText15;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', ' code', ' code']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<pre>code', 'code</pre>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText16;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '-- list2', ' code']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<ul class="list2">', '<li>list2', '<pre>code</pre>', '</li>', '</ul>',
    '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText17;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '-- list2', ' code', '- list1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<ul class="list2">', '<li>list2', '<pre>code</pre>', '</li>', '</ul>',
    '</li>', '<li>list1</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText18;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list', ' code', 'text']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list',
    '<pre>code</pre>', 'text</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText19;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote', ' code']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote</p>',
    '<pre>code</pre>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ' code ';
  Expected := '<pre>code </pre>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText20;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote', ' code', '> quote']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote</p>',
    '<pre>code</pre>', '<p>quote</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText21;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote', ' code', 'text']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote</p>',
    '<pre>code</pre>', '<p>text</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText22;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [': term | detail', ' code']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term</dt>',
    '<dd>detail', '<pre>code</pre>', '</dd>', '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText23;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := #9 + 'code';
  Expected := '<pre>' + #9 + 'code</pre>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText24;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [' code', #9 + 'code']);
  Expected := String.Join(WikiLB, ['<pre>code', #9 + 'code</pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText25;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', #9 + 'code']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<pre>' + #9 + 'code</pre>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [' code', ' code']);
  Expected := String.Join(WikiLB, ['<pre>code', 'code</pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [' code', '  code']);
  Expected := String.Join(WikiLB, ['<pre>code', ' code</pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ' ';
  Expected := '';
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText6;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ' ' + WikiLB + ' ';
  Expected := '<pre></pre>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText7;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ' ' + WikiLB + ' ' + WikiLB + ' ' + WikiLB + ' ';
  Expected := String.Join(WikiLB, ['<pre>', '', '</pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText8;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '  ';
  Expected := '<pre> </pre>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestFormattedText9;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [' code', ' <tag>']);
  Expected := String.Join(WikiLB, ['<pre>code', '&lt;tag&gt;</pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestHeader;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['* header1', '** header2', '*** header3',
    '**** header4']);
  Expected := String.Join(WikiLB, ['<h2>header1</h2>', '<h3>header2</h3>',
    '<h4>header3</h4>', '<h4>* header4</h4>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestHeader2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['* header 1 ', '* ~header1']);
  Expected := String.Join(WikiLB, ['<h2>header 1</h2>', '<h2><p>header1</p>',
    '</h2>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestHeader3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['* header1', 'text']);
  Expected := String.Join(WikiLB, ['<h2>header1</h2>', '<p>text</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestHorizontalrule;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '----';
  Expected := '<hr />' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestHorizontalrule2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '-----';
  Expected := '<hr />' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestHorizontalrule3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '----foo';
  Expected := '<hr />' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEntity;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '&copy; &reg;';
  Expected := '<p>© ®</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEntity2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '&#38; &#101;';
  Expected := '<p>&amp; e</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestEntity3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '&#x26; &#X26; &#x65;';
  Expected := '<p>&amp; &amp;#X26; e</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestInlinePlugin;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '&inline;';
  Expected := '<p>[]</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestInlinePlugin10;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo&inline(arg1,arg2){body};bar';
  Expected := '<p>foo[body][arg1][arg2]bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestInlinePlugin11;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo&inline(arg1,arg2,arg3){body};bar';
  Expected := '<p>foo[body][arg1][arg2]bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestInlinePlugin12;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo&INLINE;bar';
  Expected := '<p>foo[]bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestInlinePlugin13;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo&inline2;bar';
  Expected := '<p>foo&amp;inline2;bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestInlinePlugin2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo&inline;bar';
  Expected := '<p>foo[]bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestInlinePlugin3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo&inline();bar';
  Expected := '<p>foo[]bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestInlinePlugin4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo&inline(arg1);bar';
  Expected := '<p>foo[][arg1]bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestInlinePlugin5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo&inline(arg1,arg2);bar';
  Expected := '<p>foo[][arg1][arg2]bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestInlinePlugin6;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo&inline(arg1,arg2,arg3);bar';
  Expected := '<p>foo[][arg1][arg2]bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestInlinePlugin7;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo&inline{};bar';
  Expected := '<p>foo[]bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestInlinePlugin8;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo&inline{body};bar';
  Expected := '<p>foo[body]bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestInlinePlugin9;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo&inline(arg1){body};bar';
  Expected := '<p>foo[body][arg1]bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestInsert;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '%%%text%%%';
  Expected := '<p><ins>text</ins></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '-list1';
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1</li>',
    '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList10;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '-- list2', '--- list3',
    '---- list4', '--- list3']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<ul class="list2">', '<li>list2', '<ul class="list3">', '<li>list3</li>',
    '</ul>', '</li>', '</ul>', '</li>', '</ul>', '<hr />', '<ul class="list3">',
    '<li>list3</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList11;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '--- list3', '-- list2',
    '- list1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<ul class="list3">', '<li>list3</li>', '</ul>', '<ul class="list2">',
    '<li>list2</li>', '</ul>', '</li>', '<li>list1</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList12;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '--- list3', '- list1',
    '-- list2', '- list1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<ul class="list3">', '<li>list3</li>', '</ul>', '</li>', '<li>list1',
    '<ul class="list2">', '<li>list2</li>', '</ul>', '</li>', '<li>list1</li>',
    '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList13;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', 'sentence', '- list1',
    'sentence']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    'sentence</li>', '<li>list1', 'sentence</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList14;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '- ~list1';
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li><p>list1</p>',
    '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList15;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- ~list1', 'sentence']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li><p>list1',
    'sentence</p>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList16;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- ~list1', '~paragraph']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li><p>list1</p>',
    '<p>paragraph</p>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList17;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '> quote', '- list1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<blockquote>', '<p>quote</p>', '<ul class="list1">', '<li>list1</li>',
    '</ul>', '</blockquote>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList18;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '> quote1', '>> quote2',
    '- list1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<blockquote>', '<p>quote1</p>', '<blockquote>', '<p>quote2</p>',
    '<ul class="list1">', '<li>list1</li>', '</ul>', '</blockquote>',
    '</blockquote>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList19;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '-- list2', '> quote',
    '- list1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<ul class="list2">', '<li>list2', '<blockquote>', '<p>quote</p>',
    '<ul class="list1">', '<li>list1</li>', '</ul>', '</blockquote>', '</li>',
    '</ul>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '- list1';
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1</li>',
    '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList20;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '> quote', '-- list2',
    '- list1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<blockquote>', '<p>quote</p>', '<ul class="list2">', '<li>list2</li>',
    '</ul>', '<ul class="list1">', '<li>list1</li>', '</ul>', '</blockquote>',
    '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList21;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '> quote', '< ', '- list1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<blockquote>', '<p>quote</p>', '</blockquote>', '</li>', '<li>list1</li>',
    '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList22;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '> quote', '< quote end',
    '- list1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<blockquote>', '<p>quote</p>', '</blockquote>', 'quote end</li>',
    '<li>list1</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList23;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '> quote', '< ~quote end',
    '- list1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<blockquote>', '<p>quote</p>', '</blockquote>', '<p>quote end</p>',
    '</li>', '<li>list1</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList24;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '> quote', '< ', '-- list2',
    '- list1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<blockquote>', '<p>quote</p>', '</blockquote>', '<ul class="list2">',
    '<li>list2</li>', '</ul>', '</li>', '<li>list1</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList25;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['+ list1', '++ list2', '+++ list3',
    '++++ list4']);
  Expected := String.Join(WikiLB, ['<ol class="list1">', '<li>list1',
    '<ol class="list2">', '<li>list2', '<ol class="list3">', '<li>list3</li>',
    '<li>+ list4</li>', '</ol>', '</li>', '</ol>', '</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList26;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['+ list1', '-- list2', '+++ list3']);
  Expected := String.Join(WikiLB, ['<ol class="list1">', '<li>list1',
    '<ul class="list2">', '<li>list2', '<ol class="list3">', '<li>list3</li>',
    '</ol>', '</li>', '</ul>', '</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList27;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '++ list2', '--- list3']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<ol class="list2">', '<li>list2', '<ul class="list3">', '<li>list3</li>',
    '</ul>', '</li>', '</ol>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList28;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '-- list2', '++ list2',
    '+ list1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<ul class="list2">', '<li>list2</li>', '</ul>', '<ol class="list2">',
    '<li>list2</li>', '</ol>', '</li>', '</ul>', '<ol class="list1">',
    '<li>list1</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '- list1 ';
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1</li>',
    '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '- list1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1</li>',
    '<li>list1</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '-- list2', '--- list3']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<ul class="list2">', '<li>list2', '<ul class="list3">', '<li>list3</li>',
    '</ul>', '</li>', '</ul>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList6;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '-- list2', '-- list2',
    '- list1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<ul class="list2">', '<li>list2</li>', '<li>list2</li>', '</ul>', '</li>',
    '<li>list1</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList7;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '-- list2', '--- list3',
    '-- list2', '- list1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<ul class="list2">', '<li>list2', '<ul class="list3">', '<li>list3</li>',
    '</ul>', '</li>', '<li>list2</li>', '</ul>', '</li>', '<li>list1</li>',
    '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList8;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '-- list2', '--- list3',
    '- list1']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<ul class="list2">', '<li>list2', '<ul class="list3">', '<li>list3</li>',
    '</ul>', '</li>', '</ul>', '</li>', '<li>list1</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestList9;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- list1', '-- list2', '--- list3',
    '---- list4']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list1',
    '<ul class="list2">', '<li>list2', '<ul class="list3">', '<li>list3</li>',
    '</ul>', '</li>', '</ul>', '</li>', '</ul>', '<hr />']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestNewline;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['text~', 'text']);
  Expected := String.Join(WikiLB, ['<p>text<br />', 'text</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestNewline10;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [':term~|detail~', 'text', ':term|detail']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term~</dt>',
    '<dd>detail<br />', 'text</dd>', '<dt>term</dt>', '<dd>detail</dd>',
    '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestNewline11;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '|item~|';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item~</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestNewline12;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '|~item~|';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<th>item~</th>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestNewline2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'text~';
  Expected := String.Join(WikiLB, ['<p>text<br />', '</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestNewline3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['text~ ', 'text']);
  Expected := String.Join(WikiLB, ['<p>text~', 'text</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestNewline4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['text ~', 'text']);
  Expected := String.Join(WikiLB, ['<p>text <br />', 'text</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestNewline5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['text~', 'text~', 'text']);
  Expected := String.Join(WikiLB, ['<p>text<br />', 'text<br />', 'text</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestNewline6;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['text~' + WikiLB + '~', 'text']);
  Expected := String.Join(WikiLB, ['<p>text<br />', '<br />', 'text</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestNewline7;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['-list~', '-list']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list<br />',
    '</li>', '<li>list</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestNewline8;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['-list~', 'text', '-list']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list<br />',
    'text</li>', '<li>list</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestNewline9;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['-list~', 'text~', '-list']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list<br />',
    'text<br />', '</li>', '<li>list</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestPageName;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[[Test Page]]';
  Expected := '<p><a href="https://TestPage">Test Page</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestPageName2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[[Test Page2]]';
  Expected := '<p>[[Test Page2]]</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestPageName3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[[Alias>Test Page]]';
  Expected := '<p><a href="https://TestPage">Alias</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestPageName4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[[Alias>Test Page2]]';
  Expected := '<p>[[Alias&gt;Test Page2]]</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestPageName5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[[Alias>https://www.example.com]]';
  Expected := '<p><a href="https://www.example.com">Alias</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestParagraph;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['foo', 'bar', '', 'baz']);
  Expected := String.Join(WikiLB, ['<p>foo', 'bar</p>', '<p>baz</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestParagraph2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['~-foo', '~-bar']);
  Expected := String.Join(WikiLB, ['<p>-foo</p>', '<p>-bar</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestParagraph3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['~-foo', 'bar']);
  Expected := String.Join(WikiLB, ['<p>-foo', 'bar</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestParagraph4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo  ';
  Expected := '<p>foo</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestParagraph5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['foo  ', 'bar  ']);
  Expected := String.Join(WikiLB, ['<p>foo', 'bar</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestParagraph6;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo' + #9;
  Expected := '<p>foo</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['>quote', '> quote']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote</p>',
    '<p>quote</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote10;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote1', '>>> quote3', '>>> quote3',
    '> quote1']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote1</p>',
    '<blockquote>', '<p>quote3</p>', '<p>quote3</p>', '</blockquote>',
    '<p>quote1</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote11;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote1', '>>> quote3', '>>> quote3',
    '>> quote2', '> quote1']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote1</p>',
    '<blockquote>', '<p>quote3</p>', '<p>quote3</p>', '</blockquote>',
    '<blockquote>', '<p>quote2</p>', '</blockquote>', '<p>quote1</p>',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote12;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote1', '>>> quote3', '>>> quote3',
    '>> quote2', '>>> quote3', '> quote1']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote1</p>',
    '<blockquote>', '<p>quote3</p>', '<p>quote3</p>', '</blockquote>',
    '<blockquote>', '<p>quote2</p>', '<blockquote>', '<p>quote3</p>',
    '</blockquote>', '</blockquote>', '<p>quote1</p>', '</blockquote>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote13;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote1', '>> quote2', '>>> quote3',
    '>>>> quote4', '> quote1']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote1</p>',
    '<blockquote>', '<p>quote2</p>', '<blockquote>', '<p>quote3</p>',
    '<p>&gt; quote4</p>', '</blockquote>', '</blockquote>', '<p>quote1</p>',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote14;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote1', '> > quote1']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote1</p>',
    '<p>&gt; quote1</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote15;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> foo', 'bar', '>> foo', 'bar',
    '< baz', 'bar']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>foo', 'bar</p>',
    '<blockquote>', '<p>foo', 'bar</p>', '</blockquote>', '</blockquote>',
    '<p>baz', 'bar</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote16;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> foo', 'bar', '>> foo', 'bar',
    '<< baz', 'bar']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>foo', 'bar</p>',
    '<blockquote>', '<p>foo', 'bar</p>', '</blockquote>', '<p>baz', 'bar</p>',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote17;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote ', '> quote']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote</p>',
    '<p>quote</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote18;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> ~quote', '> quote']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote</p>',
    '<p>quote</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote1', 'quote0', 'quote0', '> quote1']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote1', 'quote0',
    'quote0</p>', '<p>quote1</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote1', 'quote0', 'quote0', '> quote1']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote1', 'quote0',
    'quote0</p>', '<p>quote1</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote1', '>> quote2', '> quote1']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote1</p>',
    '<blockquote>', '<p>quote2</p>', '</blockquote>', '<p>quote1</p>',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote1', '~paragraph', '>> quote2',
    '~paragraph']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote1</p>',
    '<p>paragraph</p>', '<blockquote>', '<p>quote2</p>', '<p>paragraph</p>',
    '</blockquote>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote6;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote1', '~paragraph', '>> quote2',
    '* head']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote1</p>',
    '<p>paragraph</p>', '<blockquote>', '<p>quote2</p>', '</blockquote>',
    '</blockquote>', '<h2>head</h2>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote7;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote1', '>> quote2', '>>> quote3',
    '>> quote2', '> quote1']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote1</p>',
    '<blockquote>', '<p>quote2</p>', '<blockquote>', '<p>quote3</p>',
    '</blockquote>', '<p>quote2</p>', '</blockquote>', '<p>quote1</p>',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote8;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote1', '>> quote2', '>>> quote3',
    '> quote1']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote1</p>',
    '<blockquote>', '<p>quote2</p>', '<blockquote>', '<p>quote3</p>',
    '</blockquote>', '</blockquote>', '<p>quote1</p>', '</blockquote>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestQuote9;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> quote1', '>>> quote3', '> quote1']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote1</p>',
    '<blockquote>', '<p>quote3</p>', '</blockquote>', '<p>quote1</p>',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header | ~header|', '| item   | item   |',
    '| item   | item   |']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<th>header</th>', '<td>~header</td>', '</tr>', '<tr>', '<td>item</td>',
    '<td>item</td>', '</tr>', '<tr>', '<td>item</td>', '<td>item</td>', '</tr>',
    '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable10;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['| item  | item  |', '| footer| footer|f']);
  Expected := String.Join(WikiLB, ['<table>', '<tfoot>', '<tr>',
    '<td>footer</td>', '<td>footer</td>', '</tr>', '</tfoot>', '<tbody>',
    '<tr>', '<td>item</td>', '<td>item</td>', '</tr>', '</tbody>', '</table>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable11;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['| item  | item  |', '| footer| footer|F']);
  Expected := String.Join(WikiLB, ['<table>', '<tfoot>', '<tr>',
    '<td>footer</td>', '<td>footer</td>', '</tr>', '</tfoot>', '<tbody>',
    '<tr>', '<td>item</td>', '<td>item</td>', '</tr>', '</tbody>', '</table>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable12;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|header|header|h', '|item  |item  |',
    '|footer|footer|f']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<td>header</td>', '<td>header</td>', '</tr>', '</thead>', '<tfoot>',
    '<tr>', '<td>footer</td>', '<td>footer</td>', '</tr>', '</tfoot>',
    '<tbody>', '<tr>', '<td>item</td>', '<td>item</td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable13;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|item  |item  |', '|footer|footer|fe']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '<td>item</td>', '</tr>', '</tbody>', '</table>',
    '<p>|footer|footer|fe</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable14;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|item  |item  |', '|footer|footer|f ']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '<td>item</td>', '</tr>', '</tbody>', '</table>',
    '<p>|footer|footer|f</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable15;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|item  |item  |', '|footer|footer| f']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '<td>item</td>', '</tr>', '</tbody>', '</table>',
    '<p>|footer|footer| f</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable16;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|item  |item  |', '|footer|footer|f',
    '|item  |item  |']);
  Expected := String.Join(WikiLB, ['<table>', '<tfoot>', '<tr>',
    '<td>footer</td>', '<td>footer</td>', '</tr>', '</tfoot>', '<tbody>',
    '<tr>', '<td>item</td>', '<td>item</td>', '</tr>', '<tr>', '<td>item</td>',
    '<td>item</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable17;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|item  |item  |', '|footer|footer|f',
    '|item  |item  |', '|footer|footer|f']);
  Expected := String.Join(WikiLB, ['<table>', '<tfoot>', '<tr>',
    '<td>footer</td>', '<td>footer</td>', '</tr>', '<tr>', '<td>footer</td>',
    '<td>footer</td>', '</tr>', '</tfoot>', '<tbody>', '<tr>', '<td>item</td>',
    '<td>item</td>', '</tr>', '<tr>', '<td>item</td>', '<td>item</td>', '</tr>',
    '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable18;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h',
    '| item  | item  | item  |']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>header</th>', '<th>header</th>', '</tr>', '</thead>', '</table>',
    '<table>', '<tbody>', '<tr>', '<td>item</td>', '<td>item</td>',
    '<td>item</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable19;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h', '||item|', '|item||']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>header</th>', '<th>header</th>', '</tr>', '</thead>', '<tbody>',
    '<tr>', '<td></td>', '<td>item</td>', '</tr>', '<tr>', '<td>item</td>',
    '<td></td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h', '| item  | item  |',
    '| item  | item  |']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>header</th>', '<th>header</th>', '</tr>', '</thead>', '<tbody>',
    '<tr>', '<td>item</td>', '<td>item</td>', '</tr>', '<tr>', '<td>item</td>',
    '<td>item</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable20;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h', '| item  | item  ',
    '| item  | item  |']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>header</th>', '<th>header</th>', '</tr>', '</thead>', '</table>',
    '<p>| item  | item</p>', '<table>', '<tbody>', '<tr>', '<td>item</td>',
    '<td>item</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable21;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|LEFT:left|CENTER:center|RIGHT:right|',
    '|LEFT: left|CENTER: center|RIGHT: right|',
    '| LEFT:left| CENTER :center|RIGHT :right|',
    '|left:left|center:center|right:right|']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td style="text-align:left;">left</td>',
    '<td style="text-align:center;">center</td>',
    '<td style="text-align:right;">right</td>', '</tr>', '<tr>',
    '<td style="text-align:left;">left</td>',
    '<td style="text-align:center;">center</td>',
    '<td style="text-align:right;">right</td>', '</tr>', '<tr>',
    '<td>LEFT:left</td>', '<td>CENTER :center</td>', '<td>RIGHT :right</td>',
    '</tr>', '<tr>', '<td>left:left</td>', '<td>center:center</td>',
    '<td>right:right</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable22;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '|LEFT:CENTER:left center|';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td style="text-align:center;">left center</td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable23;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '|CENTER:LEFT:center left|';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td style="text-align:left;">center left</td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable24;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '|CENTER: LEFT:center left|';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td style="text-align:center;">LEFT:center left</td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable25;
var
  Expected: String;
  Sentence: String;
begin
  Sentence :=
    '|COLOR(#f00):item|COLOR(#f00):BGCOLOR(#fff):item|RIGHT:COLOR(#f00):BGCOLOR(#fff):item|';
  // NOTE: The sequence of style attribute value is arranged.
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td style="color:#f00;">item</td>',
    '<td style="background-color:#fff; color:#f00;">item</td>',
    '<td style="background-color:#fff; color:#f00; text-align:right;">item</td>',
    '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable26;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|BGCOLOR(#f00):COLOR(#000):item|',
    '|COLOR(#000):BGCOLOR(#f00):item|']);
  // NOTE: The sequence of style attribute value is arranged.
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td style="background-color:#f00; color:#000;">item</td>', '</tr>', '<tr>',
    '<td style="background-color:#f00; color:#000;">item</td>', '</tr>',
    '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable27;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|COLOR(#12345):item|', '|COLOR(0#0):item|',
    '|COLOR(red):item|', '|COLOR(rad):item|', '|COLOR(red;):item|',
    '|COLOR("red"):item|']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td style="color:#12345;">item</td>', '</tr>', '<tr>',
    '<td>COLOR(0#0):item</td>', '</tr>', '<tr>',
    '<td style="color:red;">item</td>', '</tr>', '<tr>',
    '<td style="color:rad;">item</td>', '</tr>', '<tr>',
    '<td>COLOR(red;):item</td>', '</tr>', '<tr>', '<td>COLOR("red"):item</td>',
    '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable28;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '|SIZE(24):BOLD:item|';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td style="font-size:24px; font-weight:bold;">item</td>', '</tr>',
    '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable29;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '|SIZE(2e):BOLD:item|';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>SIZE(2e):BOLD:item</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|H', '| item  | item  |']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>header</th>', '<th>header</th>', '</tr>', '</thead>', '<tbody>',
    '<tr>', '<td>item</td>', '<td>item</td>', '</tr>', '</tbody>', '</table>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable30;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '|SIZE(24):ITALIC:BOLD:item|';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td style="font-size:24px;">ITALIC:BOLD:item</td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable31;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '|SIZE(24:BOLD:item|';
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>SIZE(24:BOLD:item</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable32;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h', '|>| item  |',
    '| item  | item  |']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>header</th>', '<th>header</th>', '</tr>', '</thead>', '<tbody>',
    '<tr>', '<td colspan="2">item</td>', '</tr>', '<tr>', '<td>item</td>',
    '<td>item</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable33;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|~header|h', '|>|>|item|']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>header</th>', '<th>header</th>', '<th>header</th>', '</tr>',
    '</thead>', '<tbody>', '<tr>', '<td colspan="3">item</td>', '</tr>',
    '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable34;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h', '|> |item|',
    '|item|item|']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>header</th>', '<th>header</th>', '</tr>', '</thead>', '<tbody>',
    '<tr>', '<td>&gt;</td>', '<td>item</td>', '</tr>', '<tr>', '<td>item</td>',
    '<td>item</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable35;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h', '| >|item|',
    '|item|item|']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>header</th>', '<th>header</th>', '</tr>', '</thead>', '<tbody>',
    '<tr>', '<td>&gt;</td>', '<td>item</td>', '</tr>', '<tr>', '<td>item</td>',
    '<td>item</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable36;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h', '|item|>|',
    '|item|item|']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>header</th>', '<th>header</th>', '</tr>', '</thead>', '<tbody>',
    '<tr>', '<td>item</td>', '</tr>', '<tr>', '<td>item</td>', '<td>item</td>',
    '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable37;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h', '|~|item|',
    '|item|item|']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th rowspan="2">header</th>', '<th>header</th>', '</tr>', '<tr>',
    '<td>item</td>', '</tr>', '</thead>', '<tbody>', '<tr>', '<td>item</td>',
    '<td>item</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable38;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h', '|~|item|',
    '|~|item|']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th rowspan="3">header</th>', '<th>header</th>', '</tr>', '<tr>',
    '<td>item</td>', '</tr>', '<tr>', '<td>item</td>', '</tr>', '</thead>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable39;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h', '|~|item|',
    '|item|~|']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th rowspan="2">header</th>', '<th>header</th>', '</tr>', '<tr>',
    '<td rowspan="2">item</td>', '</tr>', '<tr>', '<td>item</td>', '</tr>',
    '</thead>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['| item  | item  |', '|~header|~header|h',
    '| item  | item  |']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>header</th>', '<th>header</th>', '</tr>', '</thead>', '<tbody>',
    '<tr>', '<td>item</td>', '<td>item</td>', '</tr>', '<tr>', '<td>item</td>',
    '<td>item</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable40;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|item|item|', '|~|item|', '|item|item|',
    '|item|~|']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td rowspan="2">item</td>', '<td>item</td>', '</tr>', '<tr>',
    '<td>item</td>', '</tr>', '<tr>', '<td>item</td>',
    '<td rowspan="2">item</td>', '</tr>', '<tr>', '<td>item</td>', '</tr>',
    '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable41;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~|item|', '|item|item|']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '</tr>', '<tr>', '<td>item</td>', '<td>item</td>', '</tr>',
    '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable42;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|footer|footer|f', '|~|item|',
    '|item|item|']);
  Expected := String.Join(WikiLB, ['<table>', '<tfoot>', '<tr>',
    '<td rowspan="2">footer</td>', '<td>footer</td>', '</tr>', '<tr>',
    '<td>item</td>', '</tr>', '</tfoot>', '<tbody>', '<tr>', '<td>item</td>',
    '<td>item</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable43;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h', '| ~|item|',
    '|item|item|']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>header</th>', '<th>header</th>', '</tr>', '</thead>', '<tbody>',
    '<tr>', '<td>~</td>', '<td>item</td>', '</tr>', '<tr>', '<td>item</td>',
    '<td>item</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable44;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h', '|~ |item|',
    '|item|item|']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>header</th>', '<th>header</th>', '</tr>', '</thead>', '<tbody>',
    '<tr>', '<th></th>', '<td>item</td>', '</tr>', '<tr>', '<td>item</td>',
    '<td>item</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable45;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~|item|', '|item|item|']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '</tr>', '<tr>', '<td>item</td>', '<td>item</td>', '</tr>',
    '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable46;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h', '|>| item  |',
    '|~| item  |']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>header</th>', '<th>header</th>', '</tr>', '</thead>', '<tbody>',
    '<tr>', '<td colspan="2">item</td>', '</tr>', '<tr>', '<td>item</td>',
    '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable47;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|item|item|', '|>|item|', '|item|~|']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '<td>item</td>', '</tr>', '<tr>',
    '<td rowspan="2" colspan="2">item</td>', '</tr>', '<tr>', '<td>item</td>',
    '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable48;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['-list', '|item|', '|item|', '-list']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list', '<table>',
    '<tbody>', '<tr>', '<td>item</td>', '</tr>', '<tr>', '<td>item</td>',
    '</tr>', '</tbody>', '</table>', '</li>', '<li>list</li>', '</ul>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable49;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [':term|detail', '|item|', '|item|',
    ':term|detail']);
  Expected := String.Join(WikiLB, ['<dl class="list1">', '<dt>term</dt>',
    '<dd>detail', '<table>', '<tbody>', '<tr>', '<td>item</td>', '</tr>',
    '<tr>', '<td>item</td>', '</tr>', '</tbody>', '</table>', '</dd>',
    '<dt>term</dt>', '<dd>detail</dd>', '</dl>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h', '|~header| item  |']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>header</th>', '<th>header</th>', '</tr>', '</thead>', '<tbody>',
    '<tr>', '<th>header</th>', '<td>item</td>', '</tr>', '</tbody>', '</table>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable50;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['>quote', '|item|', '|item|', '>quote']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote</p>', '<table>',
    '<tbody>', '<tr>', '<td>item</td>', '</tr>', '<tr>', '<td>item</td>',
    '</tr>', '</tbody>', '</table>', '<p>quote</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable51;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|LEFT:   |RIGHT:  |c',
    '|itemitem|itemitem|', '|item    |item    |']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td style="text-align:left;">itemitem</td>',
    '<td style="text-align:right;">itemitem</td>', '</tr>', '<tr>',
    '<td style="text-align:left;">item</td>',
    '<td style="text-align:right;">item</td>', '</tr>', '</tbody>', '</table>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable52;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|        |RIGHT:  |c',
    '|itemitem|itemitem|', '|item    |SIZE(20):item|', '|item    |CENTER:item|',
    '|item    |item|']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>itemitem</td>', '<td style="text-align:right;">itemitem</td>', '</tr>',
    '<tr>', '<td>item</td>',
    '<td style="font-size:20px; text-align:right;">item</td>', '</tr>', '<tr>',
    '<td>item</td>', '<td style="text-align:center;">item</td>', '</tr>',
    '<tr>', '<td>item</td>', '<td style="text-align:right;">item</td>', '</tr>',
    '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable53;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|LEFT:   |RIGHT:  |c',
    '|itemitem|itemitem|', '|item    |item    |', '|RIGHT:  |LEFT:   |c',
    '|item    |item    |h', '|        |NULL:   |c', '|item    |item    |']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<td style="text-align:right;">item</td>',
    '<td style="text-align:left;">item</td>', '</tr>', '</thead>', '<tbody>',
    '<tr>', '<td style="text-align:left;">itemitem</td>',
    '<td style="text-align:right;">itemitem</td>', '</tr>', '<tr>',
    '<td style="text-align:left;">item</td>',
    '<td style="text-align:right;">item</td>', '</tr>', '<tr>', '<td>item</td>',
    '<td>item</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable54;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|LEFT:   |RIGHT:  |c',
    '|LEFT:   |RIGHT:  |c']);
  Expected := '';
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable55;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|BOLD:|RIGHT:|c', '|item|~|',
    '|item|item|']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td>item</td>', '<td>item</td>', '</tr>', '</tbody>', '</table>'])
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable56;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|BOLD:|RIGHT:|c', '|SIZE(20):|~|',
    '|item|item|']);
  Expected := String.Join(WikiLB, ['<table>', '<tbody>', '<tr>',
    '<td style="font-size:20px;">item</td>', '<td>item</td>', '</tr>',
    '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable57;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['-list', '|item|', '|item|item|']);
  Expected := String.Join(WikiLB, ['<ul class="list1">', '<li>list', '<table>',
    '<tbody>', '<tr>', '<td>item</td>', '</tr>', '</tbody>', '</table>',
    '<table>', '<tbody>', '<tr>', '<td>item</td>', '<td>item</td>', '</tr>',
    '</tbody>', '</table>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable6;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h ', '|item|item|']);
  Expected := String.Join(WikiLB, ['<p>|~header|~header|h</p>', '<table>',
    '<tbody>', '<tr>', '<td>item</td>', '<td>item</td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable7;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|he', '| item  | item  |']);
  Expected := String.Join(WikiLB, ['<p>|~header|~header|he</p>', '<table>',
    '<tbody>', '<tr>', '<td>item</td>', '<td>item</td>', '</tr>', '</tbody>',
    '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable8;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~header|~header|h', '| item  | item  |',
    '| header| header|h', '| item  | item  |']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>header</th>', '<th>header</th>', '</tr>', '<tr>', '<td>header</td>',
    '<td>header</td>', '</tr>', '</thead>', '<tbody>', '<tr>', '<td>item</td>',
    '<td>item</td>', '</tr>', '<tr>', '<td>item</td>', '<td>item</td>', '</tr>',
    '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTable9;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['|~footer|~footer|f', '|item|item|']);
  Expected := String.Join(WikiLB, ['<table>', '<tfoot>', '<tr>',
    '<th>footer</th>', '<th>footer</th>', '</tr>', '</tfoot>', '<tbody>',
    '<tr>', '<td>item</td>', '<td>item</td>', '</tr>', '</tbody>', '</table>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTextAlign;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['LEFT:text', 'CENTER:text', 'RIGHT:text',
    'RIGHT:text']);
  Expected := String.Join(WikiLB, ['<div style="text-align:left">text</div>',
    '<div style="text-align:center">text</div>',
    '<div style="text-align:right">text</div>',
    '<div style="text-align:right">text</div>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTextAlign2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'right:text';
  Expected := '<p>right:text</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTextAlign3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['RIGHT:text', 'text']);
  Expected := String.Join(WikiLB, ['<div style="text-align:right">text',
    'text</div>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTextAlign4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['RIGHT:text', '~text', 'text']);
  Expected := String.Join(WikiLB, ['<div style="text-align:right">text</div>',
    '<p>text', 'text</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTextAlign5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'RIGHT: text ';
  Expected := '<pre>text </pre>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTextAlign6;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'RIGHT:~text';
  Expected := '<p>text</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTextAlign7;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'RIGHT:';
  Expected := '';
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TPukiwikiConverterTest.TestTextAlign8;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['>quote', 'RIGHT:text', '>quote']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>quote</p>',
    '<div style="text-align:right">text</div>', '<p>quote</p>', '</blockquote>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

end.
