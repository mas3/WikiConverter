unit TextConverterTest;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  MyWiki.WikiConverter, MyWiki.TextConverter;

type

  [TestFixture]
  TTextConverterTest = class
  private
    FConverter: TWikiConverter;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestWikiToHtmlLinkDomain;
    [Test]
    procedure TestWikiToHtmlSimpleSentence;
    [Test]
    procedure TestWikiToHtmlEncode;
  end;

implementation

procedure TTextConverterTest.Setup;
begin
  FConverter := TTextConverter.Create;
end;

procedure TTextConverterTest.TearDown;
begin
  FConverter.Free;
end;

procedure TTextConverterTest.TestWikiToHtmlEncode;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<h1>Header</h1>';
  Expected := Format('<pre class="text">%s</pre>' + WikiLB,
    ['&lt;h1&gt;Header&lt;/h1&gt;']);
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TTextConverterTest.TestWikiToHtmlLinkDomain;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'Link test. https://example.com is sample URL.';
  Expected :=
    'Link test. <a href="https://example.com">https://example.com</a> is sample URL.';
  Expected := Format('<pre class="text">%s</pre>' + WikiLB, [Expected]);
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TTextConverterTest.TestWikiToHtmlSimpleSentence;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'This is test centence.';
  Expected := Format('<pre class="text">%s</pre>' + WikiLB, [Sentence]);
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

initialization

TDUnitX.RegisterTestFixture(TTextConverterTest);

end.
