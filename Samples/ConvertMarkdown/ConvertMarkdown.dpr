program ConvertMarkdown;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  MyWiki.Footnote in '..\..\MyWiki\MyWiki.Footnote.pas',
  MyWiki.Header in '..\..\MyWiki\MyWiki.Header.pas',
  MyWiki.HtmlEntities in '..\..\MyWiki\MyWiki.HtmlEntities.pas',
  MyWiki.MarkdownConverter in '..\..\MyWiki\MyWiki.MarkdownConverter.pas',
  MyWiki.PukiwikiConverter in '..\..\MyWiki\MyWiki.PukiwikiConverter.pas',
  MyWiki.TextConverter in '..\..\MyWiki\MyWiki.TextConverter.pas',
  MyWiki.WikiConverter in '..\..\MyWiki\MyWiki.WikiConverter.pas',
  MyWiki.WikiNode in '..\..\MyWiki\MyWiki.WikiNode.pas',
  MyWiki.WikiPlugin in '..\..\MyWiki\MyWiki.WikiPlugin.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
