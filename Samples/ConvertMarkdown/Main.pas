unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private 宣言 }
  public
    { Public 宣言 }
  end;

var
  Form1: TForm1;

implementation

uses
  MyWiki.WikiConverter, MyWiki.MarkdownConverter;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Markdown: String;
  Html: String;
  Converter: TWikiConverter;
begin
  Markdown := Memo1.Text;
  Converter := TMarkdownConverter.Create;
  try
    Html := Converter.WikiToHtml(Markdown);
    Memo2.Text := Html;
  finally
    Converter.Free;
  end;
end;

end.
