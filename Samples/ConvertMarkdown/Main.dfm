object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 561
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 393
    Height = 528
    Align = alLeft
    Lines.Strings = (
      '# header1'
      ''
      '## header2'
      ''
      '### header3'
      ''
      '#### header4'
      ''
      '##### header5'
      ''
      '###### header6'
      ''
      ''
      'header1'
      '======='
      ''
      'header2'
      '-------'
      ''
      ''
      '# Themantic breaks'
      ''
      '---'
      ''
      '# Code blocks'
      ''
      '    code block.'
      '    code block.'
      ''
      '```c'
      '#include <stdio.h>'
      ''
      'int main(void) {'
      '    printf("Hello, World!\n");'
      '    return 0;'
      '}'
      '```'
      ''
      ''
      '# HTML blocks'
      ''
      '<details>'
      '<summary>Summary</summary>'
      'Details'
      '</details>'
      ''
      ''
      '# Link reference definistions'
      ''
      '[foo]: /foopage "title"'
      ''
      '[foo]'
      ''
      ''
      '# Paragraphs'
      ''
      'This is Markdown to HTML test.'
      'Please push "Convert" button.'
      ''
      ''
      '# Tables'
      ''
      '| header1 | header2 |'
      '| ------- | ------- |'
      '| item1   | item2   |'
      ''
      ''
      '# block quotes'
      ''
      '> foo'
      '> bar'
      ''
      ''
      '# List items'
      ''
      '- foo'
      '- bar'
      '  - bar1'
      '  - bar2'
      ''
      '1. foo'
      '2. bar'
      '   1. bar'
      '   2. bar'
      ''
      ''
      '# Task list items'
      ''
      '- [x] foo'
      '- [ ] bar'
      ''
      ''
      '# Backslash escapes'
      ''
      '\-foo'
      ''
      ''
      '# Entity and numeric character references'
      ''
      '&copy; &#xa9; &#169;'
      ''
      ''
      '# Code spans'
      ''
      'You can embed the `code` in the text.'
      ''
      ''
      '# Emphasis and strong emphasis'
      ''
      '*empasis*'
      ''
      '**strong emphasis**'
      ''
      ''
      '# Strikethrough'
      ''
      '~~Strikethrough~~'
      ''
      ''
      '# Links'
      ''
      '[Link](/uri "title")'
      ''
      ''
      '# Images'
      ''
      '![Image](/image.png "title")'
      ''
      ''
      '# Raw HTML'
      ''
      'This is <a href="https://example.com">Example</a>.'
      ''
      ''
      '# Hard line breaks'
      ''
      'foo  '
      'baz'
      ''
      'foo\'
      'baz')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 393
    Top = 0
    Width = 391
    Height = 528
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Button1: TButton
    Left = 0
    Top = 528
    Width = 784
    Height = 33
    Align = alBottom
    Caption = 'Convert'
    TabOrder = 2
    OnClick = Button1Click
  end
end
