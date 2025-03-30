unit MyWiki.WikiPlugin;

interface

uses
  System.SysUtils, System.Classes, System.JSON.Serializers, System.IOUtils,
  System.Math;

type
  [JsonSerialize(TJsonMemberSerialization.Public)]
  TPluginInfo = class(TObject)
  private
    FAuthor: String;
    FExample: String;
    FName: String;
    FNote: String;
    FUsage: String;
    FVersion: String;
  public
    property Author: String read FAuthor write FAuthor;
    property Example: String read FExample write FExample;
    property Name: String read FName write FName;
    property Note: String read FNote write FNote;
    property Usage: String read FUsage write FUsage;
    property Version: String read FVersion write FVersion;
  end;

  [JsonSerialize(TJsonMemberSerialization.Public)]
  TPluginProcess = class(TObject)
  private
    FKind: String;
    FFormat: TArray<String>;
    FParseMultiline: Boolean;
    FValue: String;
  public
    property Kind: String read FKind write FKind;
    property Format: TArray<String> read FFormat write FFormat;
    property ParseMultiline: Boolean read FParseMultiline write FParseMultiline;
    property Value: String read FValue write FValue;
  end;

  [JsonSerialize(TJsonMemberSerialization.Public)]
  TPluginSettings = class(TObject)
  private
    FFormatVersion: Integer;
    FPlugin: TPluginInfo;
    FProcess: TPluginProcess;
  public
    property FormatVersion: Integer read FFormatVersion write FFormatVersion;
    property Plugin: TPluginInfo read FPlugin write FPlugin;
    property Process: TPluginProcess read FProcess write FProcess;

    destructor Destroy; override;
  end;

  TWikiPlugin = class(TObject)
  private
    FErrorMesssage: String;
    FFilepath: String;
    FFixed: Boolean;
    FPluginSettings: TPluginSettings;

    function GetName: String;
    function ParseRawMultiline: Boolean;
  public
    function EmbedData(const Template: String;
      const Arguments: TStringList): String;
    function Execute(const ArgumentString: String;
      const BodyString: String): String;
    function ExecuteMultiline(const ArgumentString: String;
      const MultilineString: String): String;
    procedure LoadFromFile(const Filepath: String);
    procedure SetJsonString(const JsonString: String);

    constructor Create;
    destructor Destroy; override;

    property Filepath: String read FFilepath;
    property Name: String read GetName;
    property ParseMultiline: Boolean read ParseRawMultiline;
  end;

implementation

const
  MsgFormatVersionInvalid = 'FormatVersion invalid.[%d]';
  MsgKindInvalid = 'Kind invalid.[%s]';

  { TPluginSettings }

destructor TPluginSettings.Destroy;
begin
  FreeAndNil(FPlugin);
  FreeAndNil(FProcess);

  inherited;
end;

{ TWikiPlugin }

constructor TWikiPlugin.Create;
begin
  FFilepath := '';
end;

destructor TWikiPlugin.Destroy;
begin
  FreeAndNil(FPluginSettings);

  inherited;
end;

/// <summary>
/// Embed data into the template
/// </summary>
/// <param name="Template">Template string</param>
/// <param name="Arguments">Arguments</param>
/// <returns>Embedded string</returns>
/// <remarks>
/// The template string is embedded with the arguments in the form of {0}, {1}, {2}, ...
/// </remarks>
function TWikiPlugin.EmbedData(const Template: String;
  const Arguments: TStringList): String;
var
  CPos, EPos, Copied: Integer;
  Ch: Char;
  StrLength: Integer;
  Escaped: Boolean;
begin
  if Template = '' then
    Exit('');

  StrLength := Length(Template);
  CPos := 1;
  Copied := 0;
  Result := '';
  Escaped := False;

  while CPos <= StrLength do
  begin
    Ch := Template[CPos];
    if Ch = '\' then
    begin
      Escaped := not Escaped;
      Continue;
    end;

    if Escaped then
    begin
      Escaped := False;
      Inc(CPos);
      Continue;
    end;

    if Ch = '{' then
    begin
      var
        Ch2: Char;
      var
        Buff: String := '';
      var
        NumString: String := '';
      EPos := CPos + 1;
      while EPos <= StrLength do
      begin
        Ch2 := Template[EPos];
        if CharInSet(Ch2, ['0' .. '9']) then
        begin
          Buff := Buff + Ch2;
          Inc(EPos);
          Continue;
        end;

        if Ch2 = '}' then
        begin
          NumString := Buff;
          Break;
        end;

        Break;
      end;

      if NumString <> '' then
      begin
        var
          Num: Integer := NumString.ToInteger;
        var
          Replaced: String := '';
        if Num < Arguments.Count then
          Replaced := Arguments[Num];

        Result := Result + Copy(Template, Copied + 1, CPos - Copied - 1);
        Result := Result + Replaced;
        CPos := EPos + 1;
        Copied := CPos - 1;
        Continue;
      end;
    end;

    Inc(CPos);
  end;
  Result := Result + Copy(Template, Copied + 1, CPos - Copied);
end;

/// <summary>
/// Execute the plugin
/// </summary>
/// <param name="ArgumentString">Argument string</param>
/// <param name="BodyString">Body string</param>
/// <returns>Result string</returns>
function TWikiPlugin.Execute(const ArgumentString, BodyString: String): String;
var
  Arguments: TStringList;
  Template: String;
begin
  // The following usage is assumed for PukiWiki.
  // &plugin(arg1,arg2){body};
  if FErrorMesssage <> '' then
    Exit(FErrorMesssage);

  if FFixed then
    Exit(FPluginSettings.FProcess.Value);

  Arguments := TStringList.Create;
  try
    Arguments.StrictDelimiter := True;
    Arguments.DelimitedText := ArgumentString;
    Arguments.Insert(0, BodyString);

    Template := FPluginSettings.Process.Format
      [Min(Arguments.Count, Length(FPluginSettings.Process.Format)) - 1];

    Result := EmbedData(Template, Arguments);
  finally
    Arguments.Free;
  end;
end;

/// <summary>
/// Execute the plugin with multiline string
/// </summary>
/// <param name="ArgumentString">Argument string</param>
/// <param name="MultilineString">Multiline string</param>
/// <returns>Result string</returns>
function TWikiPlugin.ExecuteMultiline(const ArgumentString: String;
  const MultilineString: String): String;
var
  Arguments: TStringList;
  Template: String;
begin
  // The following usage is assumed for PukiWiki.
  // #plugin(arg1,arg2){{
  // multiline
  // multiline
  // }}
  // and consider the multiline string as a last argument
  if FErrorMesssage <> '' then
    Exit(FErrorMesssage);

  if FFixed then
    Exit(FPluginSettings.FProcess.Value);

  Arguments := TStringList.Create;
  try
    Arguments.StrictDelimiter := True;
    Arguments.DelimitedText := ArgumentString;
    Arguments.Insert(0, '');

    if MultilineString <> '' then
      Arguments.Add(MultilineString);

    Template := FPluginSettings.Process.Format
      [Min(Arguments.Count, Length(FPluginSettings.Process.Format)) - 1];

    Result := EmbedData(Template, Arguments);
  finally
    Arguments.Free;
  end;
end;

function TWikiPlugin.GetName: String;
begin
  if FPluginSettings = nil then
    Exit('');

  Result := FPluginSettings.Plugin.Name;
end;

function TWikiPlugin.ParseRawMultiline: Boolean;
begin
  if FPluginSettings = nil then
    Exit(True);

  Result := FPluginSettings.FProcess.ParseMultiline;
end;

/// <summary>
/// Load plugin settings from file
/// </summary>
/// <param name="Filepath">File path</param>
/// <remarks>
/// The file is json format.
/// </remarks>
procedure TWikiPlugin.LoadFromFile(const Filepath: String);
var
  JsonString: String;
begin
  try
    JsonString := Tfile.ReadAllText(Filepath, TEncoding.UTF8);
    SetJsonString(JsonString);
    FFilepath := Filepath;
  except
    on E: Exception do
      FErrorMesssage := E.Message;
  end;
end;

/// <summary>
/// Set plugin settings from json string
/// </summary>
/// <param name="JsonString">Json string</param>
/// <remarks>
/// The json string is assumed to be in the format of TPluginSettings.
/// </remarks>
procedure TWikiPlugin.SetJsonString(const JsonString: String);
var
  Serializer: TJsonSerializer;
begin
  FErrorMesssage := '';
  FFixed := True;
  FreeAndNil(FPluginSettings);

  Serializer := TJsonSerializer.Create;
  try
    try
      FPluginSettings := Serializer.Deserialize<TPluginSettings>(JsonString);
      if FPluginSettings.FFormatVersion <> 1 then
      begin
        FErrorMesssage := Format(MsgFormatVersionInvalid,
          [FPluginSettings.FFormatVersion]);
      end;

      var
        Kind: String := LowerCase(FPluginSettings.Process.Kind);
      if (Kind <> 'fixed') and (Kind <> 'embedded') then
      begin
        FErrorMesssage := Format(MsgKindInvalid,
          [FPluginSettings.Process.Kind]);
        Exit;
      end;

      if Kind = 'embedded' then
        FFixed := False;
    except
      on E: Exception do
      begin
        FErrorMesssage := E.Message;
      end;
    end;
  finally
    Serializer.Free;
  end;
end;

end.
