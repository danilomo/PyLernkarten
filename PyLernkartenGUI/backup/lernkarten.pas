unit lernkarten;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, fpjson, jsonparser, Dialogs, JsonTools;

type
  TAnswer = record
    success: boolean;
    message: string;
  end;

  TCard = record
    front: string;
    notes: string;
  end;

  TFlashcards = class
  private
    FProcess: TProcess;
    FOutputLines: TStringList;
    FHasMoreCards: boolean;

    procedure SendCommand(command: ansistring) overload;
    function ReadFromProcess: ansistring;
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartGame(deck: string);
    function GetNextCard: TCard;
    function AnswerCard(answer: string): TAnswer;
    procedure ListDecks(aStringList: TStringList);
    function SendCommand(command: string): string;

    property HasMoreCards: boolean read FHasMoreCards;

  end;

implementation

constructor TFlashcards.Create;
begin
  FProcess := TProcess.Create(nil);
  FProcess.Executable := '/usr/bin/python';
  FProcess.Parameters.Add('/home/danilo/Workspace/PyLernkarten/main.py');
  FProcess.Options := [poUsePipes];
  FProcess.Execute;

  FOutputLines := TStringList.Create;
  FHasMoreCards := True;
end;

destructor TFlashcards.Destroy;
begin
  FProcess.Destroy;
  FOutputLines.Destroy;
  inherited;
end;

procedure TFlashcards.SendCommand(command: string);
begin
  command := command + LineEnding;
  FProcess.Input.Write(command[1], length(command));
end;

function TFlashcards.ReadFromProcess: string;
begin
  if FOutputLines.Count > 0 then
  begin
    Result := FOutputLines.Strings[0];
    FOutputLines.Delete(0);
  end
  else
  begin
    FOutputLines.LoadFromStream(FProcess.Output);
    Result := FOutputLines.Strings[0];
    FOutputLines.Delete(0);
  end;
end;

function TFlashcards.GetNextCard: TCard;
var
  node: TJsonNode;
  output: string;
begin
  output := ReadFromProcess ;

  //+ ReadFromProcess + ReadFromProcess + ReadFromProcess
  //+ ReadFromProcess + ReadFromProcess + ReadFromProcess + ReadFromProcess + ReadFromProcess + ReadFromProcess
  //+ ReadFromProcess + ReadFromProcess;

  //ShowMessage(output);
  node := TJsonNode.Create;
  node.Parse(output);

  output := node.Find('card').AsString;
  //output := StringReplace(output, '\u00c3\u00a4', 'ä', [rfReplaceAll]);
  //output := StringReplace(output, '\u00c3\u00b6', 'ö', [rfReplaceAll]);
  //output := StringReplace(output, '\u00c3\u00bc', 'ü', [rfReplaceAll]);
  //output := StringReplace(output, '\u00c3\u0178', 'ß', [rfReplaceAll]);
  //output := StringReplace(output, '\u00c3\u009f', 'ß', [rfReplaceAll]);
  //output := StringReplace(output, '"', '', [rfReplaceAll]);

  FHasMoreCards := node.Find('has_more').AsBoolean;

  Result.front := output;
  Result.notes := node.Find('notes').AsString;
end;

function TFlashcards.AnswerCard(answer: string): TAnswer;
var
  jData: TJSONData;
  jObject: TJSONObject;
  output: string;
begin
  SendCommand(answer);
  output := ReadFromProcess;
  jData := GetJSON(output);
  jObject := TJSONObject(jData);

  Result.success := jObject.Get('right_answer', False);
  Result.message := jObject.Get('message', '');

  jData.Free;
end;

procedure TFlashcards.StartGame(deck: string);
begin
  SendCommand('ddd ' + deck);
end;

procedure TFlashcards.ListDecks(aStringList: TStringList);
begin
  SendCommand('showdecks');
  aStringList.LoadFromStream(FProcess.Output);
end;

function TFlashcards.SendCommand(command: string): string;
var
  stringList: TStringList;
begin
  stringList := TStringList.Create();
  SendCommand(command);
  stringList.LoadFromStream(FProcess.Output);
  Result := stringList.Text;
  stringList.Free;
end;

end.
