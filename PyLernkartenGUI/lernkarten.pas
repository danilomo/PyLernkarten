unit lernkarten;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, fpjson, jsonparser, Dialogs, JsonTools, Grids;

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

    procedure SendCommandWithoutReadingOutput(command: string);
    function ReadFromProcess: ansistring;
  public
    constructor Create;
    destructor Destroy; override;

    function SendCommand(command: string): string;

    procedure ListDecks(aStringList: TStringList);
    procedure ListNouns(aStringGrid: TStringGrid; deckName: string);

    // flaschard game functions
    procedure StartGame(deck: string);
    property HasMoreCards: boolean read FHasMoreCards;
    function GetNextCard: TCard;
    function AnswerCard(answer: string): TAnswer;

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

procedure TFlashcards.SendCommandWithoutReadingOutput(command: string);
begin
  command := command + LineEnding;
  FProcess.Input.Write(command[1], length(command));
end;

function TFlashcards.SendCommand(command: string): string;
var
  stringList: TStringList;
begin
  stringList := TStringList.Create();
  SendCommandWithoutReadingOutput(command);
  stringList.LoadFromStream(FProcess.Output);
  Result := stringList.Text;
  stringList.Free;
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
  output := ReadFromProcess;

  node := TJsonNode.Create;
  node.Parse(output);

  output := node.Find('card').AsString;

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
  SendCommandWithoutReadingOutput(answer);
  output := ReadFromProcess;
  jData := GetJSON(output);
  jObject := TJSONObject(jData);

  Result.success := jObject.Get('right_answer', False);
  Result.message := jObject.Get('message', '');

  jObject.Free;
end;

procedure TFlashcards.StartGame(deck: string);
begin
  SendCommandWithoutReadingOutput('ddd ' + deck);
end;

// general functions

procedure TFlashcards.ListNouns(aStringGrid: TStringGrid; deckName: string);
var
  output: string;
  jData: TJSONData;
  jArray: TJSONArray;
  row: TJSONArray;
  i: integer;
begin
  output := SendCommand('showdeck ' + deckName + ' nouns');

  jData := GetJSON(output);
  jArray := TJSONArray(jData);

  for i := 0 to jArray.Count - 1 do begin
      row := jArray.Arrays[i];

      aStringGrid.InsertRowWithValues(i + 1, [
        IntToStr(i + 1),
        row.Strings[0],
        row.Strings[1],
        row.Strings[2],
        row.Strings[3]
      ]);
  end;

  aStringGrid.RowCount := jArray.Count;
  aStringGrid.Row := 1;

  jArray.Free;
end;

procedure TFlashcards.ListDecks(aStringList: TStringList);
var
  output: string;
  jData: TJSONData;
  jObject: TJSONArray;
  i: integer;
begin
  output := SendCommand('showdecks');

  jData := GetJSON(output);
  jObject := TJSONArray(jData);

  for i := 0 to jObject.Count - 1 do begin
    aStringList.Add(jObject.Strings[i]);
  end;

  jObject.Free;
end;


end.
