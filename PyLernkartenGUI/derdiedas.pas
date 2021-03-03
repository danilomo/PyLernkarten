unit derdiedas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, lernkarten;

type

  { TDerDieDasForm }

  TDerDieDasForm = class(TForm)
    DerButton: TButton;
    DieButton: TButton;
    DasButton: TButton;
    CardText: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    LabelResult: TLabel;
    MeaningsMemo: TMemo;
    procedure DasButtonClick(Sender: TObject);
    procedure DerButtonClick(Sender: TObject);
    procedure DieButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDeck: string;
    FLernkarten: TFlashcards;

    procedure Answer(answer: ansistring);
    procedure LoadNextCard;
  public
    property Deck: string read FDeck write FDeck;
    property Lernkarten: TFlashcards read FLernkarten write FLernkarten;
  end;

var
  DerDieDasForm: TDerDieDasForm;

implementation

{$R *.lfm}

{ TDerDieDasForm }

procedure TDerDieDasForm.DerButtonClick(Sender: TObject);
begin
  Answer('der');
end;

procedure TDerDieDasForm.DasButtonClick(Sender: TObject);
begin
  Answer('das');
end;

procedure TDerDieDasForm.DieButtonClick(Sender: TObject);
begin
  Answer('die');
end;

procedure TDerDieDasForm.FormShow(Sender: TObject);
begin
   FLernkarten.StartGame(FDeck);
   LoadNextCard;
end;

procedure TDerDieDasForm.LoadNextCard;
var
  nextCard: TCard;
begin
  nextCard := FLernkarten.GetNextCard;
  CardText.Text := nextCard.front;
  MeaningsMemo.Text := nextCard.notes;
end;

procedure TDerDieDasForm.Answer(answer: ansistring);
var
  Result: TAnswer;
begin
  Result := FLernkarten.AnswerCard(answer);

  if FLernkarten.HasMoreCards then
  begin
    LoadNextCard;
  end
  else
  begin
    ShowMessage('It''s done');
    ShowMessage(FLernkarten.SendCommand('show_errors'));
    Close;
  end;

  if Result.success then
  begin
    LabelResult.Caption := 'Right answer!';
    LabelResult.Font.Color := clBlue;
  end
  else
  begin
    LabelResult.Caption := Result.message;
    LabelResult.Font.Color := clRed;
  end;

end;

end.
