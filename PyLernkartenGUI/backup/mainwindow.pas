unit mainwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ValEdit,
  Grids, ExtCtrls, ComCtrls, derdiedas, Types, lernkarten;

type

  { TMainWindowForm }

  TMainWindowForm = class(TForm)
    DerDieDasButton: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    DecksListBox: TListBox;
    PageControl: TPageControl;
    DecksSheet: TTabSheet;
    Nouns: TTabSheet;
    TabSheet1: TTabSheet;
    Verbs: TTabSheet;
    procedure DerDieDasButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FLernkarten: TFlashcards;
  public

  end;

var
  MainWindowForm: TMainWindowForm;

implementation

{$R *.lfm}

{ TMainWindowForm }


procedure TMainWindowForm.DerDieDasButtonClick(Sender: TObject);
var
  selectedDeck: string;
begin
  selectedDeck := DecksListBox.GetSelectedText;
  selectedDeck := selectedDeck.Substring(2);
  DerDieDasForm.Deck := selectedDeck;
  DerDieDasForm.Lernkarten := FLernkarten;
  DerDieDasForm.ShowModal;
end;

procedure TMainWindowForm.FormCreate(Sender: TObject);
var
  i: integer;
  list: TStringList;
begin
  FLernkarten := TFlashcards.Create;

  list := TStringList.Create;
  FLernkarten.ListDecks(list);
  for i := 0 to list.Count - 1 do
    DecksListBox.Items.Add(list[i]);

  list.Destroy;
end;



end.

