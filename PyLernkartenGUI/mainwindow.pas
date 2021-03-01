unit mainwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, ComCtrls, SynEdit, derdiedas, lernkarten;

type

  { TMainWindowForm }

  TMainWindowForm = class(TForm)
    DerDieDasButton: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    DecksListBox: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    PageControl: TPageControl;
    DecksSheet: TTabSheet;
    Nouns: TTabSheet;
    NounsGrid: TStringGrid;
    Panel1: TPanel;
    TabControl1: TTabControl;
    Practice: TTabSheet;
    TabSheet2: TTabSheet;
    Verbs: TTabSheet;
    procedure DerDieDasButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NounsShow(Sender: TObject);
    procedure ScrollBox1Click(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
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
  begin
    DecksListBox.Items.Add(list[i]);
    TabControl1.Tabs.Add(list[i]);
  end;

  list.Destroy;
end;

procedure TMainWindowForm.NounsShow(Sender: TObject);
var
  deckName: string;
begin
  deckName := TabControl1.Tabs[TabControl1.TabIndex];
  FLernkarten.ListNouns(NounsGrid, deckName);
end;

procedure TMainWindowForm.ScrollBox1Click(Sender: TObject);
begin

end;

procedure TMainWindowForm.TabControl1Change(Sender: TObject);
var
  deckName: string;
begin
  deckName := TabControl1.Tabs[TabControl1.TabIndex];
  FLernkarten.ListNouns(NounsGrid, deckName);
end;



end.
