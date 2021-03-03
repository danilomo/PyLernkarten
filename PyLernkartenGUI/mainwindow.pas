unit mainwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, ComCtrls, SynEdit, derdiedas, lernkarten, PyLernKartenUtils;

type
  { TMainWindowForm }

  TMainWindowForm = class(TForm)
    Button1: TButton;
    SaveNounsChangesBtn: TButton;
    DerDieDasButton: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    DecksListBox: TListBox;
    CommandEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    OutputMemo: TMemo;
    PageControl: TPageControl;
    DecksSheet: TTabSheet;
    Nouns: TTabSheet;
    NounsGrid: TStringGrid;
    Panel1: TPanel;
    TabControl1: TTabControl;
    Practice: TTabSheet;
    TabSheet2: TTabSheet;
    ToolBar1: TToolBar;
    Verbs: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure DerDieDasButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NounsGridSetEditText(Sender: TObject; ACol, ARow: integer; const Value: string);
    procedure NounsShow(Sender: TObject);
    procedure SaveNounsChangesBtnClick(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
  private
    FLernkarten: TFlashcards;
  public
    FChangedRows: TIntSet;
  end;

var
  MainWindowForm: TMainWindowForm;


implementation

{$R *.lfm}

{ TMainWindowForm }

procedure TMainWindowForm.Button1Click(Sender: TObject);
begin
  FLernkarten.SendCommandGetLines(CommandEdit.Text, OutputMemo.Lines);
end;

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
  FChangedRows := TIntSet.Create;
  FChangedRows.insert(0);
  list := TStringList.Create;
  FLernkarten.ListDecks(list);
  for i := 0 to list.Count - 1 do
  begin
    DecksListBox.Items.Add(list[i]);
    TabControl1.Tabs.Add(list[i]);
  end;

  list.Destroy;
end;

procedure TMainWindowForm.NounsGridSetEditText(Sender: TObject;
  ACol, ARow: integer; const Value: string);
begin
  FChangedRows.insert(ARow);
end;

procedure TMainWindowForm.NounsShow(Sender: TObject);
var
  deckName: string;
begin
  deckName := TabControl1.Tabs[TabControl1.TabIndex];
  FLernkarten.ListNouns(NounsGrid, deckName);
end;

procedure TMainWindowForm.SaveNounsChangesBtnClick(Sender: TObject);
var
  list: TStringList;
  i: integer;
begin
  list := TStringList.Create;

  for i in FChangedRows do
  begin
    FLernkarten.UpdateNoun(
      NounsGrid.Rows[i][1],
      NounsGrid.Rows[i][2],
      NounsGrid.Rows[i][3],
      NounsGrid.Rows[i][4]);
  end;

  FLernkarten.SendCommand('saveworkspace');

  list.Free;
end;

procedure TMainWindowForm.TabControl1Change(Sender: TObject);
var
  deckName: string;
begin
  deckName := TabControl1.Tabs[TabControl1.TabIndex];
  FLernkarten.ListNouns(NounsGrid, deckName);
end;

end.
