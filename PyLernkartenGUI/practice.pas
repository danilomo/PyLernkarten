unit practice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    FlowPanel1: TFlowPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

const
NEW_LINE = '                                                                                                                             ';

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.

