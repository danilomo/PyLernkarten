program pylernkarten;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg,
  mainwindow, derdiedas, practice { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainWindowForm, MainWindowForm);
  Application.CreateForm(TDerDieDasForm, DerDieDasForm);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
