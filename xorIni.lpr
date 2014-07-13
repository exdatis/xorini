program xorIni;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, umain, uexdatis, uinputstring
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Ini creator';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  //Application.CreateForm(TdlgInputString, dlgInputString);
  Application.Run;
end.

