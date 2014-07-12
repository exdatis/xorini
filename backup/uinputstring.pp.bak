unit uinputstring;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TdlgInputString }

  TdlgInputString = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    edtValidate: TEdit;
    edtNewString: TEdit;
    edtCurrString: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure edtValidateEnter(Sender: TObject);
    procedure edtValidateKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function getNewString : String;
  end;

var
  dlgInputString: TdlgInputString;

implementation

{$R *.lfm}

{ TdlgInputString }

procedure TdlgInputString.edtValidateEnter(Sender: TObject);
begin
  {show diff color(indicate)}
  if(edtValidate.Text <> edtNewString.Text) then
    edtValidate.Color:= clRed;
end;

procedure TdlgInputString.edtValidateKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  {indicate}
  if(edtValidate.Text <> edtNewString.Text) then
    edtValidate.Color:= clRed
  else
    edtValidate.Color:= clMoneyGreen;
end;

procedure TdlgInputString.FormShow(Sender: TObject);
begin
  edtNewString.SetFocus;
  Application.ProcessMessages;
end;

function TdlgInputString.getNewString: String;
begin
  Result:= edtNewString.Text;
end;

end.

