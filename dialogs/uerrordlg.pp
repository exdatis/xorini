unit uerrordlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Arrow, StdCtrls;

type

  { TfrmErrorDlg }

  TfrmErrorDlg = class(TForm)
    arrowDetail: TArrow;
    btnClose: TButton;
    Label1: TLabel;
    memoDetails: TMemo;
    pnlQuitBtn: TPanel;
    pnlDetails: TPanel;
    pnlButton: TPanel;
    procedure arrowDetailClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Label1Click(Sender: TObject);
    procedure pnlButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure setErrorMsg(const errMsg : String);
    procedure showErrDetails;
    procedure closeErrDetails;
    procedure onClickArrowDetail;
  end;

var
  frmErrorDlg: TfrmErrorDlg;

implementation

{$R *.lfm}

{ TfrmErrorDlg }

procedure TfrmErrorDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction:= caFree;
  self:= nil;
end;

procedure TfrmErrorDlg.Label1Click(Sender: TObject);
begin
  onClickArrowDetail;
end;

procedure TfrmErrorDlg.pnlButtonClick(Sender: TObject);
begin
  onClickArrowDetail;
end;

procedure TfrmErrorDlg.arrowDetailClick(Sender: TObject);
begin
  onClickArrowDetail;
end;

procedure TfrmErrorDlg.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmErrorDlg.setErrorMsg(const errMsg: String);
begin
  memoDetails.Lines.Text:= errMsg;
end;

procedure TfrmErrorDlg.showErrDetails;
begin
  self.BorderStyle:= bsSizeable;
  pnlDetails.Height:= 223;
  self.Height:= 286;
  self.BorderStyle:= bsDialog;
  Application.ProcessMessages;
end;

procedure TfrmErrorDlg.closeErrDetails;
begin
  self.BorderStyle:= bsSizeable;
  pnlDetails.Height:= 0;
  self.Height:= 62;
  self.BorderStyle:= bsDialog;
  Application.ProcessMessages;
end;

procedure TfrmErrorDlg.onClickArrowDetail;
begin
  if(arrowDetail.ArrowType = atRight) then
    begin
      showErrDetails;
      arrowDetail.ArrowType:= atDown;
      Application.ProcessMessages;
    end
  else
    begin
      closeErrDetails;
      arrowDetail.ArrowType:= atRight;
      Application.ProcessMessages;
    end;
end;

end.

