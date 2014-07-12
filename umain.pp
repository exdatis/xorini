unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, usplashabout, Forms,
  Controls, Graphics, Dialogs, StdCtrls, Grids, ExtCtrls, Buttons, Menus,
  IniFiles, strutils;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnAdd: TButton;
    btnQuit: TButton;
    btnDelete: TButton;
    btnEnterFilePath: TButton;
    btnEnterDirPath: TButton;
    btnCreateTmpl: TButton;
    btnLoadTmpl: TButton;
    btnSetMagicalString: TButton;
    DividerBevel1: TDividerBevel;
    edtSection: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    findFile: TOpenDialog;
    Label2: TLabel;
    MenuItem1: TMenuItem;
    miClearAll: TMenuItem;
    miReindexing: TMenuItem;
    appendToFile: TOpenDialog;
    openTmpl: TOpenDialog;
    pmStringGrid: TPopupMenu;
    rgSaveAs: TRadioGroup;
    saveToIniFile: TSaveDialog;
    saveTmpl: TSaveDialog;
    selectDir: TSelectDirectoryDialog;
    sgValues: TStringGrid;
    sbSaveAsNew: TSpeedButton;
    Shape1: TShape;
    sbAppendIni: TSpeedButton;
    SplashAbout1: TSplashAbout;
    procedure btnAddClick(Sender: TObject);
    procedure btnCreateTmplClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEnterDirPathClick(Sender: TObject);
    procedure btnEnterFilePathClick(Sender: TObject);
    procedure btnLoadTmplClick(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
    procedure btnSetMagicalStringClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miClearAllClick(Sender: TObject);
    procedure miReindexingClick(Sender: TObject);
    procedure sbAppendIniClick(Sender: TObject);
    procedure sbSaveAsNewClick(Sender: TObject);
  private
    { private declarations }
    encryptionString : String; {default MAGICAL_STRING}
    procedure setColWidth;
    procedure saveConfAsTxt(const lastIndex : Integer; const filePath : String; const currSection : String);
    procedure saveConfEncrypted(const lastIndex : Integer; const filePath : String; const currSection : String);
    procedure appendConfAsTxt(const lastIndex : Integer; const filePath : String; const currSection : String);
    procedure appendConfEncrypted(const lastIndex : Integer; const filePath : String; const currSection : String);

  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;
const
  SUCCESS_MSG : String = 'Success!';
  ROW_INDEX_ERROR : String = 'Row index error.';
  HASH_ERROR : String = 'Hash error.';
  NO_SECTION_ERROR : String = 'There is no section(error).';

implementation
uses
  uexdatis, uinputstring;
{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnQuitClick(Sender: TObject);
begin
  Close;
  Application.Terminate;
end;

procedure TfrmMain.btnSetMagicalStringClick(Sender: TObject);
begin
  {create dialog}
  dlgInputString:= TdlgInputString.Create(frmMain);
  {show current string}
  dlgInputString.edtCurrString.Text:= encryptionString;
  {show dialog}
  if(dlgInputString.ShowModal = mrOK) then
    encryptionString:= dlgInputString.getNewString;
  {free dialog}
  dlgInputString.Free;
  {*****************************************************************************
  -debug msg
  ShowMessage(encryptionString);
  *****************************************************************************}
  ShowMessage(SUCCESS_MSG);
end;

procedure TfrmMain.btnAddClick(Sender: TObject);
var
  currRows : Integer;
begin
  {number of rows}
  currRows:= sgValues.RowCount;
  {add row}
  sgValues.RowCount:= currRows + 1;
  sgValues.Cells[0, currRows]:= IntToStr(currRows);
  {set focus}
  sgValues.Row:= currRows;
  sgValues.Col:= 1;
  sgValues.SetFocus;

  Application.ProcessMessages;
end;

procedure TfrmMain.btnCreateTmplClick(Sender: TObject);
var
  numOfRows : Integer;
  csvPath : String;
begin
  {Check number of rows}
  numOfRows:= sgValues.RowCount;
  if(numOfRows < 2) then
    begin
      ShowMessage(HASH_ERROR);
      Exit;
    end;
  {save to csv file}
  if(saveTmpl.Execute) then
    begin
      csvPath:= saveTmpl.FileName;
      try
        sgValues.SaveToCSVFile(csvPath,';');
      except
        on e : Exception do
        begin
          ShowMessage(e.Message);
          Exit;
        end;
      end;
    end;
  {success msg}
  ShowMessage(SUCCESS_MSG);
end;

procedure TfrmMain.btnDeleteClick(Sender: TObject);
var
  currRow : Integer;
begin
  {count rows}
  if(sgValues.RowCount < 2) then
    begin
      ShowMessage(ROW_INDEX_ERROR);
      Exit;
    end;
  {selected}
  currRow:= sgValues.Row;
  sgValues.DeleteRow(currRow);
  Application.ProcessMessages;
end;

procedure TfrmMain.btnEnterDirPathClick(Sender: TObject);
var
  currPath : String = '';
  currRow, currCol : Integer; {cells}
begin
  {number of rows}
  if(sgValues.RowCount < 2) then
    begin
      ShowMessage(ROW_INDEX_ERROR);
      Exit;
    end;
  {selectDir(SelectDirectoryDialog) run}
  if(selectDir.Execute) then
    currPath:= selectDir.FileName;
  {find position}
  currRow:= sgValues.Row;
  currCol:= sgValues.Col;
  sgValues.Cells[currCol, currRow]:= currPath;
  Application.ProcessMessages;
end;

procedure TfrmMain.btnEnterFilePathClick(Sender: TObject);
var
  currPath : String = '';
  currRow, currCol : Integer; {cells}
begin
  {number of rows}
  if(sgValues.RowCount < 2) then
    begin
      ShowMessage(ROW_INDEX_ERROR);
      Exit;
    end;
  {findFile(OpenDialog) run}
  if(findFile.Execute) then
    currPath:= findFile.FileName;
  {find position}
  currRow:= sgValues.Row;
  currCol:= sgValues.Col;
  sgValues.Cells[currCol, currRow]:= currPath;
  Application.ProcessMessages;
end;

procedure TfrmMain.btnLoadTmplClick(Sender: TObject);
var
  tmplPath : String = '';
begin
  {find template(csv)}
  if(openTmpl.Execute) then
    tmplPath:= openTmpl.FileName;
  {load from csv}
  if(FileExistsUTF8(tmplPath)) then
    begin
      sgValues.Clear;
      sgValues.LoadFromCSVFile(tmplPath, ';');
      {set cols width}
      setColWidth;
      Application.ProcessMessages;
    end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  {stringGrid captions}
  sgValues.Cells[0,0]:= 'Rb';
  sgValues.Cells[1,0]:= 'Name';
  sgValues.Cells[2,0]:= 'Value';
  {set cols width}
  setColWidth;
  {set magical string}
  encryptionString:= MAGICAL_STRING;{default}
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  {splash}
  SplashAbout1.PoweredBy.ShowPoweredByForm;
  SplashAbout1.ShowSplash;
end;

procedure TfrmMain.miClearAllClick(Sender: TObject);
begin
  {clear all}
  sgValues.Clear;
  {set captions}
  sgValues.RowCount:= 1;
  {stringGrid captions}
  sgValues.Cells[0,0]:= 'Rb';
  sgValues.Cells[1,0]:= 'Name';
  sgValues.Cells[2,0]:= 'Value';
  {set cols width}
  setColWidth;

  Application.ProcessMessages;
end;

procedure TfrmMain.miReindexingClick(Sender: TObject);
var
  i, numOfRows : Integer; {counter}
begin
  numOfRows:= sgValues.RowCount;
  if(numOfRows < 2) then
    Exit;{nothing to do}
  {reindexing}
  for i := 1 to numOfRows - 1 do
    sgValues.Cells[0, i]:= IntToStr(i);

  Application.ProcessMessages;
end;

procedure TfrmMain.sbAppendIniClick(Sender: TObject);
var
  numOfRows : Integer;
  newIniFile : String = '';
  currSection : String;
begin
  {check section}
  currSection:= edtSection.Text;
  if(Length(currSection) < 1) then
    begin
      ShowMessage(NO_SECTION_ERROR);
      Exit;
    end;
  {number of rows}
  numOfRows:= sgValues.RowCount;
  if(numOfRows < 2) then
    begin
      ShowMessage(HASH_ERROR);
      Exit;
    end;
  {where to save (save dialog)}
  if(appendToFile.Execute) then
    newIniFile:= appendToFile.FileName;
  {find lastIndex}
  numOfRows:= numOfRows - 1;{last index}
  {selected procedure}
  case rgSaveAs.ItemIndex of
    0: appendConfAsTxt(numOfRows, newIniFile, currSection);
    1: appendConfEncrypted(numOfRows, newIniFile, currSection);
  else appendConfAsTxt(numOfRows, newIniFile, currSection);
  end;
end;

procedure TfrmMain.sbSaveAsNewClick(Sender: TObject);
const
  CANCELED_PROCEDURE : String = 'Canceled procedure(by user).';
var
  numOfRows : Integer;
  newIniFile : String = '';
  currSection : String;
begin
  {check section}
  currSection:= edtSection.Text;
  if(Length(currSection) < 1) then
    begin
      ShowMessage(NO_SECTION_ERROR);
      Exit;
    end;
  {number of rows}
  numOfRows:= sgValues.RowCount;
  if(numOfRows < 2) then
    begin
      ShowMessage(HASH_ERROR);
      Exit;
    end;
  {where to save (save dialog)}
  if(saveToIniFile.Execute) then
    newIniFile:= saveToIniFile.FileName;
  {check file_name}
  if(Length(newIniFile) < 5) then
    begin
      ShowMessage(CANCELED_PROCEDURE);
      Exit;
    end;
  {find lastIndex}
  numOfRows:= numOfRows - 1;{last index}
  {selected procedure}
  case rgSaveAs.ItemIndex of
    0: saveConfAsTxt(numOfRows, newIniFile, currSection);
    1: saveConfEncrypted(numOfRows, newIniFile, currSection);
  else saveConfAsTxt(numOfRows, newIniFile, currSection);
  end;
end;

procedure TfrmMain.setColWidth;
begin
  sgValues.ColWidths[0]:= 35;
  sgValues.ColWidths[1]:= 170;
  sgValues.ColWidths[2]:= 170;
end;

procedure TfrmMain.saveConfAsTxt(const lastIndex: Integer;
  const filePath: String; const currSection: String);
var
  newIniFile : TIniFile;
  i : Integer;
  fileExist : Boolean = False;
  existingSections : TStringList;
begin
  {check file}
  if(FileExistsUTF8(filePath)) then
    fileExist:= True;
  {try to create ini}
  try
    newIniFile:= TIniFile.Create(filePath);
  except
    on e : Exception do
    begin
      ShowMessage(e.Message);
      Exit;
    end;
  end;
  {erase existing}
  if(fileExist) then
    begin
      existingSections:= TStringList.Create;
      newIniFile.ReadSections(existingSections);
      if(existingSections.Count > 0) then
        for i := 0 to existingSections.Count - 1 do
          newIniFile.EraseSection(existingSections[i]);
      {free TStrings(existingSections)}
      existingSections.Free;
    end;
  {loop (write strings)}
  for i := 1 to lastIndex do
    if(sgValues.Cells[1, i] <> '') then
      newIniFile.WriteString(currSection, sgValues.Cells[1, i], sgValues.Cells[2, i]);
  {free ini}
  newIniFile.Free;
  {success msg}
  ShowMessage(SUCCESS_MSG);
end;

procedure TfrmMain.saveConfEncrypted(const lastIndex: Integer;
  const filePath: String; const currSection: String);
var
  newIniFile : TIniFile;
  i : Integer;
  fileExist : Boolean = False;
  existingSections : TStringList;
  xorString : String; {encrypted}
begin
  {check file}
  if(FileExistsUTF8(filePath)) then
    fileExist:= True;
  {try to create ini}
  try
    newIniFile:= TIniFile.Create(filePath);
  except
    on e : Exception do
    begin
      ShowMessage(e.Message);
      Exit;
    end;
  end;
  {erase existing}
  if(fileExist) then
    begin
      existingSections:= TStringList.Create;
      newIniFile.ReadSections(existingSections);
      if(existingSections.Count > 0) then
        for i := 0 to existingSections.Count - 1 do
          newIniFile.EraseSection(existingSections[i]);
      {free TStrings(existingSections)}
      existingSections.Free;
    end;
  {loop (write strings)}
  for i := 1 to lastIndex do
    if(sgValues.Cells[1, i] <> '') then
      begin
        xorString:= XorEncode(encryptionString, sgValues.Cells[2, i]);
        newIniFile.WriteString(currSection, sgValues.Cells[1, i], xorString);
      end;
  {free ini}
  newIniFile.Free;
  {success msg}
  ShowMessage(SUCCESS_MSG);
end;

procedure TfrmMain.appendConfAsTxt(const lastIndex: Integer;
  const filePath: String; const currSection: String);
var
  newIniFile : TIniFile;
  i : Integer;
begin
  {try to create ini}
  try
    newIniFile:= TIniFile.Create(filePath);
  except
    on e : Exception do
    begin
      ShowMessage(e.Message);
      Exit;
    end;
  end;
  {loop (write strings)}
  for i := 1 to lastIndex do
    if(sgValues.Cells[1, i] <> '') then
      newIniFile.WriteString(currSection, sgValues.Cells[1, i], sgValues.Cells[2, i]);
  {free ini}
  newIniFile.Free;
  {success msg}
  ShowMessage(SUCCESS_MSG);
end;

procedure TfrmMain.appendConfEncrypted(const lastIndex: Integer;
  const filePath: String; const currSection: String);
var
  newIniFile : TIniFile;
  i : Integer;
  xorString : String;{enrypted}
begin
  {try to create ini}
  try
    newIniFile:= TIniFile.Create(filePath);
  except
    on e : Exception do
    begin
      ShowMessage(e.Message);
      Exit;
    end;
  end;
  {loop (write strings)}
  for i := 1 to lastIndex do
    if(sgValues.Cells[1, i] <> '') then
      begin
        xorString:= XorEncode(encryptionString, sgValues.Cells[2, i]);
        newIniFile.WriteString(currSection, sgValues.Cells[1, i], xorString);
      end;
  {free ini}
  newIniFile.Free;
  {success msg}
  ShowMessage(SUCCESS_MSG);
end;

end.

