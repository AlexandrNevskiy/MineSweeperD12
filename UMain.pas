unit UMain;
///
/// MineSweeperD12 (RAD Programmer Challenge #1))
///
/// Main form
///
/// Alex Nevskiy 2025-04-09
///
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.Actions, Vcl.ActnList, Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.MPlayer,
  IniFiles,
  UUtility;

type
  TFMain = class(TForm)
    MainMenu: TMainMenu;
    File0: TMenuItem;
    NewGame1: TMenuItem;
    Divider1: TMenuItem;
    Statistics1: TMenuItem;
    Options1: TMenuItem;
    Visual1: TMenuItem;
    Divider2: TMenuItem;
    Exit1: TMenuItem;
    Help0: TMenuItem;
    Help1: TMenuItem;
    Divider3: TMenuItem;
    About1: TMenuItem;
    Actions: TActionList;
    actNewGame: TAction;
    actStatistics: TAction;
    actOptions: TAction;
    actVisual: TAction;
    actExit: TAction;
    actHelp: TAction;
    actAbout: TAction;
    procedure actOptionsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    LocalParams: TMemIniFile;
  public
  end;

var
  FMain: TFMain;

implementation

uses UOptions;

{$R *.dfm}

procedure TFMain.actOptionsExecute(Sender: TObject);
begin
  FOptions.ShowModal;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  LocalParams := TMemIniFile.Create(GetParamsFileName);

  Caption := GetParamsFileName;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(LocalParams);
end;

end.
