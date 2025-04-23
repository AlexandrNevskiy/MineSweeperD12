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
  Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.Actions, Vcl.ActnList, Vcl.Imaging.pngimage, Vcl.ExtCtrls,  IniFiles,
  UUtility, UGameGrid, URenderGrid;

type
  TFMain = class(TForm)
    MainMenu: TMainMenu;
    Game0: TMenuItem;
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
    pnlBottom: TPanel;
    //pnlGrid: TCanvasPanel;
    imgTimer: TImage;
    pnlTime: TPanel;
    imgMine: TImage;
    pnlMine: TPanel;
    Timer: TTimer;
    pnlGrid_old: TPanel;
    imgGrid: TImage;
    procedure actOptionsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actNewGameExecute(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    function ClickToCell(X, Y: Integer): TPoint;
    procedure imgGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function FormHelp(Command: Word; Data: THelpEventData; var CallHelp: Boolean): Boolean;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure imgGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgGridMouseLeave(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    LocalParams: TLocalParams;
    GameGrid: TGameGridClass;
    OldCell: TPoint;
    LButton: boolean;
    RButton: boolean;
    procedure PrepareForm(aGridData: TGridData);
    function ShowDifficulty: boolean;
    procedure StartNewGame;
    procedure UpdateTimer;
  public
    procedure UpdateGraph;

  end;

var
  FMain: TFMain;

implementation

uses Math, UOptions, UDebug;

{$R *.dfm}
//{$R Graph.res}

procedure TFMain.actNewGameExecute(Sender: TObject);
begin
  StartNewGame;
end;

procedure TFMain.actOptionsExecute(Sender: TObject);
begin
  if ShowDifficulty then
    StartNewGame;
end;


function TFMain.ClickToCell(X, Y: Integer): TPoint;
var cW, cH: integer;
begin
  cW := (imgGrid.ClientRect.Width { - pnlGrid.BevelWidth * 2}) div LocalParams.GridData.Width;
  cH := (imgGrid.ClientRect.Height{ - pnlGrid.BevelWidth * 2}) div LocalParams.GridData.Height;
  //result.x := Min((X - pnlGrid.BevelWidth) div cW, LocalParams.GridData.Width);
  //result.y := Min((Y - pnlGrid.BevelWidth) div cH, LocalParams.GridData.Height);
  //result.x := Max(result.x, 0);
  //result.y := Max(result.y, 0);
  result.x := (X{ - pnlGrid.BevelWidth}) div cW;
  result.y := (Y{ - pnlGrid.BevelWidth}) div cH;
  result.x := IfThen((result.x >= 0) and (result.x < LocalParams.GridData.Width),  result.x, -1);
  result.y := IfThen((result.y >= 0) and (result.y < LocalParams.GridData.Height), result.y, -1);
end;


procedure TFMain.FormCreate(Sender: TObject);
begin
  GameGrid  := TGameGridClass.Create(Self);
  Caption := format(csFormCaption, [GetCurrentUser]);
  OldCell.SetLocation(-1, -1);
  LocalParams := TLocalParams.Create(True);
  LocalParams.LoadSettings;
  GameGrid.Settings := LocalParams.Settings;
  StartNewGame;
end;


procedure TFMain.FormDeactivate(Sender: TObject);
begin
  GameGrid.ClearDownCheck;
end;


procedure TFMain.FormDestroy(Sender: TObject);
begin
  LocalParams.SaveParams;
  FreeAndNil(GameGrid);
  //FreeAndNil(pnlGrid);
end;


function TFMain.FormHelp(Command: Word; Data: THelpEventData; var CallHelp: Boolean): Boolean;
begin
  if Command <> 12345 then exit; //Refresh - Lazy way callback
  pnlMine.Caption := IntToStr(GameGrid.Mines_Flags);
  UpdateGraph;
end;


procedure TFMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssShift in Shift) and (ssCtrl in Shift) and (Key = VK_F1)  then
    GameGrid.DEBUG_ShowMines;

  if (Key = VK_F1) then DebugForm.ShowDebug(GameGrid, 0);

end;

procedure TFMain.FormPaint(Sender: TObject);
var crdLeft, crdTop, crdRight, crdBottom: integer;
begin
  crdLeft := imgGrid.ClientToParent(imgGrid.ClientRect.TopLeft).X;// + pnlGrid.BevelWidth;
  crdTop  := imgGrid.ClientToParent(imgGrid.ClientRect.TopLeft).Y;// + pnlGrid.BevelWidth;
  crdRight := imgGrid.ClientToParent(imgGrid.ClientRect.BottomRight).X;// - pnlGrid.BevelWidth;
  crdBottom := imgGrid.ClientToParent(imgGrid.ClientRect.BottomRight).Y;// - pnlGrid.BevelWidth;
  Canvas.StretchDraw(Rect(crdLeft, crdTop, crdRight, crdBottom), GameGrid.RenderGrid.FieldImage);

//  crdLeft := Grid.ClientRect.TopLeft.X;
//  crdTop  := Grid.ClientRect.TopLeft.Y;
//  crdRight := Grid.ClientRect.BottomRight.X;
//  crdBottom := Grid.ClientRect.BottomRight.Y;
//  Grid.Canvas.StretchDraw(Rect(crdLeft, crdTop, crdRight, crdBottom), GameGrid.RenderGrid.FieldImage);
end;


procedure TFMain.imgGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  OldCell := ClickToCell(X, Y);
  GameGrid.Input.Update;
  LButton := GameGrid.Input.LButton;
  RButton := GameGrid.Input.RButton;
  if LButton and RButton then
  begin
    var NewCell: TPoint := ClickToCell(X, Y);
    GameGrid.SetDownCheck(NewCell.X, NewCell.Y);
  end;

end;


procedure TFMain.imgGridMouseLeave(Sender: TObject);
begin
  GameGrid.ClearDownCheck;
end;


procedure TFMain.imgGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  GameGrid.Input.Update;
  LButton := GameGrid.Input.LButton;
  RButton := GameGrid.Input.RButton;
  if LButton and RButton then
  begin
    var NewCell: TPoint := ClickToCell(X, Y);
    if OldCell <> NewCell then
    begin
      OldCell := ClickToCell(X, Y);
      GameGrid.SetDownCheck(NewCell.X, NewCell.Y);
    end;
  end;
end;

procedure TFMain.imgGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var NewCell: TPoint;
begin
  GameGrid.Input.Update;
  NewCell := ClickToCell(X, Y);
  if NewCell <> OldCell then
  begin
    OldCell.SetLocation(-1, -1);
    GameGrid.ClearDownCheck(true);
  end
  else
  begin
    RButton := RButton or (Button = mbRight); //Force to catch RMB
    if     LButton and not RButton then GameGrid.FieldClick(NewCell.X, NewCell.Y) //On Left Click
    else
    if not LButton and     RButton then GameGrid.FieldMark (NewCell.X, NewCell.Y) //On Righ Click
    else
    if     LButton and     RButton then GameGrid.FieldCheck(NewCell.X, NewCell.Y) //On Left-Righ Click
    else
      GameGrid.ClearDownCheck(true);
    Refresh;
  end;
end;


procedure TFMain.PrepareForm(aGridData: TGridData);
var dW, dH: integer;
begin
  if  (imgGrid.Width  = aGridData.Width  * ccCellW{ + imgGrid.BevelWidth * 2})
  and (imgGrid.Height = aGridData.Height * ccCellH{ + imgGrid.BevelWidth * 2}) then
  begin
    imgGrid.Invalidate;
    exit;
  end;

  //Only manual control works precise
  Constraints.MaxWidth := 0;
  Constraints.MaxHeight := 0;

  imgGrid.Top  := imgGrid.Margins.Top;
  imgGrid.Left := imgGrid.Margins.Left;

  imgGrid.Left := imgGrid.Left;// + imgGrid.BevelWidth;
  imgGrid.Top := imgGrid.Top;// + imgGrid.BevelWidth;
  imgGrid.Width := aGridData.Width  * ccCellW;
  imgGrid.Height := aGridData.Height * ccCellH;

  imgGrid.Width  := aGridData.Width  * ccCellW;// + imgGrid.BevelWidth * 2;
  imgGrid.Height := aGridData.Height * ccCellH;// + imgGrid.BevelWidth * 2;

  pnlBottom.Top := imgGrid.BoundsRect.Bottom + imgGrid.Margins.Bottom;
  pnlBottom.Left := pnlBottom.Margins.Left;
  pnlBottom.Width := imgGrid.Width;

  ClientWidth := imgGrid.BoundsRect.Right + imgGrid.Margins.Right;
  ClientHeight := pnlBottom.BoundsRect.Bottom + pnlBottom.Margins.Bottom;
  Constraints.MaxWidth := Width;
  Constraints.MaxHeight := Height;
  Invalidate;
end;


procedure TFMain.UpdateGraph;
begin
  //Grid.Invalidate;
  Refresh;
end;

function TFMain.ShowDifficulty: boolean;
begin
  result := false;
  FOptions := TFOptions.Create(Self);
  try
    FOptions.SetGridData(LocalParams.GridData);
    FOptions.SetSettings(LocalParams.Settings);
    FOptions.UpdateSndEnabled;
    if FOptions.ShowModal = mrOk then
    begin
      LocalParams.Settings := FOptions.GetSettings;
      GameGrid.Settings := LocalParams.Settings;
      LocalParams.SaveSettings;
      if GameGrid.GameState <> gsActive then
        LocalParams.GridData := FOptions.GetGridData
      else
      if not LocalParams.GridData.IsSame(FOptions.GetGridData) then
      begin
        var aForm: TForm := CreateMessageDialog('These settings won''t apply to the game in progress. What do you want to do?'#13#10#13#10+
        '1. Quit and start a new game with the new settings'#13#10#13#10+
        '2. Finish the game',
        mtConfirmation,
        [mbYes, mbNo], mbYes,
        ['Start a new game',
        'Finish the game']);
        aForm.Caption := 'Changed Game Settings';
        var Res: integer :=  aForm.ShowModal;
        if Res = mrYes then
        begin
          LocalParams.GridData := FOptions.GetGridData;
          result := true;
        end;
      end;
    end;
  finally
    FreeAndNil(FOptions);
  end;
end;


procedure TFMain.StartNewGame;
begin
  //TODO: Check current game status
  //CurrentTime := 0;
  GameGrid.Init(LocalParams.GridData);
  //GameGrid.
  //GameGrid.ShiftToClick(GameGrid.Width div 2, GameGrid.Height div 2);
  //FillVisualGrid(LocalParams.GridData);
  PrepareForm(LocalParams.GridData);
end;


procedure TFMain.TimerTimer(Sender: TObject);
begin
  UpdateTimer;
end;


procedure TFMain.UpdateTimer;
begin
  pnlTime.Caption := IntToStr(Min(999, GameGrid.TimerCount));
  //pnlTime.Caption := IntToStr(Min(999, CurrentTime));
end;

end.

------------------------------------------

[Window Title]
Changed Game Settings

[Main Instruction]
These settings won't apply to the game in progress. What do you want to do?

[Quit and start a new game with the new settings] [Finish the game]

------------------------------------------

[Window Title]
New Game

[Main Instruction]
What do you want to do with the game in progress?

[Quit and start a new game] [Restart this game] [Keep playing]

------------------------------------------


