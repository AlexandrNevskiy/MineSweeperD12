unit UMain;
///
/// MineSweeperD12 (RAD Programmer Challenge #1)
///
/// Main form
///
/// Alex Nevskiy 2025-04-09
///
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.Actions, Vcl.ActnList, Vcl.Imaging.pngimage, Vcl.ExtCtrls,  IniFiles,
  UUtility, UGameGrid, URenderGrid, Vcl.StdCtrls;

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
    imgTimer: TImage;
    pnlTime: TPanel;
    imgMine: TImage;
    pnlMine: TPanel;
    Timer: TTimer;
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure actStatisticsExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
  private
    LocalParams: TLocalParams;
    GameGrid: TGameGridClass;
    OldCell: TPoint;
    LButton: boolean;
    RButton: boolean;
    procedure PrepareForm(aGridData: TGridData);
    function ShowDifficulty: boolean;
    function LoadGame: boolean;
    procedure StartNewGame(aForce: boolean = false);
    procedure UpdateTimer;
    procedure Loose;
    procedure Success;
    procedure FocusMe;
  public
    procedure UpdateGraph;

  end;

var
  FMain: TFMain;

implementation

uses Math, UOptions, UStatistics;

{$R *.dfm}

procedure TFMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFMain.actNewGameExecute(Sender: TObject);
begin
  StartNewGame;
end;


procedure TFMain.actOptionsExecute(Sender: TObject);
begin
  if ShowDifficulty then
    StartNewGame(true);
end;


procedure TFMain.actStatisticsExecute(Sender: TObject);
begin
  FStatistics := TFStatistics.Create(Self);
  try
    FStatistics.Position := poMainFormCenter;
    FStatistics.ShowModal;
  finally
    FreeAndNil(FStatistics);
  end;
end;


function TFMain.ClickToCell(X, Y: Integer): TPoint;
var cW, cH: integer;
begin
  cW := imgGrid.ClientRect.Width  div LocalParams.GridData.Width;
  cH := imgGrid.ClientRect.Height div LocalParams.GridData.Height;

  result.x := X div cW;
  result.y := Y div cH;
  result.x := IfThen((result.x >= 0) and (result.x < LocalParams.GridData.Width),  result.x, -1);
  result.y := IfThen((result.y >= 0) and (result.y < LocalParams.GridData.Height), result.y, -1);
end;


procedure TFMain.FocusMe; //::AN 2025-04-25 Just Focus not works
var OldPos: TPoint;
begin
  GetCursorPos(OldPos);
  var ClientAreaPos: TPoint := ClientToScreen(Point(pnlTime.Left + 1, pnlTime.Top + 1));
  SetCursorPos(ClientAreaPos.X, ClientAreaPos.Y);
  Mouse_Event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
  Mouse_Event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
  SetCursorPos(OldPos.X, OldPos.Y);
end;


procedure TFMain.FormActivate(Sender: TObject);
begin
  if Tag > 0 then exit;
  Tag := 1;
  if GameGrid.IsSaveExists then
    LoadGame;
end;


procedure TFMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var Res: integer;
begin
  CanClose := true;
  if GameGrid.GameState <> gsActive then exit;
  var aForm: TForm := CreateMessageDialog('    What do you want to do with the game in progress?    ', mtConfirmation,
        [mbYes, mbNo, mbCancel], mbYes,
        [' Save ', ' Don''t save ', ' Cancel ']);
        aForm.Caption := 'Exit Game';
  try
    aForm.Position := poMainFormCenter;
    Res :=  aForm.ShowModal;
  finally
    FreeAndNil(aForm);
    FocusMe;
  end;
  if Res = mrYes then
  begin
    GameGrid.SaveToFile;
    Exit;
  end;
  if Res = mrCancel then
    CanClose := false;
end;


procedure TFMain.FormCreate(Sender: TObject);
begin
  GameGrid  := TGameGridClass.Create(Self);
  Caption := format(csFormCaption, [GetCurrentUser]);
  OldCell.SetLocation(-1, -1);
  LocalParams := TLocalParams.Create(True, Left, Top);
  LocalParams.LoadParams;
  LocalParams.LoadSettings;
  Left := LocalParams.Left;
  Top := LocalParams.Top;
  GameGrid.FGridData := LocalParams.GridData;
  GameGrid.Settings := LocalParams.Settings;
  StartNewGame;
end;


procedure TFMain.FormDeactivate(Sender: TObject);
begin
  GameGrid.ClearDownCheck;
end;


procedure TFMain.FormDestroy(Sender: TObject);
begin
  LocalParams.Left := Left;
  LocalParams.Top := Top;
  LocalParams.SaveParams;
  FreeAndNil(GameGrid);
end;


function TFMain.FormHelp(Command: Word; Data: THelpEventData; var CallHelp: Boolean): Boolean;
begin
  CallHelp := false;
  //Lazy way callback
  if Command = 12345 then //Refresh
  begin
    pnlMine.Caption := IntToStr(GameGrid.Mines_Flags);
    UpdateGraph;
  end;
end;


procedure TFMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin //TODO  Delete debug
  //if (Key = VK_F1) then ;
end;

procedure TFMain.FormPaint(Sender: TObject);
var crdLeft, crdTop, crdRight, crdBottom: integer;
begin
  crdLeft := imgGrid.ClientToParent(imgGrid.ClientRect.TopLeft).X;
  crdTop  := imgGrid.ClientToParent(imgGrid.ClientRect.TopLeft).Y;
  crdRight := imgGrid.ClientToParent(imgGrid.ClientRect.BottomRight).X;
  crdBottom := imgGrid.ClientToParent(imgGrid.ClientRect.BottomRight).Y;
  Canvas.StretchDraw(Rect(crdLeft, crdTop, crdRight, crdBottom), GameGrid.RenderGrid.FieldImage);
end;


procedure TFMain.FormShow(Sender: TObject);
begin
  Left := LocalParams.Left;
  Top := LocalParams.Top;
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
    LButton := LButton or (Button = mbLeft);  //More stable catch mouse buttons (for fast play)
    RButton := RButton or (Button = mbRight);
    if     LButton and not RButton then GameGrid.FieldClick(NewCell.X, NewCell.Y) //On Left Click
    else
    if not LButton and     RButton then GameGrid.FieldMark (NewCell.X, NewCell.Y) //On Righ Click
    else
    if     LButton and     RButton then GameGrid.FieldCheck(NewCell.X, NewCell.Y) //On Left-Righ Click
    else
      GameGrid.ClearDownCheck(true);
    Refresh;
  end;
  if  GameGrid.GameState = gsLoose then Loose;
  if  GameGrid.GameState = gsSuccess then Success;
end;


function TFMain.LoadGame: boolean;
var Res: integer;
begin
  PrepareForm(LocalParams.GridData);
  result := false;
  var aForm: TForm := CreateMessageDialog('Do you want to continue your saved game?', mtConfirmation,
        [mbYes, mbNo], mbYes,
        ['Yes', 'No']);
   try
    aForm.Caption := 'Saved Game Found';
    //aForm.Position := poMainFormCenter; //::AN 2025-04-24 Not works, if main form moved from code
    aForm.Left := Self.Left + (Self.Width div 2) - (aForm.Width div 2);
    aForm.Top := Self.Top + (Self.Height div 2) - (aForm.Height div 2);
    Res := aForm.ShowModal;
  finally
    FreeAndNil(aForm);
    FocusMe;
  end;
  if Res = mrYes then
    result := GameGrid.LoadFromFile;
  if result then
  begin
    LocalParams.GridData := GameGrid.FGridData;
    PrepareForm(LocalParams.GridData);
    GameGrid.RecountFlags;
    GameGrid.RenderField(true);
  end;
end;


procedure TFMain.PrepareForm(aGridData: TGridData);
var dW, dH: integer;
begin
  if  (imgGrid.Width  = aGridData.Width  * ccCellW)
  and (imgGrid.Height = aGridData.Height * ccCellH) then
  begin
    imgGrid.Invalidate;
    exit;
  end;

  //Only manual control works precise
  Constraints.MaxWidth := 0;
  Constraints.MaxHeight := 0;

  imgGrid.Top  := imgGrid.Margins.Top;
  imgGrid.Left := imgGrid.Margins.Left;

  imgGrid.Left := imgGrid.Left;
  imgGrid.Top := imgGrid.Top;
  imgGrid.Width := aGridData.Width  * ccCellW;
  imgGrid.Height := aGridData.Height * ccCellH;

  imgGrid.Width  := aGridData.Width  * ccCellW;
  imgGrid.Height := aGridData.Height * ccCellH;

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
  Refresh;
end;


function TFMain.ShowDifficulty: boolean;
var Res: integer;
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
      if LocalParams.GridData.IsSame(FOptions.GetGridData) then exit;
      if GameGrid.GameState <> gsActive then
      begin
        LocalParams.GridData := FOptions.GetGridData;
        exit(true);
      end
      else
      begin
        var aForm: TForm := CreateMessageDialog('    These settings won''t apply to the game in progress. What do you want to do?    '#13#10#13#10+
        '1. Quit and start a new game with the new settings'#13#10#13#10+
        '2. Finish the game',
        mtConfirmation,
        [mbYes, mbNo], mbYes,
        [' Start a new game ',
        ' Finish the game ']);
        try
          aForm.Caption := 'Changed Game Settings';
          //aForm.Position := poMainFormCenter; //::AN 2025-04-24 Not works, if main form moved from code
          aForm.Left := Self.Left + (Self.Width div 2) - (aForm.Width div 2);
          aForm.Top := Self.Top + (Self.Height div 2) - (aForm.Height div 2);
          Res :=  aForm.ShowModal;
        finally
          FreeAndNil(aForm);
          FocusMe;
        end;
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


procedure TFMain.StartNewGame(aForce: boolean = false);
var Res: integer;
begin
  if (not aForce) then
  begin
    if (GameGrid.GameState = gsActive) then
    begin
      var aForm: TForm := CreateMessageDialog('    What do you want to do with the game in progress?    ', mtConfirmation,
      [mbYes, mbNo], mbYes,
      [' Start a new game ',
      ' Keep playing ']);
      try
        aForm.Caption := 'New Game';
        //aForm.Position := poMainFormCenter; //::AN 2025-04-24 Not works, if main form moved from code
        aForm.Left := Self.Left + (Self.Width div 2) - (aForm.Width div 2);
        aForm.Top := Self.Top + (Self.Height div 2) - (aForm.Height div 2);
        Res :=  aForm.ShowModal;
      finally
        FreeAndNil(aForm);
        FocusMe;
      end;
    end;
    if Res = mrNo then exit;
  end;
  GameGrid.Init(LocalParams.GridData);
  PrepareForm(LocalParams.GridData);
end;


procedure TFMain.Loose;
var Res: integer;
begin
  GameGrid.PlaySound(csndLoose);
  GameGrid.RenderLoose;
  UpdateGraph;
  var aForm: TForm := CreateMessageDialog('     Sorry, you lost this game. Better luck next time!    '#13#10#13#10 +
      //format('Time: %d seconds', [GameGrid.FGridData.Ticks div 1000]) + #13#10 +
      //format('Best time: %d seconds  Date: %', [0]) + #13#10 +
      //format('Games played: %d', [0]) + #13#10 +
      #13#10, //format('Games won: %d  Percentage: %', [0]),
      mtConfirmation,
      [mbYes, mbNo], mbYes,
      [' Exit ', ' Play again ']);
  try
    aForm.Caption := 'Game Lost';
    //aForm.Position := poMainFormCenter; //::AN 2025-04-24 Not works, if main form moved from code
    aForm.Left := Self.Left + (Self.Width div 2) - (aForm.Width div 2);
    aForm.Top := Self.Top + (Self.Height div 2) - (aForm.Height div 2);
    res := aForm.ShowModal;
  finally
    FreeAndNil(aForm);
    FocusMe;
  end;
  if Res = mrNo then
    StartNewGame(true)
  else
    Close;
end;


procedure TFMain.Success;
var Res: integer;
begin
  GameGrid.PlaySound(csndSuccess);
  GameGrid.RenderSuccess;
  UpdateGraph;
  pnlMine.Caption := '0';
  var aForm: TForm := CreateMessageDialog('    Congratulations, you won the game!    '#13#10#13#10 +
      //format('Time: %d seconds   Date:%s', [GameGrid.FGridData.Ticks div 1000, '-']) + #13#10 +
      //format('Best time: %d seconds  Date: %', [0]) + #13#10 +
      //format('Games played: %d', [0]) + #13#10 +
      #13#10, //format('Games won: %d  Percentage: %', [0]),
      mtConfirmation,
      [mbYes, mbNo], mbYes,
      [' Exit ', ' Play again ']);
  try
    aForm.Caption := 'Game Won';
    //aForm.Position := poMainFormCenter; //::AN 2025-04-24 Not works, if main form moved from code
    aForm.Left := Self.Left + (Self.Width div 2) - (aForm.Width div 2);
    aForm.Top := Self.Top + (Self.Height div 2) - (aForm.Height div 2);
    res := aForm.ShowModal;
  finally
    FreeAndNil(aForm);
    FocusMe;
  end;
  if Res = mrNo then
    StartNewGame(true)
  else
    Close;
end;


procedure TFMain.TimerTimer(Sender: TObject);
begin
  UpdateTimer;
end;


procedure TFMain.UpdateTimer;
begin
  pnlTime.Caption := IntToStr(Min(999, GameGrid.TimerCount));
end;

end.
