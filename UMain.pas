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
    pnlGrid: TPanel;
    imgTimer: TImage;
    pnlTime: TPanel;
    imgMine: TImage;
    pnlMine: TPanel;
    Timer: TTimer;
    imgPlaceholder: TPaintBox;
    procedure actOptionsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actNewGameExecute(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
  private
    resMine: TPngImage;
    shX, shY: integer;
    FGraph: TBitMap;
    FField: TBitMap;
    FMineCount: integer;
    CurrentTime: integer;
    imgLayout: TBitMap;
    LocalParams: TLocalParams;
    FieldInterpolation: TFieldInterpolation;
    GameGrid: TGameGrid;
    procedure SetGridSize(aGridData: TGridData);
    procedure ShowDifficulty;
    procedure StartNewGame;
    procedure FillVisualGrid(aGridData: TGridData);
    procedure UpdateTimer;
    //property MineCount
  public
  end;

var
  FMain: TFMain;

implementation

uses Math, UOptions;

{$R *.dfm}
//{$R Graph.res}

procedure TFMain.actNewGameExecute(Sender: TObject);
begin
  StartNewGame;
end;

procedure TFMain.actOptionsExecute(Sender: TObject);
begin
  ShowDifficulty;
end;


procedure TFMain.FillVisualGrid(aGridData: TGridData);
var x, y, dx, dy: integer;
begin
  //imgGrid.Canvas.Brush.Color := clblack;
  //imgGrid.Canvas.FillRect(rect(0, 0, imgGrid.Width - 1, imgGrid.Height - 1));
  dx := 4;
  dy := 0;
  FieldInterpolation.Init(aGridData);
  FField.Width := aGridData.Width * ccCellW;
  FField.Height := aGridData.Height * ccCellH;
  FField.Canvas.Brush.Color := clBlack;
  FField.Canvas.FillRect(rect(0, 0, FField.Width - 1, FField.Height - 1));

  for x := 0 to aGridData.Width - 1 do
  for y := 0 to aGridData.Height - 1 do
    //for x := 0 to aGridData.Width - 1 do
    begin
      var Block: integer := FieldInterpolation[x, y];
      var DestRec: TRect := Rect(x * ccCellW, y * ccCellW, x * ccCellW + ccCellW, y * ccCellW + ccCellH);
      //var SrcRec:  TRect := Rect(Block * (ccCellW + dx) + 2 , 2, Block * (ccCellW + dx) + 2 + ccCellW, 2 + ccCellH);
      var SrcRec:  TRect := Rect(Block * (ccCellW + dx) + 2 , 42, Block * (ccCellW + dx) + 2 + ccCellW, 42 + ccCellH);
      //FField.Canvas.CopyRect(Rect((x * ccCellW + dx), (y * ccCellW + dy), x * ccCellW + ccCellW, y * ccCellW + ccCellH), FGraph.Canvas, Rect(602, 2, 602 + ccCellW, 2 + ccCellH));
      FField.Canvas.CopyRect(DestRec, FGraph.Canvas, SrcRec);
      if GameGrid.FPreGen[x, y] = 1 then
      begin
        //SrcRec := Rect(cpMine.X, cpMine.Y, cpMine.X + ccCellW, cpMine.Y + ccCellH);
        //FField.Canvas.BrushCopy(DestRec, FGraph, SrcRec, clFuchsia);
        //FField.Canvas.CopyMode := cmSrcCopy;
        //FField.Canvas.CopyRect(DestRec, FGraph.Canvas, SrcRec);
        resMine.Draw(FField.Canvas, DestRec);
        //FField.Canvas.TextOut(x * ccCellW + 5, y * ccCellW + 5, '*');
      end;
    end;
end;

procedure TFMain.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
var nW, nH, dX, dY: integer;
    Ratio: Double;
begin
// 	Smooth Scalling - feature cut
//  Spent too much time but still not happy with the result
//
//  nW := LocalParams.GridData.Width * ccCellW;// + shX;
//  nH := LocalParams.GridData.Height* ccCellH;// + shY;
//  if (nH = 0) or (nW = 0) then exit;
//  Ratio := nW / nH;
//  dX := NewWidth - Width;
//  dY := NewHeight - Height;
//
//  if Abs(dX) > Abs(dY) then
//  begin
//    NewHeight := Trunc(NewWidth / Ratio);
//  end
//  else
//  if Abs(dX) < Abs(dY) then
//  begin
//    NewWidth := Trunc(NewHeight * Ratio);
//  end

//  if nW = nH then
//  begin
//    if (dX <> 0) and (dY = 0) then
//      NewHeight := NewHeight + dX
//    else
//    if (dX = 0) and (dY <> 0) then
//      NewWidth := NewWidth + dY
//    else
//    if (dX < 0) and (dY < 0) then
//    begin
//      NewWidth := NewWidth + dX;
//      NewHeight := NewHeight + dX;
//    end
//    else
//    if (dX > 0) and (dY > 0) then
//    begin
//      NewWidth := NewWidth + dX;
//      NewHeight := NewHeight + dX;
//    end
//    else
//      Resize := false;
//  end;

//  if (nH = 0) or (nW = 0) then exit;
//  Ratio := nW / nH;
//  dX := NewWidth - Width;
//  dY := NewHeight - Height;
////
//  if (dX <> 0) and (dY = 0) then
//    NewHeight := trunc(NewWidth / Ratio);
//  if (dX = 0) and (dY <> 0) then
//    NewWidth := trunc(NewHeight * Ratio);

//  if NewWidth > NewHeight * Ratio then
//    NewHeight := trunc(NewWidth / Ratio);
//  if NewWidth < NewHeight * Ratio then
//    NewWidth:= trunc( NewHeight * Ratio);
//  if NewHeight > NewWidth / Ratio then
//    NewWidth := trunc(NewHeight * Ratio);
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  shX := Width  - imgPlaceholder.Width;
  shY := Height - imgPlaceholder.Height;
  Caption := format(csFormCaption, [GetCurrentUser]);
  imgLayout := TBitMap.Create;
  FField := TBitMap.Create;
  FGraph := TBitMap.Create;
  //FGraph := TPngImage.Create;
  FGraph.LoadFromResourceName(HInstance, 'Graph1');
  //FGraph.LoadFromResourceName(HInstance, 'PngImage_1');
  resMine := TPngImage.Create;
  resMine.LoadFromResourceName(HInstance, 'resMine');
  LocalParams := TLocalParams.Create(True);
  StartNewGame;
end;


procedure TFMain.FormDestroy(Sender: TObject);
begin
  LocalParams.SaveParams;
  FreeAndNil(resMine);
  FreeAndNil(FGraph);
  FreeAndNil(FField);
  FreeAndNil(imgLayout);
end;


procedure TFMain.FormPaint(Sender: TObject);
var crdLeft, crdTop, crdRight, crdBottom: integer;
begin
  //Canvas.TextOut(10, 10, 'Test');
  //Canvas.Draw(12,102,FGraph);
  //FillVisualGrid(LocalParams.GridData);
  //pnlGrid.ClientToParent(pnlGrid.ClientRect.BottomRight)
  //pnlGrid.ClientRect.TopLeft
  //Canvas.Draw(12,102,FField);
  //crdLeft := pnlGrid.ClientToParent(pnlGrid.ClientRect.TopLeft).X + 2;
  //crdTop  := pnlGrid.ClientToParent(pnlGrid.ClientRect.TopLeft).Y + 2;
  //crdRight := pnlGrid.ClientToParent(pnlGrid.ClientRect.BottomRight).X - 2;
  //crdBottom := pnlGrid.ClientToParent(pnlGrid.ClientRect.BottomRight).Y - 2;
  crdLeft := pnlGrid.ClientToParent(pnlGrid.ClientRect.TopLeft).X + pnlGrid.BevelWidth;
  crdTop  := pnlGrid.ClientToParent(pnlGrid.ClientRect.TopLeft).Y + pnlGrid.BevelWidth;
  crdRight := pnlGrid.ClientToParent(pnlGrid.ClientRect.BottomRight).X - pnlGrid.BevelWidth;
  crdBottom := pnlGrid.ClientToParent(pnlGrid.ClientRect.BottomRight).Y - pnlGrid.BevelWidth;
  //crdLeft := pnlGrid.ClientToParent(imgPlaceholder.ClientRect.TopLeft).X + 1;
  //crdTop  := pnlGrid.ClientToParent(imgPlaceholder.ClientRect.TopLeft).Y + 1;
  //crdRight := pnlGrid.ClientToParent(imgPlaceholder.ClientRect.BottomRight).X + 1;
  //crdBottom := pnlGrid.ClientToParent(imgPlaceholder.ClientRect.BottomRight).Y + 1;
  Canvas.StretchDraw(Rect(crdLeft, crdTop, crdRight, crdBottom), FField);
end;

procedure TFMain.SetGridSize(aGridData: TGridData);
var dW, dH: integer;
begin
  imgLayout.Width := aGridData.Width * ccCellW;
  imgLayout.Height := aGridData.Height * ccCellH;

  imgLayout.Canvas.Brush.Color := clblack;
  imgLayout.Canvas.FillRect(rect(0, 0, imgLayout.Width - 1, aGridData.Height - 1));
  imgLayout.Canvas.CopyMode := cmSrcCopy;

  //dW := Width - imgGrid.Width;
  //dH := Height - imgGrid.Height;
  //dW := Width - (pnlGrid.Width + 4);
  //dH := Height - (pnlGrid.Height + 4);
  //Width := aGridData.Width * ccCellW + shX;
  //Height := aGridData.Height * ccCellH + shY;
  //imgGrid.Width := aGridData.Width;
  //imgGrid.Height := aGridData.Height;
//  AutoSize := false;
//  pnlGrid.AutoSize := false;
//  pnlBottom.Align := alNone;
//  pnlGrid.Align := alNone;
//  pnlGrid.AutoSize := true; //Not works properly
//
//  imgPlaceholder.Width  := aGridData.Width  * ccCellW;
//  imgPlaceholder.Height := aGridData.Height * ccCellH;
//  pnlGrid.Visible := true;
//  imgPlaceholder.Repaint;
//  pnlGrid.AutoSize := true;
//  //pnlBottom.Width  := pnlGrid.Width;
//  //pnlBottom.Height := pnlGrid.Height;
//  ClientWidth := pnlGrid.Width;
//  AutoSize := true;
//  pnlGrid.Align := alTop;
//  pnlBottom.Align := alTop;
//  pnlBottom.Align := alBottom;
//  pnlGrid.Align := alClient;
//  AutoSize := false;
//  Invalidate;


//  pnlBottom.Align := alNone;
//  pnlGrid.Align := alNone;
//  AutoSize := false;
//  imgPlaceholder.Width  := aGridData.Width  * ccCellW;
//  imgPlaceholder.Height := aGridData.Height * ccCellH;
//  pnlGrid.Width  := imgPlaceholder.Width;
//  pnlGrid.Height := imgPlaceholder.Height;
//  pnlBottom.Top := pnlGrid.BoundsRect.Bottom ;//+ 105;
//  ClientWidth := pnlGrid.Width;
//  ClientHeight := pnlBottom.BoundsRect.Bottom;
//  AutoSize := true;
//  pnlGrid.Align := alTop;
//  pnlBottom.Align := alTop;
//  pnlBottom.Align := alBottom;
//  AutoSize := false;
//  pnlGrid.Align := alNone;
//  pnlGrid.Width  := imgPlaceholder.Width;
//  Invalidate;
//  Constraints.MaxWidth := Width;
//  Constraints.MaxHeight := Height;

  //Only manual control works precise
  Constraints.MaxWidth := 0;
  Constraints.MaxHeight := 0;

  pnlGrid.Top  := pnlGrid.Margins.Top;
  pnlGrid.Left := pnlGrid.Margins.Left;
  pnlGrid.Width  := aGridData.Width  * ccCellW + pnlGrid.BevelWidth * 2;
  pnlGrid.Height := aGridData.Height * ccCellH + pnlGrid.BevelWidth * 2;

  pnlBottom.Top := pnlGrid.BoundsRect.Bottom + pnlGrid.Margins.Bottom;
  pnlBottom.Left := pnlBottom.Margins.Left;
  pnlBottom.Width := pnlGrid.Width;

  ClientWidth := pnlGrid.BoundsRect.Right + pnlGrid.Margins.Right;
  ClientHeight := pnlBottom.BoundsRect.Bottom + pnlBottom.Margins.Bottom;
  Constraints.MaxWidth := Width;
  Constraints.MaxHeight := Height;
  Invalidate;
end;


procedure TFMain.ShowDifficulty;
begin
  FOptions := TFOptions.Create(Self);
  try
    FOptions.SetGridData(LocalParams.GridData);
    if FOptions.ShowModal = mrOk then
      LocalParams.GridData := FOptions.GetGridData;
  finally
    FreeAndNil(FOptions);
  end;
end;


procedure TFMain.StartNewGame;
begin
  //TODO: Check current game status
  CurrentTime := 0;
  GameGrid.Init(LocalParams.GridData);
  FillVisualGrid(LocalParams.GridData);
  SetGridSize(LocalParams.GridData);
end;


procedure TFMain.TimerTimer(Sender: TObject);
begin
  UpdateTimer;
end;


procedure TFMain.UpdateTimer;
begin
  pnlTime.Caption := IntToStr(Min(999, CurrentTime));
end;

end.
