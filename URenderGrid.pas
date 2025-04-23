unit URenderGrid;
///
/// MineSweeperD12 (RAD Programmer Challenge #1))
///
/// Mine fieeld class
///
/// Alex Nevskiy 2025-04-11
/// (refactoring)
///
interface

uses Classes, SysUtils, Vcl.Imaging.Pngimage, Winapi.Windows, Vcl.Graphics,
     UUtility;

type
  TRenderGrid = class
    GradientField: TGradientField;
    FGridData: TGridData;
    FDebug_ShowMines: boolean;
    //Bitmaps
    resImg: array [1..cixMax] of TPngImage;
    FGraph: TBitMap;
    FFieldImage: TBitmap;
    procedure LoadRes(aStyle: string = csStyleDef);
  public
    procedure DEBUG_ShowMines;
    function Render(var aGameGridCells: TGameGridCells; aForce: boolean = false): boolean;
    property FieldImage: TBitmap read FFieldImage;
    procedure Init(aGridData: TGridData; aStyle: string = csStyleDef);
    constructor Create;
    destructor Destroy; override;
  end;


implementation
uses UDebug;

{ TRenderGrid }

constructor TRenderGrid.Create;
begin
  FDebug_ShowMines := false;
  for var Idx: integer := Low(resImg) to High(resImg) do
    resImg[Idx] := TPngImage.Create;

  FFieldImage := TBitmap.Create;
  FGraph := TBitmap.Create;
end;


procedure TRenderGrid.DEBUG_ShowMines;
begin
  FDebug_ShowMines := not FDebug_ShowMines;
end;

destructor TRenderGrid.Destroy;
begin
  FreeAndNil(FGraph);
  FreeAndNil(FFieldImage);
  for var Idx: integer := High(resImg) downto Low(resImg) do
    FreeAndNil(resImg[Idx]);
  inherited;
end;


procedure TRenderGrid.Init(aGridData: TGridData; aStyle: string = csStyleDef);
begin
  FGridData := aGridData;
  LoadRes(aStyle);
  GradientField.Init(FGridData);
  FFieldImage.Width  := FGridData.Width  * ccCellW;
  FFieldImage.Height := FGridData.Height * ccCellH;
end;


procedure TRenderGrid.LoadRes(aStyle: string = csStyleDef);
begin
  FGraph.LoadFromResourceName(HInstance, format(csResLoad, [aStyle, 'img', 0]));
  for var Idx: integer := Low(resImg) to High(resImg) do
    resImg[Idx].LoadFromResourceName(HInstance, format(csResLoad, [aStyle, 'img', Idx]));
end;


function TRenderGrid.Render(var aGameGridCells: TGameGridCells; aForce: boolean = false): boolean;
var DestRec, SrcRec: TRect;
    x, y: integer;
begin
  result := false;
  for y := 0 to FGridData.Height - 1 do
    for x := 0 to FGridData.Width - 1 do
    begin
      var Shd: integer := 0;
      DestRec := Rect(x * ccCellW, y * ccCellW, x * ccCellW + ccCellW, y * ccCellW + ccCellH);
      if aGameGridCells[x, y].ToRender or aForce then
      begin
        result := true;
        aGameGridCells[x, y].ToRender := false;
        var Block: integer := GradientField[x, y];
        //DestRec := Rect(x * ccCellW, y * ccCellW, x * ccCellW + ccCellW, y * ccCellW + ccCellH);

        if aGameGridCells[x, y].Covered and aGameGridCells[x, y].Checked then
        begin
          SrcRec := Rect(Block * (ccCellW + 4) + 2 , ссGridY, Block * (ccCellW + 4) + 2 + ccCellW, ссGridY + ccCellH);
          FFieldImage.Canvas.CopyRect(DestRec, FGraph.Canvas, SrcRec);
          Continue;
        end;

        if aGameGridCells[x, y].Covered and (not aGameGridCells[x, y].DownCheck) then
          SrcRec := Rect(Block * (ccCellW + 4) + 2 , ссTileY, Block * (ccCellW + 4) + 2 + ccCellW, ссTileY + ccCellH)
        else
          SrcRec := Rect(Block * (ccCellW + 4) + 2 , ссGridY, Block * (ccCellW + 4) + 2 + ccCellW, ссGridY + ccCellH);
        FFieldImage.Canvas.CopyRect(DestRec, FGraph.Canvas, SrcRec);

        if aGameGridCells[x, y].Covered then
        begin
          if aGameGridCells[x, y].Flag then
            resImg[cixFlag].Draw(FFieldImage.Canvas, DestRec);
        end
        else
        begin
          if (aGameGridCells[x, y].Number > 0) then
            resImg[aGameGridCells[x, y].Number].Draw(FFieldImage.Canvas, DestRec);
        end;

        if (not aGameGridCells[x, y].Covered) or (aGameGridCells[x, y].DownCheck) then
        begin
          if (y - 1 < 0) or (aGameGridCells[x, y - 1].Covered and (not aGameGridCells[x, y - 1].DownCheck)) then Shd := Shd + 1;
          if (x - 1 < 0) or (aGameGridCells[x - 1, y].Covered and (not aGameGridCells[x - 1, y].DownCheck)) then Shd := Shd + 2;
          if Shd > 0 then
            resImg[cixShad1 + Shd - 1].Draw(FFieldImage.Canvas, DestRec);
        end;
//        if aGameGridCells[x, y].DownCheck then
//        begin
//          SrcRec := Rect(Block * (ccCellW + 4) + 2 , ссGridY, Block * (ccCellW + 4) + 2 + ccCellW, ссGridY + ccCellH);
//          FFieldImage.Canvas.CopyRect(DestRec, FGraph.Canvas, SrcRec);
//        end;

//        if not aGameGridCells[x, y].Covered then
//        begin
//          if (y - 1 < 0) or (aGameGridCells[x, y - 1].Covered) then Shd := Shd + 1;
//          if (x - 1 < 0) or (aGameGridCells[x - 1, y].Covered) then Shd := Shd + 2;
//          if Shd > 0 then
//            resImg[cixShad1 + Shd - 1].Draw(FFieldImage.Canvas, DestRec);
//        end;
        //if aGameGridCells[x, y].Mine then
        //  resImg[cixMine].Draw(FFieldImage.Canvas, DestRec);

      end;
      //if aGameGridCells[x, y].Checked then
      //  FFieldImage.Canvas.TextOut(x * ccCellW + 5, y * ccCellW + 5, 'X');
      if FDebug_ShowMines then
      begin
        if aGameGridCells[x, y].Mine then
          //FFieldImage.Canvas.TextOut(x * ccCellW + 5, y * ccCellW + 5, 'X');
          resImg[cixMine].Draw(FFieldImage.Canvas, DestRec);

       //if aGameGridCells[x, y].Number > 0 then
       //   FFieldImage.Canvas.TextOut(x * ccCellW + 15, y * ccCellW + 15, IntToStr(aGameGridCells[x, y].Number));
      end;
        //resImg[cixMine].Draw(FFieldImage.Canvas, DestRec);end;
    end;
end;

end.
