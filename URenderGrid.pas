unit URenderGrid;
///
/// MineSweeperD12 (RAD Programmer Challenge #1)
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
    resImg: array [1..cixMax] of TPngImage;
    FGraph: TBitMap;
    FFieldImage: TBitmap;
    procedure LoadRes(aStyle: string = csStyleDef);
  public
    function Render(var aGameGridCells: TGameGridCells; aForce: boolean = false): boolean;
    procedure RenderLoose(aGameGridCells: TGameGridCells);
    procedure RenderSuccess(aGameGridCells: TGameGridCells);
    property FieldImage: TBitmap read FFieldImage;
    procedure Init(aGridData: TGridData; aStyle: string = csStyleDef);
    constructor Create;
    destructor Destroy; override;
  end;


implementation

{ TRenderGrid }

constructor TRenderGrid.Create;
begin
  for var Idx: integer := Low(resImg) to High(resImg) do
    resImg[Idx] := TPngImage.Create;
  FFieldImage := TBitmap.Create;
  FGraph := TBitmap.Create;
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
      end;
    end;
end;

procedure TRenderGrid.RenderLoose(aGameGridCells: TGameGridCells);
var DestRec, SrcRec: TRect;
    x, y: integer;
begin
  //Prepare loose-specific render
  for y := 0 to FGridData.Height - 1 do
    for x := 0 to FGridData.Width - 1 do
    begin
      aGameGridCells[x, y].ToRender := not aGameGridCells[x, y].Covered; //Mark already opened cells to render numbers
      if aGameGridCells[x, y].Mine then aGameGridCells[x, y].Covered := false;
      if aGameGridCells[x, y].Flag then aGameGridCells[x, y].Covered := false;

      var Shd: integer := 0;
      var Block: integer := GradientField[x, y];
      DestRec := Rect(x * ccCellW, y * ccCellW, x * ccCellW + ccCellW, y * ccCellW + ccCellH);

        if aGameGridCells[x, y].Covered then
          SrcRec := Rect(Block * (ccCellW + 4) + 2 , ссTileY, Block * (ccCellW + 4) + 2 + ccCellW, ссTileY + ccCellH)
        else
          SrcRec := Rect(Block * (ccCellW + 4) + 2 , ссGridY, Block * (ccCellW + 4) + 2 + ccCellW, ссGridY + ccCellH);
        FFieldImage.Canvas.CopyRect(DestRec, FGraph.Canvas, SrcRec);

        if aGameGridCells[x, y].Mine then
          resImg[cixMineDark].Draw(FFieldImage.Canvas, DestRec);

        if aGameGridCells[x, y].Flag then
        begin
          if aGameGridCells[x, y].Mine then //Correct flag. Draw flag over mine
          begin
            resImg[cixFlag].Draw(FFieldImage.Canvas, DestRec);
          end
          else
          begin //Incorrect flag. Draw cross over mine
            resImg[cixMineDark].Draw(FFieldImage.Canvas, DestRec);
            resImg[cixCross].Draw(FFieldImage.Canvas, DestRec);
          end
        end;

        if aGameGridCells[x, y].Checked then //Fatal mine(s)
          resImg[cixMineRed].Draw(FFieldImage.Canvas, DestRec);

        if aGameGridCells[x, y].ToRender then //Render numbers on already opened cells
        begin
          if (aGameGridCells[x, y].Number > 0) then
            resImg[aGameGridCells[x, y].Number].Draw(FFieldImage.Canvas, DestRec);
        end;

        if (not aGameGridCells[x, y].Covered) then //Shadows
        begin
          if (y - 1 < 0) or (aGameGridCells[x, y - 1].Covered) then Shd := Shd + 1;
          if (x - 1 < 0) or (aGameGridCells[x - 1, y].Covered) then Shd := Shd + 2;
          if Shd > 0 then
            resImg[cixShad1 + Shd - 1].Draw(FFieldImage.Canvas, DestRec);
        end;
    end;
end;


procedure TRenderGrid.RenderSuccess(aGameGridCells: TGameGridCells);
var DestRec, SrcRec: TRect;
    x, y: integer;
begin
  //Prepare success-specific render
  for y := 0 to FGridData.Height - 1 do
    for x := 0 to FGridData.Width - 1 do
    begin
      var Shd: integer := 0;
      var Block: integer := GradientField[x, y];
      DestRec := Rect(x * ccCellW, y * ccCellW, x * ccCellW + ccCellW, y * ccCellW + ccCellH);

        if aGameGridCells[x, y].Covered then
          SrcRec := Rect(Block * (ccCellW + 4) + 2 , ссTileY, Block * (ccCellW + 4) + 2 + ccCellW, ссTileY + ccCellH)
        else
          SrcRec := Rect(Block * (ccCellW + 4) + 2 , ссGridY, Block * (ccCellW + 4) + 2 + ccCellW, ссGridY + ccCellH);
        FFieldImage.Canvas.CopyRect(DestRec, FGraph.Canvas, SrcRec);

        if aGameGridCells[x, y].Covered then
        begin
          if aGameGridCells[x, y].Mine then
            if aGameGridCells[x, y].Flag then
              resImg[cixFlag].Draw(FFieldImage.Canvas, DestRec)
            else
              resImg[cixMine].Draw(FFieldImage.Canvas, DestRec);
        end
        else
        if (aGameGridCells[x, y].Number > 0) then
          resImg[aGameGridCells[x, y].Number].Draw(FFieldImage.Canvas, DestRec);

        if (not aGameGridCells[x, y].Covered) then //Shadows
        begin
          if (y - 1 < 0) or (aGameGridCells[x, y - 1].Covered) then Shd := Shd + 1;
          if (x - 1 < 0) or (aGameGridCells[x - 1, y].Covered) then Shd := Shd + 2;
          if Shd > 0 then
            resImg[cixShad1 + Shd - 1].Draw(FFieldImage.Canvas, DestRec);
        end;
    end;
end;

end.
