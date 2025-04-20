unit UGameGrid;
///
/// MineSweeperD12 (RAD Programmer Challenge #1))
///
/// Mine fieeld class
///
/// Alex Nevskiy 2025-04-11
/// (refactoring)
///
interface

uses Classes, Graphics, SysUtils, Vcl.Imaging.Pngimage, Forms, Types, Generics.Collections,
     UUtility, URenderGrid;

type
  TGameState = (gsNew, {gsPrepared,} gsRestored, gsActive, gsWin, gsLoose);

  TGameGridClass = class
    FGameState: TGameState;
    FGameTicks: UInt64;
    FGridData: TGridData;
    Flags: integer;
    FDrawForm: TForm;

    DraftField: TDraftField;
    MineField: TGameGridCells;
    RenderGrid: TRenderGrid;
  private
    procedure ScrollToCell(toX, toY: integer); //+
    procedure PrepareMineField; //+
    procedure RecountFlags; //+
    procedure CellClick(X, Y: integer);
    function CheckAround(X, Y: integer; anOpenList: TList<TPoint>): boolean;
  public
    Input: TInput;
    function PlaySound(aSound: integer): boolean;
    function isValid(X, Y: integer): boolean; overload;
    function IsValid(aPoint: TPoint): boolean; overload;
    procedure FieldClick(X, Y: integer); //On Left Click
    procedure FieldMark (X, Y: integer); //On Righ Click
    procedure FieldCheck(X, Y: integer); //On Left-Righ Click     (MouseUp)
    procedure FieldCheckShow(X, Y: integer; Show: boolean); //On Left-Righ Click (MouseDown)
    procedure Init(aGridData: TGridData); //+
    function LoadFromFile: boolean; //+
    function SaveToFile: boolean;   //+
    function Mines_Flags: integer; //+
    function TimerCount: integer;  //?
    function RenderField: boolean;
    property GameState: TGameState read FGameState;

    procedure DEBUG_ShowMines;
    constructor Create(aDrawForm: TForm);
    destructor Destroy; override;
  end;

implementation

uses ZLib, StrUtils, Math, Windows, IOUtils;


{ TGameGridClass }

procedure TGameGridClass.CellClick(X, Y: integer);
var ToOpen: TList<TPoint>;
    Proceed, Start, Stop, Idx: integer;

    function Check(nX, nY: integer): integer;
    begin
      result := 0;
      if not IsValid(nX, nY) then exit;
      if MineField[nX, nY].Mine then exit;
      MineField[nX, nY].Open;
      if isValid(nX, nY + 1) then MineField[nX, nY + 1].ToRender := true; //Redraw grid to remove shadow
      if isValid(nX + 1, nY) then MineField[nX + 1, nY].ToRender := true; //Redraw grid to remove shadow
      if (MineField[nX, nY].Number = 0) and (not MineField[nX, nY].Checked) then
      begin
        ToOpen.Add(Point(nX, nY));
        result := 1;
      end;
      MineField[nX, nY].Checked := true;
    end;

begin
  if not isValid(X, Y) then exit;
  If MineField[X, Y].Mine then ;//BOOM!
  If MineField[X, Y].Number > 0 then
  begin
    MineField[X, Y].Open;
    if isValid(X, Y + 1) then MineField[X, Y + 1].ToRender := true; //Redraw grid to remove shadow
    if isValid(X + 1, Y) then MineField[X + 1, Y].ToRender := true; //Redraw grid to remove shadow
  end
  else
  begin //Open empty area
    ToOpen := TList<TPoint>.Create;
    try
      ToOpen.Clear;
      ToOpen.Add(Point(X, Y));
      MineField[X, Y].Open;
      Proceed := 1;
      Stop := 0;
      while Proceed > 0 do  //Non-Recursion version.
      begin
        Proceed := 0;
        Start := Stop;
        Stop := ToOpen.Count;
        for Idx := Start to Stop - 1 do
        begin
          x := ToOpen[Idx].X;
          y := ToOpen[Idx].Y;
          Proceed := Proceed + Check(x - 1, y - 1);
          Proceed := Proceed + Check(x    , y - 1);
          Proceed := Proceed + Check(x + 1, y - 1);

          Proceed := Proceed + Check(x - 1, y    );
          Proceed := Proceed + Check(x + 1, y    );

          Proceed := Proceed + Check(x - 1, y + 1);
          Proceed := Proceed + Check(x    , y + 1);
          Proceed := Proceed + Check(x + 1, y + 1);
        end;
        //RenderField;  //Animation
      end;
      for x := 0 to FGridData.Width - 1 do
        for y := 0 to FGridData.Height - 1 do
          MineField[x, y].Checked := false;
    finally
      FreeAndNil(ToOpen);
    end;
  end;
  RenderField;
end;


function TGameGridClass.CheckAround(X, Y: integer; anOpenList: TList<TPoint>): boolean;
var FlagsCnt: integer;
begin
  result := false;
  FlagsCnt := 0;
  anOpenList.Clear;
  if isValid(x - 1, y - 1) then
  begin
    if MineField[x - 1, y - 1].Flag then inc(FlagsCnt);
    if MineField[x - 1, y - 1].Covered then anOpenList.Add(Point(x - 1, y - 1));
  end;

  if isValid(x, y - 1) then
  begin
    if MineField[x, y - 1].Flag then inc(FlagsCnt);
    if MineField[x, y - 1].Covered then anOpenList.Add(Point(x, y - 1));
  end;

  if isValid(x + 1, y - 1) then
  begin
    if MineField[x + 1, y - 1].Flag then inc(FlagsCnt);
    if MineField[x + 1, y - 1].Covered then anOpenList.Add(Point(x + 1, y - 1));
  end;

  if isValid(x - 1, y) then
  begin
    if MineField[x - 1, y].Flag then inc(FlagsCnt);
    if MineField[x - 1, y].Covered then anOpenList.Add(Point(x - 1, y));
  end;

  if isValid(x + 1, y) then
  begin
    if MineField[x + 1, y].Flag then inc(FlagsCnt);
    if MineField[x + 1, y].Covered then anOpenList.Add(Point(x + 1, y));
  end;

  if isValid(x - 1, y + 1) then
  begin
    if MineField[x - 1, y + 1].Flag then inc(FlagsCnt);
    if MineField[x - 1, y + 1].Covered then anOpenList.Add(Point(x - 1, y + 1));
  end;

  if isValid(x, y + 1) then
  begin
    if MineField[x, y + 1].Flag then inc(FlagsCnt);
    if MineField[x, y + 1].Covered then anOpenList.Add(Point(x, y + 1));
  end;

  if isValid(x + 1, y + 1) then
  begin
    if MineField[x + 1, y + 1].Flag then inc(FlagsCnt);
    if MineField[x + 1, y + 1].Covered then anOpenList.Add(Point(x + 1, y + 1));
  end;

  if MineField[x, y].Number = FlagsCnt then exit(true);
  if MineField[x, y].Number > FlagsCnt then exit(false);
  PlaySound(csndWrong);
  anOpenList.Clear;
end;

constructor TGameGridClass.Create(aDrawForm: TForm);
begin
  FGameState := gsNew;
  FDrawForm := aDrawForm;
  FGridData.Ticks := 0;
  RenderGrid := TRenderGrid.Create;
end;


procedure TGameGridClass.DEBUG_ShowMines;
begin
  RenderGrid.DEBUG_ShowMines;
  RenderField;
end;

destructor TGameGridClass.Destroy;
begin

  FreeAndNil(RenderGrid);
  inherited;
end;


procedure TGameGridClass.FieldCheck(X, Y: integer);
var ToOpenList: TList<TPoint>;
begin
  if (MineField[x, y].Covered) or (MineField[x, y].Number = 0) then exit;
  ToOpenList := TList<TPoint>.Create;
  try
    CheckAround(X, Y, ToOpenList);
    if ToOpenList.Count = 0 then exit;
    var Idx: integer;
    for Idx := 0 to ToOpenList.Count - 1 do
    begin

    end;

  finally
    FreeAndNil(ToOpenList);
  end;
end;


procedure TGameGridClass.FieldCheckShow(X, Y: integer; Show: boolean);
begin

end;


procedure TGameGridClass.FieldClick(X, Y: integer);
begin
  if not isValid(X, Y) then exit;
  if GameState = gsNew then
  begin
    DraftField.Generate(FGridData);
    ScrollToCell(X, Y);
    FGameState := gsActive;
    FGameTicks := Windows.GetTickCount64;
    CellClick(X, Y);
    exit;
  end;
  if not MineField[X, Y].Covered then exit;
  CellClick(X, Y);
end;


procedure TGameGridClass.FieldMark(X, Y: integer);
begin
  if not isValid(X, Y) then exit;
  MineField[x, y].ToRender := true;
  MineField[x, y].Flag := not MineField[x, y].Flag;
  RecountFlags;
  RenderField;
end;


procedure TGameGridClass.Init(aGridData: TGridData);
begin
  FGridData := aGridData;
  RenderGrid.Init(aGridData);
  DraftField.Generate(aGridData);
  SetLength(MineField, FGridData.Width, FGridData.Height);
  for var x: integer := 0 to FGridData.Width - 1 do
    for var y: integer := 0 to FGridData.Height - 1 do
      MineField[x, y].Init;
  FGameState := gsNew;
  RenderField;
end;


function TGameGridClass.isValid(aPoint: TPoint): boolean;
begin
  result := isValid(aPoint.X, aPoint.Y);
end;


function TGameGridClass.isValid(X, Y: integer): boolean;
begin
  result := (x >= 0) and (x < FGridData.Width) and (y >= 0) and (y < FGridData.Height);
end;


function TGameGridClass.LoadFromFile: boolean;
var TMS: TMemoryStream;
    LZip: TZDecompressionStream;
    FS: TFileStream;
    aSize: integer;
    aFileName: string;
    aVer: string[3];
begin
  result := false;
  aFileName := GetSaveFileName(FGridData);
  if not FileExists(aFileName) then exit;
  TMS := TMemoryStream.Create;
  try
    FS := TFileStream.Create(aFileName, fmCreate);
    try
      LZip := TZDecompressionStream(FS);
      try
        TMS.CopyFrom(LZip, 0);
        TMS.Read(aVer, SizeOf(cSaveVer));
        if not SameText(aVer, cSaveVer) then exit(false);
        TMS.Read(FGridData, SizeOf(FGridData));
        for var x: integer := 0 to FGridData.Width - 1 do
          for var y: integer := 0 to FGridData.Height - 1 do
            TMS.Read(MineField[x, y], SizeOf(MineField[x, y]));
        FGameTicks := Windows.GetTickCount64 - FGridData.Ticks;
        result := true;
      finally
        FreeAndNil(LZip);
      end;
    finally
      FreeAndNil(FS);
    end;
  finally
    FreeAndNil(TMS);
  end;
  if result then
   // TFile.Delete(aFileName);
end;


function TGameGridClass.SaveToFile: boolean;
var TMS: TMemoryStream;
    LZip: TZCompressionStream;
    FS: TFileStream;
    aSize: integer;
begin
  result := false;
  TMS := TMemoryStream.Create;
  try
    TMS.Clear;
    TMS.Position := 0;
    TMS.Write(cSaveVer, SizeOf(cSaveVer));
    TMS.Write(FGridData, SizeOf(FGridData));
    for var x: integer := 0 to FGridData.Width - 1 do
      for var y: integer := 0 to FGridData.Height - 1 do
        TMS.Write(MineField[x, y], SizeOf(MineField[x, y]));
    FS := TFileStream.Create(GetSaveFileName(FGridData), fmCreate);
    try
      LZip := TZCompressionStream.Create(clDefault, FS);
      try
        LZip.CopyFrom(TMS, TMS.Size);
        result := true;
      finally
        FreeAndNil(LZip);
      end;
    finally
      FreeAndNil(FS);
    end;
  finally
    FreeAndNil(TMS);
  end;
end;


procedure TGameGridClass.ScrollToCell(toX, toY: integer);
var dX, dY: integer;
    newX, newY: integer;
begin
  dX := toX - (FGridData.Width  div 2);
  dY := toY - (FGridData.Height div 2);
  for var y: integer := 0 to FGridData.Height - 1 do
    for var x: integer := 0 to FGridData.Width - 1 do
    begin
      newX := x + dX;
      newY := y + dY;
      newX := IfThen(newX >= 0, newX mod (FGridData.Width  - 1), newX + (FGridData.Width  - 1));
      newY := IfThen(newY >= 0, newY mod (FGridData.Height - 1), newY + (FGridData.Height - 1));
      MineField[newX, newY].Mine := DraftField.FCells[x, y] = 1;
    end;
  PrepareMineField;
end;


function TGameGridClass.Mines_Flags: integer;
begin
  result := FGridData.Mines - Flags;
  if result < -99 then
    result := -99
end;


function TGameGridClass.PlaySound(aSound: integer): boolean;
begin

end;

procedure TGameGridClass.PrepareMineField;
begin
  for var y: integer := 0 to FGridData.Height - 1 do
    for var x: integer := 0 to FGridData.Width - 1 do
      if MineField[X, Y].Mine then
      begin
        if IsValid(x - 1, y - 1) then MineField[X - 1, Y - 1].IncNumber;
        if IsValid(x    , y - 1) then MineField[X    , Y - 1].IncNumber;
        if IsValid(x + 1, y - 1) then MineField[X + 1, Y - 1].IncNumber;

        if IsValid(x - 1, y    ) then MineField[X - 1, Y    ].IncNumber;
        if IsValid(x + 1, y    ) then MineField[X + 1, Y    ].IncNumber;

        if IsValid(x - 1, y + 1) then MineField[X - 1, Y + 1].IncNumber;
        if IsValid(x    , y + 1) then MineField[X    , Y + 1].IncNumber;
        if IsValid(x + 1, y + 1) then MineField[X + 1, Y + 1].IncNumber;
      end;
end;


procedure TGameGridClass.RecountFlags;
begin
  Flags := 0;
  for var x: integer := 0 to FGridData.Width - 1 do
    for var y: integer := 0 to FGridData.Height - 1 do
      if MineField[x, y].Flag then inc(Flags);
end;


function TGameGridClass.RenderField: boolean;
var Dummy: boolean;
begin
  result := RenderGrid.Render(MineField);
  if result then
    if Assigned(FDrawForm) then
      FDrawForm.OnHelp(12345, 0, Dummy); //Lazy way to make callback
end;


function TGameGridClass.TimerCount: integer;
begin
  if GameState <> gsActive then exit(0);
  FGridData.Ticks := Windows.GetTickCount64 - FGameTicks;
  result := (FGridData.Ticks) div 1000;
end;

end.
