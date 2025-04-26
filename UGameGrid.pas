unit UGameGrid;
///
/// MineSweeperD12 (RAD Programmer Challenge #1)
///
/// Mine fieeld class
///
/// Alex Nevskiy 2025-04-11
/// (refactoring)
///
interface

uses Classes, Graphics, SysUtils, Vcl.Imaging.Pngimage, Forms, Types, Generics.Collections, MMSystem,
     UUtility, URenderGrid, USound;

type
  TGameState = (gsNew, gsRestored, gsActive, gsSuccess, gsLoose);

  TGameGridClass = class
    FGameState: TGameState;
    FSettings: TSettings;
    FGameTicks: UInt64;
    FGridData: TGridData;
    Flags: integer;
    FDrawForm: TForm;

    DraftField: TDraftField;
    MineField: TGameGridCells;
    RenderGrid: TRenderGrid;
    GameSound: TSoundClass;
  private
    procedure ScrollToCell(toX, toY: integer); //+
    procedure PrepareMineField; //+
    procedure CellClick(X, Y: integer; ClickSound: boolean = true);
    function CheckAround(X, Y: integer; anOpenList: TList<TPoint>): boolean;
  public
    Input: TInput;
    procedure RecountFlags; //+
    function PlaySound(aSound: integer): boolean;
    function isValid(X, Y: integer): boolean; overload;
    function IsValid(aPoint: TPoint): boolean; overload;
    procedure FieldClick(X, Y: integer); //On Left Click
    procedure FieldMark (X, Y: integer); //On Righ Click
    function FieldCheck(X, Y: integer): boolean; //On Left-Righ Click     (MouseUp)
    procedure SetDownCheck(X, Y: integer); //On Left-Righ Click (MouseDown)
    procedure ClearDownCheck(aForce:boolean = false);
    procedure Init(aGridData: TGridData); //+
    function IsSaveExists: boolean;
    function LoadFromFile: boolean; //+
    function SaveToFile: boolean;   //+
    function Mines_Flags: integer; //+
    function TimerCount: integer;  //+
    function RenderField(aForce: boolean = false): boolean;
    procedure RenderLoose;
    procedure RenderSuccess;
    property GameState: TGameState read FGameState;
    property Settings: TSettings read FSettings write FSettings;
    function DetectSuccess: boolean;

    constructor Create(aDrawForm: TForm);
    destructor Destroy; override;
  end;

implementation

uses Controls, Dialogs, ZLib, StrUtils, Math, Windows, IOUtils;


{ TGameGridClass }

procedure TGameGridClass.CellClick(X, Y: integer; ClickSound: boolean = true);
var ToOpen: TList<TPoint>;
    Proceed, Start, Stop, Idx: integer;

    function Check(nX, nY: integer): integer;
    begin
      result := 0;
      if not IsValid(nX, nY) then exit;
      if MineField[nX, nY].Mine then exit;
      if MineField[nX, nY].Flag then exit;
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
  If MineField[X, Y].Mine then //BOOM!
  begin
    MineField[X, Y].Checked := true;
    FGameState := gsLoose;
    exit;
  end;
  If MineField[X, Y].Number > 0 then
  begin
    if ClickSound then
      PlaySound(csndClick);
    MineField[X, Y].Open;
    if isValid(X, Y + 1) then MineField[X, Y + 1].ToRender := true; //Redraw grid to remove shadow
    if isValid(X + 1, Y) then MineField[X + 1, Y].ToRender := true; //Redraw grid to remove shadow
  end
  else
  begin //Open empty area
    ToOpen := TList<TPoint>.Create;
    try
      PlaySound(csndArea);
      ToOpen.Clear;
      ToOpen.Add(Point(X, Y));
      MineField[X, Y].Open;
      RecountFlags;
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
        if Settings.Animation then
        begin
          RenderField;  //Animation
          Sleep(10);
        end;
      end;
      for x := 0 to FGridData.Width - 1 do
        for y := 0 to FGridData.Height - 1 do
          MineField[x, y].Checked := false;
    finally
      FreeAndNil(ToOpen);
    end;
  end;
  RenderField;
  DetectSuccess;
end;


function TGameGridClass.CheckAround(X, Y: integer; anOpenList: TList<TPoint>): boolean;
var FlagsCnt: integer;

    procedure CountAround(aX, aY: integer);
    begin
      if isValid(ax, ay) then
      begin
        if MineField[ax, ay].Flag then
          inc(FlagsCnt)
        else
        if MineField[ax, ay].Covered then
          anOpenList.Add(Point(ax, ay));
      end;
    end;

begin
  result := false;
  FlagsCnt := 0;
  anOpenList.Clear;
  CountAround(x - 1, y - 1);
  CountAround(x    , y - 1);
  CountAround(x + 1, y - 1);

  CountAround(x - 1, y    );
  CountAround(x + 1, y    );

  CountAround(x - 1, y + 1);
  CountAround(x    , y + 1);
  CountAround(x + 1, y + 1);

  if MineField[x, y].Number = FlagsCnt then exit(true);
  if MineField[x, y].Number > FlagsCnt then exit(false);
  PlaySound(csndCheck);
  anOpenList.Clear;
end;


procedure TGameGridClass.ClearDownCheck(aForce:boolean = false);
begin
  for var x: integer := 0 to FGridData.Width - 1 do
    for var y: integer := 0 to FGridData.Height - 1 do
      MineField[x, y].DownCheck := false;
  RenderField(aForce);
end;


constructor TGameGridClass.Create(aDrawForm: TForm);
begin
  FGameState := gsNew;
  FDrawForm := aDrawForm;
  FGridData.Ticks := 0;
  RenderGrid := TRenderGrid.Create;
  GameSound := TSoundClass.Create;
  GameSound.LoadSounds(csStyleDef);
  FSettings.Init;
end;


destructor TGameGridClass.Destroy;
begin
  FreeAndNil(GameSound);
  FreeAndNil(RenderGrid);
  inherited;
end;


function TGameGridClass.DetectSuccess: boolean;
var FlaggedMines: integer;
    CoveredMines: integer;
begin
  result := false;
  FlaggedMines := 0;
  CoveredMines := 0;
  for var x: integer := 0 to FGridData.Width - 1 do
    for var y: integer := 0 to FGridData.Height - 1 do
    begin
      if (MineField[x, y].Covered) and (not MineField[x, y].Mine) then exit(false);  //Not opened all tiles w/o mines
      if (MineField[x, y].Covered) and (MineField[x, y].Mine) and (MineField[x, y].Flag) then inc(FlaggedMines);
      if (MineField[x, y].Covered) and (MineField[x, y].Mine) and (not MineField[x, y].Flag) then inc(CoveredMines);
    end;
  result := FlaggedMines + CoveredMines = FGridData.Mines;
  if result then
  begin
    FGameState := gsSuccess;
    Flags := FGridData.Mines;
  end;
end;


function TGameGridClass.FieldCheck(X, Y: integer): boolean;
var ToOpenList: TList<TPoint>;
    Idx: integer;
begin
  result := false;
  ClearDownCheck(true);
  if (MineField[x, y].Covered) or (MineField[x, y].Number = 0) then exit;
  ToOpenList := TList<TPoint>.Create;
  try
    var Res: boolean := CheckAround(X, Y, ToOpenList);
    if ToOpenList.Count = 0 then exit;

    if Res then
    begin
      PlaySound(csndCheck);
      for Idx := ToOpenList.Count - 1 downto 0 do
        if not MineField[ToOpenList[Idx].X, ToOpenList[Idx].Y].Mine then //Open tiles w/o mines
        begin
          CellClick(ToOpenList[Idx].X, ToOpenList[Idx].Y, false);
          ToOpenList.Delete(Idx);
        end;

      if ToOpenList.Count > 0 then //Mines to explode            +
      begin
        for Idx := ToOpenList.Count - 1 downto 0 do
          MineField[ToOpenList[Idx].X, ToOpenList[Idx].Y].Checked := true;
        FGameState := gsLoose;
        result := true;
      end;
    end;
  finally
    RenderField(true);
    FreeAndNil(ToOpenList);
  end;
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
  if not MineField[x, y].Covered then exit;
  MineField[x, y].ToRender := true;
  if MineField[x, y].Flag then
  begin
    PlaySound(csndFlagOut);
    MineField[x, y].Flag := false;
  end
  else
  begin
    PlaySound(csndFlagIn);
    MineField[x, y].Flag := true;
  end;
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


function TGameGridClass.IsSaveExists: boolean;
begin
  var FileName: string := GetSaveFileName(FGridData);
  result := FileExists(FileName);
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
  aVer := '   ';
  aFileName := GetSaveFileName(FGridData);
  if not FileExists(aFileName) then exit;
  TMS := TMemoryStream.Create;
  try
    FS := TFileStream.Create(aFileName, fmOpenRead);
    try
      LZip := TZDecompressionStream.Create(FS);
      try
        TMS.CopyFrom(LZip, 0);
      finally
        FreeAndNil(LZip);
      end;
      TMS.Position := 0;
      TMS.Read(aVer[1], ByteLength(cSaveVer));
      if not SameText(aVer, cSaveVer) then exit(false);
      TMS.Read(FGridData, SizeOf(FGridData));
      for var x: integer := 0 to FGridData.Width - 1 do
        for var y: integer := 0 to FGridData.Height - 1 do
          TMS.Read(MineField[x, y], SizeOf(MineField[x, y]));
      FGameTicks := Windows.GetTickCount64 - FGridData.Ticks;
      result := true;
    finally
      FreeAndNil(FS);
    end;
  finally
    FreeAndNil(TMS);
  end;
  if result then
  begin
    FGameState := gsActive;
    //TFile.Delete(aFileName);
  end;
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
    TMS.Write(cSaveVer[1], ByteLength(cSaveVer));
    TMS.Write(FGridData, SizeOf(FGridData));
    for var x: integer := 0 to FGridData.Width - 1 do
      for var y: integer := 0 to FGridData.Height - 1 do
        TMS.Write(MineField[x, y], SizeOf(MineField[x, y]));
    TMS.Position := 0;
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
      newX := IfThen(newX >= 0, newX mod (FGridData.Width  - 0), newX + (FGridData.Width  - 0));
      newY := IfThen(newY >= 0, newY mod (FGridData.Height - 0), newY + (FGridData.Height - 0));
      MineField[newX, newY].Mine := DraftField.FCells[x, y] = 1;
    end;
  PrepareMineField;
end;


procedure TGameGridClass.SetDownCheck(X, Y: integer);

  procedure SetDown(aX, aY: integer);
  begin
    if IsValid(aX, aY) and MineField[aX, aY].Covered and (not MineField[aX, aY].Flag) then
    begin
      MineField[aX, aY].DownCheck := true;
      MineField[aX, aY].ToRender := true;
    end;
  end;

begin
  ClearDownCheck;

  SetDown(x - 1, y - 1);
  SetDown(x    , y - 1);
  SetDown(x + 1, y - 1);

  SetDown(x - 1, y    );
  SetDown(x    , y    );
  SetDown(x + 1, y    );

  SetDown(x - 1, y + 1);
  SetDown(x    , y + 1);
  SetDown(x + 1, y + 1);

  RenderField(true);
end;


function TGameGridClass.Mines_Flags: integer;
begin
  result := FGridData.Mines - Flags;
  if result < -99 then
    result := -99;
end;


function TGameGridClass.PlaySound(aSound: integer): boolean;
begin
  if not Settings.Sounds then exit;
  if Settings.snd[aSound] then
    GameSound.PlaySounds(aSound);
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


function TGameGridClass.RenderField(aForce: boolean = false): boolean;
var Dummy: boolean;
begin
  result := RenderGrid.Render(MineField, aForce);
  if result then
    if Assigned(FDrawForm) then
      FDrawForm.OnHelp(12345, 0, Dummy); //Lazy way to make callback
end;


procedure TGameGridClass.RenderLoose;
begin
  RenderGrid.RenderLoose(MineField);
end;


procedure TGameGridClass.RenderSuccess;
begin
  RenderGrid.RenderSuccess(MineField);
end;


function TGameGridClass.TimerCount: integer;
begin
  if GameState = gsNew then exit(0);
  if GameState in [gsLoose, gsSuccess] then exit((FGridData.Ticks) div 1000);  //Freeze timer for Loose/Success
  FGridData.Ticks := Windows.GetTickCount64 - FGameTicks;  //Games timer based on system timer
  result := (FGridData.Ticks) div 1000;
end;

end.
