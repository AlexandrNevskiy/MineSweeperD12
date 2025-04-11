unit UUtility;
///
/// MineSweeperD12 (RAD Programmer Challenge #1))
///
/// Utility dump^W unit
///
/// Alex Nevskiy 2025-04-09
///
interface

uses Classes, Types, System.SysUtils;

type
  TRange = record
    Min: integer;
    Max: integer;
    function IsInRange(aValue: integer): boolean;
  end;

  TGridData = record
    Diff: integer; // 0=Custom 1=Beginner 2=Intermed 3=Advanced etc for future
    Width: integer;
    Height: integer;
    Mines: integer;
  end;

  TLocalParams = record
    //GridType: integer; // 0=Custom 1=Beginner 2=Intermed 3=Advanced etc for future
    GridData :TGridData;
    FileName: string;
    //Statistics
    GamesPlayed: integer;
    GamesWon: integer;
    LongestWinningStreak: integer; // TODO
    LongestLosinggStreak: integer; //
    CurrentStreak: integer;        //
    function GetParamsFileName(ForceCreateDir: boolean = true): string;
    procedure LoadParams;
    procedure SaveParams;
    procedure Init;
    constructor Create(aForce: boolean);
  end;

type
  TInteger2D = array of array of integer;

  TFieldInterpolation = record //Visual gradient for grid and tiles
    FCells: TInteger2D;
  private
    function GetCell(X, Y: integer): Integer;
    procedure SetCell(X, Y: integer; const Value: integer);
  public
    procedure Init(aGridData: TGridData);
    property Cells[X, Y: integer]: Integer read GetCell write SetCell; default;
  end;

type
  TGameGridCell = record
    Flag: boolean;
    Mine: boolean;
    Number: integer;
    Covered: boolean;
    procedure Init;
  end;

  TGameGridCells = array of array of TGameGridCell;

  TGameGrid = record
    Width: integer;
    Height: integer;
    Mines: integer;
    FPreGen: TInteger2D; //Daft mine field.
    FCells: TGameGridCells;
  private
    function GetCell(X, Y: integer): TGameGridCell;
    procedure SetCell(X, Y: integer; const Value: TGameGridCell);
    procedure CreateAvoidanceZone;
  public
    procedure Init(GridData: TGridData);
    procedure Pregen(aSeed: integer = 0);
    property Cells[X, Y: integer]: TGameGridCell read GetCell write SetCell; default;
  end;



  THighScore = class
    FScoreList: TStringList;
  private
    function GetHighScoreFileName: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

//function GetParamsFileName: string;

const
  cptProgramDataFolder = 'MineSweeperD12';
  cptHighScoreFileName = 'HighScore.txt';
  cptParamsFileName = 'Settings.txt';

  cssParams = 'Params';
  cssStats = 'Statistics';

  csFormCaption = 'MineSweeper (%s)';
  csDiffLabel = '  %s'#13#10'  %d mines'#13#10'  %d x %d tile grid';

  cminAvoidanceZoneW = 3;
  cminAvoidanceZoneH = 3;

  //Game grid standard dimensions
  cgdBeginner: TGridData = (Diff: 1; Width: 9;  Height: 9;  Mines: 10);
  cgdIntermed: TGridData = (Diff: 2; Width: 16; Height: 16; Mines: 40);
  cgdAdvanced: TGridData = (Diff: 3; Width: 30; Height: 16; Mines: 99);

  //Game grid custom dimensions
  csCustomW: TRange = (Min:9; Max:30);
  csCustomH: TRange = (Min:9; Max:24);
  csCustomM: TRange = (Min:10; Max:668);

  //Game grid cells size
  ccCellW = 36;
  ccCellH = 36;
  ccSteps = 29;

  //Game grid graph
  cpFlag: TPoint = (X:642; Y:81);
  cpMine: TPoint = (X:682; Y:81);

function GetCurrentUser: string;

function GenSeed: integer;

//function NumbersOnly(aString: string): string;

implementation

uses Winapi.Windows, StrUtils, IniFiles, Math;

//-----------------------------------------------------

function GetCurrentUser: string;
begin
//  var nSize: DWord := 1024;
//  SetLength(Result, nSize);
//  if GetUserName(PChar(Result), nSize) then
//    SetLength(Result, nSize - 1)
//  else
//    Result := 'Incognito';
  result := GetEnvironmentVariable('USERNAME');
end;

function GenSeed: integer;
var H, M, S, MS: Word;
begin
  DecodeTime(Now, H, M, S, MS);
  result := MS + S * 100 + M * 60 * 100 + H * 60 * 60 * 100;
end;



//function NumbersOnly(aString: string): string;
//begin
//  result := '';
//  if aString = '' then exit;
//  for var Idx: integer := 1 to Length(aString) do
//    If aString[Idx] in ['0'..'9'] then
//      result := result + aString[Idx];
//end;

//function GetParamsFileName: string;
//begin
//  result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA')); //Params local for each user
//  result := IncludeTrailingPathDelimiter(result + cptProgramDataFolder);
//  result := result + cptParamsFileName;
//  //ex: "C:\Users\Alex\AppData\Roaming\MineSweeperD12\Settings.txt"
//end;

{ TRange }

function TRange.IsInRange(aValue: integer): boolean;
begin
  result := (aValue >= Min) and (aValue <= Max);
end;


{ THighScore }

constructor THighScore.Create;
begin
  FScoreList := TStringList.Create;
end;


destructor THighScore.Destroy;
begin
  FreeAndNil(FScoreList);
  inherited;
end;


function THighScore.GetHighScoreFileName: string;
begin
  result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('ALLUSERSPROFILE')); //High Score table for all users
  result := IncludeTrailingPathDelimiter(result + cptProgramDataFolder);
  result := result + cptHighScoreFileName;
  //ex: "C:\Users\All Users\MineSweeperD12\HighScore.txt"
end;


{ TLocalParams }

procedure TLocalParams.LoadParams;
var IniFile: TMemIniFile;
begin
  IniFile := TMemIniFile.Create(FileName);
  try
    GridData.Diff   := IniFile.ReadInteger(cssParams, 'GridDiff', 1);
    GridData.Width  := IniFile.ReadInteger(cssParams, 'GridWidth', 9);
    GridData.Height := IniFile.ReadInteger(cssParams, 'GridHeight', 9);
    GridData.Mines  := IniFile.ReadInteger(cssParams, 'GridMines', 10);
    //Statistics
    GamesPlayed := IniFile.ReadInteger(cssStats, 'GamesPlayed', 0);
    GamesWon    := IniFile.ReadInteger(cssStats, 'GamesWon', 0);
    LongestWinningStreak  := IniFile.ReadInteger(cssStats, 'LongestWinningStreak', 0);
    LongestLosinggStreak  := IniFile.ReadInteger(cssStats, 'LongestLosinggStreak', 0);
    CurrentStreak  := IniFile.ReadInteger(cssStats, 'CurrentStreak', 0);
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure TLocalParams.SaveParams;
var IniFile: TMemIniFile;
begin
  IniFile := TMemIniFile.Create(FileName);
  try
    IniFile.WriteInteger(cssParams, 'GridDiff',  GridData.Diff);
    IniFile.WriteInteger(cssParams, 'GridWidth', GridData.Width);
    IniFile.WriteInteger(cssParams, 'GridHeight', GridData.Height);
    IniFile.WriteInteger(cssParams, 'GridMines', GridData.Mines);
    //Statistics
    IniFile.WriteInteger(cssStats, 'GamesPlayed', GamesPlayed);
    IniFile.WriteInteger(cssStats, 'GamesWon', GamesWon);
    IniFile.WriteInteger(cssStats, 'LongestWinningStreak', LongestWinningStreak);
    IniFile.WriteInteger(cssStats, 'LongestLosinggStreak', LongestLosinggStreak);
    IniFile.WriteInteger(cssStats, 'CurrentStreak', CurrentStreak);
    IniFile.UpdateFile;
  finally
    FreeAndNil(IniFile);
  end;
end;

constructor TLocalParams.Create(aForce: boolean); //Trick to use Adv Record constructor
begin
  FileName := GetParamsFileName;
  Init;
  if FileExists(GetParamsFileName) then
    LoadParams;
end;


function TLocalParams.GetParamsFileName(ForceCreateDir: boolean = true): string;
begin
  result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA')); //Params local for each user
  result := IncludeTrailingPathDelimiter(result + cptProgramDataFolder);
  if ForceCreateDir then
    ForceDirectories(result);
  result := result + cptParamsFileName;
  //ex: "C:\Users\Alex\AppData\Roaming\MineSweeperD12\Settings.txt"
end;

procedure TLocalParams.Init;
begin
  GridData := cgdBeginner;
  //Statistics
  GamesPlayed := 0;          //
  GamesWon := 0;             //
  LongestWinningStreak := 0; // TODO
  LongestLosinggStreak := 0; //
  CurrentStreak :=0 ;        //
end;

{ TGameGrid }

function TGameGrid.GetCell(X, Y: integer): TGameGridCell;
begin

end;

procedure TGameGrid.Init(GridData: TGridData);
begin
  Width := GridData.Width;
  Height := GridData.Height;
  Mines :=  GridData.Mines;
  SetLength(FCells,  GridData.Width, GridData.Height);
  SetLength(FPreGen, GridData.Width, GridData.Height);
  for var x: integer := 0 to Width - 1 do
    for var y: integer := 0 to Height - 1 do
    begin
      FCells[x, y].Init;
      FPreGen[x, y] := 0;
    end;
  Pregen(GenSeed);
end;


procedure TGameGrid.CreateAvoidanceZone;
var AvZW, AvZH: integer;
    AvZX, AvZY: integer;
begin
  AvZW := Max(Width  div 5, cminAvoidanceZoneW);
  AvZH := Max(Height div 5, cminAvoidanceZoneH);
  AvZX := (Width  div 2) - (AvZW div 2);
  AvZY := (Height div 2) - (AvZH div 2);
  for var x: integer := 0 to AvZW do
    for var y: integer := 0 to AvZH do
      FPreGen[x + AvZX, y + AvZY] := -1;
end;

procedure CompactLArray(var aLArray: TArray<TPoint>);
var a, b: integer;
begin
  b := 0;
  for a := 0 to High(aLArray) do
    if aLArray[a].x >= 0 then
    begin
      aLArray[b] := aLArray[a];
      inc(b);
    end;
  SetLength(aLArray, b);
end;


procedure TGameGrid.Pregen(aSeed: integer = 0);
var LArray: TArray<TPoint>;
begin
  if aSeed = 0 then
    Randomize
  else
    RandSeed := aSeed;
  SetLength(LArray, Width * Height);
  CreateAvoidanceZone;
  //Prefill
  for var y: integer := 0 to Height - 1 do
    for var x: integer := 0 to Width - 1 do
    begin
      if FPreGen[x, y] = 0 then
      begin
        LArray[x + y * Width].X := x;
        LArray[x + y * Width].Y := y;
      end
      else
        LArray[x + y * Width].X := -1;
    end;
  CompactLArray(LArray);
  for var m: integer := 1 to Mines do
  begin
    var Idx: integer := Random(High(LArray) + 1);
    FPreGen[LArray[Idx].X, LArray[Idx].Y] := 1; //Place mine here
    LArray[Idx].X := -1;
    CompactLArray(LArray);
  end;
end;


procedure TGameGrid.SetCell(X, Y: integer; const Value: TGameGridCell);
begin

end;

{ TGameGridCell }

procedure TGameGridCell.Init;
begin
  Flag := false;
  Mine := false;
  Number := 0;
  Covered := true;
end;


{ TFieldInterpolation }

function TFieldInterpolation.GetCell(X, Y: integer): integer;
begin
  result := FCells[x, y];
end;

function LInt(a, b, c, steps: integer): integer;
begin
  result := trunc(a + (b - a) * (c / steps));  //Invention of the wheel detected...
end;

procedure TFieldInterpolation.Init(aGridData: TGridData);
var aSteps, a, b, c: integer;
    x, y: integer;
begin
  SetLength(FCells, aGridData.Width, aGridData.Height);
  aSteps := aGridData.Width + aGridData.Height;
  //if aSteps > ccSteps then
  //  aSteps := ccSteps;

  a := Max(0, ccSteps - aSteps);
  b := ccSteps;
  FCells[0, 0] := a;
  FCells[aGridData.Width - 1, aGridData.Height - 1] := ccSteps;
  c := aGridData.Width;
  FCells[aGridData.Width - 1, 0] := LInt(a, b, c, aSteps);
  c := aGridData.Height;
  FCells[0, aGridData.Height - 1] := LInt(a, b, c, aSteps);

  for x := 1 to aGridData.Width - 2 do
  begin
    a := FCells[0, 0];
    b := FCells[aGridData.Width - 1, 0];
    FCells[x, 0] := LInt(a, b, x, aGridData.Width - 1);
    a := FCells[0, aGridData.Height - 1];
    b := FCells[aGridData.Width - 1, aGridData.Height - 1];
    FCells[x, aGridData.Height - 1] := LInt(a, b, x, aGridData.Width - 1);
  end;

  for x := 0 to aGridData.Width - 1 do
  begin
    a := FCells[x, 0];
    b := FCells[x, aGridData.Height - 1];
    for y := 1 to aGridData.Height - 2 do
      FCells[x, y] := LInt(a, b, y, aGridData.Height - 1);
  end;
end;

procedure TFieldInterpolation.SetCell(X, Y: integer; const Value: integer);
begin
  FCells[x, y] := Value;
end;

end.
