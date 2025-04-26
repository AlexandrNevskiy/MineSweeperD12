unit UUtility;
///
/// MineSweeperD12 (RAD Programmer Challenge #1)
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

  //TMouseButtoState = (mbsNone, mbsDown, mbsPressed, mbsUp);

  TInput = record
    //OldL: TMouseButtoState;
    //OldR: TMouseButtoState;
    //function UpdateState(anOldstate: TMouseButtoState; aNew: boolean): TMouseButtoState;
  public
    //LButton: TMouseButtoState;
    //RButton: TMouseButtoState;
    LButton: boolean;
    RButton: boolean;
    procedure Update;
  end;

  TGridData = packed record
    Diff: integer; // 0=Custom 1=Beginner 2=Intermed 3=Advanced etc for future
    Width: integer;
    Height: integer;
    Mines: integer;
    Ticks: UInt64;
    function IsSame(aGridData: TGridData): boolean;
  end;

const
  //Sounds
  csndArea  = 1;
  csndCheck = 2;
  csndClick = 3;
  csndFlagIn  = 4;
  csndFlagOut = 5;
  csndLoose = 6;
  csndSuccess = 7;
  csndMax = 7;

type
  TSettings = record
    Animation: boolean;
    Sounds: boolean;
    snd: array[1..csndMax] of boolean;
    procedure Init;
    procedure SaveToIni(FileName: string);
    procedure LoadFromIni(FileName: string);
  end;

  TLocalParams = record
    //GridType: integer; // 0=Custom 1=Beginner 2=Intermed 3=Advanced etc for future
    GridData :TGridData;
    FileName: string;
    Left: integer;
    Top: integer;
    //Statistics
    //GamesPlayed: integer;
    //GamesWon: integer;
    //LongestWinningStreak: integer; // TODO
    //LongestLosinggStreak: integer; //
    //CurrentStreak: integer;        //
    Settings: TSettings;
    function GetParamsFileName(ForceCreateDir: boolean = true): string;
    function LoadParams: boolean;
    procedure SaveParams;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure Init;
    constructor Create(aForce: boolean; aLeft, aTop: integer);
  end;

  TStats = record
    GamesPlayed: integer;
    GamesWon: integer;

  end;

type
  TInteger2D = array of array of integer;

  TGradientField = record //Visual gradient for grid and tiles
    FCells: TInteger2D;
  private
    function GetCell(X, Y: integer): Integer;
    procedure SetCell(X, Y: integer; const Value: integer);
  public
    procedure Init(aGridData: TGridData);
    property Cells[X, Y: integer]: Integer read GetCell write SetCell; default;
  end;

  TDraftField = record
    FGridData: TGridData;
    FCells: TInteger2D;
  private
    procedure CreateSafeArea;
  public
    procedure Generate(aGridData: TGridData; aSeed: integer = 0);
  end;

type
  TGameGridCell = record
    Flag: boolean;    //Tile marked with flag
    Mine: boolean;    //Mine in tile
    Number: integer;  //Number of mines arount tile
    Covered: boolean; //Content of tile is hidden
    Hovered: boolean; //Not used yet (mouse hover the tile)
    Checked: boolean; //Used internally in open-area alghoritm and to display "red" mine
    DownCheck: boolean; //::AN 2025-04-22 Show L-R-Down check
    ToRender: boolean;//Force tile to re-render ant Render stage
    procedure Init;
    procedure Open;
    function IncNumber: integer;
  end;

  TGameGridCells = array of array of TGameGridCell;

//const
//  cbtMax = 5; //Best times count
//  cbtKeySize = 3;
type
  THighScore = class
    FScoreList: TStringList;
  private
    //arBestTimes: array [1..cbtMax] of string;
    GamesPlayed: integer;
    GamesWon: integer;
    LastGame: integer; //::AN 2025-04-25 -1=Loose; 0=Current; +1=Success
    StreakCurrent: integer;
    StreakWon: integer;
    StreakLoose: integer;
    function GetHighScoreFileName: string;
    procedure AddKeys;
    procedure RemoveKeys;
    function LoadHighScore(aDiff: integer): boolean; // true = highscore file/section exists
    procedure SaveHighScore(aDiff: integer);
  public
    procedure Init(aDiff: integer);
    procedure PlayedGame(aDiff: integer);
    procedure LooseGame(aDiff: integer);
    procedure WonGame(aDiff: integer; aTime: integer);

    constructor Create;
    destructor Destroy; override;
  end;

//function GetParamsFileName: string;

const
  cptProgramDataFolder = 'MineSweeperD12';
  cptHighScoreFileName = 'HighScore.txt';
  cptParamsFileName = 'Settings.txt';

  cptSaveBeginner = 'Beginner.sav';
  cptSaveIntermed = 'Intermed.sav';
  cptSaveAdvanced = 'Advanced.sav';
  cptSaveCustom = 'Custom.sav';

  cssParams = 'Params';
  cssForm = 'Form';
  //cssStats = 'Statistics';
  cssSettings = 'Settings';
  cssBeginner = 'Beginner';
  cssIntermed = 'Intermed';
  cssAdvanced = 'Advanced';
  cssBestTimes = 'BT';
  cssKey = '%2.2d';
  cbtMax = 5; //Best times count
  cbtKeySize = 3;

  csFormCaption = 'MineSweeper (%s)';
  csStatFormCaption = 'MinesweeperD12 Statistics - %s';
  csDiffLabel = '  %s'#13#10'  %d mines'#13#10'  %d x %d tile grid';

  //csStyleDef = 'res%d';
  csStyleDef = 'res';
  csResLoad = '%s_%s%d';

  cSaveVer: string[3] = '001';

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
  ссTileY = 2;   //Tile res
  ссGridY = 42;  //Empty grid res

  //Game grid graph
  //cpFlag: TPoint = (X:642; Y:81);
  //cpMine: TPoint = (X:682; Y:81);
  cixCross    =  9;
  cixFlag     = 10;
  cixMine     = 11;
  cixMineDark = 12;
  cixMineRed  = 13;
  cixShad1    = 14;
  cixShad2    = 15;
  cixShad3    = 16;
  cixMax = 16;


function GetCurrentUser: string;
function GetSaveFileName(aGridData: TGridData): string;
function GenSeed: integer;


implementation

uses Winapi.Windows, StrUtils, IniFiles, Math, Generics.Collections;

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


function GetSaveFileName(aGridData: TGridData): string;
var s: string;
begin
  result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA'));
  result := IncludeTrailingPathDelimiter(result + cptProgramDataFolder);
  ForceDirectories(result);
  case aGridData.Diff of
    0:  s := cptSaveCustom;
    2:  s := cptSaveIntermed;
    3:  s := cptSaveAdvanced;
    else
      s := cptSaveBeginner;
  end;
  result := result + s;
end;


{ TRange }

function TRange.IsInRange(aValue: integer): boolean;
begin
  result := (aValue >= Min) and (aValue <= Max);
end;


{ THighScore }

constructor THighScore.Create;
begin
  FScoreList := TStringList.Create;
  FScoreList.Duplicates := dupAccept;
end;


destructor THighScore.Destroy;
begin
  FreeAndNil(FScoreList);
  inherited;
end;


function THighScore.GetHighScoreFileName: string;
begin
  result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA')); //High Score table for each users
  result := IncludeTrailingPathDelimiter(result + cptProgramDataFolder);
  result := result + cptHighScoreFileName;
  //ex: "C:\Users\Alex\AppData\Roaming\MineSweeperD12\HighScore.txt"
end;


procedure THighScore.Init(aDiff: integer);
begin
  GamesPlayed := 0;
  GamesWon := 0;
  LastGame := 0; //::AN 2025-04-25 -1=Loose; 0=Current; +1=Success
  StreakCurrent := 0;
  StreakWon := 0;
  StreakLoose := 0;
  //for var Idx := Low(arBestTimes) to High(arBestTimes) do
  //  arBestTimes[Idx] := '-           ---';
  FScoreList.Clear;
end;


function THighScore.LoadHighScore(aDiff: integer): boolean;
var SectionName: string;
begin
  if aDiff = 0 then exit;
  var FileName: string := GetHighScoreFileName;
  result := FileExists(FileName);
  case aDiff of
    1: SectionName := cssIntermed;
    2: SectionName := cssAdvanced;
  else SectionName := cssBeginner;
  end;

  var IniFile := TMemIniFile.Create(FileName);
  try
    GamesPlayed := IniFile.ReadInteger(SectionName, 'GamesPlayed', GamesPlayed);
    GamesWon    := IniFile.ReadInteger(SectionName, 'GamesWon', GamesWon);
    LastGame    := IniFile.ReadInteger(SectionName, 'LastGame', LastGame);
    StreakCurrent := IniFile.ReadInteger(SectionName, 'StreakCurrent', StreakCurrent);
    StreakWon     := IniFile.ReadInteger(SectionName, 'StreakWon', StreakWon);
    StreakLoose   := IniFile.ReadInteger(SectionName, 'StreakLoose', StreakLoose);

    if IniFile.SectionExists(SectionName + cssBestTimes) then
    begin
      IniFile.ReadSectionValues(SectionName + cssBestTimes, FScoreList);
      FScoreList.Sort;
      RemoveKeys;
    end
    else
      FScoreList.Clear;
  finally
    FreeAndNil(IniFile);
  end;
end;


procedure THighScore.LooseGame(aDiff: integer);
begin
  if aDiff = 0 then exit;
end;


procedure THighScore.PlayedGame(aDiff: integer);
begin
  if aDiff = 0 then exit;
  //cssBeginner = 'Beginner';
  //cssIntermed = 'Intermed';
  //cssAdvanced = 'Advanced';
end;

procedure THighScore.AddKeys;
begin
  if FScoreList.Count = 0 then exit;
  FScoreList.Sort;

end;


procedure THighScore.RemoveKeys;
begin
  if FScoreList.Count = 0 then exit;
  FScoreList.Sort;
  for var Idx := 0 to FScoreList.Count - 1 do
    FScoreList[Idx] := Copy(FScoreList[Idx], cbtKeySize, Length(FScoreList[Idx]));
end;


procedure THighScore.SaveHighScore(aDiff: integer);
var SectionName: string;
begin
  if aDiff = 0 then exit;
  case aDiff of
    1: SectionName := cssIntermed;
    2: SectionName := cssAdvanced;
  else SectionName := cssBeginner;
  end;

  var IniFile := TMemIniFile.Create(GetHighScoreFileName);
  try
    IniFile.WriteInteger(SectionName, 'GamesPlayed', GamesPlayed);
    IniFile.WriteInteger(SectionName, 'GamesWon',    GamesWon);
    IniFile.WriteInteger(SectionName, 'LastGame',    LastGame);
    IniFile.WriteInteger(SectionName, 'StreakCurrent', StreakCurrent);
    IniFile.WriteInteger(SectionName, 'StreakWon',     StreakWon);
    IniFile.WriteInteger(SectionName, 'StreakLoose',   StreakLoose);

    if FScoreList.Count = 0 then exit;

    FScoreList.Sort;
    for var Idx := 0 to FScoreList.Count - 1 do
      IniFile.WriteString(SectionName + cssBestTimes, format(cssKey, [Idx]), FScoreList[Idx]);

  finally
    IniFile.UpdateFile;
    FreeAndNil(IniFile);
  end;

end;


procedure THighScore.WonGame(aDiff, aTime: integer);
begin
  if aDiff = 0 then exit;
end;


{ TLocalParams }

function TLocalParams.LoadParams: boolean;
var IniFile: TMemIniFile;
begin
  result := false;
  Settings.LoadFromIni(FileName);
  IniFile := TMemIniFile.Create(FileName);
  try
    //Grid
    GridData.Diff   := IniFile.ReadInteger(cssParams, 'GridDiff', 1);
    GridData.Width  := IniFile.ReadInteger(cssParams, 'GridWidth', 9);
    GridData.Height := IniFile.ReadInteger(cssParams, 'GridHeight', 9);
    GridData.Mines  := IniFile.ReadInteger(cssParams, 'GridMines', 10);
    //Form
    var OldLeft: integer := Left;
    var OldTop: integer := Top;
    Left := IniFile.ReadInteger(cssForm, 'Left', Left);
    Top  := IniFile.ReadInteger(cssForm, 'Top',  Top);
    result := (OldLeft = Left) and (OldTop = Top);
    //Statistics
    //GamesPlayed := IniFile.ReadInteger(cssStats, 'GamesPlayed', 0);
    //GamesWon    := IniFile.ReadInteger(cssStats, 'GamesWon', 0);
    //LongestWinningStreak  := IniFile.ReadInteger(cssStats, 'LongestWinningStreak', 0);
    //LongestLosinggStreak  := IniFile.ReadInteger(cssStats, 'LongestLosinggStreak', 0);
    //CurrentStreak  := IniFile.ReadInteger(cssStats, 'CurrentStreak', 0);
  finally
    FreeAndNil(IniFile);
  end;
end;


procedure TLocalParams.LoadSettings;
begin
  Settings.LoadFromIni(FileName);
end;


procedure TLocalParams.SaveParams;
var IniFile: TMemIniFile;
begin
  Settings.SaveToIni(FileName);
  IniFile := TMemIniFile.Create(FileName);
  try
    IniFile.WriteInteger(cssParams, 'GridDiff',  GridData.Diff);
    IniFile.WriteInteger(cssParams, 'GridWidth', GridData.Width);
    IniFile.WriteInteger(cssParams, 'GridHeight', GridData.Height);
    IniFile.WriteInteger(cssParams, 'GridMines', GridData.Mines);
    //Form
    IniFile.WriteInteger(cssForm, 'Left', Left);
    IniFile.WriteInteger(cssForm, 'Top',  Top);
    //Statistics
    //IniFile.WriteInteger(cssStats, 'GamesPlayed', GamesPlayed);
    //IniFile.WriteInteger(cssStats, 'GamesWon', GamesWon);
    //IniFile.WriteInteger(cssStats, 'LongestWinningStreak', LongestWinningStreak);
    //IniFile.WriteInteger(cssStats, 'LongestLosinggStreak', LongestLosinggStreak);
    //IniFile.WriteInteger(cssStats, 'CurrentStreak', CurrentStreak);
    IniFile.UpdateFile;
  finally
    FreeAndNil(IniFile);
  end;
end;


procedure TLocalParams.SaveSettings;
begin
  Settings.SaveToIni(FileName);
end;

constructor TLocalParams.Create(aForce: boolean; aLeft, aTop: integer); //Trick to use Adv Record constructor
begin
  Left := aLeft;
  Top  := aTop;
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
  //GamesPlayed := 0;          //
  //GamesWon := 0;             //
  //LongestWinningStreak := 0; // TODO
  //LongestLosinggStreak := 0; //
  //CurrentStreak :=0 ;        //
end;

{ TGameGrid }


{ TGameGridCell }

function TGameGridCell.IncNumber: integer;
begin
  if not Mine then
    Inc(Number);
  result := Number;
end;


procedure TGameGridCell.Init;
begin
  Flag := false;
  Mine := false;
  Number := 0;
  Covered := true;
  Hovered := false;
  Checked := false;
  DownCheck := false; //::AN 2025-04-022
  ToRender := true;
end;


procedure TGameGridCell.Open;
begin
  Covered := false;
  Flag := false;
  ToRender := true;
end;

{ TGradientField }

function TGradientField.GetCell(X, Y: integer): integer;
begin
  result := FCells[x, y];
end;


function LInt(a, b, c, steps: integer): integer;
begin
  result := trunc(a + (b - a) * (c / steps));  //Invention of the wheel detected...
end;


procedure TGradientField.Init(aGridData: TGridData);
var aSteps, a, b, c: integer;
    x, y: integer;
begin
  SetLength(FCells, aGridData.Width, aGridData.Height);
  aSteps := aGridData.Width + aGridData.Height;

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


procedure TGradientField.SetCell(X, Y: integer; const Value: integer);
begin
  FCells[x, y] := Value;
end;


{ TDraftField }

procedure TDraftField.CreateSafeArea;
var AvZW, AvZH: integer;
    AvZX, AvZY: integer;
    AreaX, AreaY: TRange;
begin
  AvZW := Max(FGridData.Width  div 5, cminAvoidanceZoneW);
  AvZH := Max(FGridData.Height div 5, cminAvoidanceZoneH);
  AvZX := (FGridData.Width  div 2) - (AvZW div 2);
  AvZY := (FGridData.Height div 2) - (AvZH div 2);
  AreaX.Min := AvZX;
  AreaX.Max := AvZX + AvZW;
  AreaY.Min := AvZY;
  AreaY.Max := AvZY + AvZH;

  for var x: integer := 0 to FGridData.Width - 1 do
    for var y: integer := 0 to FGridData.Height - 1 do
      if AreaX.IsInRange(x) and AreaY.IsInRange(y) then
        FCells[x, y] := -1
      else
        FCells[x, y] := 0;
end;


//procedure CompactLArray(var aLArray: TArray<TPoint>);
//var a, b: integer;
//begin
//  b := 0;
//  for a := 0 to High(aLArray) do
//    if aLArray[a].x >= 0 then
//    begin
//      aLArray[b] := aLArray[a];
//      inc(b);
//    end;
//  SetLength(aLArray, b);
//end;


//procedure TDraftField.Generate(aGridData: TGridData; aSeed: integer = 0);
//var LArray: TArray<TPoint>;
//begin
//  FGridData := aGridData;
//  SetLength(FCells, FGridData.Width, FGridData.Height);
//
//  if aSeed = 0 then
//    Randomize
//  else
//    RandSeed := aSeed;
//  SetLength(LArray, FGridData.Width * FGridData.Height);
//  CreateSafeArea; //Create safety area
//  //Prefill
//  for var y: integer := 0 to FGridData.Height - 1 do
//    for var x: integer := 0 to FGridData.Width - 1 do
//    begin
//      if FCells[x, y] = 0 then
//      begin
//        LArray[x + y * FGridData.Width].X := x;
//        LArray[x + y * FGridData.Width].Y := y;
//      end
//      else
//        LArray[x + y * FGridData.Width].X := -1;
//    end;
//  CompactLArray(LArray);
//  for var m: integer := 1 to FGridData.Mines do
//  begin
//    var Idx: integer := Random(High(LArray) + 1);
//    FCells[LArray[Idx].X, LArray[Idx].Y] := 1; //Place mine here
//    LArray[Idx].X := -1;
//    CompactLArray(LArray);
//  end;
//end;

//Keep It Simple S
procedure TDraftField.Generate(aGridData: TGridData; aSeed: integer = 0);
var OneD: TList<TPoint>;
begin
  FGridData := aGridData;
  SetLength(FCells, FGridData.Width, FGridData.Height);
  if aSeed = 0 then
    Randomize
  else
    RandSeed := aSeed;
  CreateSafeArea; //Create safety area
  OneD := TList<TPoint>.Create;
  try
    OneD.Capacity := FGridData.Width * FGridData.Height;
    for var y: integer := 0 to FGridData.Height - 1 do
      for var x: integer := 0 to FGridData.Width - 1 do
        if FCells[x, y] = 0 then
          OneD.Add(Point(x, y));
    for var m: integer := 1 to FGridData.Mines do
    begin
      var Idx: integer := Random(OneD.Count);
      FCells[OneD[Idx].X, OneD[Idx].Y] := 1; //Place mine here
      OneD.Delete(Idx);
    end;
  finally
    FreeAndNil(OneD);
  end;
end;


{ TInput }
procedure TInput.Update;
begin
//  NewL := (GetAsyncKeyState(VK_LBUTTON) and $8000 > 0);
//  NewR := (GetAsyncKeyState(VK_RBUTTON) and $8000 > 0);
//  LButton := UpdateState(LButton, NewL);
//  RButton := UpdateState(RButton, NewR);
  LButton :=  (GetAsyncKeyState(VK_LBUTTON) and $8000 > 0);
  RButton :=  (GetAsyncKeyState(VK_RBUTTON) and $8000 > 0);
end;


//function TInput.UpdateState(anOldstate: TMouseButtoState; aNew: boolean): TMouseButtoState;
//begin
//  result := mbsNone;
//  if (anOldstate = mbsNone) and aNew  then exit(mbsDown);
//  if (anOldstate = mbsDown) and aNew  then exit(mbsPressed);
//  if (anOldstate = mbsDown) and not aNew  then exit(mbsUp);
//  if (anOldstate = mbsPressed) and aNew  then exit(mbsPressed);
//  if (anOldstate = mbsPressed) and not aNew  then exit(mbsUp);
//  if (anOldstate = mbsUp) and aNew  then exit(mbsDown);
////if (anOldstate = mbsUp) and not aNew  then exit(mbsNone);
////(mbsNone, mbsDown, mbsPressed, mbsUp)
//end;

{ TSettings }

procedure TSettings.Init;
begin
  Animation := false;
  Sounds := false;
  for var Idx: integer := Low(snd) to High(snd) do
    snd[Idx] := false;
end;


procedure TSettings.LoadFromIni(FileName: string);
var IniFile: TMemIniFile;
begin
  IniFile := TMemIniFile.Create(FileName);
  try
    Animation := IniFile.ReadBool(cssSettings, 'Animation',  true);
    Sounds := IniFile.ReadBool(cssSettings, 'Sounds', true);
    for var Idx: integer := Low(snd) to High(snd) do
      Snd[Idx] := IniFile.ReadBool(cssSettings, 'Snd' + IntToStr(Idx), true);
  finally
    FreeAndNil(IniFile);
  end;
end;


procedure TSettings.SaveToIni(FileName: string);
var IniFile: TMemIniFile;
begin
  IniFile := TMemIniFile.Create(FileName);
  try
    IniFile.WriteBool(cssSettings, 'Animation',  Animation);
    IniFile.WriteBool(cssSettings, 'Sounds', Sounds);
    for var Idx: integer := Low(snd) to High(snd) do
      IniFile.WriteBool(cssSettings, 'Snd' + IntToStr(Idx), Snd[Idx]);
    IniFile.UpdateFile;
  finally
    FreeAndNil(IniFile);
  end;
end;

{ TGridData }

function TGridData.IsSame(aGridData: TGridData): boolean;
begin
  result :=
  (Diff   = aGridData.Diff) and
  (Width  = aGridData.Width) and
  (Height = aGridData.Height) and
  (Mines  = aGridData.Mines);
end;

end.
