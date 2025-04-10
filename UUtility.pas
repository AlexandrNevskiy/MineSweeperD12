unit UUtility;
///
/// MineSweeperD12 (RAD Programmer Challenge #1))
///
/// Utility dump^W unit
///
/// Alex Nevskiy 2025-04-09
///
interface

uses Classes;

type
  TRange = record
    Min: integer;
    Max: integer;
    function IsInRange(aValue: integer): boolean;
  end;

  TGridData = record
    Width: integer;
    Height: integer;
    Mines: integer;
  end;

  TLocalParams = record
    GridType: integer; // 0=Custom 1=Beginner 2=Intermed 3=Advanced etc for future
    GridData :TGridData;
    FileName: string;
    //Statistics
    GamesPlayed: integer;
    GamesWon: integer;
    LongestWinningStreak: integer; // TODO
    LongestLosinggStreak: integer; //
    CurrentStreak: integer;        //
    function GetParamsFileName: string;
    procedure LoadParams;
    procedure SaveParams;
    procedure Init;
    constructor Create(aForce: boolean);
  end;

  THighScore = class
    FScoreList: TStringList;
  private
    function GetHighScoreFileName: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function GetParamsFileName: string;

const
  cptProgramDataFolder = 'MineSweeperD12';
  cptHighScoreFileName = 'HighScore.txt';
  cptParamsFileName = 'Settings.txt';

  cssParams = 'Params';
  cssStats = 'Statistics';

  //Game grid standard dimensions
  cgdBeginner: TGridData = (Width: 9;  Height: 9;  Mines: 10);
  cgdIntermed: TGridData = (Width: 16; Height: 16; Mines: 40);
  cgdAdvanced: TGridData = (Width: 30; Height: 16; Mines: 99);

  //Game grid custom dimensions
  csCustomW: TRange = (Min:9; Max:30);
  csCustomH: TRange = (Min:9; Max:24);
  csCustomM: TRange = (Min:10; Max:668);

  //Game grid cells size
  ccCellW = 34;
  ccCellH = 34;

function GetCurrentUser: string;

//function NumbersOnly(aString: string): string;

implementation

uses Winapi.Windows, System.SysUtils, StrUtils, IniFiles;

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


//function NumbersOnly(aString: string): string;
//begin
//  result := '';
//  if aString = '' then exit;
//  for var Idx: integer := 1 to Length(aString) do
//    If aString[Idx] in ['0'..'9'] then
//      result := result + aString[Idx];
//end;

function GetParamsFileName: string;
begin
  result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA')); //Params local for each user
  result := IncludeTrailingPathDelimiter(result + cptProgramDataFolder);
  result := result + cptParamsFileName;
  //ex: "C:\Users\Alex\AppData\Roaming\MineSweeperD12\Settings.txt"
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
    GridType := IniFile.ReadInteger(cssParams, 'GridType', 1);
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
    IniFile.WriteInteger(cssParams, 'GridType',  GridType);
    IniFile.WriteInteger(cssParams, 'GridWidth', GridData.Width);
    IniFile.WriteInteger(cssParams, 'GridHeight', GridData.Height);
    IniFile.WriteInteger(cssParams, 'GridMines', GridData.Mines);
    //Statistics
    IniFile.WriteInteger(cssStats, 'GamesPlayed', GamesPlayed);
    IniFile.WriteInteger(cssStats, 'GamesWon', GamesWon);
    IniFile.WriteInteger(cssStats, 'LongestWinningStreak', LongestWinningStreak);
    IniFile.WriteInteger(cssStats, 'LongestLosinggStreak', LongestLosinggStreak);
    IniFile.WriteInteger(cssStats, 'CurrentStreak', CurrentStreak);
  finally
    FreeAndNil(IniFile);
  end;
end;

constructor TLocalParams.Create(aForce: boolean);
begin
  FileName := GetParamsFileName;
  Init;
  if FileExists(GetParamsFileName) then
    LoadParams;
end;


function TLocalParams.GetParamsFileName: string;
begin
  result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA')); //Params local for each user
  result := IncludeTrailingPathDelimiter(result + cptProgramDataFolder);
  result := result + cptParamsFileName;
  //ex: "C:\Users\Alex\AppData\Roaming\MineSweeperD12\Settings.txt"
end;

procedure TLocalParams.Init;
begin
  GridType := 1; // 0=Custom 1=Beginner 2=Intermed 3=Advanced etc for future
  GridData := cgdBeginner;
  //Statistics
  GamesPlayed := 0;          //
  GamesWon := 0;             //
  LongestWinningStreak := 0; // TODO
  LongestLosinggStreak := 0; //
  CurrentStreak :=0 ;        //
end;

end.
