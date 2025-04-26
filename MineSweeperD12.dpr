program MineSweeperD12;
///
/// MineSweeperD12 (RAD Programmer Challenge #1)
///
/// Project file
///
/// Alex Nevskiy 2025-04-09
///


{$R *.dres}

uses
  Vcl.Forms,
  UMain in 'UMain.pas' {FMain},
  UOptions in 'UOptions.pas' {FOptions},
  UUtility in 'UUtility.pas',
  UGameGrid in 'UGameGrid.pas',
  URenderGrid in 'URenderGrid.pas',
  USound in 'USound.pas',
  UStatistics in 'UStatistics.pas' {FStatistics},
  UAbout in 'UAbout.pas' {FAbout};

{$R *.RES}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFMain, FMain);
  //Application.CreateForm(TFAbout, FAbout);
  //Application.CreateForm(TFStatistics, FStatistics);
  //Application.CreateForm(TFOptions, FOptions);
  Application.Run;
end.
