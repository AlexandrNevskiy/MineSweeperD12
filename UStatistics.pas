unit UStatistics;
///
/// MineSweeperD12 (RAD Programmer Challenge #1)
///
/// Statistics form
///
/// Alex Nevskiy 2025-04-24
///
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, UUtility, Vcl.Buttons;

type
  TFStatistics = class(TForm)
    lbDiff: TListBox;
    gbBestTimes: TGroupBox;
    lbBestTimes: TListBox;
    lbStat: TListBox;
    btnClose: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure lbDiffClick(Sender: TObject);
    procedure lbDiffKeyPress(Sender: TObject; var Key: Char);
  private

  public
    lHighScore: THighScore;
    procedure UpdateStat(aDiff: integer);
  end;

var
  FStatistics: TFStatistics;

implementation


{$R *.dfm}

procedure TFStatistics.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFStatistics.FormCreate(Sender: TObject);
begin
  Caption := format(csStatFormCaption, [GetCurrentUser]);
end;


procedure TFStatistics.lbDiffClick(Sender: TObject);
begin
  UpdateStat(lbDiff.ItemIndex + 1);
end;

procedure TFStatistics.lbDiffKeyPress(Sender: TObject; var Key: Char);
begin
 UpdateStat(lbDiff.ItemIndex + 1);
end;

procedure TFStatistics.UpdateStat(aDiff: integer);
begin
  lHighScore.LoadHighScore(aDiff);

  LbStat.Clear;
  LbStat.Items.Add(format('Games played: %d', [lHighScore.GamesPlayed]));
  LbStat.Items.Add(format('Games won: %d', [lHighScore.GamesWon]));
  LbStat.Items.Add(format('Win percentage: %d%%', [lHighScore.Percentage]));
  LbStat.Items.Add(format('Longest winning streak: %d', [lHighScore.StreakWon]));
  LbStat.Items.Add(format('Longest losing streak: %d', [lHighScore.StreakLoose]));
  LbStat.Items.Add(format('Current streak: %d', [lHighScore.LastGame]));

  lbBestTimes.Clear;
  for var Idx := 0 to cbtMax - 1 do
  begin
    var sTime := lHighScore.BestTime_Time(Idx);
    var sDate := lHighScore.BestTime_Date(Idx);
    var sRec := format('%3s    %s', [sTime, sDate]);
    lbBestTimes.Items.Add(sRec);
  end;
end;

end.
