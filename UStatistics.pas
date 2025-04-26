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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFStatistics = class(TForm)
    lbDiff: TListBox;
    gbBestTimes: TGroupBox;
    lbBestTimes: TListBox;
    lbStat: TListBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FStatistics: TFStatistics;

implementation
uses UUtility;

{$R *.dfm}

procedure TFStatistics.FormCreate(Sender: TObject);
begin
  Caption := format(csStatFormCaption, [GetCurrentUser]);
end;

end.
