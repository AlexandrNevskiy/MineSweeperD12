unit UAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TFAbout = class(TForm)
    Image1: TImage;
    imgClose: TImage;
    lblRAD2: TLabel;
    lblRAD1: TLabel;
    lblEmail: TLabel;
    procedure imgCloseClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lblRAD1Click(Sender: TObject);
    procedure lblEmailClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FAbout: TFAbout;

implementation
uses ShellAPI;

{$R *.dfm}

procedure ShellOpen(const Url: string; const Params: string = '');
begin
  ShellExecute(Application.Handle, 'Open', PChar(Url), PChar(Params), nil, SW_SHOWNORMAL);
end;


procedure TFAbout.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then Close;
end;


procedure TFAbout.imgCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TFAbout.lblEmailClick(Sender: TObject);
begin
  ShellOpen('mailto:shirson@gmail.com');
end;


procedure TFAbout.lblRAD1Click(Sender: TObject);
begin
  ShellOpen('https://ideasawakened.com/post/rad-programmer-challenge-number-1-minesweeper-game-build');
end;

end.
