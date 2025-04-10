unit UOptions;
///
/// MineSweeperD12 (RAD Programmer Challenge #1))
///
/// Options (Difficulty) Dialog
///
/// Alex Nevskiy 2025-04-09
///
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask, Vcl.Buttons;

type
  TFOptions = class(TForm)
    pnlDifficulty: TPanel;
    pnlDiffStandard: TPanel;
    lblDiffCaption: TLabel;
    rbIntermed: TRadioButton;
    rbAdvanced: TRadioButton;
    rbCustom: TRadioButton;
    rbBegginer: TRadioButton;
    edWidth: TLabeledEdit;
    edHeight: TLabeledEdit;
    edMines: TLabeledEdit;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    procedure rbClick(Sender: TObject);
    procedure CheckCustomControls;
    procedure EnableCustom(anEnable: boolean);
    procedure CheckCustomValid;
    procedure edCustomChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
//    edWidthCaption: string;
//    edHeightCaption: string;
//    edMinesCaption: string;
  public

  end;

var
  FOptions: TFOptions;

implementation

uses UUtility;

{$R *.dfm}

{ TFOptions }

procedure TFOptions.CheckCustomControls;
begin
  EnableCustom(rbCustom.Checked);
end;


procedure TFOptions.CheckCustomValid;
var Proceed: boolean;
begin
  Proceed := true;
  btnOk.Enabled := true;
  edWidth.EditLabel.Font.Color  := clDefault;
  edHeight.EditLabel.Font.Color := clDefault;
  edMines.EditLabel.Font.Color  := clDefault;
  if not rbCustom.Checked then exit;
  btnOk.Enabled := false;
  if not csCustomW.IsInRange(StrToIntDef(edWidth.Text, 0)) then
  begin
    edWidth.EditLabel.Font.Color := clred;
    Proceed := false;
  end;
  if not csCustomH.IsInRange(StrToIntDef(edHeight.Text, 0)) then
  begin
    edHeight.EditLabel.Font.Color := clred;
    Proceed := false;
  end;
  if not csCustomM.IsInRange(StrToIntDef(edMines.Text, 0)) then
  begin
    edMines.EditLabel.Font.Color := clred;
    Proceed := false;
  end;
  if not Proceed then exit;
  btnOk.Enabled := true;
end;


procedure TFOptions.edCustomChange(Sender: TObject);
begin
  // if Sender is TLabeledEdit then
  //  (Sender as TLabeledEdit).Text := NumbersOnly((Sender as TLabeledEdit).Text); //Prevents to paste non-number text into control
  if not rbCustom.Checked then exit;
  CheckCustomValid;
end;


procedure TFOptions.EnableCustom(anEnable: boolean);
begin
  edWidth.Enabled := anEnable;
  edHeight.Enabled := anEnable;
  edMines.Enabled := anEnable;
end;


procedure TFOptions.FormCreate(Sender: TObject);
begin
  rbBegginer.Caption := format('  Beginner'#13#10'  %d mines'#13#10'  %d x %d tile grid', [cgdBeginner.Mines, cgdBeginner.Width, cgdBeginner.Height]);
  rbIntermed.Caption := format('  Intermediate'#13#10'  %d mines'#13#10'  %d x %d tile grid', [cgdIntermed.Mines, cgdIntermed.Width, cgdIntermed.Height]);
  rbAdvanced.Caption := format('  Advanced'#13#10'  %d mines'#13#10'  %d x %d tile grid', [cgdAdvanced.Mines, cgdAdvanced.Width, cgdAdvanced.Height]);

  edWidth.EditLabel.Caption  := format('Width (%d-%d):',  [csCustomW.Min, csCustomW.Max]);
  edHeight.EditLabel.Caption := format('Height (%d-%d):', [csCustomH.Min, csCustomH.Max]);
  edMines.EditLabel.Caption  := format('Mines (%d-%d):',  [csCustomM.Min, csCustomM.Max]);
end;


procedure TFOptions.rbClick(Sender: TObject);
begin
  CheckCustomControls;
  CheckCustomValid;
end;

end.

