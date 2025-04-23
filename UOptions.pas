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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask, Vcl.Buttons,
  UUtility;

type
  TFOptions = class(TForm)
    pnlDifficulty: TPanel;
    pnlDiffStandard: TPanel;
    lblDiffCaption: TLabel;
    rbIntermed: TRadioButton;
    rbAdvanced: TRadioButton;
    rbCustom: TRadioButton;
    rbBeginner: TRadioButton;
    edWidth: TLabeledEdit;
    edHeight: TLabeledEdit;
    edMines: TLabeledEdit;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    pnlSettings: TPanel;
    cbAnimations: TCheckBox;
    cbSounds: TCheckBox;
    cbSnd1: TCheckBox;
    cbSnd3: TCheckBox;
    cbSnd2: TCheckBox;
    cbSnd4: TCheckBox;
    cbSnd5: TCheckBox;
    cbSnd6: TCheckBox;
    procedure rbClick(Sender: TObject);
    procedure CheckCustomControls;
    procedure EnableCustom(anEnable: boolean);
    procedure CheckCustomValid;
    procedure edCustomChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbSoundsClick(Sender: TObject);
  private
//    edWidthCaption: string;
//    edHeightCaption: string;
//    edMinesCaption: string;

  public
    procedure SetGridData(aValue: TGridData);
    function GetGridData: TGridData;
    procedure SetSettings(aValue: TSettings);
    function GetSettings:TSettings;
    procedure UpdateSndEnabled;
  end;

var
  FOptions: TFOptions;

implementation


{$R *.dfm}

{ TFOptions }

procedure TFOptions.cbSoundsClick(Sender: TObject);
begin
  UpdateSndEnabled;
end;


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
  rbBeginner.Caption := format(csDiffLabel, ['Beginner', cgdBeginner.Mines, cgdBeginner.Width, cgdBeginner.Height]);
  rbIntermed.Caption := format(csDiffLabel, ['Intermediate', cgdIntermed.Mines, cgdIntermed.Width, cgdIntermed.Height]);
  rbAdvanced.Caption := format(csDiffLabel, ['Advanced', cgdAdvanced.Mines, cgdAdvanced.Width, cgdAdvanced.Height]);

  edWidth.EditLabel.Caption  := format('Width (%d-%d):',  [csCustomW.Min, csCustomW.Max]);
  edHeight.EditLabel.Caption := format('Height (%d-%d):', [csCustomH.Min, csCustomH.Max]);
  edMines.EditLabel.Caption  := format('Mines (%d-%d):',  [csCustomM.Min, csCustomM.Max]);
end;


function TFOptions.GetGridData: TGridData;
begin
//  if rbBeginner.Checked then
//    result := cgdBeginner
//  else
  if rbIntermed.Checked then
    result := cgdIntermed
  else
  if rbAdvanced.Checked then
    result := cgdAdvanced
  else
  if rbCustom.Checked then
  begin
    result.Diff := 0;
    result.Width  := StrToIntDef(edWidth.Text , cgdBeginner.Width);
    result.Height := StrToIntDef(edHeight.Text, cgdBeginner.Height);
    result.Mines  := StrToIntDef(edMines.Text,  cgdBeginner.Mines);
  end
  else
    result := cgdBeginner; //Default state for fuzzy input
end;


function TFOptions.GetSettings: TSettings;
begin
  result.Animation := cbAnimations.Checked;
  result.Sounds := cbSounds.Checked;
  for var Idx: integer := Low(result.snd) to High(result.snd) do
  begin
    var cbControl: TControl := pnlSettings.FindChildControl('cbSnd' + IntToStr(Idx));
    if cbControl is TCheckBox then
      result.snd[Idx] := (cbControl as TCheckBox).Checked;
  end;
end;


procedure TFOptions.SetGridData(aValue: TGridData);
begin
  case aValue.Diff of
    //1:  rbBeginner.Checked := true;
    0:  begin
          rbCustom.Checked := true;
          edWidth.Text := IntToStr(aValue.Width);
          edHeight.Text := IntToStr(aValue.Height);
          edMines.Text := IntToStr(aValue.Mines);
        end;
    2: rbIntermed.Checked := true;
    3: rbAdvanced.Checked := true;
    else
      rbBeginner.Checked := true; //Default state for fuzzy input
  end;
end;


procedure TFOptions.SetSettings(aValue: TSettings);
begin
  cbAnimations.Checked := aValue.Animation;
  cbSounds.Checked := aValue.Sounds;
  for var Idx: integer := Low(aValue.snd) to High(aValue.snd) do
  begin
    var cbControl: TControl := pnlSettings.FindChildControl('cbSnd' + IntToStr(Idx));
    if cbControl is TCheckBox then
      (cbControl as TCheckBox).Checked := aValue.snd[Idx];
  end;
end;


procedure TFOptions.UpdateSndEnabled;
var Idx: integer;
    Enable: boolean;
begin
  Enable := cbSounds.Checked;
  for Idx := 1 to csndMax do
  begin
    var cbControl: TControl := pnlSettings.FindChildControl('cbSnd' + IntToStr(Idx));
    if cbControl is TCheckBox then
      (cbControl as TCheckBox).Enabled := Enable;
  end;
end;


procedure TFOptions.rbClick(Sender: TObject);
begin
  CheckCustomControls;
  CheckCustomValid;
end;

end.

