object FOptions: TFOptions
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 557
  ClientWidth = 272
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    272
    557)
  TextHeight = 17
  object pnlDifficulty: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 252
    Height = 471
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object lblDiffCaption: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 49
      Height = 17
      Align = alTop
      Caption = 'Difficulty'
    end
    object pnlDiffStandard: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 28
      Width = 204
      Height = 438
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object rbIntermed: TRadioButton
        AlignWithMargins = True
        Left = 10
        Top = 79
        Width = 191
        Height = 70
        Margins.Left = 10
        Align = alTop
        Caption = '-'#13#10'-'#13#10'-'
        TabOrder = 1
        WordWrap = True
        OnClick = rbClick
      end
      object rbAdvanced: TRadioButton
        AlignWithMargins = True
        Left = 10
        Top = 155
        Width = 191
        Height = 70
        Margins.Left = 10
        Align = alTop
        Caption = '-'#13#10'-'#13#10'-'
        TabOrder = 2
        WordWrap = True
        OnClick = rbClick
      end
      object rbCustom: TRadioButton
        AlignWithMargins = True
        Left = 10
        Top = 248
        Width = 191
        Height = 22
        Margins.Left = 10
        Margins.Top = 20
        Margins.Bottom = 5
        Align = alTop
        Caption = '  Custom'
        TabOrder = 3
        OnClick = rbClick
      end
      object rbBeginner: TRadioButton
        AlignWithMargins = True
        Left = 10
        Top = 3
        Width = 191
        Height = 70
        Margins.Left = 10
        Align = alTop
        Caption = '-'#13#10'-'#13#10'-'
        Checked = True
        TabOrder = 0
        TabStop = True
        WordWrap = True
        OnClick = rbClick
      end
      object edWidth: TLabeledEdit
        AlignWithMargins = True
        Left = 150
        Top = 280
        Width = 44
        Height = 25
        Margins.Left = 150
        Margins.Top = 5
        Margins.Right = 10
        Margins.Bottom = 10
        Align = alTop
        EditLabel.Width = 49
        EditLabel.Height = 25
        EditLabel.Caption = 'Width ():'
        EditLabel.Color = clBtnFace
        EditLabel.ParentColor = False
        Enabled = False
        LabelPosition = lpLeft
        LabelSpacing = 30
        NumbersOnly = True
        TabOrder = 4
        Text = '9'
        OnChange = edCustomChange
      end
      object edHeight: TLabeledEdit
        AlignWithMargins = True
        Left = 150
        Top = 325
        Width = 44
        Height = 25
        Margins.Left = 150
        Margins.Top = 10
        Margins.Right = 10
        Margins.Bottom = 10
        Align = alTop
        EditLabel.Width = 53
        EditLabel.Height = 25
        EditLabel.Caption = 'Height ():'
        EditLabel.Color = clBtnFace
        EditLabel.ParentColor = False
        Enabled = False
        LabelPosition = lpLeft
        LabelSpacing = 30
        MaxLength = 2
        NumbersOnly = True
        TabOrder = 5
        Text = '9'
        OnChange = edCustomChange
      end
      object edMines: TLabeledEdit
        AlignWithMargins = True
        Left = 150
        Top = 370
        Width = 44
        Height = 25
        Margins.Left = 150
        Margins.Top = 10
        Margins.Right = 10
        Margins.Bottom = 10
        Align = alTop
        EditLabel.Width = 50
        EditLabel.Height = 25
        EditLabel.Caption = 'Mines ():'
        EditLabel.Color = clBtnFace
        EditLabel.ParentColor = False
        Enabled = False
        LabelPosition = lpLeft
        LabelSpacing = 30
        MaxLength = 3
        NumbersOnly = True
        TabOrder = 6
        Text = '10'
        OnChange = edCustomChange
      end
    end
  end
  object btnOk: TBitBtn
    Left = 25
    Top = 514
    Width = 113
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TBitBtn
    Left = 144
    Top = 514
    Width = 113
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
