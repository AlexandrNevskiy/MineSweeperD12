object FStatistics: TFStatistics
  Left = 0
  Top = 0
  ActiveControl = lbDiff
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'MinesweeperD12 Statistics - '
  ClientHeight = 202
  ClientWidth = 632
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Consolas'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object lbDiff: TListBox
    Left = 8
    Top = 44
    Width = 129
    Height = 73
    ItemHeight = 15
    Items.Strings = (
      'Beginner'
      'Intermediate'
      'Advanced')
    TabOrder = 0
    OnClick = lbDiffClick
    OnKeyPress = lbDiffKeyPress
  end
  object gbBestTimes: TGroupBox
    Left = 155
    Top = 24
    Width = 222
    Height = 113
    Caption = ' Best Times '
    TabOrder = 1
    object lbBestTimes: TListBox
      AlignWithMargins = True
      Left = 27
      Top = 20
      Width = 190
      Height = 88
      Margins.Left = 25
      Style = lbOwnerDrawFixed
      AutoComplete = False
      Align = alClient
      BorderStyle = bsNone
      Color = clBtnFace
      Enabled = False
      ExtendedSelect = False
      Items.Strings = (
        '1          2025-04-01'
        '22         2025-04-02'
        '333        2025-04-03'
        '4444       2025-04-04'
        '55555      2025-04-05')
      Sorted = True
      TabOrder = 0
      ExplicitWidth = 174
    end
  end
  object lbStat: TListBox
    AlignWithMargins = True
    Left = 384
    Top = 37
    Width = 217
    Height = 105
    Margins.Left = 25
    Style = lbOwnerDrawFixed
    AutoComplete = False
    BorderStyle = bsNone
    Color = clBtnFace
    Enabled = False
    ExtendedSelect = False
    TabOrder = 2
  end
  object btnClose: TBitBtn
    Left = 400
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 3
    OnClick = btnCloseClick
  end
end
