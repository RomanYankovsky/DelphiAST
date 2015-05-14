object Form2: TForm2
  Left = -3
  Height = 231
  Top = 81
  Width = 687
  Caption = 'DelphiAST Test Application'
  ClientHeight = 231
  ClientWidth = 687
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  LCLVersion = '1.5'
  object memLog: TMemo
    Left = 0
    Height = 193
    Top = 0
    Width = 687
    Anchors = [akTop, akLeft, akRight, akBottom]
    Font.CharSet = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Lucida Console'
    ParentFont = False
    TabOrder = 0
  end
  object btnRun: TButton
    Left = 604
    Height = 25
    Top = 198
    Width = 75
    Anchors = [akRight, akBottom]
    Caption = 'Run'
    OnClick = btnRunClick
    TabOrder = 1
  end
end
