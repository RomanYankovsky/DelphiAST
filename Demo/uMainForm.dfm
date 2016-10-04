object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'DelphiAST Demo'
  ClientHeight = 389
  ClientWidth = 666
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object OutputMemo: TMemo
    Left = 0
    Top = 0
    Width = 666
    Height = 370
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitHeight = 389
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 370
    Width = 666
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitLeft = 344
    ExplicitTop = 216
    ExplicitWidth = 0
  end
  object MainMenu: TMainMenu
    Left = 224
    Top = 96
    object OpenDelphiUnit1: TMenuItem
      Caption = 'Open Delphi Unit...'
      OnClick = OpenDelphiUnit1Click
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'Delphi Unit|*.pas|Delphi Package|*.dpk|Delphi Project|*.dpr'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 272
    Top = 96
  end
end
