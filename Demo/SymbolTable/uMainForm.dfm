object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'DelphiAST SymbolTable Demo'
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 370
    Width = 666
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 666
    Height = 370
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 1
    object TabSheet2: TTabSheet
      Caption = 'Symbols'
      ImageIndex = 1
      object Splitter1: TSplitter
        Left = 0
        Top = 121
        Width = 658
        Height = 3
        Cursor = crVSplit
        Align = alTop
        ExplicitWidth = 198
      end
      object SymbolsBox: TListBox
        Left = 0
        Top = 0
        Width = 658
        Height = 121
        Align = alTop
        ItemHeight = 13
        TabOrder = 0
        OnClick = SymbolsBoxClick
      end
      object DeclarationMemo: TMemo
        Left = 0
        Top = 124
        Width = 658
        Height = 218
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 1
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 224
    Top = 96
    object OpenDelphiProject1: TMenuItem
      Caption = 'Open Delphi Project...'
      OnClick = OpenDelphiProject1Click
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'Delphi Project (*.dpr)|*.dpr'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 272
    Top = 96
  end
end
