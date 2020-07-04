object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 90
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object EditURL: TEdit
    Left = 24
    Top = 16
    Width = 225
    Height = 21
    TabOrder = 0
    Text = 'https://speedtest.selectel.ru/10MB'
  end
  object ButtonSend: TButton
    Left = 264
    Top = 14
    Width = 97
    Height = 25
    Caption = #1054#1090#1087#1088#1072#1074#1080#1090#1100
    TabOrder = 1
    OnClick = ButtonSendClick
  end
  object ProgressBar1: TProgressBar
    Left = 24
    Top = 56
    Width = 337
    Height = 17
    TabOrder = 2
  end
  object ApplicationEvents1: TApplicationEvents
    OnMessage = ApplicationEvents1Message
    Left = 184
    Top = 48
  end
end
