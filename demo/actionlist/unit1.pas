unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList, LCLProc,
  LazHotKey, LazHotKeyType;

type

  { TForm1 }

  TForm1 = class(TForm)
    Action1: TAction;
    Action2: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    GlobalHotKey1: TGlobalHotKey;
    Memo1: TMemo;
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure GlobalHotKey1HotKey(AHotKeyInfo: PTHotKeyInfo);
    procedure GlobalHotKey1Register(AHotKeyInfo: PTHotKeyInfo;
      ASuccess: Boolean; AErrorCode: LongInt);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.GlobalHotKey1HotKey(AHotKeyInfo: PTHotKeyInfo);
begin

  if AHotKeyInfo^.ShortCut = Action1.ShortCut then
    Action1.Execute;
  if AHotKeyInfo^.ShortCut = Action2.ShortCut then
    Action2.Execute;

end;

procedure TForm1.GlobalHotKey1Register(AHotKeyInfo: PTHotKeyInfo; ASuccess: Boolean; AErrorCode: LongInt);
begin

  // THotKeyInfo.ShortCut is only filled, if we use AddGlobalHotkey(TShortCut)
  // In other way you have to decode THotKeyInfo.NativeShortCut using other tools.
  if ASuccess then
    Memo1.Append(Format('Successfully registered %s.', [{%H-}ShortCutToText(AHotKeyInfo^.ShortCut)]))
  else
    Memo1.Append(Format('Failed to register %s with error: %d.', [{%H-}ShortCutToText(AHotKeyInfo^.ShortCut), AErrorCode]));

end;

procedure TForm1.Button1Click(Sender: TObject);
begin

  GlobalHotKey1.AddGlobalHotkey(Action1.ShortCut);
  GlobalHotKey1.AddGlobalHotkey(Action2.ShortCut);

end;

procedure TForm1.Action1Execute(Sender: TObject);
begin

  Memo1.Append('Action1 executed.');

end;

procedure TForm1.Action2Execute(Sender: TObject);
begin

  Memo1.Append('Action2 executed.');

end;

end.

