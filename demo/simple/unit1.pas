unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LazHotKey, LazHotKeyType, LCLProc;

type

  { TForm1 }

  TForm1 = class(TForm)
    GlobalHotKey1: TGlobalHotKey;
    Memo1: TMemo;
    procedure GlobalHotKey1HotKey(AHotKeyInfo: PTHotKeyInfo);
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
  Memo1.Append(ShortCutToTextRaw(AHotKeyInfo^.ShortCut));
end;

end.

