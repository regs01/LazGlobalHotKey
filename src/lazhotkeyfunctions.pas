unit LazHotKeyFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc;

type
  TEMetaKeys = (mkSuper, mkWin, mkCommand);

const
  META_KEY_WIN = 'Win';
  META_KEY_SUPER = 'Super';
  META_KEY_COMMAND = 'Command';
  META_KEYS: array[TEMetaKeys] of string = (META_KEY_SUPER, META_KEY_WIN, META_KEY_COMMAND);

  function PlatformMetaKeyString: String;
  function PlatformMetaKey: TEMetaKeys;
  function ShortCutToTextFix(const AShortCut: TShortCut): String;
  function TextToShortCutFix(AShortCutString: String): TShortCut;


implementation

function PlatformMetaKeyString: String;
begin

  Result := META_KEYS[PlatformMetaKey];

end;

function PlatformMetaKey: TEMetaKeys;
begin

  Result := TEMetaKeys(0);

  {$IfDef MSWindows}
    Result := mkWin;
  {$EndIf}
  {$IfDef Unix}
  {$IfNDef Darwin}
    Result := mkSuper;
  {$Else}
    Result := mkCommand;
  {$EndIf}
  {$EndIf}

end;

function ShortCutToTextFix(const AShortCut: TShortCut): String;
var
  LMetaKey: TEMetaKeys;
begin

  Result := ShortCutToTextRaw(AShortCut);

  Result := Result.Replace('Meta', PlatformMetaKeyString);

  for LMetaKey := Low(TEMetaKeys) to High(TEMetaKeys) do
  begin
    if PlatformMetaKey = LMetaKey then
      Continue;
    Result := Result.Replace(META_KEYS[LMetaKey], PlatformMetaKeyString);
  end;

end;

function TextToShortCutFix(AShortCutString: String): TShortCut;
var
  LMetaKey: TEMetaKeys;
begin

  for LMetaKey := Low(TEMetaKeys) to High(TEMetaKeys) do
  begin
    AShortCutString := AShortCutString.Replace(META_KEYS[LMetaKey], 'Meta');
  end;

  Result := TextToShortCutRaw(AShortCutString);

end;

end.

