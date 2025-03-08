unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LMessages, TypInfo, LCLProc,
  {$IfDef MSWindows}
  Windows,  LazHotKeyFunctionsWin,
  {$EndIf}
  {$IfDef Unix}
  {$IfNDef Darwin}
  Unix, X, xlib, keysym, LazHotKeyPlatformUnix, LazHotKeyFunctionsUnix,
  {$EndIf}
  {$EndIf}
  {$IfDef Darwin}
  MacOSAll, LazHotKeyFunctionsDarwin,
  {$EndIf}
  LazHotKey, LazHotKeyType;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    GlobalHotKey1: TGlobalHotKey;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
  protected
    procedure HotKeyMessageHandler(var Msg: TLMessage); message SC_HOTKEY;
    procedure DoSomething;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.DoSomething;
begin
  Memo1.Append('Doing something.');
  Memo1.Append('');
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  bResult: Boolean;
begin


  // AddGlobalHotkeyWindows / AddGlobalHotkeyUnix / AddGlobalHotkeyDarwin
  // are useful when you need to register something very specific to every platform, like IME keys, Media keys etc.
  // As example we are registering Alt+Shift+Pause for Windows, Ctrl+Alt+Pause for X11 Unix and Cmd+Alt+F15 for macOS.
  // Those function do not adjusts shortcuts for platforms.

  // Mind that functions return True, if GlobalHotKey is not Active.
  // When GlobalHotKey is active, call GlobalHotKey.Platform.GetError to retrive platform error, if registration is failed.

  // We can read result directly, or we can use OnRegister event. Event example is shows in ActionList demo.
  bResult := False;

  // Instead of OnHotKey event we are going to use SC_HOTKEY message.
  // Which will bypass system event data via lParam.
  GlobalHotKey1.EventOutput := False;

  // Activating in front to see if there are any errors during registration.
  GlobalHotKey1.Active := True;

  {$IfDef MSWindows} // MSWindows is for desktop windows only
  // Any combination involving Ctrl and Pause wouldn't work in Windows. Ctrl+Pause is a Break system shortcut.
  bResult := GlobalHotKey1.AddGlobalHotkeyWindows(VK_PAUSE, MOD_SHIFT or MOD_ALT);
  {$EndIf}

  {$IfDef Unix} // Most type of Unix (Linux, BSD, like FreeBSD, etc), as most support X11
  {$IfNDef Darwin} // But not Darwin BSD (macOS)
  // Mind, with X11 if you use shifted IDs, like XK_Break, it will still report back non-shifted ID - XK_Pause.
  // Even if you pressed Shift+Pause. You can register XK_Break, for this instance,
  // but you still need to check for XK_Pause instead.
  bResult := GlobalHotKey1.AddGlobalHotkeyUnix(XK_Pause, ControlMask or Mod1Mask);
  {$EndIf}
  {$EndIf}

  {$IfDef Darwin} // Darwin BSD (macOS)
  // Mind, macOS does not have PrintScreen, Scroll Lock and Pause/Break.
  // Instead there are F13, F14 and F15 binded to those buttons.
  bResult := GlobalHotKey1.AddGlobalHotkeyDarwin(kVK_F15, cmdKey or optionKey);
  {$EndIf}

  if not bResult then
    Memo1.Append(Format('Failed to register HotKey with error: %d.', [GlobalHotKey1.Platform.GetError]));

end;

procedure TForm1.HotKeyMessageHandler(var Msg: TLMessage);
var
{$IfDef MSWindows}
  LMsg: PMsg;
  wKeyCode: Word;
  wModifiers: DWord;
{$EndIf}
{$IfDef Unix}
{$IfNDef Darwin}
  LXEvent: PXEvent;
  wKeySym: TKeySym;
  wXMask: cuint;
{$EndIf}
{$EndIf}
{$IfDef Darwin}
  LEventRef: EventRef;
  LEventHotKeyID: EventHotKeyID;
  wKeyCode: UInt32;
  wModifiers: UInt32;
  iHotKeyIndex: Integer;
{$EndIf}
  LShiftState: TShiftState;

  procedure PrintShiftState(AShiftState: TShiftState);
  var
    sString: String;
    LShiftStateItem: TShiftStateEnum;
  begin

    sString := '';
    for LShiftStateItem in AShiftState do
    begin
      if sString <> '' then
        sString := sString + ', ';
      sString := sString + GetEnumName(TypeInfo(TShiftStateEnum), Ord(LShiftStateItem));
    end;
    Memo1.Append(Format('Shift states: [%s]', [sString]));

  end;

begin

  // Checking for MESSAGE_HOTKEY_PLATFORMDATA in wParam.
  // So we ensure this SC_HOTKEY message is coming from GlobalHotKey component.
  if not Msg.WParam = MESSAGE_HOTKEY_PLATFORMDATA then
    Exit;

  LShiftState := [];


  {$IfDef MSWindows}

  // For WinAPI original WM_HOTKEY message is wrapped in lParam.

  LMsg := PMsg(Msg.lParam);
  try

    // wParam of WM_HOTKEY stores hotkey ID, which is same as in THotKeyInfo.Id.
    // You can find it in the list with GlobalHotKey.HotKeyList.IndexById
    Memo1.Append(Format('SC_HOTKEY: wParam=%d', [ DWord(LMsg^.wParam) ]));
    // lParam of WM_HOTKEY stores hotkey code
    Memo1.Append(Format('SC_HOTKEY: lParam=$%s', [ IntToHex(LMsg^.lParam, 8) ]));

    // You can use VKHotKeyToVKKeycodeAndWinModifiers and VKHotKeyToVKKeycodeAndShiftState functions
    // to split key code and modifiers.
    VKHotKeyToVKKeycodeAndWinModifiers(LMsg^.lParam, wKeyCode, wModifiers);
    LShiftState := WinModifiersToShiftState(wModifiers);

    // Ouputting codes
    Memo1.Append(Format('Keycode: %d ($%s)', [wKeyCode, IntToHex(wKeyCode, 2)]));
    Memo1.Append(Format('Modifiers: %d', [wModifiers]));
    PrintShiftState(LShiftState);

    // Check for hotkey to do what we need.
    if (wKeyCode = VK_PAUSE) and (wModifiers = MOD_SHIFT or MOD_ALT) then
      DoSomething;

  finally
    // Disposing LMsg to avoid memory leak.
    Dispose(LMsg);
  end;

  {$EndIf}


  {$IfDef Unix}
  {$IfNDef Darwin}

  // For X11 XEvent is wrapped in lParam.

  LXEvent := PXEvent(Msg.LParam);
  try
    Memo1.Append(Format('SC_HOTKEY: XEvent.xkey.keycode = %d', [LXEvent^.xkey.keycode]));
    Memo1.Append(Format('SC_HOTKEY: XEvent.xkey.state = %d', [LXEvent^.xkey.state]));

    // X keycode doesn't tell us much. It may vary from one layout to another.
    // So should ve converted to X keysym by local X server.
    // You can use XKeycodeToXKeysym function, but you need to esablish connection to X server to feed Display ID.
    // Or you can use wrapper in THotKeyPlatformUnix that will use THotKeyPlatformUnix's connection.
    wKeySym := THotKeyPlatformUnix(GlobalHotKey1.Platform).GetKeysym(LXEvent^.xkey.keycode);

    // Mind, that X11 treating Num and Caps Lock as modifiers, so you need to filter then out
    // wXMask := wXMask and not (LockMask or Mod2Mask);
    // But if you are using XKeysymMaskToShiftState or XKeysymMaskToWinModifiers function,
    // they will do it for you.
    wXMask := LXEvent^.xkey.state;
    LShiftState := XKeysymMaskToShiftState(wXMask);
    wXMask := wXMask and not (LockMask or Mod2Mask);

    // Outputting codes
    Memo1.Append(Format('SC_HOTKEY: KeySym = %d ($%s)', [wKeySym, IntToHex(wKeySym, 4)]));
    Memo1.Append(Format('SC_HOTKEY: Mask = %d ($%s)', [wXMask, IntToHex(wXMask, 2)]));
    PrintShiftState(LShiftState);

    // Check for hotkey to do what we need to do
    if (wKeySym = XK_Pause) and (wXMask = ControlMask or Mod1Mask) then
      DoSomething;

  finally
    // Disposing LXEvent to avoid memory leak.
    Dispose(LXEvent);
  end;

  {$EndIf}
  {$EndIf}


  {$IfDef Darwin}

    // For macOS EventRef is wrapped in lParam.
    // With EventRef you can retrive some HotKey data, but not much. Only program ID and hotkey ID,
    // that is THotKeyInfo.Id.
    // You can find it in the list with GlobalHotKey.HotKeyList.IndexById

    LEventRef := EventRef(Msg.LParam);
    GetEventParameter(LEventRef, kEventParamDirectObject, typeEventHotKeyID, nil, SizeOf(LEventHotKeyID), nil, @LEventHotKeyID);
    Memo1.Append(Format('SC_HOTKEY: EventHotKeyID = %d', [LEventHotKeyID.id]));

    iHotKeyIndex := GlobalHotKey1.HotKeyList.IndexById(LEventHotKeyID.id);
    if iHotKeyIndex < 0 then
      Exit; // Not found for whatever reason.

    wKeyCode := GlobalHotKey1.HotKeyList.Items[iHotKeyIndex].GetNativeKeyCode;
    // Do not use GetShortCutShiftState with AddGlobalHotkeyWindows / AddGlobalHotkeyUnix / AddGlobalHotkeyDarwin,
    // as THotKeyItem.ShortCut is not being used with them. It can't hold large amount of different keys
    wModifiers := GlobalHotKey1.HotKeyList.Items[iHotKeyIndex].GetNativeModifiers;
    LShiftState := MacModifiersToShiftState(wModifiers, GlobalHotKey1.HotKeyList.Items[iHotKeyIndex].PlatformAdjusted);

    // Outputting codes
    Memo1.Append(Format('SC_HOTKEY: Mac keycode = %d ($%s)', [wKeyCode, IntToHex(wKeyCode, 2)]));
    Memo1.Append(Format('SC_HOTKEY: Mac modifiers = %d ($%s)', [wModifiers, IntToHex(wModifiers, 2)]));
    PrintShiftState(LShiftState);

    // Check for hotkey to do what we need to do
    if (wKeyCode = kVK_F15) and (wModifiers = cmdKey or optionKey) then
      DoSomething;

    // EventRef will be disposed automatically.

  {$EndIf}

end;

end.

