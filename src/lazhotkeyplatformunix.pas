unit LazHotKeyPlatformUnix;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LMessages, LCLIntf,
  BaseUnix, X, Xlib, xkblib, Unix, fptimer,
  LazHotKeyType, LazHotKeyFunctionsUnix, LazHotKeyPlatform;

type

  // Switching to FPTimer, instead of GLib timer.
  //TEventData = record
  //  Display: PDisplay;
  //  FormHandle: HWND;
  //  HotKeyList: THotKeyList;
  //  SimpleOutput: Boolean;
  //end;
  //PEventData = ^TEventData;

  THotKeyPlatformUnix = class (THotKeyPlatform)
  protected var
    FDisplay: PDisplay;
    FRootWindow: TWindow;
    //FGLibTimeoutId: guint;
    //FEventData: TEventData;
    FTimer: TFPTimer;
    FUnixInterval: Word;
    FError: cint;
    FErrorSilent: Boolean;
  protected
    function GetProcessId: Integer; override;
    procedure SetEventData; override;
    procedure SetUnixInterval(AValue: Word);
    procedure TimerHandler(Sender: TObject);
    procedure TriggerEvent(ANativeShortCut: TNativeShortCut);
  protected
    property XDisplay: PDisplay read FDisplay; // Forwarding readonly properties to GLib
  public
    constructor Create(const AFormHandle: HWND; AHotKeyList: THotKeyList);
    destructor Destroy; override;
    function GetError: Longint; override;
    function GetNativeShortCut(const AShortCut: TShortCut; const {%H-}APlatformAdjusted: Boolean = True): TNativeShortCut; override;
    function GetNativeShortCut(const AKeysym: TKeySym; const AModifiers: cuint): TNativeShortCut;
    function GetShortCut(const ANativeShortCut: TNativeShortCut; const {%H-}APlatformAdjusted: Boolean): TShortCut; override;
    function GetKeysym(const AXKeycode: TKeyCode): TKeySym;
    procedure StartHandler; override;
    procedure StopHandler; override;
    function DoRegisterGlobalHotKey(const AHotKeyInfo: PTHotKeyInfo): Boolean; override;
    function DoUnregisterGlobalHotkey(const AHotKeyInfo: PTHotKeyInfo): Boolean; override;
  public
    property Error: cint read FError;
    property UnixInterval: Word read FUnixInterval write SetUnixInterval;
  end;

var
  XError: cuchar;

implementation

// Filter handler is only working in limited set of environments
// function EventFilterHandler(xevent:PGdkXEvent; event:PGdkEvent; data:pointer):TGdkFilterReturn; cdecl;

// Moving to internal FP timer instead
// function EventTimeoutHandler(data: gpointer): gboolean; cdecl;

function XErrorHandler({%H-}Display: PDisplay; ErrorEvent: PXErrorEvent): cint; cdecl;
begin

  XError := ErrorEvent^.error_code;
  Result := 0;

end;

constructor THotKeyPlatformUnix.Create(const AFormHandle: HWND; AHotKeyList: THotKeyList);
begin

  inherited;

  WriteLn('THotKeyPlatformUnix.Create');

  FError := Success;
  XError := 0;
  //FGLibTimeoutId := 0;
  FDisplay := XOpenDisplay(Nil);
  FRootWindow := XDefaultRootWindow(FDisplay);
  if FDisplay = Nil then 
    Exception.Create('Failed to establish X11 connection.');
  //FEventData.Display := XDisplay;
  //FEventData.FormHandle := FormHandle;
  //FEventData.HotKeyList := HotKeyList; //
  //FEventData.SimpleOutput := True;

  FTimer := TFPTimer.Create(nil);
  FTimer.Interval := 100;
  FTimer.OnTimer := @TimerHandler;

end;

destructor THotKeyPlatformUnix.Destroy;
begin

  try

    if FDisplay <> nil then
    begin
      if XSync(FDisplay, False) = 0 then
        XCloseDisplay(FDisplay);
    end;

    StopHandler;
    FTimer.OnTimer := nil;

  finally
    FTimer.Free;
  end;

  inherited;

end;

function THotKeyPlatformUnix.GetProcessId: Integer;
begin

  Result := BaseUnix.FpGetPId;

end;

function THotKeyPlatformUnix.GetError: Longint;
begin

  Result := FError;

end;

procedure THotKeyPlatformUnix.SetEventData;
begin

  inherited;

  //FEventData.SimpleOutput := FEventOutput;

end;

procedure THotKeyPlatformUnix.SetUnixInterval(AValue: Word);
begin

  if AValue = 0 then
    AValue := 100;

  if FTimer.Interval <> AValue then
    FTimer.Interval := AValue;

end;

function THotKeyPlatformUnix.GetNativeShortCut(const AShortCut: TShortCut; const APlatformAdjusted: Boolean = True): TNativeShortCut;
begin

  Result := ShortCutToNativeShortCut(AShortCut, FDisplay);

end;

function THotKeyPlatformUnix.GetNativeShortCut(const AKeysym: TKeySym; const AModifiers: cuint): TNativeShortCut;
begin

  Result := XKeysymToNativeShortCut(AKeysym, AModifiers, FDisplay);

end;

function THotKeyPlatformUnix.GetShortCut(const ANativeShortCut: TNativeShortCut; const APlatformAdjusted: Boolean): TShortCut;
begin

  Result := NativeShortCutToShortCut(ANativeShortCut, FDisplay);

end;

function THotKeyPlatformUnix.GetKeysym(const AXKeycode: TKeyCode): TKeySym;
begin

  Result := XKeycodeToXKeysym(AXKeycode, FDisplay);

end;

procedure THotKeyPlatformUnix.TimerHandler(Sender: TObject);
var
  LXEvent: TXEvent;
  LXKeyPressEvent: PXEvent;
  wKeysym: TKeySym;
  wKeycode, wMask: cuint;
  LNativeShortCut: TNativeShortCut;
begin

  //WriteLn('TimerHandler: ', DateTimeToTimeStamp(Now).Time.ToString);

  try
    while XPending(FDisplay) > 0 do
    begin
      XNextEvent(FDisplay, @LXEvent);
      if LXEvent.xany._type = KeyPress then
      begin

        if FEventOutput then
        begin

          wKeycode := LXEvent.xkey.keycode;
          wKeysym := XkbKeycodeToKeysym(FDisplay, wKeycode, 0, 0);
          wMask := LXEvent.xkey.state;

          LNativeShortCut := GetNativeShortCut(wKeysym, wMask);
          TriggerEvent(LNativeShortCut);

        end
        else
        begin

          // Creating a copy before passing it into a message, as X11 will dispose data before message reaches recipient
          New(LXKeyPressEvent);
          LXKeyPressEvent^ := LXEvent;
          WriteLn('EventTimeoutHandler: FullOutput');
          try
            PostMessage(FFormHandle, SC_HOTKEY, WPARAM(MESSAGE_HOTKEY_PLATFORMDATA), {%H-}LPARAM(LXKeyPressEvent));
          except
            Dispose(LXKeyPressEvent);
            raise;
          end;

        end;

      end;
    end;
  except
    //Exit(False); {G_SOURCE_REMOVE} // Removing timer handler, if something failed
  end;

  //Result := True; {G_SOURCE_CONTINUE}

end;

procedure THotKeyPlatformUnix.TriggerEvent(ANativeShortCut: TNativeShortCut);
var
  iHotKeyIndex: Integer;
begin

  iHotKeyIndex := FHotKeyList.IndexByNativeShortCut(ANativeShortCut);

  if iHotKeyIndex >= 0 then
    DoTriggerEvent(FHotKeyList.Items[iHotKeyIndex]{%H-}.Id);


end;

procedure THotKeyPlatformUnix.StartHandler;
begin

  inherited;

  // review: process errors? 
  //FGLibTimeoutId := glib2.g_timeout_add(100, @EventTimeoutHandler, gpointer(@FEventData));

  XSelectInput(FDisplay, FRootWindow, KeyPressMask);
  FTimer.Enabled := True;
  FTimer.StartTimer;

end;

procedure THotKeyPlatformUnix.StopHandler;
begin

  // review: process errors? 
  FTimer.StopTimer;
  FTimer.Enabled := False;
  XSelectInput(FDisplay, FRootWindow, NoEventMask);

  //if FGLibTimeoutId > 0 then
  //begin
  //  if glib2.g_source_remove(FGLibTimeoutId) then
  //    FGLibTimeoutId := 0
  //  else
  //    Exception.Create('Failed to stop timeout hot key function.');
  //end;

  WriteLn('THotKeyPlatformUnix.StopHandler.');

  inherited;

end;

function THotKeyPlatformUnix.DoRegisterGlobalHotKey(const AHotKeyInfo: PTHotKeyInfo): Boolean;
var
  //iKeyCode: Word;
  //iXKeySym: TKeySym;
  //LShiftState: TShiftState;
  iXKeyCode: TKeyCode; //Byte;
  iXModifiers: cuint;  //LongWord;
  LPreviousErrorHandler: TXErrorHandler;

  function DoXGrabKey(AXKeyCode: cint; AXmodifiers: cuint): Boolean;
  begin

    Result := False;
    FError := XGrabKey(FDisplay, AXKeyCode, AXmodifiers, FRootWindow, 0, GrabModeAsync, GrabModeAsync);

    // 0 - Success
    // 1 - Undocummented, but appears to be Success in some X11 builds and neither BadRequest, nor AlreadyGrabbed.
    //     XGrabKeyboard can return AlreadyGrabbed, but that's not the case with XGrabKey.
    if not(FError in [0, 1]) then
    begin
      // Silently attempt to unregister everything, if there is an error.
      FErrorSilent := True;
      DoUnregisterGlobalHotkey(AHotKeyInfo);
      Exit;
    end;

    // Now reading X errors
    XSync(FDisplay, False);
    FError := XError;
    if FError <> Success then
    begin
      // Silently attempt to unregister everything, if there is an error.
      FErrorSilent := True;
      DoUnregisterGlobalHotkey(AHotKeyInfo);
      Exit;
    end;

    Result := True;

  end;

begin

  Result := False;

  if AHotKeyInfo = Nil then
    Exit;

  NativeShortCutToKeyCode(AHotKeyInfo^.NativeShortCut, iXKeyCode, iXModifiers);
  //iXKeySym := VKKeycodeToXKeysym(iKeyCode);
  //iXKeyCode := XKeysymToKeycode(FDisplay, iXKeySym);
  //iXModifiers := ShiftStateToKeysymMask(LShiftState);
  //WriteLn('Grabbing: iKeyCode=', iKeyCode.ToString, ',  iXKeySym=', iXKeySym.ToString, ', iXKeyCode=', iXKeyCode.ToString);
  WriteLn('Grabbing: iXKeyCode=', iXKeyCode.ToString, ' ,iXModifiers=$', iXModifiers.ToHexString(2));
  if iXKeyCode = 0 then
    Exit;

  FError := 0;
  XError := 0;
  FErrorSilent := False;

  // https://www.x.org/releases/current/doc/libX11/libX11/libX11.html
  // Some of possible errors
  // BadValue (2).
  // BadWindow (3).
  // BadAccess (10) - means already grabbed.

  LPreviousErrorHandler := XSetErrorHandler(@XErrorHandler);

  // X11 incorrectly threating Lock keys as Modifiers.
  // So making grabs for any possible variation of Caps Lock and Num Lock.
  // Scroll Lock is not a Modifier in X11.
  // Note: ISO Shifts require testing.
  //   1. ShiftMask   : Shift
  //   2. LockMask    : Caps Lock
  //   4. ControlMask : Ctrl
  //   8. Mod1Mask    : Alt (could also be Meta (not ssMeta))
  //  16. Mod2Mask    : Num Lock
  //  32. Mod3Mask    : ISO_Level5_Shift?
  //  64. Mod4Mask    : Super, Win (ssMeta) (could also be Hyper)
  // 128. Mod5Mask    : ISO_Level3_Shift? (AltGr?)

  try

    if not DoXGrabKey(iXKeyCode, iXModifiers) then
      Exit;
    if not DoXGrabKey(iXKeyCode, iXModifiers or (LockMask)) then
      Exit;
    if not DoXGrabKey(iXKeyCode, iXModifiers or (Mod2Mask)) then
      Exit;
    if not DoXGrabKey(iXKeyCode, iXModifiers or (LockMask or Mod2Mask)) then
      Exit;

  finally
    XSetErrorHandler(LPreviousErrorHandler);
  end;

  WriteLn('After grab.');

  Result := True;

end;

function THotKeyPlatformUnix.DoUnregisterGlobalHotkey(const AHotKeyInfo: PTHotKeyInfo): Boolean;
var
  //LNativeShortCut: TNativeShortCut;
  //iKeyCode: Word;
  //iXKeySym: TKeySym;
  //LShiftState: TShiftState;
  iXKeyCode: TKeyCode;
  iXModifiers: cuint;
  LPreviousErrorHandler: TXErrorHandler;

  function DoXUngrabKey(AXKeyCode: cint; AXmodifiers: cuint): Boolean;
  var
    iError: cint;
  begin

    XError := 0;

    Result := False;
    iError := XUngrabKey(FDisplay, AXKeyCode, AXmodifiers, FRootWindow);

    // 0 - Success
    // 1 - Undocummented, but appears to be Success in some X11 builds and neither BadRequest.
    if not (iError in [0, 1]) and (not FErrorSilent) then
      FError := iError;
    if not iError in [0, 1] then
      Exit(False);

    // Now reading X errors
    XSync(FDisplay, False);
    if (XError<>0) and not FErrorSilent then
      FError := XError;
    if XError <> Success then
      Exit(False);

    Result := True;

  end;

begin

  Result := False;

  if AHotKeyInfo = Nil then
    Exit;

  //LNativeShortCut := FShortCutList.Keys[AShortCutIndex];
  NativeShortCutToKeyCode(AHotKeyInfo^.NativeShortCut, iXKeyCode, iXModifiers);
  //iXKeySym := VKKeycodeToXKeysym(iKeyCode);
  //iXKeyCode := XKeysymToKeycode(FDisplay, iXKeySym);
  //iXModifiers := ShiftStateToKeysymMask(LShiftState);
  //WriteLn('Ungrabbing: iKeyCode=', iKeyCode.ToString, ',  iXKeySym=', iXKeySym.ToString, ', iXKeyCode=', iXKeyCode.ToString);
  WriteLn('Ungrabbing: iXKeyCode=', iXKeyCode.ToString, ' ,iXModifiers=$', iXModifiers.ToHexString(2));
  if iXKeyCode = 0 then
    Exit;

  if not FErrorSilent then
    FError := 0;

  LPreviousErrorHandler := XSetErrorHandler(@XErrorHandler);

  // We do not stop if one failed. We keep attempting others. And keep shortcut in the list, if one is failed.
  // Some of possible errors
  // BadValue (2).
  // BadWindow (3).
  Result := Result and DoXUngrabKey(iXKeyCode, iXModifiers) and (not FErrorSilent);
  Result := Result and DoXUngrabKey(iXKeyCode, iXModifiers or (LockMask)) and (not FErrorSilent);
  Result := Result and DoXUngrabKey(iXKeyCode, iXModifiers or (Mod2Mask)) and (not FErrorSilent);
  Result := Result and DoXUngrabKey(iXKeyCode, iXModifiers or (LockMask or Mod2Mask)) and (not FErrorSilent);

  XSetErrorHandler(LPreviousErrorHandler);

  WriteLn('After ungrabbing.');

end;

end.

