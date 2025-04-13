unit LazHotKeyPlatformDarwin;

{$mode ObjFPC}{$H+}
{$LinkFramework Carbon}

interface

uses
  Classes, SysUtils, LCLType, LMessages, LCLIntf,
  BaseUnix, MacOSAll,
  LazHotKeyType, LazHotKeyFunctionsDarwin, LazHotKeyPlatform;

type

  TEventData = record
    FormHandle: HWND;
    HotKeyList: THotKeyList;
    EventOutput: Boolean;
  end;
  PEventData = ^TEventData;

  THotKeyPlatformDarwin = class (THotKeyPlatform)
  protected var
    FEventHanderRef: EventHandlerRef;
    FEventData: TEventData;
    FError: OSStatus;
  protected
    function GetProcessId: Integer; override; 
    procedure SetEventData; override;
    procedure TriggerEvent(AHotKeyId: Integer);
  public
    constructor Create(const AFormHandle: HWND; AHotKeyList: THotKeyList);
    function GetError: Longint; override;
    function GetNativeShortCut(const AShortCut: TShortCut; const APlatformAdjusted: Boolean = True): TNativeShortCut; override;
    function GetNativeShortCut(const AKeyCode, AModifiers: UInt32): TNativeShortCut;
    function GetShortCut(const ANativeShortCut: TNativeShortCut; const APlatformAdjusted: Boolean): TShortCut; override;
    procedure StartHandler; override;
    procedure StopHandler; override;
    function DoRegisterGlobalHotKey(const AHotKeyInfo: PTHotKeyInfo): Boolean; override;
    function DoUnregisterGlobalHotkey(const AHotKeyInfo: PTHotKeyInfo): Boolean; override;
  public
    property Error: OSStatus read FError;
  end;

implementation

function EventHandler(inHandlerCallRef: EventHandlerCallRef; inEvent: EventRef; inUserData: UnivPtr): OSStatus; mwpascal;
var
  LEventHotKeyID: EventHotKeyID;

  iHotKeyId: Integer;
  LNativeShortCut: TNativeShortCut;

  bEventOutput, bPlatformAdjusted : Boolean;

begin

  if GetEventClass(inEvent) = kEventClassKeyboard then
  begin
    if GetEventKind(inEvent) = kEventHotKeyPressed then
    begin

      //bEventOutput := PEventData(data)^.EventOutput;
      GetEventParameter(inEvent, kEventParamDirectObject, typeEventHotKeyID, nil, SizeOf(LEventHotKeyID), nil, @LEventHotKeyID);
      //iHotKeyId := PEventData(data)^.HotKeyList.IndexById(LEventHotKeyID.id);
      //PEventData(data)^.

      try

        if not Assigned(inUserData) then
          raise EArgumentNilException.Create('inUserData is nil.') ;

        if THotKeyPlatformDarwin(inUserData).EventOutput then
          THotKeyPlatformDarwin(inUserData).TriggerEvent(LEventHotKeyID.id)
        else
          PostMessage(THotKeyPlatformDarwin(inUserData).FormHandle, SC_HOTKEY, WPARAM(MESSAGE_HOTKEY_PLATFORMDATA), LPARAM(EventRefPtr(inEvent)));

      except
        Exit(eventParameterNotFoundErr);
      end;

    end;
  end;

  Result := noErr;

end;

constructor THotKeyPlatformDarwin.Create(const AFormHandle: HWND; AHotKeyList: THotKeyList);
begin

  inherited;

  WriteLn('THotKeyPlatformDarwin.Create');

  FError := noErr;
  FEventData.FormHandle := FormHandle;
  FEventData.HotKeyList := HotKeyList;
  FEventData.EventOutput := True;

  FEventHanderRef := nil;
  
end;

function THotKeyPlatformDarwin.GetProcessId: Integer;
begin

  Result := BaseUnix.FpGetPId;

end;

function THotKeyPlatformDarwin.GetError: Longint;
begin

  Result := FError;

end;

procedure THotKeyPlatformDarwin.SetEventData;
begin

  inherited;

  FEventData.EventOutput := FEventOutput;

end;

procedure THotKeyPlatformDarwin.TriggerEvent(AHotKeyId: Integer);
begin

  DoTriggerEvent(AHotKeyId);

end;

function THotKeyPlatformDarwin.GetNativeShortCut(const AShortCut: TShortCut; const APlatformAdjusted: Boolean = True): TNativeShortCut;
begin

  Result := ShortCutToNativeShortCut(AShortCut, APlatformAdjusted);

end;

function THotKeyPlatformDarwin.GetNativeShortCut(const AKeyCode, AModifiers: UInt32): TNativeShortCut;
begin

  Result := KeyCodeToNativeShortCut(AKeyCode, AModifiers);

end;

function THotKeyPlatformDarwin.GetShortCut(const ANativeShortCut: TNativeShortCut; const APlatformAdjusted: Boolean): TShortCut;
begin

  Result := NativeShortCutToShortCut(ANativeShortCut, APlatformAdjusted);

end;

procedure THotKeyPlatformDarwin.StartHandler;
var
  LEventTypeSpec: EventTypeSpec;
begin

  inherited;

  if FEventHanderRef <> nil then
    StopHandler;

  LEventTypeSpec.eventClass := kEventClassKeyboard;
  LEventTypeSpec.eventKind := kEventHotKeyPressed;

// review: process errors?
InstallEventHandler(GetApplicationEventTarget, @EventHandler, 1, @LEventTypeSpec, Pointer(Self), @FEventHanderRef);

end;

procedure THotKeyPlatformDarwin.StopHandler;
begin

  // review: process errors? 
  RemoveEventHandler(FEventHanderRef);
  FEventHanderRef := nil;

  inherited;

end;

function THotKeyPlatformDarwin.DoRegisterGlobalHotKey(const AHotKeyInfo: PTHotKeyInfo): Boolean;
var
  iKeyCode: UInt32;
  iModifiers: UInt32;
  LHotKeyID: EventHotKeyID;
begin

  Result := False;

  NativeShortCutToKeyCode(AHotKeyInfo^.NativeShortCut, iKeyCode, iModifiers);

  LHotKeyID.signature := FProcessId;
  LHotKeyID.id := AHotKeyInfo^.Id;

  // Some of possible errors
  //noErr (0): The hot key was successfully registered.
  //eventNotAvailableErr (-9874): The specified hot key is already in use.
  //eventParameterErr (-9873): An invalid parameter was passed to the function.
  //coreEventNotAvailableErr (-9872): The core event system is not available.
  //coreEventParameterErr (-9871): An invalid parameter was passed to the core event system.
  //coreEventFailedErr (-9870): The core event system failed to register the hot key.

  FError := RegisterEventHotKey(iKeyCode, iModifiers, LHotKeyID, GetApplicationEventTarget, 0, EventHotKeyRef(AHotKeyInfo^.PlatformData));

  if FError = noErr then
    Result := True;

end;

function THotKeyPlatformDarwin.DoUnregisterGlobalHotkey(const AHotKeyInfo: PTHotKeyInfo): Boolean;
begin

  Result := False;

  if AHotKeyInfo = Nil then
    Exit;

  FError := UnregisterEventHotKey(EventHotKeyRef(AHotKeyInfo^.PlatformData));

  if FError = noErr then
    Result := True;

end;

end.

