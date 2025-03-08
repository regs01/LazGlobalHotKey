unit FormHotKeyListEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ActnList, ComCtrls, LCLProc, LazHotKey, LazHotKeyType;

type

  { TfrmHotKeyListEditor }

  TfrmHotKeyListEditor = class(TForm)
    cAddHotKey: TAction;
    cRemoveHotKey: TAction;
    cRefreshList: TAction;
    alHotKeys: TActionList;
    lbHotKeys: TListBox;
    mnuAdd: TMenuItem;
    mnuRemove: TMenuItem;
    mnuRefresh: TMenuItem;
    pmnuContextMenu: TPopupMenu;
    FGlobalHotKeyComponent: TGlobalHotKey;
    Separator1: TMenuItem;
    tbMain: TToolBar;
    tbtnAddHotKey: TToolButton;
    tbtnRemoveHotKey: TToolButton;
    tbtnDivider1: TToolButton;
    tbtnRefreshList: TToolButton;
    procedure cAddHotKeyExecute(Sender: TObject);
    procedure cRefreshListExecute(Sender: TObject);
    procedure cRemoveHotKeyExecute(Sender: TObject);
  protected var
    FModified: Boolean;
  public
    constructor {%H-}Create(AGlobalHotKeyComponent: TGlobalHotKey);
  public
    property Modified: Boolean read FModified default False;
  end;

var
  frmHotKeyListEditor: TfrmHotKeyListEditor;

implementation

uses
  FormShortCutGrabber, IDEImagesIntf, IDEIntf;

{$R *.lfm}

constructor TfrmHotKeyListEditor.Create(AGlobalHotKeyComponent: TGlobalHotKey);
begin

  inherited Create(Nil);

  alHotKeys.Images := IDEImages.Images_16;
  pmnuContextMenu.Images := IDEImages.Images_16;;
  tbMain.Images := IDEImages.Images_16;;
  cAddHotKey.ImageIndex := IDEImages.LoadImage('laz_add');
  cRemoveHotKey.ImageIndex := IDEImages.GetImageIndex('laz_delete');
  cRefreshList.ImageIndex := IDEImages.GetImageIndex('laz_refresh');

  FGlobalHotKeyComponent := AGlobalHotKeyComponent;
  FModified := False;

  cRefreshList.Execute;

end;

procedure TfrmHotKeyListEditor.cAddHotKeyExecute(Sender: TObject);
var
  LFormShortCutGrabber: TfrmShortCutGrabber;
begin

  LFormShortCutGrabber := TfrmShortCutGrabber.Create(Self);

  try

    if LFormShortCutGrabber.ShowModal = mrOK then
    begin
      FGlobalHotKeyComponent.AddGlobalHotkey(LFormShortCutGrabber.ShortCut);
      FModified := True;
      cRefreshList.Execute;
    end;

  finally
    LFormShortCutGrabber.Free;
  end;


end;

procedure TfrmHotKeyListEditor.cRemoveHotKeyExecute(Sender: TObject);
var
  iHotKeyId, iHotKeyIndex: Integer;
begin

  if lbHotKeys.ItemIndex < 0 then
    Exit;

  iHotKeyId := -1;
  iHotKeyIndex := -1;

  iHotKeyId := Integer(PtrUInt(lbHotKeys.Items.Objects[lbHotKeys.ItemIndex]));
  iHotKeyIndex := FGlobalHotKeyComponent.HotKeyList.IndexById(iHotKeyId);
  FGlobalHotKeyComponent.HotKeyList.Delete(iHotKeyIndex);
  FModified := True;

  cRefreshList.Execute;

end;

procedure TfrmHotKeyListEditor.cRefreshListExecute(Sender: TObject);
var
  iHotKeyIndex: Integer;
  iHotKeyId: Word;
  sShortCut: String;
  LNativeShortCut: TNativeShortCut;
  LShortCut: TShortCut;
  bPlatformAdjusted: Boolean;
begin

  lbHotKeys.Clear;

  for iHotKeyIndex := 0 to FGlobalHotKeyComponent.HotKeyList.Count-1 do
  begin

    LNativeShortCut := FGlobalHotKeyComponent.HotKeyList.Items[iHotKeyIndex]{%H-}.NativeShortCut;
    bPlatformAdjusted := FGlobalHotKeyComponent.HotKeyList.Items[iHotKeyIndex]{%H-}.PlatformAdjusted;;
    LShortCut := FGlobalHotKeyComponent.Platform.GetShortCut(LNativeShortCut, bPlatformAdjusted);
    iHotKeyId := FGlobalHotKeyComponent.HotKeyList.Items[iHotKeyIndex]{%H-}.Id;


    sShortCut := Format('%s (NativeShortCut: $%s)', [{%H-}ShortCutToText(LShortCut), IntToHex(LNativeShortCut, 4)]);

    lbHotKeys.AddItem(sShortCut, TObject(PtrUInt(iHotKeyId)));

  end;

end;

end.

