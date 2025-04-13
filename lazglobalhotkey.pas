{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazGlobalHotKey;

{$warn 5023 off : no warning about unused units}
interface

uses
  LazHotKey, LazHotKeyType, LazHotKeyPlatform, LazHotKeyFunctions, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazHotKey', @LazHotKey.Register);
end;

initialization
  RegisterPackage('LazGlobalHotKey', @Register);
end.
