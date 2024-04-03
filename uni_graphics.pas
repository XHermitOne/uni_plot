{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit uni_graphics;

{$warn 5023 off : no warning about unused units}
interface

uses
  uni_graphic, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uni_graphic', @uni_graphic.Register);
end;

initialization
  RegisterPackage('uni_graphics', @Register);
end.
