{
Модуль сервисных функций

Версия: 0.0.2.1
}
unit toolfunc;


{$mode objfpc}{$H+}

interface

uses
  SysUtils;


function StrTimeToLong(ABuffer: String): LongInt;
function LongToStrTime(ALTime: LongInt): String;

implementation

function StrTimeToLong(ABuffer: String): LongInt;
var
  _time: LongInt = 0;
  i: Integer = 0;
  j: Integer = 0;
  k: Integer = 0;

begin
  for i := 0 to 2 do
  begin
    k := StrToInt(Copy(ABuffer, j, 2));
    _time := _time * 60 + k;
    while ABuffer[j] <> ':' do
      Inc(j);
    Inc(j);
  end;

  Result := _time;
end;


function LongToStrTime(ALTime: LongInt): String;
var
  buffer: String;
begin
  if (ALTime >= 0) and (ALTime <> 1000000000) then
    buffer := Format('%.2d:%.2d:%.2d', [(ALTime div 3600) mod 24, (ALTime div 60) mod 60, ALTime mod 60])
  else
    buffer := '##:##:##';
  if (ALTime <> 1000000000) and (ALTime >= 86400) then
    buffer := Format('%.6ld', [ALTime / 86400]);
  Result := buffer;
end;


end.
