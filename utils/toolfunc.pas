{
Модуль сервисных функций

Версия: 0.0.0.1
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
  while i < 3 do
  begin
    k := StrToInt(ABuffer[j]);
    _time := _time * 60 + k;

    //while (ABuffer[j] <> ':') and (ABuffer[j]) do
    while (ABuffer[j] <> ':') and (j < Length(ABuffer)) do
      Inc(j);

    //if not ABuffer[j] then
    if j >= Length(ABuffer) then
      break
    else
      Inc(j);
    Inc(i);
  end;
  Result := _time;
end;


function LongToStrTime(ALTime: LongInt): String;
var
  buffer: String;
begin
  if (ALTime >= 0) and (ALTime <> 1000000000) then
    buffer := Format('%02ld:%02ld:%02ld', [(ALTime div 3600) mod 24, (ALTime div 60) mod 60, ALTime mod 60])
  else
    buffer := '##:##:##';
  if (ALTime <> 1000000000) and (ALTime >= 86400) then
    //buffer[9] := Format('%06ld', [ALTime / 86400]);
    buffer := Format('%06ld', [ALTime / 86400]);
  Result := buffer;
end;

end.
