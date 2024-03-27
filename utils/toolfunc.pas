{
Модуль сервисных функций

Версия: 0.0.0.1
}
unit toolfunc;


{$mode objfpc}{$H+}

interface

function StrTimeToLong(ABuffer: String): LongInt;
function LongToStrTime(ALTime: LongInt): String

implementation

function StrTimeToLong(ABuffer: String): LongInt
var
  _time: LongInt = 0L;
  i: Integer = 0;
  j: Integer = 0;
  k: Integer = 0;
begin
  while i < 3 do
  begin
    k := StrToInt(ABuffer[j]);
    _time := _time * 60 + k;

    while (ABuffer[j] <> ':') and (ABuffer[j]) do
      Inc(j);

    if not buffer[j] then
      break;
    else
      Inc(j);
    Inc(i);
  end;
  Result := _time;
end;


function LongToStrTime(ALTime: LongInt): String
var
  buffer: String;
begin
  if (ALTime >= 0) and (ALTime <> 1000000000) then
    buffer := Format('%02ld:%02ld:%02ld', [(ALTime / 3600) % 24, (ALTime / 60) % 60, ALTime % 60]);
  else
    buffer := '##:##:##';
  if (ALTime <> 1000000000) and (ALTime >= 86400) then
    buffer[9] := Format('%06ld', [ltime / 86400]);
  Result := buffer;
end;

end.
