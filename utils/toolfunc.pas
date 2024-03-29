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
  dt: TDateTime;
  time_stamp: TTimeStamp;
begin
  dt := StrToTime(ABuffer);
  time_stamp := DateTimeToTimeStamp(dt);
  Result := time_stamp.Time;
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
    //buffer[9] := Format('%06ld', [ALTime / 86400]);
    buffer := Format('%.6ld', [ALTime / 86400]);
  Result := buffer;
end;

end.
