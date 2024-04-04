program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils,
  Forms, unit1
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

  // Учет утечек памяти. Вывод делаем в текстовый файл *.mem
  {$if declared(UseHeapTrace)}
  if UseHeapTrace then // Test if reporting is on
     SetHeapTraceOutput(ChangeFileExt(ParamStr(0), '.mem'));
  {$ifend}
end.

