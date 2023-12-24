program test;

{$mode objfpc}{$H+}

uses
  Classes,
  consoletestrunner,
  TestDataUtils,
  testcase_RBTree,
  testcase_StringRBTree,
  testcase_StringObjectRBTree,
  testcase_StringPairRBTree;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
    // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  DefaultFormat := fPlain;
  DefaultRunAllTests := True;

  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
  ReadLn;
end.
