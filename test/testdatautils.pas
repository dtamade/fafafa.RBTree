unit TestDataUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math;

type

  PTestPair = ^TTestPair;

  TTestPair = record
    Key: string;
    Value: string;
  end;

var
  TestPairs: array of TTestPair;

const

  TEST_COUNT = 999;

function RandomStr(aMinLen, aMaxLen: integer): string;
function DoHash(const aStr: string): integer;
procedure InitTestData;

implementation

uses
  fafafa.RBTree;

function RandomStr(aMinLen, aMaxLen: integer): string;
var
  LLen, i: integer;
begin
  LLen := RandomRange(aMinLen, aMaxLen);
  SetLength(Result, LLen);
  for i := 1 to LLen do
    Result[i] := Chr(RandomRange(Ord('a'), Ord('z')));
end;

function DoHash(const aStr: string): integer;
begin
  Result := integer(CRC32_LONG(aStr));
end;

procedure InitTestData;
var
  i: integer;
begin
  SetLength(TestPairs, TEST_COUNT);
  for i := 0 to Pred(TEST_COUNT) do
  begin
    TestPairs[i].Key :=i.ToString();// RandomStr(8, 18);
    TestPairs[i].Value := i.ToString();//RandomStr(4, 12);
  end;
end;

initialization
  Randomize;
  InitTestData;

finalization
  SetLength(TestPairs, 0);

end.
