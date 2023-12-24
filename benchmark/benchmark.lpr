program benchmark;

{$Codepage UTF8}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  Math,
  AVL_Tree,
  fafafa.rbtree { you can add units after this };

type

  PTreeKey = ^TTreeKey;

  TTreeKey = record
    KeyHash: integer;
    Str: string;
  end;

  PAVLData = ^TAVLData;

  TAVLData = record
    KeyHash: integer;
    Str: string;
    Data: Pointer;
  end;

  { TStrObj }

  TStrObj = class
  public
    constructor Create(const aStr: string);
  public
    Str: string;
  end;

var
  TESTDATA: array of string;

  function RandomStr(aCount: integer): string;
  var
    i: integer;
  begin
    SetLength(Result, aCount);
    for i := 1 to aCount do
      Result[i] := chr(RandomRange(Ord('a'), Ord('z')));
  end;

  procedure CreateTestData(aCount: integer);
  var
    i: integer;
  begin
    SetLength(TESTDATA, aCount);
    for i := 0 to Pred(aCount) do
      TESTDATA[i] := RandomStr(24);
  end;

  function DoAVLCompare(Item1, Item2: Pointer): integer; inline;
  var
    LP1, LP2: PAVLData;
    LLen1, LLen2: integer;
  begin
    LP1 := PAVLData(Item1);
    LP2 := PAVLData(Item2);

    if LP1^.KeyHash <> LP2^.KeyHash then
    begin
      if LP1^.KeyHash > LP2^.KeyHash then
        Result := 1
      else
        Result := -1;
    end
    else
    begin
      LLen1 := Length(LP1^.Str);
      LLen2 := Length(LP2^.Str);
      if LLen1 <> LLen2 then
      begin
        if LLen1 > LLen2 then
          Result := 1
        else
          Result := -1;
      end
      else
      begin
        Result := strcomp(PChar(LP1^.Str), PChar(LP2^.Str));
      end;
    end;
  end;

  function DoRBTreeCompare(aKey1, aKey2: PTreeKey): PtrInt; inline;
  var
    LLen1, LLen2: integer;
  begin
    if aKey1^.KeyHash <> aKey2^.KeyHash then
    begin
      if aKey1^.KeyHash > aKey2^.KeyHash then
        Result := 1
      else
        Result := -1;
    end
    else
    begin
      LLen1 := Length(aKey1^.Str);
      LLen2 := Length(aKey2^.Str);
      if LLen1 <> LLen2 then
      begin
        if LLen1 > LLen2 then
          Result := 1
        else
          Result := -1;
      end
      else
      begin
        Result := strcomp(PChar(aKey1^.Str), PChar(aKey2^.Str));
      end;
    end;
  end;

  function BenchmarkHash(const aStr: string): integer;
  begin
    Result := integer(CRC32_LONG(aStr));
  end;

  procedure Benchmark(aCount: integer);
  var
    LAVL: TAVLTree;
    LRBTree: TRBTree;
    LStringRBTree: IStringRBTree;
    LStringObjectRBTree: IStringObjectRBTree;
    LStringPairRBTree: IStringPairRBTree;

    LTick: QWord;
    i: integer;
    LPAVL: PAVLData;
    LPRBKey: PTreeKey;
    LTmp: string;
    LRBNode: PRBTreeNode;
    LAVLNode: TAVLTreeNode;
    LTmpKey: TTreeKey;
    LStringRBNode: PStringRBTreeNode;
    LStringObjectRBNode: PStringObjectRBTreeNode;
    LStringPairRBNode: PStringPairRBTreeNode;
  begin
    WriteLn('# Insert');

    Write('AVLTree ... ');
    LAVL := TAVLTree.Create(@DoAVLCompare);
    LTick := GetTickCount64;
    for i := 0 to Pred(aCount) do
    begin
      New(LPAVL);
      LTmp := TESTDATA[i];
      LPAVL^.KeyHash := BenchmarkHash(LTmp);
      LPAVL^.Str := LTmp;
      LPAVL^.Data := pointer(LTmp);
      LAVL.Add(LPAVL);
    end;
    LTick := GetTickCount64 - LTick;
    WriteLn(LTick, ' ms');

    Write('RBTree ... ');
    LRBTree := TRBTree.Create(TRBTreeCompareCB(@DoRBTreeCompare));
    LTick := GetTickCount64;
    for i := 0 to Pred(aCount) do
    begin
      LTmp := TESTDATA[i];
      New(LPRBKey);
      LPRBKey^.KeyHash := BenchmarkHash(LTmp);
      LPRBKey^.Str := LTmp;
      LRBTree.Insert(LPRBKey, Pointer(LTmp));
    end;
    LTick := GetTickCount64 - LTick;
    WriteLn(LTick, ' ms');

    Write('StringRBTree ... ');
    LStringRBTree := MakeStringRBTree;
    LTick := GetTickCount64;
    for i := 0 to Pred(aCount) do
    begin
      LTmp := TESTDATA[i];
      LStringRBTree.Insert(LTmp, Pointer(LTmp));
    end;
    LTick := GetTickCount64 - LTick;
    WriteLn(LTick, ' ms');

    Write('StringObjectRBTree ... ');
    LStringObjectRBTree := MakeStringObjectRBTree();
    LTick := GetTickCount64;
    for i := 0 to Pred(aCount) do
    begin
      LTmp := TESTDATA[i];
      LStringObjectRBTree.Insert(LTmp, TStrObj.Create(LTmp));
    end;
    LTick := GetTickCount64 - LTick;
    WriteLn(LTick, ' ms');

    Write('StringPairRBTree ... ');
    LStringPairRBTree := MakeStringPairRBTree();
    LTick := GetTickCount64;
    for i := 0 to Pred(aCount) do
    begin
      LTmp := TESTDATA[I];
      LStringPairRBTree.Insert(LTmp, LTmp);
    end;
    LTick := GetTickCount64 - LTick;
    WriteLn(LTick, ' ms');

    WriteLn('');
    WriteLn('# Find');

    Write('AVLTree ... ');
    LTick := GetTickCount64;
    for i := 0 to Pred(aCount) do
    begin
      LTmpKey.Str := TESTDATA[i];
      LTmpKey.KeyHash := BenchmarkHash(LTmpKey.Str);
      LAVLNode := LAVL.Find(@LTmpKey);
    end;
    WriteLn(GetTickCount64 - LTick, ' ms');

    Write('RBTree ... ');
    LTick := GetTickCount64;
    for i := 0 to Pred(aCount) do
    begin
      LTmpKey.Str := TESTDATA[i];
      LTmpKey.KeyHash := BenchmarkHash(LTmpKey.Str);
      LRBNode := LRBTree.Find(@LTmpKey);
    end;
    WriteLn(GetTickCount64 - LTick, ' ms');

    Write('StringRBTree ... ');
    LTick := GetTickCount64;
    for i := 0 to Pred(aCount) do
      LStringRBNode := LStringRBTree.Find(TESTDATA[i]);
    WriteLn(GetTickCount64 - LTick, ' ms');

    Write('StringObjectRBTree ... ');
    LTick := GetTickCount64;
    for i := 0 to Pred(aCount) do
      LStringObjectRBNode := LStringObjectRBTree.Find(TESTDATA[i]);
    WriteLn(GetTickCount64 - LTick, ' ms');

    Write('StringPairRBTree ... ');
    LTick := GetTickCount64;
    for i := 0 to Pred(aCount) do
      LStringPairRBNode := LStringPairRBTree.Find(TESTDATA[i]);
    WriteLn(GetTickCount64 - LTick, ' ms');


    { Cleanup }
    for LRBNode in LRBTree do
      Dispose(PTreeKey(LRBNode^.Key));

    for LAVLNode in LAVL do
      Dispose(PAVLData(LAVLNode.Data));

    LAVL.Free;
    LRBTree.Free;

  end;

  { TStrObj }

  constructor TStrObj.Create(const aStr: string);
  begin
    inherited Create;
    Str := aStr;
  end;

const
  TEST_COUNT = 888888;


begin
  Write('Create BenchmarkData...');
  CreateTestData(TEST_COUNT);
  WriteLn(' Success');

  Benchmark(TEST_COUNT);
  SetLength(TESTDATA, 0);
  WriteLn('bye.');
end.
