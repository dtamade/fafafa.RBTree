unit testcase_RBTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  //.
  fafafa.RBTree,
  TestDataUtils;

type

  PTestData = ^TTestData;

  TTestData = record
    Key: TStringKey;
    Value: Pointer;
  end;

  { TTestCase_RBTree }

  TTestCase_RBTree = class(TTestCase)
  private
    function CreateDataTree: IRBTree;
    procedure CleanupTree(aTree: IRBTree);
  published
    procedure test_Count;
    procedure test_Clear;
    procedure test_Insert;
    procedure test_Find;
    procedure test_RemoveNode;
    procedure test_Remove;
    procedure test_Delete;
    procedure test_FindSuccessor;
    procedure test_FindPrecessor;
    procedure test_FindLowest;
    procedure test_FindHighest;
    procedure test_Enumerator;
    procedure test_Enumerator_HightToLow;

  end;

implementation

function DoKeyCompare(aKey1, aKey2: Pointer): PtrInt;
var
  LKey1, LKey2: PStringKey;
  LLen1, LLen2: SizeInt;
begin
  LKey1 := PStringKey(aKey1);
  LKey2 := PStringKey(aKey2);
  if LKey1^.KeyHash <> LKey2^.KeyHash then
  begin
    if LKey1^.KeyHash > LKey2^.KeyHash then
      Result := 1
    else
      Result := -1;
  end
  else
  begin
    LLen1 := Length(LKey1^.Key);
    LLen2 := Length(LKey2^.Key);
    if LLen1 <> LLen2 then
    begin
      if LLen1 > LLen2 then
        Result := 1
      else
        Result := -1;
    end
    else
    begin
      Result := strcomp(PChar(LKey1^.Key), PChar(LKey2^.Key));
    end;
  end;

end;

function TTestCase_RBTree.CreateDataTree: IRBTree;
var
  LPair: TTestPair;
  LData: PTestData;
begin
  Result :=MakeRBTree(@DoKeyCompare);
  for LPair in TestPairs do
  begin
    New(LData);
    LData^.Key.Key := LPair.Key;
    LData^.Key.KeyHash := DoHash(LPair.Key);
    LData^.Value := Pointer(LPair.Value);
    Result.Insert(@LData^.Key, LData);
  end;
end;

procedure TTestCase_RBTree.CleanupTree(aTree: IRBTree);
var
  LNode: PRBTreeNode;
begin
  for LNode in aTree do
    Dispose(PTestData(LNode^.Value));
end;

procedure TTestCase_RBTree.test_Count;
var
  LTree: IRBTree;
begin
  LTree := CreateDataTree;
  AssertTrue(LTree.Count = Length(TestPairs));
  CleanupTree(LTree);
end;

procedure TTestCase_RBTree.test_Clear;
var
  LTree: IRBTree;
begin
  LTree := CreateDataTree;
  CleanupTree(LTree);
  LTree.Clear;
  AssertTrue(LTree.Count = 0);
end;

procedure TTestCase_RBTree.test_Insert;
var
  LTree: IRBTree;
  i: integer;
  LData: PTestData;
begin
  LTree := CreateDataTree;

  for i := 1 to 8 do
  begin
    New(LData);
    LData^.Key.Key := i.ToString;
    LData^.Key.KeyHash := DoHash(LData^.Key.Key);
    LData^.Value := Pointer(RandomStr(8, 18));
    LTree.Insert(@LData^.Key, LData);
  end;

  AssertTrue(LTree.Count = Length(TestPairs) + 8);
  CleanupTree(LTree);
end;

procedure TTestCase_RBTree.test_Find;
var
  LTree: IRBTree;
  LPair: TTestPair;
  LKey: TStringKey;
  LNode: PRBTreeNode;
begin
  LTree := CreateDataTree;
  for LPair in TestPairs do
  begin
    LKey.Key := LPair.Key;
    LKey.KeyHash := DoHash(LPair.Key);

    LNode := LTree.Find(@LKey);
    AssertTrue(LNode <> nil);
    AssertTrue(SameText(LPair.Value, string(PTestData(LNode^.Value)^.Value)));
  end;
  CleanupTree(LTree);
end;

procedure TTestCase_RBTree.test_RemoveNode;
var
  LTree: IRBTree;
  LPair: TTestPair;
  LKey: TStringKey;
  LNode: PRBTreeNode;
  LData: PTestData;
begin
  LTree := CreateDataTree;
  for LPair in TestPairs do
  begin
    LKey.Key := LPair.Key;
    LKey.KeyHash := DoHash(LPair.Key);

    LNode := LTree.RemoveNode(@LKey);
    AssertTrue(LNode <> nil);
    LData := PTestData(LNode^.Value);
    AssertTrue(SameText(LPair.Value, string(LData^.Value)));
    AssertTrue(SameText(LPair.Key, LData^.Key.Key));
    Dispose(LData);
    Dispose(LNode);
  end;
  CleanupTree(LTree);
end;

procedure TTestCase_RBTree.test_Remove;
var
  LTree: IRBTree;
  LPair: TTestPair;
  LKey: TStringKey;
  LData: PTestData;
begin
  ;
  LTree := CreateDataTree;
  for LPair in TestPairs do
  begin
    LKey.Key := LPair.Key;
    LKey.KeyHash := DoHash(LKey.Key);
    LData := PTestData(LTree.Remove(@LKey));
    AssertTrue(LData <> nil);
    AssertTrue(SameText(LData^.Key.Key, LPair.Key));
    AssertTrue(SameText(string(LData^.Value), LPair.Value));
    Dispose(LData);
  end;
  CleanupTree(LTree);
end;

procedure TTestCase_RBTree.test_Delete;
var
  LTree: IRBTree;
  LNode, LNext: PRBTreeNode;
begin
  LTree := CreateDataTree;
  CleanupTree(LTree);
  LNode := LTree.FindLowest;
  while LNode <> nil do
  begin
    LNext := LTree.FindSuccessor(LNode);
    LTree.Delete(LNode);
    LNode := LNext;
  end;

  AssertTrue(LTree.Count = 0);
end;

procedure TTestCase_RBTree.test_FindSuccessor;
var
  LTree: IRBTree;
  i: integer;
  LNode: PRBTreeNode;
begin
  LTree := CreateDataTree;
  LNode := LTree.FindLowest;
  i := 0;
  while LNode <> nil do
  begin
    Inc(i);
    LNode := LTree.FindSuccessor(LNode);
  end;
  AssertTrue(i = LTree.Count);
  CleanupTree(LTree);
end;

procedure TTestCase_RBTree.test_FindPrecessor;
var
  LTree: IRBTree;
  LNode: PRBTreeNode;
  i: integer;
begin
  LTree := CreateDataTree;
  LNode := LTree.FindHighest;
  i := 0;
  while LNode <> nil do
  begin
    Inc(i);
    LNode := LTree.FindPrecessor(LNode);
  end;
  AssertTrue(i = LTree.Count);
  CleanupTree(LTree);
end;

procedure TTestCase_RBTree.test_FindLowest;
var
  LTree: IRBTree;
  LNode: PRBTreeNode;
begin
  LTree := CreateDataTree;
  LNode := LTree.FindLowest;
  AssertTrue(LTree.FindPrecessor(LNode) = nil);
  CleanupTree(LTree);
end;

procedure TTestCase_RBTree.test_FindHighest;
var
  LTree: IRBTree;
  LNode: PRBTreeNode;
begin
  LTree := CreateDataTree;
  LNode := LTree.FindHighest;
  AssertTrue(LTree.FindSuccessor(LNode) = nil);
  CleanupTree(LTree);
end;

procedure TTestCase_RBTree.test_Enumerator;
var
  LTree: IRBTree;
  i: integer;
  LEnu: TRBTreeNodeEnumerator;
  LNode: PRBTreeNode;
begin
  LTree := CreateDataTree;
  i := 0;
  for LNode in LTree do
    Inc(i);
  AssertTrue(i = LTree.Count);

  i := 0;
  LEnu := LTree.GetEnumerator;
  while LEnu.MoveNext do
    Inc(i);
  AssertTrue(i = LTree.Count);
  LEnu.Free;
  CleanupTree(LTree);
end;

procedure TTestCase_RBTree.test_Enumerator_HightToLow;
var
  LTree: IRBTree;
  i: integer;
  LEnu: TRBTreeNodeEnumerator;
begin
  LTree := CreateDataTree;
  i := 0;
  LEnu := LTree.GetEnumeratorHighToLow;
  while LEnu.MoveNext do
    Inc(i);

  LEnu.Free;
  CleanupTree(LTree);
end;



initialization

  RegisterTest(TTestCase_RBTree);
end.
