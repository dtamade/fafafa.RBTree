unit testcase_StringRBTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  //.
  TestDataUtils,
  fafafa.RBTree;

type

  { TTestCase_StringRBTree }

  TTestCase_StringRBTree = class(TTestCase)

  private
    function CreateDataTree: TStringRBTree;
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




{ TTestCase_StringRBTree }

function TTestCase_StringRBTree.CreateDataTree: TStringRBTree;
var
  LPair: TTestPair;
begin
  Result := TStringRBTree.Create;
  for LPair in TestPairs do
    Result.Insert(LPair.Key, pointer(LPair.Value));
end;

procedure TTestCase_StringRBTree.test_Count;
var
  LTree: TStringRBTree;
begin
  LTree := CreateDataTree;
  AssertTrue(LTree.Count = Length(TestPairs));
  LTree.Free;
end;

procedure TTestCase_StringRBTree.test_Clear;
var
  LTree: TStringRBTree;
begin
  LTree := CreateDataTree;
  LTree.Clear;
  AssertTrue(LTree.Count = 0);
  LTree.Free;
end;

procedure TTestCase_StringRBTree.test_Insert;
var
  LTree: TStringRBTree;
  i: integer;
  LValue: string;
begin
  LTree := CreateDataTree;

  for i := 1 to 8 do
  begin
    LValue := RandomStr(8, 18);
    LTree.Insert(i.ToString, Pointer(LValue));
  end;

  AssertTrue(LTree.Count = Length(TestPairs) + 8);
  LTree.Free;
end;

procedure TTestCase_StringRBTree.test_Find;
var
  LTree: TStringRBTree;
  LPair: TTestPair;
  LNode: PStringRBTreeNode;
begin
  LTree := CreateDataTree;
  for LPair in TestPairs do
  begin
    LNode := LTree.Find(LPair.Key);
    AssertTrue(LNode <> nil);
    AssertTrue(SameText(LPair.Value, string(LNode^.Value)));
  end;
  LTree.Free;
end;

procedure TTestCase_StringRBTree.test_RemoveNode;
var
  LTree: TStringRBTree;
  LPair: TTestPair;
  LNode: PStringRBTreeNode;
begin
  LTree := CreateDataTree;
  for LPair in TestPairs do
  begin
    LNode := LTree.RemoveNode(LPair.Key);
    AssertTrue(LNode <> nil);
    AssertTrue(SameText(LPair.Value, string(LNode^.Value)));
    AssertTrue(SameText(LPair.Key, LNode^.Key^.Key));
    Dispose(LNode^.Key);
    Dispose(LNode);
  end;
  LTree.Free;
end;

procedure TTestCase_StringRBTree.test_Remove;
var
  LTree: TStringRBTree;
  LPair: TTestPair;
  LRet: Pointer;
begin
  LTree := CreateDataTree;
  for LPair in TestPairs do
  begin
    LRet := LTree.Remove(LPair.Key);
    AssertTrue(LRet <> nil);
    AssertTrue(SameText(string(LRet), LPair.Value));
  end;
  LTree.Free;
end;

procedure TTestCase_StringRBTree.test_Delete;
var
  LTree: TStringRBTree;
  LNode, LNext: PStringRBTreeNode;
begin
  LTree := CreateDataTree;
  LNode := LTree.FindLowest;
  while LNode <> nil do
  begin
    LNext := LTree.FindSuccessor(LNode);
    LTree.Delete(LNode);
    LNode := LNext;
  end;
  AssertTrue(LTree.Count = 0);
  LTree.Free;
end;

procedure TTestCase_StringRBTree.test_FindSuccessor;
var
  LTree: TStringRBTree;
  i: integer;
  LNode: PStringRBTreeNode;
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
  LTree.Free;
end;

procedure TTestCase_StringRBTree.test_FindPrecessor;
var
  LTree: TStringRBTree;
  LNode: PStringRBTreeNode;
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
  LTree.Free;
end;

procedure TTestCase_StringRBTree.test_FindLowest;
var
  LTree: TStringRBTree;
  LNode: PStringRBTreeNode;
begin
  LTree := CreateDataTree;
  LNode := LTree.FindLowest;
  AssertTrue(LTree.FindPrecessor(LNode) = nil);
  LTree.Free;
end;

procedure TTestCase_StringRBTree.test_FindHighest;
var
  LTree: TStringRBTree;
  LNode: PStringRBTreeNode;
begin
  LTree := CreateDataTree;
  LNode := LTree.FindHighest;
  AssertTrue(LTree.FindSuccessor(LNode) = nil);
  LTree.Free;
end;

procedure TTestCase_StringRBTree.test_Enumerator;
var
  LTree: TStringRBTree;
  i: integer;
  LEnu: TStringRBTreeNodeEnumerator;
  LNode: PStringRBTreeNode;
begin
  LTree := CreateDataTree;
  i := 0;
  for LNode in LTree do
    Inc(i);
  AssertTrue('i=' + i.ToString + ' LTree.Count=' + LTree.Count.ToString, i = LTree.Count);

  i := 0;
  LEnu := LTree.GetEnumerator;
  while LEnu.MoveNext do
    Inc(i);
  AssertTrue(i = LTree.Count);
  LEnu.Free;
  LTree.Free;
end;

procedure TTestCase_StringRBTree.test_Enumerator_HightToLow;
var
  LTree: TStringRBTree;
  i: integer;
  LEnu: TStringRBTreeNodeEnumerator;
begin
  LTree := CreateDataTree;
  i := 0;
  LEnu := LTree.GetEnumeratorHighToLow;
  while LEnu.MoveNext do
    Inc(i);

  LEnu.Free;
  LTree.Free;
end;

initialization

  RegisterTest(TTestCase_StringRBTree);
end.
