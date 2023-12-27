unit testcase_StringPairRBTree;

{$Codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  //.
  TestDataUtils,
  fafafa.RBTree;

type

  { TTestCase_StringPairRBTree }

  TTestCase_StringPairRBTree = class(TTestCase)
  private
    function CreateDataTree: TStringPairRBTree;
  published
    procedure test_RemoveNode;
    procedure test_Count;
    procedure test_Clear;
    procedure test_Insert;
    procedure test_Find;
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

function TTestCase_StringPairRBTree.CreateDataTree: TStringPairRBTree;
var
  LPair: TTestPair;
begin
  Result := TStringPairRBTree.Create;
  for LPair in TestPairs do
    Result.Insert(LPair.Key, LPair.Value);
end;

procedure TTestCase_StringPairRBTree.test_Count;
var
  LTree: TStringPairRBTree;
begin
  LTree := CreateDataTree;
  AssertTrue(LTree.Count = Length(TestPairs));
  LTree.Free;
end;

procedure TTestCase_StringPairRBTree.test_Clear;
var
  LTree: TStringPairRBTree;
begin
  LTree := CreateDataTree;
  LTree.Clear;
  AssertTrue(LTree.Count = 0);
  LTree.Free;
end;

procedure TTestCase_StringPairRBTree.test_Insert;
var
  LTree: TStringPairRBTree;
  i: integer;
  LValue: string;
begin
  LTree := CreateDataTree;

  for i := 1 to 8 do
  begin
    LValue := RandomStr(8, 18);
    LTree.Insert(i.ToString, LValue);
  end;

  AssertTrue(LTree.Count = Length(TestPairs) + 8);
  LTree.Free;
end;

procedure TTestCase_StringPairRBTree.test_Find;
var
  LTree: TStringPairRBTree;
  LPair: TTestPair;
  LNode: PRBTreeStringPairNode;
begin
  LTree := CreateDataTree;
  for LPair in TestPairs do
  begin
    LNode := LTree.Find(LPair.Key);
    AssertTrue(LNode <> nil);
    AssertTrue(SameText(LPair.Value, LNode^.Value));
  end;
  LTree.Free;
end;

procedure TTestCase_StringPairRBTree.test_RemoveNode;
var
  LNodeX: PRBTreeStringPairNode;
  LTree: TStringPairRBTree;
  LPair: PTestPair;

  LLen: SizeInt;
  i: integer;
begin
  LTree := CreateDataTree;
  LLen := Length(TestPairs);
  for i := 0 to Pred(LLen) do
  begin
    LPair := @TestPairs[i];
    LNodeX := LTree.RemoveNode(LPair^.Key);
    AssertTrue(LNodeX <> nil);
    AssertTrue(SameText(LPair^.Value, LNodeX^.Value));
    AssertTrue(SameText(LPair^.Key, LNodeX^.Key^.Key));
    DisposeStringPairRBTreeNode(LNodeX);
  end;
  LTree.Free;
end;


procedure TTestCase_StringPairRBTree.test_Remove;
var
  LTree: TStringPairRBTree;
  LPair: TTestPair;
  LRet: string;
begin
  LTree := CreateDataTree;
  for LPair in TestPairs do
  begin
    LRet := LTree.Remove(LPair.Key);
    AssertTrue(not LRet.IsEmpty);
    AssertTrue(SameText(LRet, LPair.Value));
  end;
  LTree.Free;
end;

procedure TTestCase_StringPairRBTree.test_Delete;
var
  LTree: TStringPairRBTree;
  LNode, LNext: PRBTreeStringPairNode;
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

procedure TTestCase_StringPairRBTree.test_FindSuccessor;
var
  LTree: TStringPairRBTree;
  i: integer;
  LNode: PRBTreeStringPairNode;
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

procedure TTestCase_StringPairRBTree.test_FindPrecessor;
var
  LTree: TStringPairRBTree;
  LNode: PRBTreeStringPairNode;
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

procedure TTestCase_StringPairRBTree.test_FindLowest;
var
  LTree: TStringPairRBTree;
  LNode: PRBTreeStringPairNode;
begin
  LTree := CreateDataTree;
  LNode := LTree.FindLowest;
  AssertTrue(LTree.FindPrecessor(LNode) = nil);
  LTree.Free;
end;

procedure TTestCase_StringPairRBTree.test_FindHighest;
var
  LTree: TStringPairRBTree;
  LNode: PRBTreeStringPairNode;
begin
  LTree := CreateDataTree;
  LNode := LTree.FindHighest;
  AssertTrue(LTree.FindSuccessor(LNode) = nil);
  LTree.Free;
end;

procedure TTestCase_StringPairRBTree.test_Enumerator;
var
  LTree: TStringPairRBTree;
  i: integer;
  LEnu: TRBTreeStringPairNodeEnumerator;
  LNode: PRBTreeStringPairNode;
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

  LTree.Free;
end;

procedure TTestCase_StringPairRBTree.test_Enumerator_HightToLow;
var
  LTree: TStringPairRBTree;
  i: integer;
  LEnu: TRBTreeStringPairNodeEnumerator;
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

  RegisterTest(TTestCase_StringPairRBTree);
end.
