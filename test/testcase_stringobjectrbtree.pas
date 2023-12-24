unit testcase_StringObjectRBTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  //.
  TestDataUtils,
  fafafa.RBTree;

type

  { TTestObj }

  TTestObj = class
  public
    constructor Create(const aStr: string);
  public
    Str: string;
  end;

  { TTestCase_StringObjectRBTree }

  TTestCase_StringObjectRBTree = class(TTestCase)
  private
    function CreateDataTree: TStringObjectRBTree;
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

{ TTestObj }

constructor TTestObj.Create(const aStr: string);
begin
  inherited Create;
  Str := aStr;
end;

function TTestCase_StringObjectRBTree.CreateDataTree: TStringObjectRBTree;
var
  LPair: TTestPair;
begin
  Result := TStringObjectRBTree.Create;
  for LPair in TestPairs do
    Result.Insert(LPair.Key, TTestObj.Create(LPair.Value));
end;

procedure TTestCase_StringObjectRBTree.test_Count;
var
  LTree: TStringObjectRBTree;
begin
  LTree := CreateDataTree;
  AssertTrue(LTree.Count = Length(TestPairs));
  LTree.Free;
end;

procedure TTestCase_StringObjectRBTree.test_Clear;
var
  LTree: TStringObjectRBTree;
begin
  LTree := CreateDataTree;
  LTree.Clear;
  AssertTrue(LTree.Count = 0);
  LTree.Free;
end;

procedure TTestCase_StringObjectRBTree.test_Insert;
var
  LTree: TStringObjectRBTree;
  i: integer;
  LValue: string;
begin
  LTree := CreateDataTree;

  for i := 1 to 8 do
  begin
    LValue := RandomStr(8, 18);
    LTree.Insert(i.ToString, TTestObj.Create(LValue));
  end;

  AssertTrue(LTree.Count = Length(TestPairs) + 8);
  LTree.Free;
end;

procedure TTestCase_StringObjectRBTree.test_Find;
var
  LTree: TStringObjectRBTree;
  LPair: TTestPair;
  LNode: PStringObjectRBTreeNode;
begin
  LTree := CreateDataTree;
  for LPair in TestPairs do
  begin
    LNode := LTree.Find(LPair.Key);
    AssertTrue(LNode <> nil);
    AssertTrue(SameText(LPair.Value, TTestObj(LNode^.Value).Str));
  end;
  LTree.Free;
end;

procedure TTestCase_StringObjectRBTree.test_RemoveNode;
var
  LTree: TStringObjectRBTree;
  LPair: TTestPair;
  LNode: PStringObjectRBTreeNode;
begin
  LTree := CreateDataTree;
  for LPair in TestPairs do
  begin
    LNode := LTree.RemoveNode(LPair.Key);
    AssertTrue(LNode <> nil);
    AssertTrue(SameText(LPair.Value, TTestObj(LNode^.Value).Str));
    AssertTrue(SameText(LPair.Key, LNode^.Key^.Key));
    LNode^.Value.Free;
    DisposeStringObjectRBTreeNode(LNode);
  end;
  LTree.Free;
end;

procedure TTestCase_StringObjectRBTree.test_Remove;
var
  LTree: TStringObjectRBTree;
  LPair: TTestPair;
  LRet: TTestObj;
begin
  LTree := CreateDataTree;
  for LPair in TestPairs do
  begin
    LRet := TTestObj(LTree.Remove(LPair.Key));
    AssertTrue(LRet <> nil);
    AssertTrue(SameText(LRet.Str, LPair.Value));
    LRet.Free;
  end;
  LTree.Free;
end;

procedure TTestCase_StringObjectRBTree.test_Delete;
var
  LTree: TStringObjectRBTree;
  LNode, LNext: PStringObjectRBTreeNode;
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

procedure TTestCase_StringObjectRBTree.test_FindSuccessor;
var
  LTree: TStringObjectRBTree;
  i: integer;
  LNode: PStringObjectRBTreeNode;
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

procedure TTestCase_StringObjectRBTree.test_FindPrecessor;
var
  LTree: TStringObjectRBTree;
  LNode: PStringObjectRBTreeNode;
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

procedure TTestCase_StringObjectRBTree.test_FindLowest;
var
  LTree: TStringObjectRBTree;
  LNode: PStringObjectRBTreeNode;
begin
  LTree := CreateDataTree;
  LNode := LTree.FindLowest;
  AssertTrue(LTree.FindPrecessor(LNode) = nil);
  LTree.Free;
end;

procedure TTestCase_StringObjectRBTree.test_FindHighest;
var
  LTree: TStringObjectRBTree;
  LNode: PStringObjectRBTreeNode;
begin
  LTree := CreateDataTree;
  LNode := LTree.FindHighest;
  AssertTrue(LTree.FindSuccessor(LNode) = nil);
  LTree.Free;
end;

procedure TTestCase_StringObjectRBTree.test_Enumerator;
var
  LTree: TStringObjectRBTree;
  i: integer;
  LEnu: TStringObjectRBTreeNodeEnumerator;
  LNode: PStringObjectRBTreeNode;
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

procedure TTestCase_StringObjectRBTree.test_Enumerator_HightToLow;
var
  LTree: TStringObjectRBTree;
  i: integer;
  LEnu: TStringObjectRBTreeNodeEnumerator;
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

  RegisterTest(TTestCase_StringObjectRBTree);
end.
