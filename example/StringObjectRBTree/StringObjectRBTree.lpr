program StringObjectRBTree;

{$Codepage UTF8}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  fafafa.rbtree,
  fafafa.RBTree.TreePainter,
  ExampleUtils { you can add units after this };

type

  { TStrObject }

  TStrObject = class
  public
    constructor Create(const aStr: string);
  public
    Str: string;
  end;

  { TStrObject }

  constructor TStrObject.Create(const aStr: string);
  begin
    inherited Create;
    Str := aStr;
  end;

  function GetNodeKeyValue(aNode: PRBTreeNode): TRBKeyValue;
  var
    LNode: PRBTreeStringObjectNode;
  begin
    LNode := PRBTreeStringObjectNode(aNode);
    Result.Key := LNode^.Key^.Key;
    Result.Value := TStrObject(LNode^.Value).Str + ' (' + IntToHex(LNode^.Key^.KeyHash) + ')';
  end;

  procedure RunExample;
  var
    LTree: IStringObjectRBTree;
    LNode: PRBTreeStringObjectNode;
    LPair: TTestPair;
    LTestPairLen: SizeInt;
    LEnu: TRBTreeStringObjectNodeEnumerator;
    LIT: integer;
    LFound, i: integer;
    LP: PTestPair;
    LStr: string;
    LObject: TObject;
  begin
    LTestPairLen := Length(TestPairs);
    LTree := MakeStringObjectRBTree(True);
      Write('IsEmpty?');
      if LTree.IsEmpty then
        WriteLn(' Yes')
      else
        WriteLn(' No');

      WriteLn('');
      Write('# TestPairs Count:', LTestPairLen, ' Insert....');

      for i := 0 to Pred(LTestPairLen) do
      begin
        LP := @TestPairs[i];
        LTree.Insert(LP^.Key, TStrObject.Create(LP^.Value));
      end;
      Write(' Inserts:', LTree.Count);
      if LTree.Count = LTestPairLen then
        WriteLn(' Success')
      else
        WriteLn(' Fail');

      WriteLn('');
      WriteLn('# FindLowest ');
      PrintNode(PRBTreeNode(LTree.FindLowest()), @GetNodeKeyValue);

      WriteLn('');
      WriteLn('# FindHighest');
      PrintNode(PRBTreeNode(LTree.FindHighest), @GetNodeKeyValue);


      WriteLn('');
      WriteLn('# FindSuccessor');
      WriteHeader;
      LNode := LTree.FindLowest;
      LIT := 0;
      while LNode <> nil do
      begin
        PrintNode(PRBTreeNode(LNode), @GetNodeKeyValue);
        LNode := LTree.FindSuccessor(LNode);
        Inc(LIT);
      end;
      WriteLn('iterations:', LIT);

      WriteLn('');
      WriteLn('# FindPrecessor');
      WriteHeader;
      LIT := 0;
      LNode := LTree.FindHighest;
      while LNode <> nil do
      begin
        PrintNode(PRBTreeNode(LNode), @GetNodeKeyValue);
        LNode := LTree.FindPrecessor(LNode);
        Inc(LIT);
      end;
      WriteLn('iterations:', LIT);

      WriteLn('');
      WriteLn('# Enumerator - For in');
      WriteHeader;
      LIT := 0;
      for LNode in LTree do
      begin
        PrintNode(PRBTreeNode(LNode), @GetNodeKeyValue);
        Inc(LIT);
      end;
      WriteLn('iterations:', LIT);

      WriteLn('');
      WriteLn('# Enumerator(LowToHigh)');
      LEnu := LTree.GetEnumerator;
      PrintNodeList(LEnu, @GetNodeKeyValue);
      LEnu.Free;

      WriteLn('');
      WriteLn('# Enumerator(HighToLow)');
      LEnu := LTree.GetEnumeratorHighToLow;
      PrintNodeList(LEnu, @GetNodeKeyValue);
      LEnu.Free;

      WriteLn('');
      Write('# TestPairs Count:', Length(TestPairs), ' Find...');
      LFound := 0;
      for LPair in TestPairs do
      begin
        LNode := LTree.Find(LPair.Key);
        LStr := TStrObject(LNode^.Value).Str;
        if SameText(LStr, LPair.Value) then
          Inc(LFound);
      end;
      Write(' Found:', LFound);
      if LFound = LTestPairLen then
        WriteLn(' Success')
      else
        WriteLn(' Fail');

      WriteLn('');
      Write('# RemoveNode - ', TestPairs[0].Key);
      LNode := LTree.RemoveNode(TestPairs[0].Key);
      if LTree.Find(TestPairs[0].Key) = nil then
        WriteLn('  Success')
      else
        WriteLn(' Fail');
      Write(' - ');
      PrintNode(PRBTreeNode(LNode), @GetNodeKeyValue);
      Dispose(LNode^.Key);
      LNode^.Value.Free;
      Dispose(LNode);
      LEnu := LTree.GetEnumerator;
      PrintNodeList(LEnu, @GetNodeKeyValue);
      LEnu.Free;

      WriteLn('');
      Write('# Remove - ', TestPairs[1].Key);
      LObject := LTree.Remove(TestPairs[1].Key);
      if LObject <> nil then
      begin
        LObject.Free;
        WriteLn(' Success');
      end
      else
        WriteLn(' Fail');
      LEnu := LTree.GetEnumerator;
      PrintNodeList(LEnu, @GetNodeKeyValue);
      LEnu.Free;

      WriteLn('');
      Write('# PrintTree - Root - ');
      PrintNode(PRBTreeNode(LTree.Root), @GetNodeKeyValue);
      WriteLn('Tree');
      PrintTree(PRBTreeNode(LTree.Root), '', True, @GetNodeKeyValue);

      WriteLn('');
      Write('# Clear');
      LTree.Clear;
      if LTree.Count = 0 then
        WriteLn(' Success')
      else
        WriteLn(' Fail');

      WriteLn('Bye!');
  end;

begin
  WriteLn('exeample for fafafa.RBTree IStringObjectRBTree');
  RunExample;
end.
