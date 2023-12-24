program RBTree;

{$Codepage UTF8}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  fafafa.RBTree,
  fafafa.RBTree.TreePainter,
  ExampleUtils { you can add units after this };

type

  PNodeKey = ^TNodeKey;

  TNodeKey = record
    Hash: integer;
    Str: string;
  end;

  PNodeData = ^TNodeData;

  TNodeData = record
    Key: TNodeKey;
    Value: string;
  end;


  function DoCompareCB(aKey1, aKey2: Pointer): PtrInt; inline;
  var
    LLen1, LLen2: SizeInt;
  begin
    if PNodeKey(aKey1)^.Hash <> PNodeKey(aKey2)^.Hash then
    begin
      if PNodeKey(aKey1)^.Hash > PNodeKey(aKey2)^.Hash then
        Result := 1
      else
        Result := -1;
    end
    else
    begin
      LLen1 := Length(PNodeKey(aKey1)^.Str);
      LLen2 := Length(PNodeKey(aKey2)^.Str);
      if LLen1 <> LLen2 then
      begin
        if LLen1 > LLen2 then
          Result := 1
        else
          Result := -1;
      end
      else
      begin
        Result := strcomp(PChar(PNodeKey(aKey1)^.Str), PChar(PNodeKey(aKey2)^.Str));
      end;
    end;
  end;

  function GetNodeKeyValue(aNode: PRBTreeNode): TRBKeyValue;
  var
    LValue: PNodeData;
  begin
    LValue := PNodeData(aNode^.Value);
    Result.Key := LValue^.Key.Str;
    Result.Value := LValue^.Value + ' (' + IntToHex(LValue^.Key.Hash) + ')';
  end;

  procedure RunExample;
  var
    LIT: integer;
    LTestPairLen: SizeInt;
    LTree: IRBTree;
    LPair: TTestPair;
    LNode: PRBTreeNode;
    LFound, i: integer;
    LP: PTestPair;
    LStr: string;

    LValue: PNodeData;
    LKeyTmp: TNodeKey;
    LEnu: TRBTreeNodeEnumerator;
  begin
    LTestPairLen := Length(TestPairs);
    LTree := MakeRBTree(@DoCompareCB);

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
      New(LValue);
      LValue^.Value := LP^.Value;
      LValue^.Key.Hash := Hash(LP^.Key);
      LValue^.Key.Str := LP^.Key;
      LTree.Insert(@(LValue^.Key), LValue);
    end;
    Write(' Inserts:', LTree.Count);
    if LTree.Count = LTestPairLen then
      WriteLn(' Success')
    else
      WriteLn(' Fail');

    WriteLn('');
    WriteLn('# FindLowest ');
    PrintNode(LTree.FindLowest(), @GetNodeKeyValue);

    WriteLn('');
    WriteLn('# FindHighest');
    PrintNode(LTree.FindHighest(), @GetNodeKeyValue);

    WriteLn('');
    WriteLn('# FindSuccessor');
    WriteHeader;
    LNode := LTree.FindLowest;
    LIT := 0;
    while LNode <> nil do
    begin
      PrintNode(LNode, @GetNodeKeyValue);
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
      PrintNode(LNode, @GetNodeKeyValue);
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
      PrintNode(LNode, @GetNodeKeyValue);
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
      LKeyTmp.Hash := Hash(LPair.Key);
      LKeyTmp.Str := LPair.Key;
      LNode := LTree.Find(@LKeyTmp);
      LStr := PNodeData(LNode^.Value)^.Value;
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
    LKeyTmp.Hash := Hash(TestPairs[0].Key);
    LKeyTmp.Str := TestPairs[0].Key;
    LNode := LTree.RemoveNode(@LKeyTmp);
    if LTree.Find(@LKeyTmp) = nil then
      WriteLn('  Success')
    else
      WriteLn(' Fail');
    Write(' - ');
    PrintNode(LNode, @GetNodeKeyValue);
    Dispose(PNodeData(LNode^.Value));
    Dispose(LNode);
    LEnu := LTree.GetEnumerator;
    PrintNodeList(LEnu, @GetNodeKeyValue);
    LEnu.Free;

    WriteLn('');
    Write('# Remove - ', TestPairs[1].Key);
    LKeyTmp.Hash := Hash(TestPairs[1].Key);
    LKeyTmp.Str := TestPairs[1].Key;
    LValue := LTree.Remove(@LKeyTmp);
    if LValue <> nil then
      WriteLn(' Success')
    else
      WriteLn(' Fail');
    Dispose(LValue);
    LEnu := LTree.GetEnumerator;
    PrintNodeList(LEnu, @GetNodeKeyValue);
    LEnu.Free;

    WriteLn('');
    Write('# PrintTree - Root - ');
    PrintNode(LTree.Root, @GetNodeKeyValue);
    WriteLn('Tree');
    PrintTree(LTree.Root, '', True, @GetNodeKeyValue);

    WriteLn('');
    WriteLn('Cleanup...');
    for LNode in LTree do
      Dispose(PNodeData(LNode^.Value));

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
  WriteLn('exeample for fafafa.RBTree IRBTree');
  RunExample;
end.
