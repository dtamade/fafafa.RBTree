unit fafafa.RBTree.TreePainter;

{$Codepage UTF8}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  //.
  fafafa.RBTree;

type

  PRBKeyValue = ^TRBKeyValue;

  TRBKeyValue = record
    Key, Value: string;
  end;

  TGetNodeTextCB = function(aNode: PRBTreeNode): TRBKeyValue;

procedure WriteHeader;
procedure PrintNode(aNode: PRBTreeNode; aGetNodeKeyValueCB: TGetNodeTextCB);
procedure PrintNodeList(aEnumerator: TRBTreeNodeEnumerator; aGetNodeKeyValueCB: TGetNodeTextCB);
procedure PrintTree(aNode: PRBTreeNode; aIndent: string; aIsLast: boolean; aGetNodeKeyValueCB: TGetNodeTextCB);

implementation

const
  IndentStep: string = ' │  '; // 缩进步长
  HEADER_KEY_MAXLEN = 18;

procedure WriteHeader;
begin
  Write(Format('%-' + HEADER_KEY_MAXLEN.ToString + 's', ['Key']));
  WriteLn('Value');
end;

procedure PrintNode(aNode: PRBTreeNode; aGetNodeKeyValueCB: TGetNodeTextCB);
var
  LKeyValue: TRBKeyValue;
begin
  LKeyValue := aGetNodeKeyValueCB(aNode);
  Write(Format('%-' + HEADER_KEY_MAXLEN.ToString + 's', [LKeyValue.Key]));
  WriteLn(LKeyValue.Value);
end;

procedure PrintNodeList(aEnumerator: TRBTreeNodeEnumerator; aGetNodeKeyValueCB: TGetNodeTextCB);
var
  LIT: integer;
  LKeyValue: TRBKeyValue;
begin
  WriteHeader;
  LIT := 0;
  while aEnumerator.MoveNext do
  begin
    PrintNode(aEnumerator.Current, aGetNodeKeyValueCB);
    Inc(LIT);
  end;
  WriteLn('iterations:', LIT);
end;

procedure PrintTree(aNode: PRBTreeNode; aIndent: string; aIsLast: boolean; aGetNodeKeyValueCB: TGetNodeTextCB);
begin
  if aNode = nil then
    Exit;

  if aIndent <> '' then
    Write(aIndent);

  if aIsLast then
  begin
    Write(' └─');
    aIndent := aIndent + '   ';
  end
  else
  begin
    Write(' ├─');
    aIndent := aIndent + IndentStep;
  end;

  if aNode^.Value <> nil then
  begin
    Write('+');
    PrintNode(aNode, aGetNodeKeyValueCB);
  end
  else
  begin
    WriteLn('NULL');
  end;

  PrintTree(aNode^.Left, aIndent, aNode^.Right = nil, aGetNodeKeyValueCB);
  PrintTree(aNode^.Right, aIndent, True, aGetNodeKeyValueCB);
end;

end.
