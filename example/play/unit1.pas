unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  laz.VirtualTrees, VirtualTrees,
  //.
  ExampleUtils,
  fafafa.RBTree;

type
  PPRBTreeStringPairNode = ^PRBTreeStringPairNode;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    edtKey: TEdit;
    edtValue: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    vt: TVirtualStringTree;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);

    procedure vtGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure vtPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

  private
    FTree: IStringPairRBTree;
    FLastNode: PRBTreeStringPairNode;

    function GetSelectedNode: PRBTreeStringPairNode;
    procedure GoNode(aNode: PRBTreeStringPairNode);
  public
    procedure DoUpdate;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTree := MakeStringPairRBTree();
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FLastNode := FTree.Insert(edtKey.Text, edtValue.Text);
  DoUpdate;
  GoNode(FLastNode);
end;

procedure TForm1.Button10Click(Sender: TObject);
var
  LNode, LLowNode: PRBTreeStringPairNode;
begin
  LNode := GetSelectedNode;
  if LNode = nil then
    LNode := FTree.Root;
  LLowNode := FTree.FindLowest(LNode);
  if LLowNode <> nil then
    GoNode(LLowNode);
end;

procedure TForm1.Button11Click(Sender: TObject);
var
  LNode, LHiNode: PRBTreeStringPairNode;
begin
  LNode := GetSelectedNode;
  if LNode = nil then
    LNode := FTree.Root;
  LHiNode := FTree.FindHighest(LNode);
  if LHiNode <> nil then
    GoNode(LHiNode);
end;

procedure TForm1.Button12Click(Sender: TObject);
var
  LNode: PRBTreeStringPairNode;
begin
  LNode := FTree.Root;
  if LNode <> nil then
    GoNode(LNode);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  LKey: string;
  LNode: PRBTreeStringPairNode;
begin
  LKey := edtKey.Text;
  if not LKey.IsEmpty then
  begin
    LNode := FTree.Find(LKey);

    if LNode <> nil then
      GoNode(LNode);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  LStr: string;
begin
  LStr := FTree.Remove(edtKey.Text);
  ShowMessage(LStr);
  DoUpdate;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FTree.Clear;
  DoUpdate;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  LPair: TTestPair;
begin
  FTree.Clear;
  for LPair in TestPairs do
    FLastNode := FTree.Insert(LPair.Key, LPair.Value);

  DoUpdate;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  LNode: PRBTreeStringPairNode;
begin
  LNode := FTree.RemoveNode(edtKey.Text);
  if LNode <> nil then
  begin
    ShowMessage(LNode^.Key^.Key + ' : ' + LNode^.Value);
    DisposeStringPairRBTreeNode(LNode);
    DoUpdate;
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  LNode: PRBTreeStringPairNode;
begin
  LNode := FTree.Find(edtKey.Text);
  if LNode <> nil then
  begin
    FTree.Delete(LNode);
    DoUpdate;
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  LNode, LSucNode: PRBTreeStringPairNode;
begin
  LNode := GetSelectedNode;
  if LNode = nil then
    LNode := FTree.Root;

  LSucNode := FTree.FindPrecessor(LNode);
  if LSucNode <> nil then
    GoNode(LSucNode);
end;

procedure TForm1.Button9Click(Sender: TObject);
var
  LNode, LPreNode: PRBTreeStringPairNode;
begin
  LNode := GetSelectedNode;
  if LNode <> nil then
  begin
    LPreNode := FTree.FindPrecessor(LNode);
    if LPreNode <> nil then
      GoNode(LPreNode);
  end;
end;

procedure TForm1.vtGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  LData: PPRBTreeStringPairNode;
begin
  LData := Sender.GetNodeData(Node);
  if LData^ <> nil then
  begin
    if LData^ <> PRBTreeStringPairNode(FTree.Sentinel) then
    begin
      case Column of
        2: CellText := IntToHex(LData^^.Key^.KeyHash);
        0: CellText := LData^^.Key^.Key;
        1: CellText := LData^^.Value;
      end;
    end
    else
      CellText := 'NULL';
  end;
end;

procedure TForm1.vtNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
var
  LData: PPRBTreeStringPairNode;
begin
  if HitInfo.HitNode <> nil then
  begin
    LData := Sender.GetNodeData(HitInfo.HitNode);
    if LData^ <> nil then
    begin
      if LData^ <> PRBTreeStringPairNode(FTree.Sentinel) then
      begin
        edtKey.Text := LData^^.Key^.Key;
        edtValue.Text := LData^^.Value;
      end;
    end;
  end;
end;

procedure TForm1.vtPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  LData: PPRBTreeStringPairNode;
begin
  if not (vsSelected in Node^.States) then
  begin
    LData := Sender.GetNodeData(Node);
    if LData^ <> nil then
    begin
      if LData^ = PRBTreeStringPairNode(FTree.Sentinel) then
      begin
        TargetCanvas.Font.Color := clGray;
      end
      else
      begin
        if LData^ = FLastNode then
          TargetCanvas.Font.Color := clGreen;
      end;
    end;
  end;

end;

function TForm1.GetSelectedNode: PRBTreeStringPairNode;
var
  LVTNode: PVirtualNode;
  LData: PPRBTreeStringPairNode;
begin
  Result := nil;
  LVTNode := vt.GetFirstSelected();
  if LVTNode <> nil then
  begin
    LData := vt.GetNodeData(LVTNode);
    if LData^ <> nil then
      Result := LData^;
  end;
end;

procedure TForm1.GoNode(aNode: PRBTreeStringPairNode);
var
  LEnu: TVTVirtualNodeEnumerator;
  LVTNode: PVirtualNode;
  LData: PPRBTreeStringPairNode;
begin
  LEnu := vt.Nodes().GetEnumerator;
  while LEnu.MoveNext do
  begin
    LVTNode := LEnu.Current;
    LData := vt.GetNodeData(LVTNode);
    if LData^ = aNode then
    begin
      vt.ScrollIntoView(LVTNode, True);
      vt.ClearSelection;
      vt.Selected[LVTNode] := True;
    end;
  end;
  LEnu.Free;
end;

procedure TForm1.DoUpdate;

  procedure AddRBNode(aParent: PVirtualNode; aNode: PRBTreeStringPairNode);
  var
    LNode: PVirtualNode;
  begin
    LNode := vt.AddChild(aParent, aNode);
    if aNode^.Left <> nil then
      AddRBNode(LNode, PRBTreeStringPairNode(aNode^.Left));

    if aNode^.Right <> nil then
      AddRBNode(LNode, PRBTreeStringPairNode(aNode^.Right));
  end;

begin
  vt.Clear;
  if FTree.Root <> nil then
    AddRBNode(nil, FTree.Root);

  vt.FullExpand();
end;

end.
