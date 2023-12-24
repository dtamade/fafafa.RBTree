unit fafafa.RBTree;

{

  红黑树 by fafafaStudio 179033731@qq.com

  IRBTree 指针:指针
  IStringRBTree 字符串:指针
  IObjectRBTree 字符串:对象
  IStringPairRBTree 字符串:字符串

}


{$Inline On}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  PPRBTreeNode = ^PRBTreeNode;
  PRBTreeNode = ^TRBTreeNode;

  { TRBTreeNode 红黑树节点 }
  TRBTreeNode = record
    Left: PRBTreeNode;
    Right: PRBTreeNode;
    Parent: PRBTreeNode;
    Color: byte;
    Key: Pointer;
    Value: Pointer;
  end;

procedure DisposeRBTreeNode(var aNode: PRBTreeNode); inline;

type

  { 对比回调 }
  TRBTreeCompareCB = function(aKey1, aKey2: Pointer): PtrInt;

  IRBTreeNodePool = interface;

  { IRBTreeBase }

  IRBTreeBase = interface
    { 清空 }
    procedure Clear;
    function GetCount: SizeInt;
    function GetNodePool: IRBTreeNodePool;
    function GetSentinel: PRBTreeNode;

    { 是否为空 }
    function IsEmpty: boolean;
    procedure SetNodePool(AValue: IRBTreeNodePool);

    { 树节点数量 }
    property Count: SizeInt read GetCount;

    { 节点池 }
    property NodePool: IRBTreeNodePool read GetNodePool write SetNodePool;

    { 哨兵节点 }
    property Sentinel: PRBTreeNode read GetSentinel;
  end;

  TRBTree = class;

  { TRBTreeNodeEnumerator 迭代器实现 }

  TRBTreeNodeEnumerator = class
  private
    function GetCurrent: PRBTreeNode;
  protected
    FCurrent: PRBTreeNode;
    FLowToHigh: boolean;
    FTree: TRBTree;
  public
    constructor Create(aTree: TRBTree; aLowToHigh: boolean = True);
    function GetEnumerator: TRBTreeNodeEnumerator; inline;
    function MoveNext: boolean;
    property Current: PRBTreeNode read GetCurrent;

    { 是否从低到高的方向迭代 }
    property LowToHigh: boolean read FLowToHigh;
  end;

  { IRBTree }

  IRBTree = interface(IRBTreeBase)
    ['{993FDEA4-4F18-48FF-9511-1E63D7C76A72}']

    function GetCompareCB: TRBTreeCompareCB;
    function GetRoot: PRBTreeNode;

    { 插入节点 }
    function Insert(aKey: Pointer; aValue: Pointer): PRBTreeNode;

    { 查找 }
    function Find(aRoot: PRBTreeNode; aKey: Pointer): PRBTreeNode; overload;
    function Find(aKey: Pointer): PRBTreeNode; overload;

    { 通过Key从红黑树中移除节点并返回该节点 }
    function RemoveNode(aRoot: PRBTreeNode; aKey: Pointer): PRBTreeNode; overload;
    function RemoveNode(aKey: Pointer): PRBTreeNode; overload;

    { 通过Key从红黑树中移除并释放该节点 返回该键的值 }
    function Remove(aRoot: PRBTreeNode; aKey: Pointer): Pointer; overload;
    function Remove(aKey: Pointer): Pointer; overload;

    { 从红黑树删除指定节点 todo: 此接口本不应该公开,某种性能优化需求保留了它,不要滥用 }
    procedure Delete(aNode: PRBTreeNode);

    { 遍历后序节点 }
    function FindSuccessor(aNode: PRBTreeNode): PRBTreeNode;

    { 遍历前序节点 }
    function FindPrecessor(aNode: PRBTreeNode): PRBTreeNode;

    { 最小节点 }
    function FindLowest: PRBTreeNode; overload;

    { 指定节点的子节点下的最小节点 }
    function FindLowest(aRoot: PRBTreeNode): PRBTreeNode; overload;

    { 最大节点 }
    function FindHighest: PRBTreeNode; overload;

    { 指定节点的子节点下的最大节点 }
    function FindHighest(aRoot: PRBTreeNode): PRBTreeNode; overload;

    { 获取迭代器实例 }
    function GetEnumerator: TRBTreeNodeEnumerator;

    { 获取从高到低的迭代器 }
    function GetEnumeratorHighToLow: TRBTreeNodeEnumerator;

    { 根节点 }
    property Root: PRBTreeNode read GetRoot;

    { 比较回调 }
    property CompareCB: TRBTreeCompareCB read GetCompareCB;
  end;

  TRBTree = class(TInterfacedObject, IRBTree)
  private
    FCount: SizeInt;
    FSentinel: TRBTreeNode;
    FNodePool: IRBTreeNodePool;
    FRootNode: PRBTreeNode;
    FCompareCB: TRBTreeCompareCB;
    function GetCompareCB: TRBTreeCompareCB;

    function GetCount: SizeInt;
    function GetNodePool: IRBTreeNodePool;
    function GetSentinel: PRBTreeNode;
    procedure SetNodePool(AValue: IRBTreeNodePool);
  protected
    procedure InternalInsert(aRoot, aNode, aSentinel: PRBTreeNode); overload;
    procedure InternalInsert(aNode: PRBTreeNode); overload;
    procedure DisconnectNode(aNode: PRBTreeNode);

    procedure DoUpdateNode(aNode: PRBTreeNode; aValue: Pointer); virtual;
    function DoCompare(aPtr1, aPtr2: Pointer): PtrInt; virtual;
    function GetRoot: PRBTreeNode;

    function AllocNode: PRBTreeNode; virtual;
    procedure DisposeNode(aNode: PRBTreeNode); virtual;
  public
    constructor Create; virtual;
    constructor Create(aCompareCB: TRBTreeCompareCB);
    destructor Destroy; override;

    function IsEmpty: boolean;
    procedure Clear; virtual;

    function Insert(aKey: Pointer; aValue: Pointer): PRBTreeNode;
    function Find(aRoot: PRBTreeNode; aKey: Pointer): PRBTreeNode; overload;
    function Find(aKey: Pointer): PRBTreeNode; overload;
    function RemoveNode(aRoot: PRBTreeNode; aKey: Pointer): PRBTreeNode; overload;
    function RemoveNode(aKey: Pointer): PRBTreeNode; overload;
    function Remove(aRoot: PRBTreeNode; aKey: Pointer): Pointer; overload;
    function Remove(aKey: Pointer): Pointer; overload;
    procedure Delete(aNode: PRBTreeNode);
    function FindSuccessor(aNode: PRBTreeNode): PRBTreeNode;
    function FindPrecessor(aNode: PRBTreeNode): PRBTreeNode;
    function FindLowest(aRoot: PRBTreeNode): PRBTreeNode; overload;
    function FindLowest: PRBTreeNode; overload;
    function FindHighest(aRoot: PRBTreeNode): PRBTreeNode; overload;
    function FindHighest: PRBTreeNode; overload;

    function GetEnumerator: TRBTreeNodeEnumerator;
    function GetEnumeratorHighToLow: TRBTreeNodeEnumerator;

    property Root: PRBTreeNode read GetRoot;
    property CompareCB: TRBTreeCompareCB read GetCompareCB;
    property NodePool: IRBTreeNodePool read GetNodePool write SetNodePool;
    property Sentinel: PRBTreeNode read GetSentinel;
    property Count: SizeInt read GetCount;
  end;

function MakeRBTree: IRBTree; overload;
function MakeRBTree(aCompareCB: TRBTreeCompareCB): IRBTree; overload;


type

  PStringKey = ^TStringKey;

  TStringKey = record
    KeyHash: integer; // 缓存字符串hash
    Key: string;
  end;

  PStringRBTreeNode = ^TStringRBTreeNode;

  { TStringRBTreeNode 字符串键节点 }
  TStringRBTreeNode = record
    Left: PStringRBTreeNode;
    Right: PStringRBTreeNode;
    Parent: PStringRBTreeNode;
    Color: byte;
    Key: PStringKey;
    Value: Pointer;
  end;

procedure DisposeStringRBTreeNode(var aNode: PStringRBTreeNode); inline;

type

  THashCB = function(const aStr: string): integer;

  { IStringRBTreeBase }

  IStringRBTreeBase = interface(IRBTreeBase)
    function GetHashCB: THashCB;
    property HashCB: THashCB read GetHashCB;
  end;

  { TStringRBTreeBase }

  TStringRBTreeBase = class(TRBTree)
  private
    FHashCB: THashCB;
    function GetHashCB: THashCB;
  public
    constructor Create(); virtual; reintroduce;
    constructor Create(aHashCB: THashCB);

    property HashCB: THashCB read GetHashCB;
  end;

  { TStringRBTreeNodeEnumerator }

  TStringRBTreeNodeEnumerator = class(TRBTreeNodeEnumerator)
  private
    function GetCurrent: PStringRBTreeNode;
  public
    property Current: PStringRBTreeNode read GetCurrent;
  end;


  { IStringRBTree }

  IStringRBTree = interface(IStringRBTreeBase)
    function GetRoot: PStringRBTreeNode;

    { 插入节点 }
    function Insert(const aKey: string; aValue: Pointer): PStringRBTreeNode;

    { 查找 }
    function Find(aRoot: PStringRBTreeNode; const aKey: string): PStringRBTreeNode; overload;
    function Find(const aKey: string): PStringRBTreeNode; overload;

    { 通过Key从红黑树中移除节点并返回该节点 }
    function RemoveNode(aRoot: PStringRBTreeNode; const aKey: string): PStringRBTreeNode; overload;
    function RemoveNode(const aKey: string): PStringRBTreeNode; overload;

    { 通过Key从红黑树中移除并释放该节点 返回该键的值 }
    function Remove(aRoot: PStringRBTreeNode; const aKey: string): Pointer; overload;
    function Remove(const aKey: string): Pointer; overload;

    { 从红黑树删除指定节点 todo: 此接口本不应该公开,某种性能优化需求保留了它,不要滥用 }
    procedure Delete(aNode: PStringRBTreeNode);

    { 遍历后序节点 }
    function FindSuccessor(aNode: PStringRBTreeNode): PStringRBTreeNode;

    { 遍历前序节点 }
    function FindPrecessor(aNode: PStringRBTreeNode): PStringRBTreeNode;

    { 指定节点的子节点下的最小节点 }
    function FindLowest(aRoot: PStringRBTreeNode): PStringRBTreeNode; overload;

    { 最小节点 }
    function FindLowest: PStringRBTreeNode; overload;

    { 指定节点的子节点下的最大节点 }
    function FindHighest(aRoot: PStringRBTreeNode): PStringRBTreeNode; overload;

    { 最大节点 }
    function FindHighest: PStringRBTreeNode; overload;

    function GetEnumerator: TStringRBTreeNodeEnumerator;
    function GetEnumeratorHighToLow: TStringRBTreeNodeEnumerator;

    { 根节点 }
    property Root: PStringRBTreeNode read GetRoot;
  end;

  TStringRBTree = class(TStringRBTreeBase, IStringRBTree)
  private
    FTmpKey: TStringKey;
    function GetRoot: PStringRBTreeNode;
  protected
    procedure DisposeNode(aNode: PRBTreeNode); override;

    function CreateKey(const aKey: string): PStringKey;
    procedure SetKey(const aKeyStr: string; aKey: PStringKey);
    function DoHash(const aStr: string): integer;
  public
    function Insert(const aKey: string; aValue: Pointer): PStringRBTreeNode;
    function Find(aRoot: PStringRBTreeNode; const aKey: string): PStringRBTreeNode; reintroduce;
    function Find(const aKey: string): PStringRBTreeNode; reintroduce;
    function RemoveNode(aRoot: PStringRBTreeNode; const aKey: string): PStringRBTreeNode; reintroduce;
    function RemoveNode(const aKey: string): PStringRBTreeNode; reintroduce;
    function Remove(aRoot: PStringRBTreeNode; const aKey: string): Pointer; reintroduce;
    function Remove(const aKey: string): Pointer; reintroduce;
    procedure Delete(aNode: PStringRBTreeNode);
    function FindSuccessor(aNode: PStringRBTreeNode): PStringRBTreeNode;
    function FindPrecessor(aNode: PStringRBTreeNode): PStringRBTreeNode;
    function FindLowest(aRoot: PStringRBTreeNode): PStringRBTreeNode; reintroduce;
    function FindLowest: PStringRBTreeNode; reintroduce;
    function FindHighest(aRoot: PStringRBTreeNode): PStringRBTreeNode; reintroduce;
    function FindHighest: PStringRBTreeNode; reintroduce;

    function GetEnumerator: TStringRBTreeNodeEnumerator;
    function GetEnumeratorHighToLow: TStringRBTreeNodeEnumerator;

    property Root: PStringRBTreeNode read GetRoot;
  end;

function MakeStringRBTree: IStringRBTree; overload;
function MakeStringRBTree(aHashCB: THashCB): IStringRBTree; overload;


type


  PStringObjectRBTreeNode = ^TStringObjectRBTreeNode;

  { TStringObjectRBTreeNode 字符串:对象 键值对节点 }
  TStringObjectRBTreeNode = record
    Left: PStringObjectRBTreeNode;
    Right: PStringObjectRBTreeNode;
    Parent: PStringObjectRBTreeNode;
    Color: byte;
    Key: PStringKey;
    Value: TObject;
  end;

procedure DisposeStringObjectRBTreeNode(var aNode: PStringObjectRBTreeNode); inline;

type

  { TStringObjectRBTreeNodeEnumerator }

  TStringObjectRBTreeNodeEnumerator = class(TRBTreeNodeEnumerator)
  private
    function GetCurrent: PStringObjectRBTreeNode;
  public
    property Current: PStringObjectRBTreeNode read GetCurrent;
  end;


  { IStringObjectRBTree }

  IStringObjectRBTree = interface(IStringRBTreeBase)

    function GetRoot: PStringObjectRBTreeNode;
    function GetFreeObjects: boolean;
    procedure SetFreeObjects(AValue: boolean);

    { 插入节点 }
    function Insert(const aKey: string; aObj: TObject): PStringObjectRBTreeNode;

    { 查找 }
    function Find(aRoot: PStringObjectRBTreeNode; const aKey: string): PStringObjectRBTreeNode; overload;
    function Find(const aKey: string): PStringObjectRBTreeNode; overload;

    { 通过Key从红黑树中移除节点并返回该节点 }
    function RemoveNode(aRoot: PStringObjectRBTreeNode; const aKey: string): PStringObjectRBTreeNode; overload;
    function RemoveNode(const aKey: string): PStringObjectRBTreeNode; overload;

    { 通过Key从红黑树中移除节点并释放该节点 }
    function Remove(aRoot: PStringObjectRBTreeNode; const aKey: string): TObject; overload;
    function Remove(const aKey: string): TObject; overload;

    { 从红黑树删除指定节点 todo: 此接口本不应该公开,某种性能优化需求保留了它,不要滥用 }
    procedure Delete(aNode: PStringObjectRBTreeNode);

    { 遍历后序节点 }
    function FindSuccessor(aNode: PStringObjectRBTreeNode): PStringObjectRBTreeNode;

    { 遍历前序节点 }
    function FindPrecessor(aNode: PStringObjectRBTreeNode): PStringObjectRBTreeNode;

    { 最小节点 }
    function FindLowest: PStringObjectRBTreeNode; overload;

    { 指定节点的子节点下的最小节点 }
    function FindLowest(aRoot: PStringObjectRBTreeNode): PStringObjectRBTreeNode; overload;

    { 最大节点 }
    function FindHighest: PStringObjectRBTreeNode; overload;

    { 指定节点的子节点下的最大节点 }
    function FindHighest(aRoot: PStringObjectRBTreeNode): PStringObjectRBTreeNode; overload;

    function GetEnumerator: TStringObjectRBTreeNodeEnumerator;
    function GetEnumeratorHighToLow: TStringObjectRBTreeNodeEnumerator;


    property FreeObjects: boolean read GetFreeObjects write SetFreeObjects;

    { 根节点 }
    property Root: PStringObjectRBTreeNode read GetRoot;
  end;

  TStringObjectRBTree = class(TStringRBTree, IStringObjectRBTree)
  private
    FFreeObjects: boolean;
    function GetFreeObjects: boolean;
    function GetRoot: PStringObjectRBTreeNode;
    procedure SetFreeObjects(AValue: boolean);
  protected
    procedure DisposeNode(aNode: PRBTreeNode); override;
  public
    constructor Create; override; overload;
    constructor Create(aFreeObjects: boolean); overload;
    constructor Create(aHashCB: THashCB; aFreeObjects: boolean); overload;

    function Insert(const aKey: string; aObj: TObject): PStringObjectRBTreeNode;
    function Find(aRoot: PStringObjectRBTreeNode; const aKey: string): PStringObjectRBTreeNode; reintroduce;
    function Find(const aKey: string): PStringObjectRBTreeNode; reintroduce;
    function RemoveNode(aRoot: PStringObjectRBTreeNode; const aKey: string): PStringObjectRBTreeNode; reintroduce;
    function RemoveNode(const aKey: string): PStringObjectRBTreeNode; reintroduce;
    function Remove(aRoot: PStringObjectRBTreeNode; const aKey: string): TObject; reintroduce;
    function Remove(const aKey: string): TObject; reintroduce;
    procedure Delete(aNode: PStringObjectRBTreeNode);
    function FindSuccessor(aNode: PStringObjectRBTreeNode): PStringObjectRBTreeNode;
    function FindPrecessor(aNode: PStringObjectRBTreeNode): PStringObjectRBTreeNode;
    function FindLowest: PStringObjectRBTreeNode; reintroduce;
    function FindLowest(aRoot: PStringObjectRBTreeNode): PStringObjectRBTreeNode; reintroduce;
    function FindHighest: PStringObjectRBTreeNode; reintroduce;
    function FindHighest(aRoot: PStringObjectRBTreeNode): PStringObjectRBTreeNode; reintroduce;
    function GetEnumerator: TStringObjectRBTreeNodeEnumerator;
    function GetEnumeratorHighToLow: TStringObjectRBTreeNodeEnumerator;

    property FreeObjects: boolean read GetFreeObjects write SetFreeObjects;
    property Root: PStringObjectRBTreeNode read GetRoot;
  end;

function MakeStringObjectRBTree: IStringObjectRBTree; overload;
function MakeStringObjectRBTree(aFreeObjects: boolean): IStringObjectRBTree; overload;
function MakeStringObjectRBTree(aHashCB: THashCB): IStringObjectRBTree; overload;
function MakeStringObjectRBTree(aHashCB: THashCB; aFreeObjects: boolean): IStringObjectRBTree; overload;


type

  PStringPairRBTreeNode = ^TStringPairRBTreeNode;

  { TRBTreeNode 红黑树节点 }
  TStringPairRBTreeNode = record
    Left: PStringPairRBTreeNode;
    Right: PStringPairRBTreeNode;
    Parent: PStringPairRBTreeNode;
    Color: byte;
    Key: PStringKey;
    Value: string;
  end;

procedure DisposeStringPairRBTreeNode(var aNode: PStringPairRBTreeNode);

type

  { TStringPairRBTreeNodeEnumerator }

  TStringPairRBTreeNodeEnumerator = class(TRBTreeNodeEnumerator)
  private
    function GetCurrent: PStringPairRBTreeNode;
  public
    property Current: PStringPairRBTreeNode read GetCurrent;
  end;


  { IStringPairRBTree }

  IStringPairRBTree = interface(IStringRBTreeBase)
    function GetRoot: PStringPairRBTreeNode;

    { 插入节点 }
    function Insert(const aKey, aValue: string): PStringPairRBTreeNode;

    { 查找 }
    function Find(aRoot: PStringPairRBTreeNode; const aKey: string): PStringPairRBTreeNode; overload;
    function Find(const aKey: string): PStringPairRBTreeNode; overload;

    { 通过Key从红黑树中移除节点并返回该节点 }
    function RemoveNode(aRoot: PStringPairRBTreeNode; const aKey: string): PStringPairRBTreeNode; overload;
    function RemoveNode(const aKey: string): PStringPairRBTreeNode; overload;

    { 通过Key移除 }
    function Remove(aRoot: PStringPairRBTreeNode; const aKey: string): string; overload;
    function Remove(const aKey: string): string; overload;

    { 从红黑树删除指定节点 todo: 此接口本不应该公开,某种性能优化需求保留了它,不要滥用 }
    procedure Delete(aNode: PStringPairRBTreeNode);

    { 遍历后序节点 }
    function FindSuccessor(aNode: PStringPairRBTreeNode): PStringPairRBTreeNode;

    { 遍历前序节点 }
    function FindPrecessor(aNode: PStringPairRBTreeNode): PStringPairRBTreeNode;

    { 指定节点的子节点下的最小节点 }
    function FindLowest(aRoot: PStringPairRBTreeNode): PStringPairRBTreeNode; overload;

    { 最小节点 }
    function FindLowest: PStringPairRBTreeNode; overload;

    { 指定节点的子节点下的最大节点 }
    function FindHighest(aRoot: PStringPairRBTreeNode): PStringPairRBTreeNode; overload;

    { 最大节点 }
    function FindHighest: PStringPairRBTreeNode; overload;

    function GetEnumerator: TStringPairRBTreeNodeEnumerator;
    function GetEnumeratorHighToLow: TStringPairRBTreeNodeEnumerator;

    { 根节点 }
    property Root: PStringPairRBTreeNode read GetRoot;
  end;

  TStringPairRBTree = class(TStringRBTree, IStringPairRBTree)
  private
    function GetRoot: PStringPairRBTreeNode;
  protected
    procedure DoUpdateNode(aNode: PRBTreeNode; aValue: Pointer); override;
    procedure DisposeNode(aNode: PRBTreeNode); override;
  public
    function Insert(const aKey, aValue: string): PStringPairRBTreeNode;
    function Find(aRoot: PStringPairRBTreeNode; const aKey: string): PStringPairRBTreeNode; reintroduce;
    function Find(const aKey: string): PStringPairRBTreeNode; reintroduce;
    function RemoveNode(aRoot: PStringPairRBTreeNode; const aKey: string): PStringPairRBTreeNode; reintroduce;
    function RemoveNode(const aKey: string): PStringPairRBTreeNode; reintroduce;
    function Remove(aRoot: PStringPairRBTreeNode; const aKey: string): string; reintroduce;
    function Remove(const aKey: string): string; reintroduce;
    procedure Delete(aNode: PStringPairRBTreeNode);
    function FindSuccessor(aNode: PStringPairRBTreeNode): PStringPairRBTreeNode;
    function FindPrecessor(aNode: PStringPairRBTreeNode): PStringPairRBTreeNode;
    function FindLowest(aRoot: PStringPairRBTreeNode): PStringPairRBTreeNode; reintroduce;
    function FindLowest: PStringPairRBTreeNode; reintroduce;
    function FindHighest(aRoot: PStringPairRBTreeNode): PStringPairRBTreeNode; reintroduce;
    function FindHighest: PStringPairRBTreeNode; reintroduce;

    function GetEnumerator: TStringPairRBTreeNodeEnumerator;
    function GetEnumeratorHighToLow: TStringPairRBTreeNodeEnumerator;

    { 根节点 }
    property Root: PStringPairRBTreeNode read GetRoot;
  end;

function MakeStringPairRBTree: IStringPairRBTree; overload;
function MakeStringPairRBTree(aHashCB: THashCB): IStringPairRBTree; overload;

type

  { IRBTreeNodePool }

  IRBTreeNodePool = interface
    function GetCount: integer;
    function GetMaxCount: integer;
    procedure SetMaxCount(AValue: integer);


    { 清空节点池 }
    procedure Clear;

    { 分配节点 }
    function AllocNode: PRBTreeNode;

    { 释放节点 }
    procedure DisposeNode(aNode: PRBTreeNode);

    { 当前缓存节点数量 }
    property Count: integer read GetCount;

    { 最大缓存节点数量,当超过此数量的节点被缓存进来后将不再缓存,直接释放 }
    property MaxCount: integer read GetMaxCount write SetMaxCount;
  end;

  { TRBTreeNodePool 缓存红黑树节点 }
  TRBTreeNodePool = class(TInterfacedObject, IRBTreeNodePool)
  private
  class var FDefaultInstance: TRBTreeNodePool;
  private
    FEntry: PRBTreeNode;
    FCount: integer;
    FMaxCount: integer;
    function GetCount: integer;
    function GetMaxCount: integer;
    procedure SetMaxCount(AValue: integer);
  public
    class destructor Destroy;

    { 延迟加载的默认实例 它不是线程安全的,如果涉及多线程,请给线程独占单独的实例或者进行同步 }
    class function GetDefaultInstance: TRBTreeNodePool;

    constructor Create; virtual; overload;
    constructor Create(aMaxCount: integer); overload;
    destructor Destroy; override;

    procedure Clear;

    function AllocNode: PRBTreeNode; inline;
    procedure DisposeNode(aNode: PRBTreeNode); inline;

    { 当前缓存节点数量 }
    property Count: integer read GetCount;

    { 最大缓存节点数量,当超过此数量的节点被缓存进来后将不再缓存,直接释放 }
    property MaxCount: integer read GetMaxCount write SetMaxCount;
  end;

function MakeRBTreeNodePool: IRBTreeNodePool; overload;
function MakeRBTreeNodePool(aMaxCount: integer): IRBTreeNodePool; overload;

function CRC32_LONG(const aStr: string): integer;

implementation

const
  CRC32_TABLE256: array[0..255] of longword = (
    $00000000, $77073096, $ee0e612c, $990951ba,
    $076dc419, $706af48f, $e963a535, $9e6495a3,
    $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988,
    $09b64c2b, $7eb17cbd, $e7b82d07, $90bf1d91,
    $1db71064, $6ab020f2, $f3b97148, $84be41de,
    $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7,
    $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec,
    $14015c4f, $63066cd9, $fa0f3d63, $8d080df5,
    $3b6e20c8, $4c69105e, $d56041e4, $a2677172,
    $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b,
    $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940,
    $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59,
    $26d930ac, $51de003a, $c8d75180, $bfd06116,
    $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
    $2802b89e, $5f058808, $c60cd9b2, $b10be924,
    $2f6f7c87, $58684c11, $c1611dab, $b6662d3d,
    $76dc4190, $01db7106, $98d220bc, $efd5102a,
    $71b18589, $06b6b51f, $9fbfe4a5, $e8b8d433,
    $7807c9a2, $0f00f934, $9609a88e, $e10e9818,
    $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,
    $6b6b51f4, $1c6c6162, $856530d8, $f262004e,
    $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457,
    $65b0d9c6, $12b7e950, $8bbeb8ea, $fcb9887c,
    $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65,
    $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2,
    $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb,
    $4369e96a, $346ed9fc, $ad678846, $da60b8d0,
    $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
    $5005713c, $270241aa, $be0b1010, $c90c2086,
    $5768b525, $206f85b3, $b966d409, $ce61e49f,
    $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4,
    $59b33d17, $2eb40d81, $b7bd5c3b, $c0ba6cad,
    $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a,
    $ead54739, $9dd277af, $04db2615, $73dc1683,
    $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8,
    $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1,
    $f00f9344, $8708a3d2, $1e01f268, $6906c2fe,
    $f762575d, $806567cb, $196c3671, $6e6b06e7,
    $fed41b76, $89d32be0, $10da7a5a, $67dd4acc,
    $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
    $d6d6a3e8, $a1d1937e, $38d8c2c4, $4fdff252,
    $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
    $d80d2bda, $af0a1b4c, $36034af6, $41047a60,
    $df60efc3, $a867df55, $316e8eef, $4669be79,
    $cb61b38c, $bc66831a, $256fd2a0, $5268e236,
    $cc0c7795, $bb0b4703, $220216b9, $5505262f,
    $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04,
    $c2d7ffa7, $b5d0cf31, $2cd99e8b, $5bdeae1d,
    $9b64c2b0, $ec63f226, $756aa39c, $026d930a,
    $9c0906a9, $eb0e363f, $72076785, $05005713,
    $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38,
    $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21,
    $86d3d2d4, $f1d4e242, $68ddb3f8, $1fda836e,
    $81be16cd, $f6b9265b, $6fb077e1, $18b74777,
    $88085ae6, $ff0f6a70, $66063bca, $11010b5c,
    $8f659eff, $f862ae69, $616bffd3, $166ccf45,
    $a00ae278, $d70dd2ee, $4e048354, $3903b3c2,
    $a7672661, $d06016f7, $4969474d, $3e6e77db,
    $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0,
    $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
    $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6,
    $bad03605, $cdd70693, $54de5729, $23d967bf,
    $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94,
    $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d
    );

  DEFAULT_RBTREENODEPOOL_MAXCOUNT = 1024;


function CRC32_LONG(aStr: PChar; aLen: integer): uint32;
var
  i: SizeUInt;
begin
  Result := $FFFFFFFF;

  for i := 0 to pred(aLen) do
    Result := (CRC32_TABLE256[uint8((Result xor byte(aStr[i])) and $FF)] xor (Result shr 8));

  Result := (Result xor $FFFFFFFF);
end;

procedure DisposeStringRBTreeNode(var aNode: PStringRBTreeNode);
begin
  Dispose(aNode^.Key);
  Dispose(aNode);
end;

function MakeStringRBTree: IStringRBTree;
begin
  Result := TStringRBTree.Create();
end;

function MakeStringRBTree(aHashCB: THashCB): IStringRBTree;
begin
  Result := TStringRBTree.Create(aHashCB);
end;

procedure DisposeStringObjectRBTreeNode(var aNode: PStringObjectRBTreeNode);
begin
  Dispose(aNode^.Key);
  Dispose(aNode);
end;

function MakeStringObjectRBTree: IStringObjectRBTree;
begin
  Result := TStringObjectRBTree.Create;
end;

function MakeStringObjectRBTree(aFreeObjects: boolean): IStringObjectRBTree;
begin
  Result := TStringObjectRBTree.Create(aFreeObjects);
end;

function MakeStringObjectRBTree(aHashCB: THashCB): IStringObjectRBTree;
begin
  Result := TStringObjectRBTree.Create(aHashCB);
end;

function MakeStringObjectRBTree(aHashCB: THashCB; aFreeObjects: boolean): IStringObjectRBTree;
begin
  Result := TStringObjectRBTree.Create(aHashCB, aFreeObjects);
end;

procedure DisposeStringPairRBTreeNode(var aNode: PStringPairRBTreeNode);
begin
  Dispose(aNode^.Key);
 aNode^.Value:='';
  Dispose(aNode);
end;

procedure DisposeRBTreeNode(var aNode: PRBTreeNode);
begin
  Dispose(aNode);
end;

function MakeRBTree: IRBTree;
begin
  Result := TRBTree.Create;
end;

function MakeRBTree(aCompareCB: TRBTreeCompareCB): IRBTree;
begin
  Result := TRBTree.Create(aCompareCB);
end;

function MakeStringPairRBTree: IStringPairRBTree;
begin
  Result := TStringPairRBTree.Create();
end;

function MakeStringPairRBTree(aHashCB: THashCB): IStringPairRBTree;
begin
  Result := TStringPairRBTree.Create(aHashCB);
end;

function MakeRBTreeNodePool: IRBTreeNodePool;
begin
  Result := TRBTreeNodePool.Create();
end;

function MakeRBTreeNodePool(aMaxCount: integer): IRBTreeNodePool;
begin
  Result := TRBTreeNodePool.Create(aMaxCount);
end;

function CRC32_LONG(const aStr: string): integer;
begin
  Result := integer(CRC32_LONG(PChar(aStr), Length(aStr)));
end;

procedure RedNode(aNode: PRBTreeNode); inline;
begin
  aNode^.Color := 1;
end;

procedure BlackNode(aNode: PRBTreeNode); inline;
begin
  aNode^.Color := 0;
end;

function NodeIsRed(aNode: PRBTreeNode): boolean; inline;
begin
  Result := (aNode^.Color > 0);
end;

function NodeIsBlack(aNode: PRBTreeNode): boolean; inline;
begin
  Result := (not NodeIsRed(aNode));
end;

procedure CopyColor(aNode1, aNode2: PRBTreeNode); inline;
begin
  aNode1^.Color := aNode2^.Color;
end;

procedure LeftRotate(aRoot: PPRBTreeNode; aSentinel, aNode: PRBTreeNode); inline;
var
  LTemp: PRBTreeNode;
begin
  LTemp := aNode^.Right;
  aNode^.Right := LTemp^.Left;

  if LTemp^.Left <> aSentinel then
    LTemp^.Left^.Parent := aNode;

  LTemp^.Parent := aNode^.Parent;

  if aNode = aRoot^ then
    aRoot^ := LTemp
  else if aNode = aNode^.Parent^.Left then
    aNode^.Parent^.Left := LTemp
  else
    aNode^.Parent^.Right := LTemp;

  LTemp^.Left := aNode;
  aNode^.Parent := LTemp;
end;

procedure RightRotate(aRoot: PPRBTreeNode; aSentinel, aNode: PRBTreeNode); inline;
var
  LTemp: PRBTreeNode;
begin
  LTemp := aNode^.Left;

  aNode^.Left := LTemp^.Right;

  if LTemp^.Right <> aSentinel then
    LTemp^.Right^.Parent := aNode;

  LTemp^.Parent := aNode^.Parent;

  if aNode = aRoot^ then
    aRoot^ := LTemp
  else if aNode = aNode^.Parent^.Right then
    aNode^.Parent^.Right := LTemp
  else
    aNode^.Parent^.Left := LTemp;

  LTemp^.Right := aNode;
  aNode^.Parent := LTemp;
end;

procedure StuffNode(aNode: PRBTreeNode); inline;
begin
  aNode^.Color := 0;
  aNode^.Left := nil;
  aNode^.Right := nil;
  aNode^.Parent := nil;
end;

function DoDefaultCompare(aKey1, aKey2: Pointer): PtrInt; inline;
begin
  if aKey1 > aKey2 then
    Result := 1
  else if aKey1 < aKey2 then
    Result := -1
  else
    Result := 0;
end;

{ 字符串比对 }
function DoStringCompare(aKey1, aKey2: PStringKey): PtrInt; inline;
var
  LLen1, LLen2: SizeInt;
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
    LLen1 := Length(aKey1^.Key);
    LLen2 := Length(aKey2^.Key);
    if LLen1 <> LLen2 then
    begin
      if LLen1 > LLen2 then
        Result := 1
      else
        Result := -1;
    end
    else
    begin
      Result := strcomp(PChar(aKey1^.Key), PChar(aKey2^.Key));
    end;
  end;
end;

{ TRBTreeNodeEnumerator }

function TRBTreeNodeEnumerator.GetCurrent: PRBTreeNode;
begin
  Result := FCurrent;
end;

constructor TRBTreeNodeEnumerator.Create(aTree: TRBTree; aLowToHigh: boolean);
begin
  inherited Create;
  FTree := aTree;
  FLowToHigh := aLowToHigh;
  FCurrent := nil;
end;

function TRBTreeNodeEnumerator.GetEnumerator: TRBTreeNodeEnumerator;
begin
  Result := Self;
end;

function TRBTreeNodeEnumerator.MoveNext: boolean;
begin
  if FLowToHigh then
  begin
    if FCurrent <> nil then
      FCurrent := FTree.FindSuccessor(FCurrent)
    else
      FCurrent := FTree.FindLowest;
  end
  else
  begin
    if FCurrent <> nil then
      FCurrent := FTree.FindPrecessor(FCurrent)
    else
      FCurrent := FTree.FindHighest;
  end;
  Result := FCurrent <> nil;
end;

function TRBTree.GetCompareCB: TRBTreeCompareCB;
begin
  Result := FCompareCB;
end;

function TRBTree.GetCount: SizeInt;
begin
  Result := FCount;
end;

function TRBTree.GetNodePool: IRBTreeNodePool;
begin
  Result := FNodePool;
end;

function TRBTree.GetSentinel: PRBTreeNode;
begin
  Result := @FSentinel;
end;

procedure TRBTree.SetNodePool(AValue: IRBTreeNodePool);
begin
  FNodePool := AValue;
end;

procedure TRBTree.InternalInsert(aRoot, aNode, aSentinel: PRBTreeNode);
var
  LTemp: PRBTreeNode;
  LNode: PPRBTreeNode;
  LCompareResult: integer;
begin
  LTemp := aRoot;

  while True do
  begin
    LCompareResult := DoCompare(aNode^.Key, LTemp^.Key);
    if LCompareResult < 0 then
      LNode := @(LTemp^.Left)
    else if LCompareResult > 0 then
      LNode := @(LTemp^.Right)
    else
      LNode := @(LTemp^.Right); // 重复键

    if LNode^ = aSentinel then
      Break;

    LTemp := LNode^;
  end;

  LNode^ := aNode;
  aNode^.Parent := LTemp;
  aNode^.Left := aSentinel;
  aNode^.Right := aSentinel;
  RedNode(aNode);
end;

procedure TRBTree.InternalInsert(aNode: PRBTreeNode);
var
  LPRoot: PPRBTreeNode;
  LTemp: PRBTreeNode;
  LSentinel: PRBTreeNode;
begin
  LPRoot := @FRootNode;
  LSentinel := @FSentinel;

  if LPRoot^ = LSentinel then
  begin
    aNode^.Parent := nil;
    aNode^.Left := LSentinel;
    aNode^.Right := LSentinel;
    BlackNode(aNode);
    LPRoot^ := aNode;
    Exit;
  end;

  InternalInsert(LPRoot^, aNode, LSentinel);

  { re-balance tree }
  while (aNode <> LPRoot^) and NodeIsRed(aNode^.Parent) do
  begin
    if aNode^.Parent = aNode^.Parent^.Parent^.Left then
    begin
      LTemp := aNode^.Parent^.Parent^.Right;
      if NodeIsRed(LTemp) then
      begin
        BlackNode(aNode^.Parent);
        BlackNode(LTemp);
        RedNode(aNode^.Parent^.Parent);
        aNode := aNode^.Parent^.Parent;
      end
      else
      begin
        if aNode = aNode^.Parent^.Right then
        begin
          aNode := aNode^.Parent;
          LeftRotate(LPRoot, LSentinel, aNode);
        end;
        BlackNode(aNode^.Parent);
        RedNode(aNode^.Parent^.Parent);
        RightRotate(LPRoot, LSentinel, aNode^.Parent^.Parent);
      end;
    end
    else
    begin
      LTemp := aNode^.Parent^.Parent^.Left;
      if NodeIsRed(LTemp) then
      begin
        BlackNode(aNode^.Parent);
        BlackNode(LTemp);
        RedNode(aNode^.Parent^.Parent);
        aNode := aNode^.Parent^.Parent;
      end
      else
      begin
        if aNode = aNode^.Parent^.Left then
        begin
          aNode := aNode^.Parent;
          RightRotate(LPRoot, LSentinel, aNode);
        end;
        BlackNode(aNode^.Parent);
        RedNode(aNode^.Parent^.Parent);
        LeftRotate(LPRoot, LSentinel, aNode^.Parent^.Parent);
      end;
    end;
  end;
  BlackNode(LPRoot^);
end;

procedure TRBTree.DisconnectNode(aNode: PRBTreeNode);
label
  CLEANUP_EXIT;
var
  LIsRed: boolean;
  LPRoot: PPRBTreeNode;
  LSentinel, LSubst, LTemp, LW: PRBTreeNode;
begin
  LPRoot := @FRootNode;
  LSentinel := @FSentinel;

  if aNode^.Left = LSentinel then
  begin
    LTemp := aNode^.Right;
    LSubst := aNode;
  end
  else if aNode^.Right = LSentinel then
  begin
    LTemp := aNode^.Left;
    LSubst := aNode;
  end
  else
  begin
    LSubst := FindLowest(aNode^.Right);
    LTemp := LSubst^.Right;
  end;

  if LSubst = LPRoot^ then
  begin
    LPRoot^ := LTemp;
    BlackNode(LTemp);

    goto CLEANUP_EXIT;
  end;

  LIsRed := NodeIsRed(LSubst);
  if LSubst = LSubst^.Parent^.Left then
    LSubst^.Parent^.Left := LTemp
  else
    LSubst^.Parent^.Right := LTemp;

  if LSubst = aNode then
    LTemp^.Parent := LSubst^.Parent
  else
  begin
    if LSubst^.Parent = aNode then
      LTemp^.Parent := LSubst
    else
      LTemp^.Parent := LSubst^.Parent;

    LSubst^.Left := aNode^.Left;
    LSubst^.Right := aNode^.Right;
    LSubst^.Parent := aNode^.Parent;
    CopyColor(LSubst, aNode);

    if aNode = LPRoot^ then
      LPRoot^ := LSubst
    else
    begin
      if aNode = aNode^.Parent^.Left then
        aNode^.Parent^.Left := LSubst
      else
        aNode^.Parent^.Right := LSubst;
    end;

    if LSubst^.Left <> LSentinel then
      LSubst^.Left^.Parent := LSubst;

    if LSubst^.Right <> LSentinel then
      LSubst^.Right^.Parent := LSubst;
  end;


  if LIsRed then
    goto CLEANUP_EXIT;
  //Exit;

  { a fixup }
  while (LTemp <> LPRoot^) and NodeIsBlack(LTemp) do
  begin
    if LTemp = LTemp^.Parent^.Left then
    begin
      LW := LTemp^.Parent^.Right;

      if NodeIsRed(LW) then
      begin
        BlackNode(LW);
        RedNode(LTemp^.Parent);
        LeftRotate(LPRoot, LSentinel, LTemp^.Parent);
        LW := LTemp^.Parent^.Right;
      end;

      if NodeIsBlack(LW^.Left) and NodeIsBlack(LW^.Right) then
      begin
        RedNode(LW);
        LTemp := LTemp^.Parent;
      end
      else
      begin
        if NodeIsBlack(LW^.Right) then
        begin
          BlackNode(LW^.Left);
          RedNode(LW);
          RightRotate(LPRoot, LSentinel, LW);
          LW := LTemp^.Parent^.Right;
        end;

        CopyColor(LW, LTemp^.Parent);
        BlackNode(LTemp^.Parent);
        BlackNode(LW^.Right);
        LeftRotate(LPRoot, LSentinel, LTemp^.Parent);
        LTemp := LPRoot^;
      end;
    end
    else
    begin
      LW := LTemp^.Parent^.Left;

      if NodeIsRed(LW) then
      begin
        BlackNode(LW);
        RedNode(LTemp^.Parent);
        RightRotate(LPRoot, LSentinel, LTemp^.Parent);
        LW := LTemp^.Parent^.Left;
      end;

      if NodeIsBlack(LW^.Left) and NodeIsBlack(LW^.Right) then
      begin
        RedNode(LW);
        LTemp := LTemp^.Parent;
      end
      else
      begin
        if NodeIsBlack(LW^.Left) then
        begin
          BlackNode(LW^.Right);
          RedNode(LW);
          LeftRotate(LPRoot, LSentinel, LW);
          LW := LTemp^.Parent^.Left;
        end;

        CopyColor(LW, LTemp^.Parent);
        BlackNode(LTemp^.Parent);
        BlackNode(LW^.Left);
        RightRotate(LPRoot, LSentinel, LTemp^.Parent);
        LTemp := LPRoot^;
      end;
    end;
  end;

  BlackNode(LTemp);

  CLEANUP_EXIT:
  begin
    StuffNode(aNode);
    Dec(FCount);
  end;
end;

procedure TRBTree.DoUpdateNode(aNode: PRBTreeNode; aValue: Pointer);
begin
  aNode^.Value := aValue;
end;

function TRBTree.DoCompare(aPtr1, aPtr2: Pointer): PtrInt;
begin
  Result := FCompareCB(aPtr1, aPtr2);
end;

function TRBTree.GetRoot: PRBTreeNode;
begin
  Result := FRootNode;
end;

function TRBTree.AllocNode: PRBTreeNode;
begin
  if FNodePool <> nil then
    Result := FNodePool.AllocNode()
  else
    New(Result);

  StuffNode(Result);
  Result^.Value := nil;
end;

procedure TRBTree.DisposeNode(aNode: PRBTreeNode);
begin
  if FNodePool <> nil then
    FNodePool.DisposeNode(aNode)
  else
    DisposeRBTreeNode(aNode);
end;

constructor TRBTree.Create;
begin
  inherited Create;
  FCount := 0;
  FNodePool := nil;
  BlackNode(@FSentinel);
  FRootNode := @FSentinel;
  FCompareCB := @DoDefaultCompare;
end;

constructor TRBTree.Create(aCompareCB: TRBTreeCompareCB);
begin
  Create;
  FCompareCB := aCompareCB;
end;

destructor TRBTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TRBTree.IsEmpty: boolean;
begin
  Result := (GetCount <= 0);
end;

procedure TRBTree.Clear;
var
  LSentinel: PRBTreeNode;

  procedure DeleteNode(aNode: PRBTreeNode);
  begin
    if aNode <> nil then
    begin
      if (aNode^.Left <> nil) and (aNode^.Left <> LSentinel) then
        DeleteNode(aNode^.Left);
      if (aNode^.Right <> nil) and (aNode^.Right <> LSentinel) then
        DeleteNode(aNode^.Right);
    end;
    if (aNode <> LSentinel) then
      DisposeNode(aNode);
  end;

begin
  LSentinel := @FSentinel;
  DeleteNode(FRootNode);
  FRootNode := LSentinel;
  FCount := 0;
end;

function TRBTree.Insert(aKey: Pointer; aValue: Pointer): PRBTreeNode;
begin
  Result := AllocNode;
  Result^.Key := aKey;
  InternalInsert(Result);
  DoUpdateNode(Result,aValue);
  Inc(FCount);
end;

function TRBTree.Find(aRoot: PRBTreeNode; aKey: Pointer): PRBTreeNode;
var
  LNode: PRBTreeNode;
  LCompareResult: PtrInt;
begin
  LNode := aRoot;
  while LNode <> @FSentinel do
  begin
    LCompareResult := DoCompare(aKey, LNode^.Key);
    if LCompareResult < 0 then
      LNode := LNode^.Left
    else if LCompareResult > 0 then
      LNode := LNode^.Right
    else
      exit(LNode);
  end;
  Result := nil;
end;

function TRBTree.Find(aKey: Pointer): PRBTreeNode;
begin
  Result := Find(Root, aKey);
end;

function TRBTree.RemoveNode(aRoot: PRBTreeNode; aKey: Pointer): PRBTreeNode;
begin
  Result := Find(aRoot, aKey);
  if Result <> nil then
    DisconnectNode(Result);
end;

function TRBTree.RemoveNode(aKey: Pointer): PRBTreeNode;
begin
  Result := RemoveNode(Root, aKey);
end;

function TRBTree.Remove(aRoot: PRBTreeNode; aKey: Pointer): Pointer;
var
  LNode: PRBTreeNode;
begin
  LNode := RemoveNode(aRoot, aKey);
  if LNode <> nil then
  begin
    Result := LNode^.Value;
    DoUpdateNode(LNode,nil);
    DisposeNode(LNode);
  end
  else
    Result := nil;
end;

function TRBTree.Remove(aKey: Pointer): Pointer;
begin
  Result := Remove(Root, aKey);
end;

procedure TRBTree.Delete(aNode: PRBTreeNode);
begin
  DisconnectNode(aNode);
  DisposeNode(aNode);
end;

function TRBTree.FindSuccessor(aNode: PRBTreeNode): PRBTreeNode;
var
  LNode, LSentinel, LParent: PRBTreeNode;
begin
  LNode := aNode;
  LSentinel := @FSentinel;

  if LNode^.Right <> LSentinel then
    exit(FindLowest(LNode^.Right));

  while True do
  begin
    LParent := LNode^.Parent;

    if LNode = FRootNode then
      exit(nil);

    if LNode = LParent^.Left then
      exit(LParent);

    LNode := LParent;
  end;
end;

function TRBTree.FindPrecessor(aNode: PRBTreeNode): PRBTreeNode;
var
  LNode, LSentinel, LParent: PRBTreeNode;
begin
  LNode := aNode;
  LSentinel := @FSentinel;

  if LNode^.Left <> LSentinel then
    exit(FindHighest(LNode^.Left));

  while True do
  begin
    LParent := LNode^.Parent;

    if LNode = FRootNode then
      exit(nil);

    if LNode = LParent^.Right then
      exit(LParent);

    LNode := LParent;
  end;
end;


function TRBTree.FindLowest(aRoot: PRBTreeNode): PRBTreeNode;
var
  LSentinel: PRBTreeNode;
begin
  Result := aRoot;
  LSentinel := @FSentinel;
  while (Result <> nil) and (Result^.Left <> LSentinel) do
    Result := Result^.Left;
end;

function TRBTree.FindLowest: PRBTreeNode;
begin
  Result := FindLowest(FRootNode);
end;

function TRBTree.FindHighest(aRoot: PRBTreeNode): PRBTreeNode;
var
  LSentinel: PRBTreeNode;
begin
  Result := aRoot;
  LSentinel := @FSentinel;
  while (Result <> nil) and (Result^.Right <> LSentinel) do
    Result := Result^.Right;
end;

function TRBTree.FindHighest: PRBTreeNode;
begin
  Result := FindHighest(FRootNode);
end;

function TRBTree.GetEnumerator: TRBTreeNodeEnumerator;
begin
  Result := TRBTreeNodeEnumerator.Create(Self, True);
end;

function TRBTree.GetEnumeratorHighToLow: TRBTreeNodeEnumerator;
begin
  Result := TRBTreeNodeEnumerator.Create(Self, False);
end;

{ TStringRBTreeBase }

function TStringRBTreeBase.GetHashCB: THashCB;
begin
  Result := FHashCB;
end;

constructor TStringRBTreeBase.Create();
begin
  inherited Create(TRBTreeCompareCB(@DoStringCompare));
  FHashCB := @CRC32_LONG;
end;

constructor TStringRBTreeBase.Create(aHashCB: THashCB);
begin
  Create;
  FHashCB := aHashCB;
end;

{ TStringRBTreeNodeEnumerator }

function TStringRBTreeNodeEnumerator.GetCurrent: PStringRBTreeNode;
begin
  Result := PStringRBTreeNode(FCurrent);
end;

function TStringRBTree.GetRoot: PStringRBTreeNode;
begin
  Result := PStringRBTreeNode(inherited GetRoot);
end;

procedure TStringRBTree.DisposeNode(aNode: PRBTreeNode);
begin
  Dispose(PStringRBTreeNode(aNode)^.Key);
  inherited DisposeNode(aNode);
end;

function TStringRBTree.CreateKey(const aKey: string): PStringKey;
begin
  New(Result);
  SetKey(aKey, Result);
end;

procedure TStringRBTree.SetKey(const aKeyStr: string; aKey: PStringKey);
begin
  with aKey^ do
  begin
    KeyHash := DoHash(aKeyStr);
    Key := aKeyStr;
  end;
end;

function TStringRBTree.DoHash(const aStr: string): integer;
begin
  Result := FHashCB(aStr);
end;

function TStringRBTree.Insert(const aKey: string; aValue: Pointer): PStringRBTreeNode;
begin
  Result := PStringRBTreeNode(inherited Insert(CreateKey(aKey), aValue));
end;

function TStringRBTree.Find(aRoot: PStringRBTreeNode; const aKey: string): PStringRBTreeNode;
begin
  SetKey(aKey, @FTmpKey);
  Result := PStringRBTreeNode(inherited Find(PRBTreeNode(aRoot), @FTmpKey));
end;

function TStringRBTree.Find(const aKey: string): PStringRBTreeNode;
begin
  Result := Find(Root, aKey);
end;

function TStringRBTree.RemoveNode(aRoot: PStringRBTreeNode; const aKey: string): PStringRBTreeNode;
begin
  SetKey(aKey, @FTmpKey);
  Result := PStringRBTreeNode(inherited RemoveNode(PRBTreeNode(aRoot), @FTmpKey));
end;

function TStringRBTree.RemoveNode(const aKey: string): PStringRBTreeNode;
begin
  Result := RemoveNode(Root, aKey);
end;

function TStringRBTree.Remove(aRoot: PStringRBTreeNode; const aKey: string): Pointer;
begin
  SetKey(aKey, @FTmpKey);
  Result := inherited Remove(PRBTreeNode(aRoot), @FTmpKey);
end;

function TStringRBTree.Remove(const aKey: string): Pointer;
begin
  Result := Remove(Root, aKey);
end;

procedure TStringRBTree.Delete(aNode: PStringRBTreeNode);
begin
  inherited Delete(PRBTreeNode(aNode));
end;

function TStringRBTree.FindSuccessor(aNode: PStringRBTreeNode): PStringRBTreeNode;
begin
  Result := PStringRBTreeNode(inherited FindSuccessor(PRBTreeNode(aNode)));
end;

function TStringRBTree.FindPrecessor(aNode: PStringRBTreeNode): PStringRBTreeNode;
begin
  Result := PStringRBTreeNode(inherited FindPrecessor(PRBTreeNode(aNode)));
end;

function TStringRBTree.FindLowest(aRoot: PStringRBTreeNode): PStringRBTreeNode;
begin
  Result := PStringRBTreeNode(inherited FindLowest(PRBTreeNode(aRoot)));
end;

function TStringRBTree.FindLowest: PStringRBTreeNode;
begin
  Result := PStringRBTreeNode(inherited FindLowest());
end;

function TStringRBTree.FindHighest(aRoot: PStringRBTreeNode): PStringRBTreeNode;
begin
  Result := PStringRBTreeNode(inherited FindHighest(PRBTreeNode(aRoot)));
end;

function TStringRBTree.FindHighest: PStringRBTreeNode;
begin
  Result := PStringRBTreeNode(inherited FindHighest());
end;

function TStringRBTree.GetEnumerator: TStringRBTreeNodeEnumerator;
begin
  Result := TStringRBTreeNodeEnumerator.Create(Self, True);
end;

function TStringRBTree.GetEnumeratorHighToLow: TStringRBTreeNodeEnumerator;
begin
  Result := TStringRBTreeNodeEnumerator.Create(Self, False);
end;

{ TStringObjectRBTreeNodeEnumerator }

function TStringObjectRBTreeNodeEnumerator.GetCurrent: PStringObjectRBTreeNode;
begin
  Result := PStringObjectRBTreeNode(FCurrent);
end;

function TStringObjectRBTree.GetRoot: PStringObjectRBTreeNode;
begin
  Result := PStringObjectRBTreeNode(FRootNode);
end;

function TStringObjectRBTree.GetFreeObjects: boolean;
begin
  Result := FFreeObjects;
end;

procedure TStringObjectRBTree.SetFreeObjects(AValue: boolean);
begin
  FFreeObjects := AValue;
end;

procedure TStringObjectRBTree.DisposeNode(aNode: PRBTreeNode);
var
  LNode: PStringObjectRBTreeNode;
begin
  if FFreeObjects then
  begin
    LNode := PStringObjectRBTreeNode(aNode);
    if (LNode^.Value <> nil) then
      FreeAndNil(LNode^.Value);
  end;
  inherited DisposeNode(aNode);
end;

constructor TStringObjectRBTree.Create;
begin
  inherited Create;
  FFreeObjects := True;
end;

constructor TStringObjectRBTree.Create(aFreeObjects: boolean);
begin
  Create;
  FFreeObjects := aFreeObjects;
end;

constructor TStringObjectRBTree.Create(aHashCB: THashCB; aFreeObjects: boolean);
begin
  inherited Create(aHashCB);
  FFreeObjects := aFreeObjects;
end;

function TStringObjectRBTree.Insert(const aKey: string; aObj: TObject): PStringObjectRBTreeNode;
begin
  Result := PStringObjectRBTreeNode(inherited Insert(aKey, Pointer(aObj)));
end;

function TStringObjectRBTree.Find(aRoot: PStringObjectRBTreeNode; const aKey: string): PStringObjectRBTreeNode;
begin
  Result := PStringObjectRBTreeNode(inherited Find(PStringRBTreeNode(aRoot), aKey));
end;

function TStringObjectRBTree.Find(const aKey: string): PStringObjectRBTreeNode;
begin
  Result := Find(Root, aKey);
end;

function TStringObjectRBTree.RemoveNode(aRoot: PStringObjectRBTreeNode; const aKey: string): PStringObjectRBTreeNode;
begin
  Result := PStringObjectRBTreeNode(inherited RemoveNode(PStringRBTreeNode(aRoot), aKey));
end;

function TStringObjectRBTree.RemoveNode(const aKey: string): PStringObjectRBTreeNode;
begin
  Result := RemoveNode(Root, aKey);
end;

function TStringObjectRBTree.Remove(aRoot: PStringObjectRBTreeNode; const aKey: string): TObject;
begin
  Result := TObject(inherited Remove(PStringRBTreeNode(aRoot), aKey));
end;

function TStringObjectRBTree.Remove(const aKey: string): TObject;
begin
  Result := Remove(Root, aKey);
end;

procedure TStringObjectRBTree.Delete(aNode: PStringObjectRBTreeNode);
begin
  inherited Delete(PStringRBTreeNode(aNode));
end;

function TStringObjectRBTree.FindSuccessor(aNode: PStringObjectRBTreeNode): PStringObjectRBTreeNode;
begin
  Result := PStringObjectRBTreeNode(inherited FindSuccessor(PStringRBTreeNode(aNode)));
end;

function TStringObjectRBTree.FindPrecessor(aNode: PStringObjectRBTreeNode): PStringObjectRBTreeNode;
begin
  Result := PStringObjectRBTreeNode(inherited FindPrecessor(PStringRBTreeNode(aNode)));
end;

function TStringObjectRBTree.FindLowest(aRoot: PStringObjectRBTreeNode): PStringObjectRBTreeNode;
begin
  Result := PStringObjectRBTreeNode(inherited FindLowest(PStringRBTreeNode(aRoot)));
end;

function TStringObjectRBTree.FindLowest: PStringObjectRBTreeNode;
begin
  Result := FindLowest(Root);
end;

function TStringObjectRBTree.FindHighest(aRoot: PStringObjectRBTreeNode): PStringObjectRBTreeNode;
begin
  Result := PStringObjectRBTreeNode(inherited FindHighest(PStringRBTreeNode(aRoot)));
end;

function TStringObjectRBTree.FindHighest: PStringObjectRBTreeNode;
begin
  Result := FindHighest(Root);
end;

function TStringObjectRBTree.GetEnumerator: TStringObjectRBTreeNodeEnumerator;
begin
  Result := TStringObjectRBTreeNodeEnumerator.Create(Self, True);
end;

function TStringObjectRBTree.GetEnumeratorHighToLow: TStringObjectRBTreeNodeEnumerator;
begin
  Result := TStringObjectRBTreeNodeEnumerator.Create(Self, False);
end;

{ TStringPairRBTreeNodeEnumerator }

function TStringPairRBTreeNodeEnumerator.GetCurrent: PStringPairRBTreeNode;
begin
  Result := PStringPairRBTreeNode(FCurrent);
end;

function TStringPairRBTree.GetRoot: PStringPairRBTreeNode;
begin
  Result := PStringPairRBTreeNode(FRootNode);
end;

procedure TStringPairRBTree.DoUpdateNode(aNode: PRBTreeNode; aValue: Pointer);
begin
  PStringPairRBTreeNode(aNode)^.Value:=string(aValue);
end;

procedure TStringPairRBTree.DisposeNode(aNode: PRBTreeNode);
begin
  PStringPairRBTreeNode(aNode)^.Value := '';
  inherited DisposeNode(aNode);
end;

function TStringPairRBTree.Insert(const aKey, aValue: string): PStringPairRBTreeNode;
begin
  Result := PStringPairRBTreeNode(inherited Insert(aKey, Pointer(aValue)));
end;

function TStringPairRBTree.Find(aRoot: PStringPairRBTreeNode; const aKey: string): PStringPairRBTreeNode;
begin
  Result := PStringPairRBTreeNode(inherited Find(PStringRBTreeNode(aRoot), aKey));
end;

function TStringPairRBTree.Find(const aKey: string): PStringPairRBTreeNode;
begin
  Result := Find(Root, aKey);
end;

function TStringPairRBTree.RemoveNode(aRoot: PStringPairRBTreeNode; const aKey: string): PStringPairRBTreeNode;
begin
  Result := PStringPairRBTreeNode(inherited RemoveNode(PStringRBTreeNode(aRoot), aKey));
end;

function TStringPairRBTree.RemoveNode(const aKey: string): PStringPairRBTreeNode;
begin
  Result := RemoveNode(Root, aKey);
end;

function TStringPairRBTree.Remove(aRoot: PStringPairRBTreeNode; const aKey: string): string;
begin
  Result := string(inherited Remove(PStringRBTreeNode(aRoot), aKey));
end;

function TStringPairRBTree.Remove(const aKey: string): string;
begin
  Result := Remove(Root, aKey);
end;

procedure TStringPairRBTree.Delete(aNode: PStringPairRBTreeNode);
begin
  inherited Delete(PStringRBTreeNode(aNode));
end;

function TStringPairRBTree.FindSuccessor(aNode: PStringPairRBTreeNode): PStringPairRBTreeNode;
begin
  Result := PStringPairRBTreeNode(inherited FindSuccessor(PStringRBTreeNode(aNode)));
end;

function TStringPairRBTree.FindPrecessor(aNode: PStringPairRBTreeNode): PStringPairRBTreeNode;
begin
  Result := PStringPairRBTreeNode(inherited FindPrecessor(PStringRBTreeNode(aNode)));
end;

function TStringPairRBTree.FindLowest(aRoot: PStringPairRBTreeNode): PStringPairRBTreeNode;
begin
  Result := PStringPairRBTreeNode(inherited FindLowest(PStringRBTreeNode(aRoot)));
end;

function TStringPairRBTree.FindLowest: PStringPairRBTreeNode;
begin
  Result := FindLowest(Root);
end;

function TStringPairRBTree.FindHighest(aRoot: PStringPairRBTreeNode): PStringPairRBTreeNode;
begin
  Result := PStringPairRBTreeNode(inherited FindHighest(PStringRBTreeNode(aRoot)));
end;

function TStringPairRBTree.FindHighest: PStringPairRBTreeNode;
begin
  Result := FindHighest(Root);
end;

function TStringPairRBTree.GetEnumerator: TStringPairRBTreeNodeEnumerator;
begin
  Result := TStringPairRBTreeNodeEnumerator.Create(Self, True);
end;

function TStringPairRBTree.GetEnumeratorHighToLow: TStringPairRBTreeNodeEnumerator;
begin
  Result := TStringPairRBTreeNodeEnumerator.Create(Self, False);
end;

{ TRBTreeNodePool }

function TRBTreeNodePool.GetCount: integer;
begin
  Result := FCount;
end;

function TRBTreeNodePool.GetMaxCount: integer;
begin
  Result := FMaxCount;
end;

procedure TRBTreeNodePool.SetMaxCount(AValue: integer);
var
  LNode: PRBTreeNode;
begin
  if FCount > AValue then
  begin
    while (FCount > AValue) and (FEntry <> nil) do
    begin
      LNode := FEntry;
      FEntry := FEntry^.Right;
      Dispose(LNode);
      Dec(FCount);
    end;
  end;
  FMaxCount := AValue;
end;

class destructor TRBTreeNodePool.Destroy;
begin
  if FDefaultInstance <> nil then
    FDefaultInstance.Free;
end;

class function TRBTreeNodePool.GetDefaultInstance: TRBTreeNodePool;
begin
  if FDefaultInstance = nil then
    FDefaultInstance := TRBTreeNodePool.Create;
  Result := FDefaultInstance;
end;

constructor TRBTreeNodePool.Create;
begin
  inherited Create;
  FEntry := nil;
  FCount := 0;
  FMaxCount := DEFAULT_RBTREENODEPOOL_MAXCOUNT;
end;

constructor TRBTreeNodePool.Create(aMaxCount: integer);
begin
  Create;
  FMaxCount := aMaxCount;
end;

destructor TRBTreeNodePool.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TRBTreeNodePool.Clear;
var
  LNext: PRBTreeNode;
begin
  if FEntry <> nil then
  begin
    while FEntry <> nil do
    begin
      LNext := FEntry^.Right;
      Dispose(FEntry);
      FEntry := LNext;
    end;
  end;
  FCount := 0;
end;

function TRBTreeNodePool.AllocNode: PRBTreeNode;
begin
  if FEntry <> nil then
  begin
    Result := FEntry;
    FEntry := FEntry^.Right;
    Dec(FCount);
  end
  else
    New(Result);
end;

procedure TRBTreeNodePool.DisposeNode(aNode: PRBTreeNode);
begin
  if FCount < FMaxCount then
  begin
    aNode^.Right := FEntry;
    FEntry := aNode;
    Inc(FCount);
  end
  else
    Dispose(aNode);
end;

end.
