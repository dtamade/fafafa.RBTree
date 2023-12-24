unit ExampleUtils;

{$Codepage UTF8}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fafafa.RBTree;

type

  PTestPair = ^TTestPair;

  TTestPair = record
    Key, Value: string;
  end;

const

  TestPairs: array[0..30] of TTestPair = (
    (key: 'china'; Value: '中华人民共和国'),
    (key: 'Japanese'; Value: '日本子'),
    (key: 'USA'; Value: '美国'),
    (key: 'Italy'; Value: '意大利'),
    (key: 'Germany'; Value: '德国'),
    (key: 'France'; Value: '法国'),
    (key: 'Afghanistan'; Value: '阿富汗'),
    (key: 'Gibraltar'; Value: '直布罗陀'),
    (key: 'Greece'; Value: '希腊'),
    (key: 'Grenada'; Value: '格林纳达'),
    (key: 'Guatemala'; Value: '危地马拉'),
    (key: 'Guinea'; Value: '几内亚'),
    (key: 'Haiti'; Value: '海地'),
    (key: 'Honduras'; Value: '洪都拉斯'),
    (key: 'Hungary'; Value: '匈牙利'),
    (key: 'Iceland'; Value: '冰岛'),
    (key: 'India'; Value: '印度'),
    (key: 'Indonesia'; Value: '印度尼西亚'),
    (key: 'Iran'; Value: '伊朗'),
    (key: 'Iraq'; Value: '伊拉克'),
    (key: 'Ireland'; Value: '爱尔兰'),
    (key: 'Israel'; Value: '以色列'),
    (key: 'Jamaica'; Value: '牙买加'),
    (key: 'Kazakhstan'; Value: '哈萨克斯坦'),
    (key: 'Kenya'; Value: '肯尼亚'),
    (key: 'Korea'; Value: '韩国'),
    (key: 'Korea (North)'; Value: '朝鲜'),
    (key: 'Lazarus'; Value: '辣子肉丝'),
    (key: 'freePascal'; Value: '辣子黑丝'),
    (key: 'fafafa'; Value: '發發發'),
    (key: 'LOL mobile'; Value: '经济全被你拿了')
    );

function Hash(const aStr: string): integer;

implementation

function Hash(const aStr: string): integer;
begin
  Result := integer(CRC32_LONG(aStr));
end;


end.
