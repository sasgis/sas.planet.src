unit i_LastResponseInfo;

interface

uses
  i_ConfigDataElement;

type
  ILastResponseInfo = interface(IConfigDataElement)
    ['{42E58C67-DC1F-4077-8E23-C65785AC3C37}']
    function GetResponseHead: string;
    procedure SetResponseHead(const AValue: string);
    property ResponseHead: string read GetResponseHead write SetResponseHead;
  end;

implementation

end.
