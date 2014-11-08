unit i_TextByVectorItem;

interface

uses
  i_VectorDataItemSimple;

type
  ITextByVectorItem = interface
    ['{819B9C8D-FF92-4AD7-A9B2-7E38FBBC558E}']
    function GetText(const AItem: IVectorDataItem): string;
  end;

implementation

end.
