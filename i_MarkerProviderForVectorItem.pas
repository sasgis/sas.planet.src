unit i_MarkerProviderForVectorItem;

interface

uses
  i_MarkerDrawable,
  i_MarksDrawConfig,
  i_VectorDataItemSimple;

type
  IMarkerProviderForVectorItem = interface
    ['{1E439CF5-173A-401A-A67A-B501F133FD01}']
    function GetMarker(
      const AConfig: ICaptionDrawConfigStatic;
      const AItem: IVectorDataItemSimple
    ): IMarkerDrawable;
  end;

implementation

end.
