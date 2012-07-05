unit i_FindVectorItems;

interface

uses
  Types,
  i_LocalCoordConverter,
  i_VectorDataItemSimple;

type
  IFindVectorItems = interface
    ['{083071F7-53EB-4257-A445-86E72A9914F0}']
    function FindItem(
      const AVisualConverter: ILocalCoordConverter;
      const ALocalPoint: TPoint;
      out AItemS: Double
    ): IVectorDataItemSimple;
  end;

implementation

end.
