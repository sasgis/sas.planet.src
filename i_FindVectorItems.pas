unit i_FindVectorItems;

interface

uses
  Types,
  i_LocalCoordConverter,
  i_VectorItemSubset;

type
  IFindVectorItems = interface
    ['{083071F7-53EB-4257-A445-86E72A9914F0}']
    function FindItems(
      const AVisualConverter: ILocalCoordConverter;
      const ALocalPoint: TPoint
    ): IVectorItemSubset;
  end;

implementation

end.
