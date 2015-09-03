unit i_ProjectionSet;

interface

uses
  t_Hash,
  i_CoordConverter,
  i_ProjectionInfo;

type
  IProjectionSet = interface
    ['{4B23F7C1-A818-459C-BCD8-F28BA96EEC82}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function IsSame(const AProjectionSet: IProjectionSet): Boolean;

    function GetZoomCount: Byte;
    property ZoomCount: Byte read GetZoomCount;

    function GetZoom(const AIndex: Byte): IProjectionInfo;
    property Zooms[const AIndex: Byte]: IProjectionInfo read GetZoom; default;

    function GetSuitableProjection(const AProjection: IProjectionInfo): IProjectionInfo;
    function GetSuitableZoom(const AProjection: IProjectionInfo): Byte;

    function GetGeoConvert: ICoordConverter; // TODO: Deleate later
    property GeoConvert: ICoordConverter read GetGeoConvert; // TODO: Deleate later
  end;

implementation

end.
