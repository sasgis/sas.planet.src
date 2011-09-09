unit i_TilePostDownloadCropConfig;

interface

uses
  Types;

type
  ITilePostDownloadCropConfigStatic = interface
    ['{D7C63FFC-132A-4E6D-ABD0-CC3935A64D0F}']
    function GetIsCropOnDownload: Boolean;
    property IsCropOnDownload: Boolean read GetIsCropOnDownload;

    function GetCropRect: TRect;
    property CropRect: TRect read GetCropRect;
  end;

implementation

end.
