unit u_TilePostDownloadCropConfigStatic;

interface

uses
  Types,
  i_TilePostDownloadCropConfig;

type
  TTilePostDownloadCropConfigStatic = class(TInterfacedObject, ITilePostDownloadCropConfigStatic)
  private
    FIsCropOnDownload: Boolean;
    FCropRect: TRect;
  protected
    function GetIsCropOnDownload: Boolean;
    function GetCropRect: TRect;
  public
    constructor Create(
      ACropRect: TRect
    );
  end;

implementation

{ TTilePostDownloadCropConfigStatic }

constructor TTilePostDownloadCropConfigStatic.Create(ACropRect: TRect);
begin
  if
    (ACropRect.Left >= 0) and
    (ACropRect.Top >= 0) and
    (ACropRect.Right > ACropRect.Left) and
    (ACropRect.Bottom > ACropRect.Top) and
    (ACropRect.Right < 10000) and
    (ACropRect.Bottom < 10000)
  then begin
    FIsCropOnDownload := True;
    FCropRect := ACropRect;
  end else begin
    FIsCropOnDownload := False;
    FCropRect := Rect(0, 0, 0, 0);
  end;
end;

function TTilePostDownloadCropConfigStatic.GetCropRect: TRect;
begin
  Result := FCropRect;
end;

function TTilePostDownloadCropConfigStatic.GetIsCropOnDownload: Boolean;
begin
  Result := FIsCropOnDownload;
end;

end.
