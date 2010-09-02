unit u_ThreadExportAbstract;

interface

uses
  Classes,
  t_GeoTypes,
  u_ThreadRegionProcessAbstract,
  UResStrings;

type
  TThreadExportAbstract = class(TThreadRegionProcessAbstract)
  protected
    FZoomArr: array [0..23] of boolean;
    FZooms: array of Byte;
    procedure ProgressFormUpdateOnProgress; virtual;
  public
    constructor Create(
      APolygon: TExtendedPointArray;
      Azoomarr: array of boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

constructor TThreadExportAbstract.Create(APolygon: TExtendedPointArray;
  Azoomarr: array of boolean);
var
  i: Integer;
  VZoomCount: Integer;
begin
  inherited Create(APolygon);
  SetLength(FZooms, 24);
  VZoomCount := 0;
  for i := 0 to 23 do begin
    FZoomArr[i] := Azoomarr[i];
    if Azoomarr[i] then begin
      FZooms[VZoomCount] := i;
      Inc(VZoomCount);
    end;
  end;
  SetLength(FZooms, VZoomCount);
end;

destructor TThreadExportAbstract.Destroy;
begin
  inherited;
  FZooms := nil;
end;

procedure TThreadExportAbstract.ProgressFormUpdateOnProgress;
begin
  ProgressFormUpdateProgressAndLine1(
    round((FTilesProcessed / FTilesToProcess) * 100),
    SAS_STR_Processed + ' ' + inttostr(FTilesProcessed)
  );
end;

end.
