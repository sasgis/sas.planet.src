unit u_ThreadExportAbstract;

interface

uses
  Classes,
  i_VectorItemLonLat,
  u_ThreadRegionProcessAbstract,
  u_ResStrings;

type
  TThreadExportAbstract = class(TThreadRegionProcessAbstract)
  protected
    FZooms: array of Byte;
    procedure ProgressFormUpdateOnProgress; virtual;
    procedure ProcessRegion; override;
  public
    constructor Create(
      APolygon: ILonLatPolygonLine;
      Azoomarr: array of boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

constructor TThreadExportAbstract.Create(APolygon: ILonLatPolygonLine;
  Azoomarr: array of boolean);
var
  i: Integer;
  VZoomCount: Integer;
begin
  inherited Create(APolygon);
  SetLength(FZooms, 24);
  VZoomCount := 0;
  for i := 0 to 23 do begin
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

procedure TThreadExportAbstract.ProcessRegion;
begin
  inherited;
  if Length(FZooms) <= 0 then
    raise Exception.Create('Не выбрано ни одного зума');
end;

procedure TThreadExportAbstract.ProgressFormUpdateOnProgress;
begin
  ProgressFormUpdateProgressAndLine1(
    round((FTilesProcessed / FTilesToProcess) * 100),
    SAS_STR_Processed + ' ' + inttostr(FTilesProcessed)
  );
end;

end.
