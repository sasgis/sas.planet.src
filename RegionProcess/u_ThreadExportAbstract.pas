unit u_ThreadExportAbstract;

interface

uses
  Classes,
  i_VectorItemLonLat,
  i_OperationNotifier,
  i_RegionProcessProgressInfo,
  u_ThreadRegionProcessAbstract,
  u_ResStrings;

type
  TThreadExportAbstract = class(TThreadRegionProcessAbstract)
  protected
    FZooms: array of Byte;
    procedure ProgressFormUpdateOnProgress(AProcessed, AToProcess: Int64);
    procedure ProcessRegion; override;
  public
    constructor Create(
      const ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      const AProgressInfo: IRegionProcessProgressInfo;
      const APolygon: ILonLatPolygon;
      const Azoomarr: array of boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

constructor TThreadExportAbstract.Create(
  const ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  const AProgressInfo: IRegionProcessProgressInfo;
  const APolygon: ILonLatPolygon;
  const Azoomarr: array of boolean
);
var
  i: Integer;
  VZoomCount: Integer;
begin
  inherited Create(
    ACancelNotifier,
    AOperationID,
    AProgressInfo,
    APolygon
  );
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
  if Length(FZooms) <= 0 then begin
    raise Exception.Create('Не выбрано ни одного зума');
  end;
end;

procedure TThreadExportAbstract.ProgressFormUpdateOnProgress(AProcessed, AToProcess: Int64);
begin
  ProgressInfo.ProcessedRatio := AProcessed / AToProcess;
  ProgressInfo.SecondLine := SAS_STR_Processed + ' ' + inttostr(AProcessed);
end;

end.
