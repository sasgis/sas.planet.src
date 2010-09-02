unit u_ThreadExportAbstract;

interface

uses
  Classes,
  Forms,
  t_GeoTypes,
  unit4,
  u_ThreadRegionProcessAbstract,
  UResStrings;

type
  TThreadExportAbstract = class(TThreadRegionProcessAbstract)
  protected
    FZoomArr: array [0..23] of boolean;
    procedure ProgressFormUpdateOnProgress; virtual;
  public
    constructor Create(
      APolygon: TExtendedPointArray;
      Azoomarr: array of boolean
    );
  end;

implementation

uses
  SysUtils,
  Dialogs;

constructor TThreadExportAbstract.Create(APolygon: TExtendedPointArray;
  Azoomarr: array of boolean);
var
  i: Integer;
begin
  inherited Create(APolygon);
  for i := 0 to 23 do begin
    FZoomArr[i] := Azoomarr[i];
  end;
end;

procedure TThreadExportAbstract.ProgressFormUpdateOnProgress;
begin
  ProgressFormUpdateProgressAndLine1(
    round((FTilesProcessed / FTilesToProcess) * 100),
    SAS_STR_Processed + ' ' + inttostr(FTilesProcessed)
  );
end;

end.
