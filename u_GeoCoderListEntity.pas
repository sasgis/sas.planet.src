unit u_GeoCoderListEntity;

interface

uses
  i_GeoCoder,
  i_IGeoCoderList;

type
  TGeoCoderListEntity = class(TInterfacedObject, IGeoCoderListEntity)
  private
    FGUID: TGUID;
    FCaption: WideString;
    FGeoCoder: IGeoCoder;
  protected
    function GetGUID: TGUID;
    function GetCaption: WideString;
    function GetGeoCoder: IGeoCoder;
  public
    constructor Create(
      AGUID: TGUID;
      ACaption: WideString;
      AGeoCoder: IGeoCoder
    );
  end;

implementation

{ TGeoCoderListEntity }

constructor TGeoCoderListEntity.Create(AGUID: TGUID; ACaption: WideString;
  AGeoCoder: IGeoCoder);
begin
  FGUID := AGUID;
  FCaption := ACaption;
  FGeoCoder := AGeoCoder;
end;

function TGeoCoderListEntity.GetCaption: WideString;
begin
  Result := FCaption;
end;

function TGeoCoderListEntity.GetGeoCoder: IGeoCoder;
begin
  Result := FGeoCoder;
end;

function TGeoCoderListEntity.GetGUID: TGUID;
begin
  Result := FGUID;
end;

end.
