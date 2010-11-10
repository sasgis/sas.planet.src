unit u_ContentConverterKmz2Kml;

interface

uses
  Classes,
  i_ContentTypeInfo,
  u_ContentConverterBase;

type
  TContentConverterKmz2Kml = class(TContentConverterBase)
  protected
    procedure ConvertStream(ASource, ATarget: TStream); override;
  end;

implementation

{ TContentConverterKmz2Kml }

procedure TContentConverterKmz2Kml.ConvertStream(ASource, ATarget: TStream);
begin
  inherited;

end;

end.
