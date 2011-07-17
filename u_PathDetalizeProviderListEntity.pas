unit u_PathDetalizeProviderListEntity;

interface

uses
  t_GeoTypes,
  i_PathDetalizeProvider,
  u_UserInterfaceItemBase,
  i_PathDetalizeProviderList;

type
  TPathDetalizeProviderListEntity = class(TUserInterfaceItemBase, IPathDetalizeProviderListEntity, IPathDetalizeProvider)
  protected { IPathDetalizeProviderListEntity }
    function GetProvider: IPathDetalizeProvider;
  protected { IPathDetalizeProvider }
    function GetPath(ASource: TArrayOfDoublePoint; var AComment: string): TArrayOfDoublePoint; virtual; abstract;
  end;


implementation

{ TPathDetalizeProviderListEntity }

function TPathDetalizeProviderListEntity.GetProvider: IPathDetalizeProvider;
begin
  Result := Self;
end;

end.
