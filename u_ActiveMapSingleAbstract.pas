unit u_ActiveMapSingleAbstract;

interface

uses
  i_JclNotify,
  i_MapTypes,
  i_IConfigDataElement,
  i_IActiveMapsConfig;

type
  TActiveMapSingleAbstract = class(TInterfacedObject, IActiveMapSingle)
  private
    FMapType: IMapType;
  protected
    function GetMapType: IMapType;
    function GetIsActive: Boolean; virtual; abstract;
  public
    constructor Create(AMapType: IMapType);
    destructor Destroy; override;
  end;

implementation

end.
