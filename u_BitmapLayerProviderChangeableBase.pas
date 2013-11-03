unit u_BitmapLayerProviderChangeableBase;

interface

uses
  i_BitmapLayerProvider,
  i_BitmapLayerProviderChangeable,
  i_ListenerNotifierLinksList,
  u_ConfigDataElementBase;

type
  TBitmapLayerProviderChangeableBase = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IBitmapLayerProviderChangeable)
  private
    FLinksList: IListenerNotifierLinksList;
  private
    function GetStatic: IBitmapLayerProvider;

  protected
    property LinksList: IListenerNotifierLinksList read FLinksList;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    constructor Create;
  end;

implementation

uses
  u_ListenerNotifierLinksList;

{ TBitmapLayerProviderChangeableBase }

constructor TBitmapLayerProviderChangeableBase.Create;
begin
  inherited Create;

  FLinksList := TListenerNotifierLinksList.Create;
end;

procedure TBitmapLayerProviderChangeableBase.AfterConstruction;
begin
  inherited;
  FLinksList.ActivateLinks;
end;

procedure TBitmapLayerProviderChangeableBase.BeforeDestruction;
begin
  inherited;
  FLinksList.DeactivateLinks;
end;

function TBitmapLayerProviderChangeableBase.GetStatic: IBitmapLayerProvider;
begin
  Result := IBitmapLayerProvider(GetStaticInternal);
end;

end.
