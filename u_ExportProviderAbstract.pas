unit u_ExportProviderAbstract;

interface

uses
  Classes,
  Controls,
  Forms,
  t_GeoTypes;

type
  TExportProviderAbstract = class
  protected
    FOwner: TComponent;
    FParent: TWinControl;
  public
    constructor Create(AOwner: TComponent; AParent: TWinControl);
    function GetCaption: string; virtual; abstract;
    function GetDialogFrame(Azoom: byte): TFrame; virtual; abstract;
    procedure StartProcess(APolygon: TExtendedPointArray); virtual; abstract;
  end;
implementation

{ TExportProviderAbstract }

constructor TExportProviderAbstract.Create(AOwner: TComponent; AParent: TWinControl);
begin
  FOwner := AOwner;
  FParent := AParent;
end;

end.
