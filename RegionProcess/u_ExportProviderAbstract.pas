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
    FParent: TWinControl;
  public
    constructor Create(AParent: TWinControl);
    function GetCaption: string; virtual; abstract;
    procedure InitFrame(Azoom: byte; APolygon: TArrayOfDoublePoint); virtual; abstract;
    procedure Show; virtual; abstract;
    procedure Hide; virtual; abstract;
    procedure RefreshTranslation; virtual; abstract;
    procedure StartProcess(APolygon: TArrayOfDoublePoint); virtual; abstract;
  end;

implementation

{ TExportProviderAbstract }

constructor TExportProviderAbstract.Create(AParent: TWinControl);
begin
  FParent := AParent;
end;

end.
