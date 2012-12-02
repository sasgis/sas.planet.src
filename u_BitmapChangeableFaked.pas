unit u_BitmapChangeableFaked;

interface

uses
  i_Notifier,
  i_Bitmap32Static,
  u_BaseInterfacedObject;

type
  TBitmapChangeableFaked = class(TBaseInterfacedObject, IBitmapChangeable)
  private
    FBitmap: IBitmap32Static;
    FChangeNotifier: INotifier;
  private
    function GetStatic: IBitmap32Static;
    function GetBeforeChangeNotifier: INotifier;
    function GetChangeNotifier: INotifier;
    function GetAfterChangeNotifier: INotifier;
  public
    constructor Create(const ABitmap: IBitmap32Static);
  end;

implementation

uses
  u_Notifier;

{ TBitmapChangeableFaked }

constructor TBitmapChangeableFaked.Create(const ABitmap: IBitmap32Static);
begin
  inherited Create;
  FBitmap := ABitmap;
  FChangeNotifier := TNotifierFaked.Create;
end;

function TBitmapChangeableFaked.GetAfterChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TBitmapChangeableFaked.GetBeforeChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TBitmapChangeableFaked.GetChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TBitmapChangeableFaked.GetStatic: IBitmap32Static;
begin
  Result := FBitmap;
end;

end.
