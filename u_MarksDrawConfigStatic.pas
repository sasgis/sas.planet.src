unit u_MarksDrawConfigStatic;

interface

uses
  Types,
  i_MarksDrawConfig;

type
  TMarksDrawConfigStatic = class(TInterfacedObject, IMarksDrawConfigStatic)
  private
    FShowPointCaption: Boolean;
    FUseSimpleDrawOrder: Boolean;
    FOverSizeRect: TRect;
  protected
    function GetShowPointCaption: Boolean;
    function GetUseSimpleDrawOrder: Boolean;
    function GetOverSizeRect: TRect;
  public
    constructor Create(
      AShowPointCaption: Boolean;
      AUseSimpleDrawOrder: Boolean;
      AOverSizeRect: TRect
    );
  end;

implementation

{ TMarksDrawConfigStatic }

constructor TMarksDrawConfigStatic.Create(AShowPointCaption,
  AUseSimpleDrawOrder: Boolean; AOverSizeRect: TRect);
begin
  FShowPointCaption := AShowPointCaption;
  FUseSimpleDrawOrder := AUseSimpleDrawOrder;
  FOverSizeRect := AOverSizeRect;
end;

function TMarksDrawConfigStatic.GetOverSizeRect: TRect;
begin
  Result := FOverSizeRect;
end;

function TMarksDrawConfigStatic.GetShowPointCaption: Boolean;
begin
  Result := FShowPointCaption;
end;

function TMarksDrawConfigStatic.GetUseSimpleDrawOrder: Boolean;
begin
  Result := FUseSimpleDrawOrder;
end;

end.
