unit u_UsedMarksConfigStatic;

interface

uses
  i_IUsedMarksConfig;

type
  TUsedMarksConfigStatic = class(TInterfacedObject, IUsedMarksConfigStatic)
  private
    FIsUseMarks: Boolean;
    FIgnoreMarksVisible: Boolean;
    FIgnoreCategoriesVisible: Boolean;
  protected
    function GetIsUseMarks: Boolean;
    function GetIgnoreCategoriesVisible: Boolean;
    function GetIgnoreMarksVisible: Boolean;
  public
    constructor Create(
      AIsUseMarks: Boolean;
      AIgnoreMarksVisible: Boolean;
      AIgnoreCategoriesVisible: Boolean
    );
  end;

implementation

{ TUsedMarksConfigStatic }

constructor TUsedMarksConfigStatic.Create(AIsUseMarks, AIgnoreMarksVisible,
  AIgnoreCategoriesVisible: Boolean);
begin
  FIsUseMarks := AIsUseMarks;
  FIgnoreCategoriesVisible := AIgnoreCategoriesVisible;
  FIgnoreMarksVisible := AIgnoreMarksVisible;
end;

function TUsedMarksConfigStatic.GetIgnoreCategoriesVisible: Boolean;
begin
  Result := FIgnoreCategoriesVisible;
end;

function TUsedMarksConfigStatic.GetIgnoreMarksVisible: Boolean;
begin
  Result := FIgnoreMarksVisible;
end;

function TUsedMarksConfigStatic.GetIsUseMarks: Boolean;
begin
  Result := FIsUseMarks;
end;

end.
