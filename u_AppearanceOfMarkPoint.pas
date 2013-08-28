unit u_AppearanceOfMarkPoint;

interface

uses
  GR32,
  t_Hash,
  i_MarkPicture,
  i_Appearance,
  i_AppearanceOfVectorItem,
  u_BaseInterfacedObject;

type
  TAppearanceOfMarkPoint = class(TBaseInterfacedObject, IAppearance, IAppearancePointCaption, IAppearancePointIcon)
  private
    FHashCommon: THashValue;
    FHashCaption: THashValue;
    FHashIcon: THashValue;
    FTextColor: TColor32;
    FTextBgColor: TColor32;
    FFontSize: Integer;
    FMarkerSize: Integer;
    FPicName: string;
    FPic: IMarkPicture;
  private
    function GetHashCommon: THashValue;
    function GetHashCaption: THashValue;
    function GetHashIcon: THashValue;
  private { IAppearance }
    function IAppearance.GetHash = GetHashCommon;
    function IsEqual(const AValue: IAppearance): Boolean;
  private { IAppearancePointCaption }
    function IAppearancePointCaption.GetHash = GetHashCaption;
    function GetTextColor: TColor32;
    function GetTextBgColor: TColor32;
    function GetFontSize: Integer;
  private { IAppearancePointIcon }
    function IAppearancePointIcon.GetHash = GetHashIcon;
    function GetMarkerSize: Integer;
    function GetPicName: string;
    function GetPic: IMarkPicture;
  public
    constructor Create(
      const AHashCommon: THashValue;
      const AHashCaption: THashValue;
      const AHashIcon: THashValue;
      const ATextColor: TColor32;
      const ATextBgColor: TColor32;
      const AFontSize: Integer;
      const AMarkerSize: Integer;
      const APicName: string;
      const APic: IMarkPicture
    );
  end;

implementation

{ TAppearanceOfMarkPoint }

constructor TAppearanceOfMarkPoint.Create(
  const AHashCommon, AHashCaption, AHashIcon: THashValue;
  const ATextColor, ATextBgColor: TColor32;
  const AFontSize, AMarkerSize: Integer;
  const APicName: string;
  const APic: IMarkPicture
);
begin
  inherited Create;
  FHashCommon := AHashCommon;
  FHashCaption := AHashCaption;
  FHashIcon := AHashIcon;
  FTextColor := ATextColor;
  FTextBgColor := ATextBgColor;
  FFontSize := AFontSize;
  FMarkerSize := AMarkerSize;
  FPicName := APicName;
  FPic := APic;
  if Assigned(FPic) then begin
    FPicName := FPic.GetName;
  end;
end;

function TAppearanceOfMarkPoint.GetFontSize: Integer;
begin
  Result := FFontSize;
end;

function TAppearanceOfMarkPoint.GetHashCaption: THashValue;
begin
  Result := FHashCaption;
end;

function TAppearanceOfMarkPoint.GetHashCommon: THashValue;
begin
  Result := FHashCommon;
end;

function TAppearanceOfMarkPoint.GetHashIcon: THashValue;
begin
  Result := FHashIcon;
end;

function TAppearanceOfMarkPoint.GetMarkerSize: Integer;
begin
  Result := FMarkerSize;
end;

function TAppearanceOfMarkPoint.GetPic: IMarkPicture;
begin
  Result := FPic;
end;

function TAppearanceOfMarkPoint.GetPicName: string;
begin
  Result := FPicName;
end;

function TAppearanceOfMarkPoint.GetTextBgColor: TColor32;
begin
  Result := FTextBgColor;
end;

function TAppearanceOfMarkPoint.GetTextColor: TColor32;
begin
  Result := FTextColor;
end;

function TAppearanceOfMarkPoint.IsEqual(const AValue: IAppearance): Boolean;
begin
  if not Assigned(AValue) then begin
    Result := False;
  end else begin
    Result := FHashCommon = AValue.Hash;
  end;
end;

end.
