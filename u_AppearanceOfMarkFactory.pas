unit u_AppearanceOfMarkFactory;

interface

uses
  GR32,
  t_Hash,
  i_MarkPicture,
  i_HashFunction,
  i_Appearance,
  i_AppearanceOfMarkFactory,
  u_HashCacheWithQueuesAbstract;

type
  TAppearanceOfMarkFactory = class(THashCacheWithQueuesAbstract, IAppearanceOfMarkFactory)
  private
    FHashFunction: IHashFunction;
  protected
    function CreateByKey(
      const AKey: THashValue;
      AData: Pointer
    ): IInterface; override;
  private
    function CreatePointAppearance(
      const ATextColor: TColor32;
      const ATextBgColor: TColor32;
      const AFontSize: Integer;
      const APicName: string;
      const APic: IMarkPicture;
      const AMarkerSize: Integer
    ): IAppearance;
    function CreateLineAppearance(
      const ALineColor: TColor32;
      const ALineWidth: Integer
    ): IAppearance;
    function CreatePolygonAppearance(
      const ALineColor: TColor32;
      const ALineWidth: Integer;
      const AFillColor: TColor32
    ): IAppearance;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

implementation

uses
  u_AppearanceOfMarkPoint,
  u_AppearanceOfMarkLine,
  u_AppearanceOfMarkPolygon;

type
  PDataRecord = ^TDataRecord;
  TDataRecord = record
    Hash1: THashValue;
    Hash2: THashValue;
    Color1: TColor32;
    Color2: TColor32;
    Size1: Integer;
    Size2: Integer;
    Pic: IMarkPicture;
    PicName: string;
    MarkType: Byte;
  end;

{ TAppearanceOfMarkFactory }

constructor TAppearanceOfMarkFactory.Create(const AHashFunction: IHashFunction);
begin
  inherited Create(10, 10, 256, 100);
  FHashFunction := AHashFunction;
end;

function TAppearanceOfMarkFactory.CreateByKey(
  const AKey: THashValue;
  AData: Pointer
): IInterface;
var
  VData: PDataRecord;
  VResult: IAppearance;
begin
  inherited;
  VResult := nil;
  VData := PDataRecord(AData);
  case VData.MarkType of
    1: begin
      VResult :=
        TAppearanceOfMarkPoint.Create(
          AKey,
          VData^.Hash1,
          VData^.Hash2,
          VData^.Color1,
          VData^.Color2,
          VData^.Size1,
          VData^.Size2,
          VData^.PicName,
          VData^.Pic
        );
    end;
    2: begin
      VResult :=
        TAppearanceOfMarkLine.Create(
          AKey,
          VData^.Color1,
          VData^.Size1
        );
    end;
    3: begin
      VResult :=
        TAppearanceOfMarkPolygon.Create(
          AKey,
          VData^.Hash1,
          VData^.Hash2,
          VData^.Color1,
          VData^.Size1,
          VData^.Color2
        );
    end;
  end;

  Result := VResult;
end;

function TAppearanceOfMarkFactory.CreateLineAppearance(
  const ALineColor: TColor32;
  const ALineWidth: Integer
): IAppearance;
var
  VHash: THashValue;
  VData: TDataRecord;
begin
  VHash := FHashFunction.CalcHashByInteger(ALineColor);
  FHashFunction.UpdateHashByInteger(VHash, ALineWidth);
  VData.MarkType := 2;
  VData.Color1 := ALineColor;
  VData.Size1 := ALineWidth;

  Result := IAppearance(GetOrCreateItem(VHash, @VData));
end;

function TAppearanceOfMarkFactory.CreatePointAppearance(
  const ATextColor, ATextBgColor: TColor32;
  const AFontSize: Integer;
  const APicName: string;
  const APic: IMarkPicture;
  const AMarkerSize: Integer
): IAppearance;
var
  VHash: THashValue;
  VData: TDataRecord;
begin
  VData.Hash1 := FHashFunction.CalcHashByInteger(ATextColor);
  FHashFunction.UpdateHashByInteger(VData.Hash1, ATextBgColor);
  FHashFunction.UpdateHashByInteger(VData.Hash1, AFontSize);

  if Assigned(APic) then begin
    VData.Hash2 := APic.Hash;
  end else begin
    VData.Hash2 := FHashFunction.CalcHashByString(APicName);
  end;
  FHashFunction.UpdateHashByInteger(VData.Hash2, AMarkerSize);

  VHash := VData.Hash1;
  FHashFunction.UpdateHashByHash(VHash, VData.Hash2);

  VData.MarkType := 1;
  VData.Color1 := ATextColor;
  VData.Color2 := ATextBgColor;
  VData.Size1 := AFontSize;
  VData.Size2 := AMarkerSize;
  VData.Pic := APic;
  VData.PicName := APicName;
  Result := IAppearance(GetOrCreateItem(VHash, @VData));
end;

function TAppearanceOfMarkFactory.CreatePolygonAppearance(
  const ALineColor: TColor32;
  const ALineWidth: Integer;
  const AFillColor: TColor32
): IAppearance;
var
  VHash: THashValue;
  VData: TDataRecord;
begin
  VData.Hash1 := FHashFunction.CalcHashByInteger(ALineColor);
  FHashFunction.UpdateHashByInteger(VData.Hash1, ALineWidth);

  VData.Hash2 := FHashFunction.CalcHashByInteger(AFillColor);

  VHash := VData.Hash1;
  FHashFunction.UpdateHashByHash(VHash, VData.Hash2);

  VData.MarkType := 3;
  VData.Color1 := ALineColor;
  VData.Color2 := AFillColor;
  VData.Size1 := ALineWidth;
  Result := IAppearance(GetOrCreateItem(VHash, @VData));
end;

end.
