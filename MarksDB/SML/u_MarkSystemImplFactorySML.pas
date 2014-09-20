unit u_MarkSystemImplFactorySML;

interface

uses
  i_HashFunction,
  i_GeometryLonLatFactory,
  i_VectorItemSubsetBuilder,
  i_InternalPerformanceCounter,
  i_AppearanceOfMarkFactory,
  i_MarkPicture,
  i_HtmlToHintTextConverter,
  i_MarkFactory,
  i_MarkSystemImpl,
  i_MarkSystemImplFactory,
  u_BaseInterfacedObject;

type
  TMarkSystemImplFactorySML = class(TBaseInterfacedObject, IMarkSystemImplFactory)
  private
    FMarkPictureList: IMarkPictureList;
    FHashFunction: IHashFunction;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FMarkFactory: IMarkFactory;
    FLoadDbCounter: IInternalPerformanceCounter;
    FSaveDbCounter: IInternalPerformanceCounter;
    FHintConverter: IHtmlToHintTextConverter;
  private
    function GetIsInitializationRequired: Boolean;
    function Build(
      const ABasePath: string;
      const AReadOnly: Boolean = False
    ): IMarkSystemImpl;
  public
    constructor Create(
      const AMarkPictureList: IMarkPictureList;
      const AHashFunction: IHashFunction;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AMarkFactory: IMarkFactory;
      const ALoadDbCounter: IInternalPerformanceCounter;
      const ASaveDbCounter: IInternalPerformanceCounter;
      const AHintConverter: IHtmlToHintTextConverter
    );
  end;

implementation

uses
  u_MarkSystemSml;

{ TMarkSystemImplFactorySML }

constructor TMarkSystemImplFactorySML.Create(
  const AMarkPictureList: IMarkPictureList;
  const AHashFunction: IHashFunction;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AMarkFactory: IMarkFactory;
  const ALoadDbCounter, ASaveDbCounter: IInternalPerformanceCounter;
  const AHintConverter: IHtmlToHintTextConverter
);
begin
  Assert(Assigned(AMarkPictureList));
  Assert(Assigned(AHashFunction));
  Assert(Assigned(AAppearanceOfMarkFactory));
  Assert(Assigned(AVectorGeometryLonLatFactory));
  Assert(Assigned(AVectorItemSubsetBuilderFactory));
  Assert(Assigned(AMarkFactory));
  Assert(Assigned(AHintConverter));
  inherited Create;
  FMarkPictureList := AMarkPictureList;
  FHashFunction := AHashFunction;
  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FMarkFactory := AMarkFactory;
  FLoadDbCounter := ALoadDbCounter;
  FSaveDbCounter := ASaveDbCounter;
  FHintConverter := AHintConverter;
end;

function TMarkSystemImplFactorySML.GetIsInitializationRequired: Boolean;
begin
  Result := True;
end;

function TMarkSystemImplFactorySML.Build(
  const ABasePath: string;
  const AReadOnly: Boolean
): IMarkSystemImpl;
begin
  Result :=
    TMarkSystemSml.Create(
      ABasePath,
      FMarkPictureList,
      FHashFunction,
      FAppearanceOfMarkFactory,
      FVectorGeometryLonLatFactory,
      FVectorItemSubsetBuilderFactory,
      FMarkFactory,
      FLoadDbCounter,
      FSaveDbCounter,
      FHintConverter,
      AReadOnly
    );
end;

end.
