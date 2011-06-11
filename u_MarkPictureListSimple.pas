unit u_MarkPictureListSimple;

interface

uses
  Classes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarkPicture,
  i_BitmapTypeExtManager,
  u_ConfigDataElementBase;

type
  TMarkPictureListSimple = class(TConfigDataElementBase, IMarkPictureList)
  private
    FList: TStringList;
    FBasePath: string;
    FBitmapTypeManager: IBitmapTypeExtManager;
    procedure Clear;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetCount: Integer;

    function Get(AIndex: Integer): IMarkPicture;
    function GetName(AIndex: Integer): string;
    function GetIndexByName(AValue: string): Integer;

    function GetDefaultPicture: IMarkPicture;
  public
    constructor Create(ABasePath: string; ABitmapTypeManager: IBitmapTypeExtManager);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32,
  i_BitmapTileSaveLoad,
  u_MarkPictureSimple;

{ TMarkPictureListSimple }

constructor TMarkPictureListSimple.Create(ABasePath: string; ABitmapTypeManager: IBitmapTypeExtManager);
begin
  inherited Create;
  FBasePath := ABasePath;
  FBitmapTypeManager := ABitmapTypeManager;
  FList := TStringList.Create;
end;

destructor TMarkPictureListSimple.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TMarkPictureListSimple.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do begin
    IInterface(Pointer(FList.Objects[i]))._Release;
  end;
  FList.Clear;
end;

procedure TMarkPictureListSimple.DoReadConfig(AConfigData: IConfigDataProvider);
var
  SearchRec: TSearchRec;
  VLoader: IBitmapTileLoader;
  VPicture: IMarkPicture;
  VFullName: string;
begin
  inherited;
  Clear;
  VLoader := FBitmapTypeManager.GetBitmapLoaderForExt('.png');
  if FindFirst(FBasePath + '*.png', faAnyFile, SearchRec) = 0 then begin
    try
      repeat
        if (SearchRec.Attr and faDirectory) <> faDirectory then begin
          VFullName := FBasePath + SearchRec.Name;
          VPicture := TMarkPictureSimple.Create(VFullName, SearchRec.Name, VLoader);
          VPicture._AddRef;
          FList.AddObject(SearchRec.Name, TObject(Pointer(VPicture)));
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

procedure TMarkPictureListSimple.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
end;

function TMarkPictureListSimple.Get(AIndex: Integer): IMarkPicture;
begin
  LockRead;
  try
    Result := IMarkPicture(Pointer(FList.Objects[AIndex]));
  finally
    UnlockRead;
  end;
end;

function TMarkPictureListSimple.GetCount: Integer;
begin
  LockRead;
  try
    Result := FList.Count;
  finally
    UnlockRead;
  end;
end;

function TMarkPictureListSimple.GetDefaultPicture: IMarkPicture;
begin
  LockRead;
  try
    if GetCount > 0 then begin
      Result := Get(0);
    end;
  finally
    UnlockRead;
  end;
end;

function TMarkPictureListSimple.GetIndexByName(AValue: string): Integer;
begin
  LockRead;
  try
    Result := FList.IndexOf(AValue);
  finally
    UnlockRead;
  end;
end;

function TMarkPictureListSimple.GetName(AIndex: Integer): string;
begin
  LockRead;
  try
    Result := FList.Strings[AIndex];
  finally
    UnlockRead;
  end;
end;

end.
