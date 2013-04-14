unit u_MarkCategoryDbByImpl;

interface

uses
  SysUtils,
  Classes,
  t_GeoTypes,
  i_Listener,
  i_Notifier,
  i_Category,
  i_MarkCategory,
  i_VectorItemSubset,
  i_Mark,
  i_MarkCategoryFactory,
  i_MarkCategoryDB,
  i_MarkSystemImplChangeable,
  u_BaseInterfacedObject;

type
  TMarkCategoryDbByImpl = class(TBaseInterfacedObject, IMarkCategoryDB)
  private
    FMarkSystemImpl: IMarkSystemImplChangeable;
    FNotifier: INotifier;

    FMarkCategoryFactory: IMarkCategoryFactory;
    FChangeNotifier: INotifier;
    FChangeNotifierInternal: INotifierInternal;
    FImplChangeListener: IListener;
    FDbImplChangeListener: IListener;
  private
    procedure OnImplChange;
    procedure OnDBImplChange;
  private
    function GetCategoryByName(const AName: string): IMarkCategory;
    function UpdateCategory(
      const AOldCategory: IMarkCategory;
      const ANewCategory: IMarkCategory
    ): IMarkCategory;
    function UpdateCategoryList(
      const AOldCategory: IInterfaceList;
      const ANewCategory: IInterfaceList
    ): IInterfaceList;

    function GetCategoriesList: IInterfaceList;
    procedure SetAllCategoriesVisible(ANewVisible: Boolean);

    function GetFactory: IMarkCategoryFactory;
    function GetChangeNotifier: INotifier;
  public
    constructor Create(
      const AMarkSystemImpl: IMarkSystemImplChangeable;
      const AMarkCategoryFactory: IMarkCategoryFactory
    );
    destructor Destroy; override;
  end;


implementation

uses
  i_MarkSystemImpl,
  u_Notifier,
  u_ListenerByEvent;

{ TMarkCategoryDbByImpl }

constructor TMarkCategoryDbByImpl.Create(
  const AMarkSystemImpl: IMarkSystemImplChangeable;
  const AMarkCategoryFactory: IMarkCategoryFactory);
begin
  inherited Create;
  FMarkSystemImpl := AMarkSystemImpl;
  FMarkCategoryFactory := AMarkCategoryFactory;
  FChangeNotifierInternal := TNotifierBase.Create;
  FChangeNotifier := FChangeNotifierInternal;
  FImplChangeListener := TNotifyNoMmgEventListener.Create(Self.OnImplChange);
  FDbImplChangeListener := TNotifyNoMmgEventListener.Create(Self.OnDbImplChange);

  FMarkSystemImpl.ChangeNotifier.Add(FImplChangeListener);
  OnDBImplChange;
end;

destructor TMarkCategoryDbByImpl.Destroy;
begin
  if FNotifier <> nil then begin
    FNotifier.Remove(FDbImplChangeListener);
    FNotifier := nil;
  end;
  if FMarkSystemImpl <> nil then begin
    FMarkSystemImpl.ChangeNotifier.Remove(FImplChangeListener);
    FMarkSystemImpl := nil;
  end;
  inherited;
end;

function TMarkCategoryDbByImpl.GetCategoriesList: IInterfaceList;
var
  VImpl: IMarkSystemImpl;
begin
  Result := nil;
  VImpl := FMarkSystemImpl.GetStatic;
  if VImpl <> nil then begin
    Result := VImpl.CategoryDB.GetCategoriesList;
  end;
end;

function TMarkCategoryDbByImpl.GetCategoryByName(
  const AName: string): IMarkCategory;
var
  VImpl: IMarkSystemImpl;
begin
  Result := nil;
  VImpl := FMarkSystemImpl.GetStatic;
  if VImpl <> nil then begin
    Result := VImpl.CategoryDB.GetCategoryByName(AName);
  end;
end;

function TMarkCategoryDbByImpl.GetChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TMarkCategoryDbByImpl.GetFactory: IMarkCategoryFactory;
begin
  Result := FMarkCategoryFactory;
end;

procedure TMarkCategoryDbByImpl.OnImplChange;
var
  VImpl: IMarkSystemImpl;
begin
  if FNotifier <> nil then begin
    FNotifier.Remove(FDbImplChangeListener);
    FNotifier := nil;
  end;
  VImpl := FMarkSystemImpl.GetStatic;
  if VImpl <> nil then begin
    FNotifier := VImpl.CategoryDB.ChangeNotifier;
    FNotifier.Add(FDbImplChangeListener);
  end;
end;

procedure TMarkCategoryDbByImpl.OnDbImplChange;
begin
  FChangeNotifierInternal.Notify(nil);
end;

procedure TMarkCategoryDbByImpl.SetAllCategoriesVisible(ANewVisible: Boolean);
var
  VImpl: IMarkSystemImpl;
begin
  VImpl := FMarkSystemImpl.GetStatic;
  if VImpl <> nil then begin
    VImpl.CategoryDB.SetAllCategoriesVisible(ANewVisible);
  end;
end;

function TMarkCategoryDbByImpl.UpdateCategory(const AOldCategory,
  ANewCategory: IMarkCategory): IMarkCategory;
var
  VImpl: IMarkSystemImpl;
begin
  Result := nil;
  VImpl := FMarkSystemImpl.GetStatic;
  if VImpl <> nil then begin
    Result := VImpl.CategoryDB.UpdateCategory(AOldCategory, ANewCategory);
  end;
end;

function TMarkCategoryDbByImpl.UpdateCategoryList(const AOldCategory,
  ANewCategory: IInterfaceList): IInterfaceList;
var
  VImpl: IMarkSystemImpl;
begin
  Result := nil;
  VImpl := FMarkSystemImpl.GetStatic;
  if VImpl <> nil then begin
    Result := VImpl.CategoryDB.UpdateCategoryList(AOldCategory, ANewCategory);
  end;
end;

end.
