unit u_SynchronizerBase;

interface

uses
  i_ReadWriteSyncFactory,
  i_Synchronizer;

type
  TSynchronizerBase = class(TInterfacedObject, ISynchronizer)
  private
    FSyncVariable: IReadWriteSyncFactory;
    FSyncVariableRecursive: IReadWriteSyncFactory;
    FSyncSymmetrical: IReadWriteSyncFactory;
    FSyncSymmetricalRecursive: IReadWriteSyncFactory;
    FSyncStd: IReadWriteSyncFactory;
    FSyncStdRecursive: IReadWriteSyncFactory;
    FSyncBig: IReadWriteSyncFactory;
    FSyncBigRecursive: IReadWriteSyncFactory;
  private
    function GetSyncVariable: IReadWriteSyncFactory;
    function GetSyncVariableRecursive: IReadWriteSyncFactory;
    function GetSyncSymmetrical: IReadWriteSyncFactory;
    function GetSyncSymmetricalRecursive: IReadWriteSyncFactory;
    function GetSyncStd: IReadWriteSyncFactory;
    function GetSyncStdRecursive: IReadWriteSyncFactory;
    function GetSyncBig: IReadWriteSyncFactory;
    function GetSyncBigRecursive: IReadWriteSyncFactory;
  public
    constructor Create(
      const ASyncVariable: IReadWriteSyncFactory;
      const ASyncVariableRecursive: IReadWriteSyncFactory;
      const ASyncSymmetrical: IReadWriteSyncFactory;
      const ASyncSymmetricalRecursive: IReadWriteSyncFactory;
      const ASyncStd: IReadWriteSyncFactory;
      const ASyncStdRecursive: IReadWriteSyncFactory;
      const ASyncBig: IReadWriteSyncFactory;
      const ASyncBigRecursive: IReadWriteSyncFactory
    );
  end;

implementation

{ TSynchronizerBase }

constructor TSynchronizerBase.Create(
  const ASyncVariable: IReadWriteSyncFactory;
  const ASyncVariableRecursive: IReadWriteSyncFactory;
  const ASyncSymmetrical: IReadWriteSyncFactory;
  const ASyncSymmetricalRecursive: IReadWriteSyncFactory;
  const ASyncStd: IReadWriteSyncFactory;
  const ASyncStdRecursive: IReadWriteSyncFactory;
  const ASyncBig: IReadWriteSyncFactory;
  const ASyncBigRecursive: IReadWriteSyncFactory
);
begin
  inherited Create;
  FSyncVariable := ASyncVariable;
  FSyncVariableRecursive := ASyncVariableRecursive;
  FSyncSymmetrical := ASyncSymmetrical;
  FSyncSymmetricalRecursive := ASyncSymmetricalRecursive;
  FSyncStd := ASyncStd;
  FSyncStdRecursive := ASyncStdRecursive;
  FSyncBig := ASyncBig;
  FSyncBigRecursive := ASyncBigRecursive;
end;

function TSynchronizerBase.GetSyncBig: IReadWriteSyncFactory;
begin
  Result := FSyncBig;
end;

function TSynchronizerBase.GetSyncBigRecursive: IReadWriteSyncFactory;
begin
  Result := FSyncBigRecursive;
end;

function TSynchronizerBase.GetSyncStd: IReadWriteSyncFactory;
begin
  Result := FSyncStd;
end;

function TSynchronizerBase.GetSyncStdRecursive: IReadWriteSyncFactory;
begin
  Result := FSyncStdRecursive;
end;

function TSynchronizerBase.GetSyncSymmetrical: IReadWriteSyncFactory;
begin
  Result := FSyncSymmetrical;
end;

function TSynchronizerBase.GetSyncSymmetricalRecursive: IReadWriteSyncFactory;
begin
  Result := FSyncSymmetricalRecursive;
end;

function TSynchronizerBase.GetSyncVariable: IReadWriteSyncFactory;
begin
  Result := FSyncVariable;
end;

function TSynchronizerBase.GetSyncVariableRecursive: IReadWriteSyncFactory;
begin
  Result := FSyncVariableRecursive;
end;

end.
