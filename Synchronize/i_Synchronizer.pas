unit i_Synchronizer;

interface

uses
  i_ReadWriteSyncFactory;

type
  ISynchronizer = interface
    ['{D483F620-6E80-4BBF-885B-CFCE2F5D6ADE}']
    // very short operation - about 1 or 2 simple variables read or write
    // makes first available from { MakeSyncSRW, MakeSyncSpinLock, MakeSyncRes, MakeSyncMREW }
    function GetSyncVariable: IReadWriteSyncFactory;
    property SyncVariable: IReadWriteSyncFactory read GetSyncVariable;

    function GetSyncVariableRecursive: IReadWriteSyncFactory;
    property SyncVariableRecursive: IReadWriteSyncFactory read GetSyncVariableRecursive;

    // small symmetrical operation
    // makes first available from { MakeSyncRes, MakeSyncSRW, MakeSyncMREW }
    function GetSyncSymmetrical: IReadWriteSyncFactory;
    property SyncSymmetrical: IReadWriteSyncFactory read GetSyncSymmetrical;

    function GetSyncSymmetricalRecursive: IReadWriteSyncFactory;
    property SyncSymmetricalRecursive: IReadWriteSyncFactory read GetSyncVariableRecursive;

    // others (many readers with 1-2 writers)
    // makes first available from { MakeSyncSRW, MakeSyncRes, MakeSyncMREW }
    function GetSyncStd: IReadWriteSyncFactory;
    property SyncStd: IReadWriteSyncFactory read GetSyncStd;

    function GetSyncStdRecursive: IReadWriteSyncFactory;
    property SyncStdRecursive: IReadWriteSyncFactory read GetSyncStdRecursive;

    // many concurrent readers and writers
    // makes first available from { MakeSyncRes, MakeSyncMREW }
    function GetSyncBig: IReadWriteSyncFactory;
    property SyncBig: IReadWriteSyncFactory read GetSyncBig;

    function GetSyncBigRecursive: IReadWriteSyncFactory;
    property SyncBigRecursive: IReadWriteSyncFactory read GetSyncBigRecursive;
  end;

implementation

end.
