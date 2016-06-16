unit c_TileStorageSQLite;

interface

const
  cSQLiteDBFileExt = '.sqlitedb';

  cStorageSyncInterval = 180000; // 3-6 min after last operation

  c_Log_Init    = 'i';
  c_Log_Delete  = 'd';
  c_Log_Select  = 's';
  c_Log_Replace = 'r';
  c_Log_GetVer  = 'v';
  c_Log_SetVer  = 'w';
  c_Log_GetMap  = 'm';


  //ToDo: replace this flags with sets

  // save/delete/replace
  c_Default_TileFlags = 0;

  // save tile flags
  STF_KEEP_EXISTING = $1;
  STF_SKIP_IF_SAME_AS_PREV = $2;

  // delete flags
  DTF_ONLY_IF_SAME_AS_PREV_VERSION = $1;

  // replace version flags
  RVF_OVERWRITE_EXISTING = $1;

implementation

end.
