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

  cDefaultVersionAsIntValue = 0;
  cDefaultVersionAsStrValue = ''; // empty string

implementation

end.
