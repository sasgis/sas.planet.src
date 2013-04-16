unit c_CacheTypeCodes;

interface

const
  c_File_Cache_Id_DEFAULT = 0; // subst only
  c_File_Cache_Id_GMV = 1;  // old
  c_File_Cache_Id_SAS = 2;  // new
  c_File_Cache_Id_ES = 3;
  c_File_Cache_Id_GM = 4;
  c_File_Cache_Id_GM_Aux = 41; // auxillary
  c_File_Cache_Id_GM_Bing = 42; // "Bing Maps (Virtual Earth) Tiles"
  c_File_Cache_Id_GE = 5;  // GE cache direct access
  c_File_Cache_Id_BDB = 6;
  c_File_Cache_Id_BDB_Versioned = 61;
  c_File_Cache_Id_DBMS = 7;
  c_File_Cache_Id_GC = 8;  // GeoCacher.LOCAL direct access
  c_File_Cache_Id_RAM = 9; // only in-memory cache    

  c_File_Cache_Default_GMV  = 'cache_old';     // for 1
  c_File_Cache_Default_SAS  = 'cache';         // for 2
  c_File_Cache_Default_ES   = 'cache_ES';      // for 3
  c_File_Cache_Default_GM   = 'cache_gmt';     // for 4x
  c_File_Cache_Default_GE   = 'cache_GE';      // for 5
  c_File_Cache_Default_BDB  = 'cache_db';      // for 6
  c_File_Cache_Default_DBMS = 'SASGIS_DBMS\$'; // for 7
  c_File_Cache_Default_GC   = 'cache_GC';      // for 8
  c_File_Cache_Default_RAM  = '';              // for 9

implementation

end.
