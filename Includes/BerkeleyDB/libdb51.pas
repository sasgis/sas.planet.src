{===========================================================================}
{ Berkeley DB 5.1.29 Header File                                            }
{                                                                           }
{ http://www.oracle.com/technetwork/database/berkeleydb/                    }
{ http://en.wikipedia.org/wiki/Berkeley_DB                                  }
{                                                                           }
{ File: db.h                                                                }
{ Copyright (c) 1996, 2010 Oracle and/or its affiliates.                    }
{ All rights reserved.                                                      }
{                                                                           }
{ Translator:                                                               }
{   JRL                                http://www.demonak.com/              }
{   Copyright (c) 2011 Jose R. Lopez - A Coruña, Spain                      }
{   All rights reserved.                                                    }
{                                                                           }
{ With some (little) help from:                                             }
{   C to Pascal Converter 2.19.8.2010  http://cc.embarcadero.com/Item/26951 }
{===========================================================================}

unit libdb51;

{$ALIGN ON}
{$MINENUMSIZE 4}

interface

uses
  SysUtils;

type
  EBerkeleyDBExeption = class(Exception);

(*
  TRANSLATOR NOTES:

  1) Refactoring: Types need to be defined alltogether in an unique type
     declaraction so pointers to records can be defined and used prior to the
     record definition, so consts are also grouped together (and declared
     before types because some consts are used in type definition).

  2) Dbm/Ndbm/Hsearch historic interfaces are not translated.

  3) Data types not present in the C source but needed for Pascal syntax are
     marked with @@PAS_HELPER(pascaltype)

  4) Names used in C but reserved in Pascal are renamed and marked
     with @@PAS_MAPPING(pascalname=cname)
*)


// TRANSLATION: CONSTS...

const
  (*
    Berkeley DB version information.
  *)
  DB_VERSION_FAMILY      = 11;
  DB_VERSION_RELEASE     = 2;
  DB_VERSION_MAJOR       = 5;
  DB_VERSION_MINOR       = 1;
  DB_VERSION_PATCH       = 29;
  DB_VERSION_STRING      = 'Berkeley DB 5.1.29: (October 25, 2011)';
  DB_VERSION_FULL_STRING = 'Berkeley DB 11g Release 2, library version 11.2.5.1.29: (October 25, 2011)';

  DB_MAX_PAGES   = $ffffffff;      (* >= # of pages in a file *)

  DB_MAX_RECORDS = $ffffffff;      (* >= # of records in a tree *)

  (*
    The Berkeley DB API flags are automatically-generated -- the following flag
    names are no longer used, but remain for compatibility reasons.
  *)
  //DB_DEGREE_2   = DB_READ_COMMITTED;
  //DB_DIRTY_READ = DB_READ_UNCOMMITTED;
  DB_JOINENV    = $0;

  DB_DBT_APPMALLOC = $001; (* Callback allocated memory. *)
  DB_DBT_BULK      = $002; (* Internal; Insert if duplicate. *)
  DB_DBT_DUPOK     = $004; (* Internal; Insert if duplicate. *)
  DB_DBT_ISSET     = $008; (* Lower level calls set value. *)
  DB_DBT_MALLOC    = $010; (* Return in malloc'd memory. *)
  DB_DBT_MULTIPLE  = $020; (* References multiple records. *)
  DB_DBT_PARTIAL   = $040; (* Partial put/get. *)
  DB_DBT_REALLOC   = $080; (* Return in realloc'd memory. *)
  DB_DBT_STREAMING = $100; (* Internal; DBT is being streamed. *)
  DB_DBT_USERCOPY  = $200; (* Use the user-supplied callback. *)
  DB_DBT_USERMEM   = $400; (* Return in user's memory. *)

(* This is the length of the buffer passed to DB_ENV->thread_id_string() *)
  DB_THREADID_STRLEN =  128;

(*******************************************************
   Locking.
  ******************************************************)
  DB_LOCKVERSION = 1;
  DB_FILE_ID_LEN = 20;     (* Unique file ID length. *)

  (*
    Deadlock detector modes; used in the DB_ENV structure to configure the
    locking subsystem.
  *)
  DB_LOCK_NORUN    = 0;
  DB_LOCK_DEFAULT  = 1;      (* Default policy. *)
  DB_LOCK_EXPIRE   = 2;      (* Only expire locks, no detection. *)
  DB_LOCK_MAXLOCKS = 3;      (* Select locker with max locks. *)
  DB_LOCK_MAXWRITE = 4;      (* Select locker with max writelocks. *)
  DB_LOCK_MINLOCKS = 5;      (* Select locker with min locks. *)
  DB_LOCK_MINWRITE = 6;      (* Select locker with min writelocks. *)
  DB_LOCK_OLDEST   = 7;      (* Select oldest locker. *)
  DB_LOCK_RANDOM   = 8;      (* Select random locker. *)
  DB_LOCK_YOUNGEST = 9;      (* Select youngest locker. *)

  DB_HANDLE_LOCK   = 1;
  DB_RECORD_LOCK   = 2;
  DB_PAGE_LOCK     = 3;
  DB_DATABASE_LOCK = 4;

(*******************************************************
   Logging.
  ******************************************************)
  DB_LOGVERSION          = 17; (* Current log version. *)
  DB_LOGVERSION_LATCHING = 15; (* Log version using latching. *)
  DB_LOGCHKSUM           = 12; (* Check sum headers. *)
  DB_LOGOLDVER           = 8;  (* Oldest log version supported. *)
  DB_LOGMAGIC            = $040988;

  (*
    Application-specified log record types start at DB_user_BEGIN, and must not
    equal or exceed DB_debug_FLAG.

    DB_debug_FLAG is the high-bit of the u_int32_t that specifies a log record
    type.  If the flag is set, it's a log record that was logged for debugging
    purposes only, even if it reflects a database change -- the change was part
    of a non-durable transaction.
  *)
  DB_user_BEGIN = 10000;
  DB_debug_FLAG = $80000000;

  DB_LOG_DISK       = $01;    (* Log record came from disk. *)
  DB_LOG_LOCKED     = $02;    (* Log region already locked *)
  DB_LOG_SILENT_ERR = $04;    (* Turn-off error messages. *)

  (*
    MP_FILEID_SET, MP_OPEN_CALLED and MP_READONLY do not need to be
    thread protected because they are initialized before the file is
    linked onto the per-process lists, and never modified.

    MP_FLUSH is thread protected because it is potentially read/set by
    multiple threads of control.
  *)
  MP_FILEID_SET   = $001;           (* Application supplied a file ID. *)
  MP_FLUSH        = $002;           (* Was opened to flush a buffer. *)
  MP_MULTIVERSION = $004;           (* Opened for multiversion access. *)
  MP_OPEN_CALLED  = $008;           (* file opened. *)
  MP_READONLY     = $010;           (* file is readonly. *)
  MP_DUMMY        = $020;           (* file is dummy for __memp_fput. *)

(*******************************************************
   Transactions and recovery.
  ******************************************************)
  DB_TXNVERSION        = 1;

  TXN_CHILDCOMMIT      = $00001; (* Txn has committed. *)
  TXN_COMPENSATE       = $00002; (* Compensating transaction. *)
  TXN_DEADLOCK         = $00004; (* Txn has deadlocked. *)
  TXN_FAMILY           = $00008; (* Cursors/children are independent. *)
  TXN_IGNORE_LEASE     = $00010; (* Skip lease check at commit time. *)
  TXN_INFAMILY         = $00020; (* Part of a transaction family. *)
  TXN_LOCKTIMEOUT      = $00040; (* Txn has a lock timeout. *)
  TXN_MALLOC           = $00080; (* Structure allocated by TXN system. *)
  TXN_NOSYNC           = $00100; (* Do not sync on prepare and commit. *)
  TXN_NOWAIT           = $00200; (* Do not wait on locks. *)
  TXN_PRIVATE          = $00400; (* Txn owned by cursor. *)
  TXN_READONLY         = $00800; (* CDS group handle. *)
  TXN_READ_COMMITTED   = $01000; (* Txn has degree 2 isolation. *)
  TXN_READ_UNCOMMITTED = $02000; (* Txn has degree 1 isolation. *)
  TXN_RESTORED         = $04000; (* Txn has been restored. *)
  TXN_SNAPSHOT         = $08000; (* Snapshot Isolation. *)
  TXN_SYNC             = $10000; (* Write and sync on prepare/commit. *)
  TXN_WRITE_NOSYNC     = $20000; (* Write only on prepare/commit. *)
  TXN_BULK             = $40000; (* Enable bulk loading optimization. *)

  TXN_SYNC_FLAGS       = (TXN_SYNC or TXN_NOSYNC or TXN_WRITE_NOSYNC);

  (*
    Structure used for two phase commit interface.
    We set the size of our global transaction id (gid) to be 128 in order
    to match that defined by the XA X/Open standard.
  *)
  DB_GID_SIZE = 128;

  TXN_ABORTED   = 1;
  TXN_COMMITTED = 2;
  TXN_PREPARED  = 3;
  TXN_RUNNING   = 4;

  DB_TXN_TOKEN_SIZE = 20;

(*******************************************************
   Replication.
  ******************************************************)
  (* Special, out-of-band environment IDs. *)
  DB_EID_BROADCAST             = -1;
  DB_EID_INVALID               = -2;

  DB_REP_DEFAULT_PRIORITY      = 100;

  (* Acknowledgement policies. *)
  DB_REPMGR_ACKS_ALL           = 1;
  DB_REPMGR_ACKS_ALL_AVAILABLE = 2;
  DB_REPMGR_ACKS_ALL_PEERS     = 3;
  DB_REPMGR_ACKS_NONE          = 4;
  DB_REPMGR_ACKS_ONE           = 5;
  DB_REPMGR_ACKS_ONE_PEER      = 6;
  DB_REPMGR_ACKS_QUORUM        = 7;

  (* Replication timeout configuration values. *)
  DB_REP_ACK_TIMEOUT           = 1;      (* RepMgr acknowledgements. *)
  DB_REP_CHECKPOINT_DELAY      = 2;      (* Master checkpoint delay. *)
  DB_REP_CONNECTION_RETRY      = 3;      (* RepMgr connections. *)
  DB_REP_ELECTION_RETRY        = 4;      (* RepMgr elect retries. *)
  DB_REP_ELECTION_TIMEOUT      = 5;      (* Rep normal elections. *)
  DB_REP_FULL_ELECTION_TIMEOUT = 6;      (* Rep full elections. *)
  DB_REP_HEARTBEAT_MONITOR     = 7;      (* RepMgr client HB monitor. *)
  DB_REP_HEARTBEAT_SEND        = 8;      (* RepMgr master send freq. *)
  DB_REP_LEASE_TIMEOUT         = 9;      (* Master leases. *)

  (* Event notification types. *)
  DB_EVENT_PANIC               = 0;
  DB_EVENT_REG_ALIVE           = 1;
  DB_EVENT_REG_PANIC           = 2;
  DB_EVENT_REP_CLIENT          = 3;
  DB_EVENT_REP_DUPMASTER       = 4;
  DB_EVENT_REP_ELECTED         = 5;
  DB_EVENT_REP_ELECTION_FAILED = 6;
  DB_EVENT_REP_JOIN_FAILURE    = 7;
  DB_EVENT_REP_MASTER          = 8;
  DB_EVENT_REP_MASTER_FAILURE  = 9;
  DB_EVENT_REP_NEWMASTER       = 10;
  DB_EVENT_REP_PERM_FAILED     = 11;
  DB_EVENT_REP_STARTUPDONE     = 12;
  DB_EVENT_WRITE_FAILED        = 13;
  DB_EVENT_NO_SUCH_EVENT       = $ffffffff; (* OOB sentinel value *)

  DB_REPMGR_CONNECTED          = 1;
  DB_REPMGR_DISCONNECTED       = 2;

  DB_REPMGR_ISPEER             = $01;

(*******************************************************
   Access methods.
  ******************************************************)
  DB_RENAMEMAGIC      = $030800; (* File has been renamed. *)

  DB_BTREEVERSION     = 9;       (* Current btree version. *)
  DB_BTREEOLDVER      = 8;       (* Oldest btree version supported. *)
  DB_BTREEMAGIC       = $053162;

  DB_HASHVERSION      = 9;       (* Current hash version. *)
  DB_HASHOLDVER       = 7;       (* Oldest hash version supported. *)
  DB_HASHMAGIC        = $061561;

  DB_QAMVERSION       = 4;       (* Current queue version. *)
  DB_QAMOLDVER        = 3;       (* Oldest queue version supported. *)
  DB_QAMMAGIC         = $042253;

  DB_SEQUENCE_VERSION = 2;       (* Current sequence version. *)
  DB_SEQUENCE_OLDVER  = 1;       (* Oldest sequence version supported. *)

  (*
    DB access method and cursor operation values.  Each value is an operation
    code to which additional bit flags are added.
  *)
  DB_AFTER            = 1;  (* Dbc.put *)
  DB_APPEND           = 2;  (* Db.put *)
  DB_BEFORE           = 3;  (* Dbc.put *)
  DB_CONSUME          = 4;  (* Db.get *)
  DB_CONSUME_WAIT     = 5;  (* Db.get *)
  DB_CURRENT          = 6;  (* Dbc.get, Dbc.put, DbLogc.get *)
  DB_FIRST            = 7;  (* Dbc.get, DbLogc->get *)
  DB_GET_BOTH         = 8;  (* Db.get, Dbc.get *)
  DB_GET_BOTHC        = 9;  (* Dbc.get (internal) *)
  DB_GET_BOTH_RANGE   = 10; (* Db.get, Dbc.get *)
  DB_GET_RECNO        = 11; (* Dbc.get *)
  DB_JOIN_ITEM        = 12; (* Dbc.get; don't do primary lookup *)
  DB_KEYFIRST         = 13; (* Dbc.put *)
  DB_KEYLAST          = 14; (* Dbc.put *)
  DB_LAST             = 15; (* Dbc.get, DbLogc->get *)
  DB_NEXT             = 16; (* Dbc.get, DbLogc->get *)
  DB_NEXT_DUP         = 17; (* Dbc.get *)
  DB_NEXT_NODUP       = 18; (* Dbc.get *)
  DB_NODUPDATA        = 19; (* Db.put, Dbc.put *)
  DB_NOOVERWRITE      = 20; (* Db.put *)
  DB_OVERWRITE_DUP    = 21; (* Dbc.put, Db.put; no DB_KEYEXIST *)
  DB_POSITION         = 22; (* Dbc.dup *)
  DB_PREV             = 23; (* Dbc.get, DbLogc->get *)
  DB_PREV_DUP         = 24; (* Dbc.get *)
  DB_PREV_NODUP       = 25; (* Dbc.get *)
  DB_SET              = 26; (* Dbc.get, DbLogc->get *)
  DB_SET_RANGE        = 27; (* Dbc.get *)
  DB_SET_RECNO        = 28; (* Db.get, Dbc.get *)
  DB_UPDATE_SECONDARY = 29; (* Dbc.get, Dbc.del (internal) *)
  DB_SET_LTE          = 30; (* Dbc.get (internal) *)
  DB_GET_BOTH_LTE     = 31; (* Dbc.get (internal) *)

  (* This has to change when the max opcode hits 255. *)
  DB_OPFLAGS_MASK = $000000ff;      (* Mask for operations flags. *)

  (*
    DB (user visible) error return codes.

    !!!
    We don't want our error returns to conflict with other packages where
    possible, so pick a base error value that's hopefully not common.  We
    document that we own the error name space from -30,800 to -30,999.
 *)
  (* DB (public) error return codes. *)
  DB_BUFFER_SMALL      = (-30999);(* User memory too small for return. *)
  DB_DONOTINDEX        = (-30998);(* "Null" return from 2ndary callbk. *)
  DB_FOREIGN_CONFLICT  = (-30997);(* A foreign db constraint triggered. *)
  DB_KEYEMPTY          = (-30996);(* Key/data deleted or never created. *)
  DB_KEYEXIST          = (-30995);(* The key/data pair already exists. *)
  DB_LOCK_DEADLOCK     = (-30994);(* Deadlock. *)
  DB_LOCK_NOTGRANTED   = (-30993);(* Lock unavailable. *)
  DB_LOG_BUFFER_FULL   = (-30992);(* In-memory log buffer full. *)
  DB_LOG_VERIFY_BAD    = (-30991);(* Log verification failed. *)
  DB_NOSERVER          = (-30990);(* Server panic return. *)
  DB_NOSERVER_HOME     = (-30989);(* Bad home sent to server. *)
  DB_NOSERVER_ID       = (-30988);(* Bad ID sent to server. *)
  DB_NOTFOUND          = (-30987);(* Key/data pair not found (EOF). *)
  DB_OLD_VERSION       = (-30986);(* Out-of-date version. *)
  DB_PAGE_NOTFOUND     = (-30985);(* Requested page not found. *)
  DB_REP_DUPMASTER     = (-30984);(* There are two masters. *)
  DB_REP_HANDLE_DEAD   = (-30983);(* Rolled back a commit. *)
  DB_REP_HOLDELECTION  = (-30982);(* Time to hold an election. *)
  DB_REP_IGNORE        = (-30981);(* This msg should be ignored.*)
  DB_REP_ISPERM        = (-30980);(* Cached not written perm written.*)
  DB_REP_JOIN_FAILURE  = (-30979);(* Unable to join replication group. *)
  DB_REP_LEASE_EXPIRED = (-30978);(* Master lease has expired. *)
  DB_REP_LOCKOUT       = (-30977);(* API/Replication lockout now. *)
  DB_REP_NEWSITE       = (-30976);(* New site entered system. *)
  DB_REP_NOTPERM       = (-30975);(* Permanent log record not written. *)
  DB_REP_UNAVAIL       = (-30974);(* Site cannot currently be reached. *)
  DB_RUNRECOVERY       = (-30973);(* Panic return. *)
  DB_SECONDARY_BAD     = (-30972);(* Secondary index corrupt. *)
  DB_TIMEOUT           = (-30971);(* Timed out on read consistency. *)
  DB_VERIFY_BAD        = (-30970);(* Verify failed; bad format. *)
  DB_VERSION_MISMATCH  = (-30969);(* Environment version mismatch. *)

  (* DB (private) error return codes. *)
  DB_ALREADY_ABORTED   = (-30899);
  DB_DELETED           = (-30898);(* Recovery file marked deleted. *)
  DB_EVENT_NOT_HANDLED = (-30897);(* Forward event to application. *)
  DB_NEEDSPLIT         = (-30896);(* Page needs to be split. *)
  DB_REP_BULKOVF       = (-30895);(* Rep bulk buffer overflow. *)
  DB_REP_LOGREADY      = (-30894);(* Rep log ready for recovery. *)
  DB_REP_NEWMASTER     = (-30893);(* We have learned of a new master. *)
  DB_REP_PAGEDONE      = (-30892);(* This page was already done. *)
  DB_SURPRISE_KID      = (-30891);(* Child commit where parent didn't know it was a parent. *)
  DB_SWAPBYTES         = (-30890);(* Database needs byte swapping. *)
  DB_TXN_CKP           = (-30889);(* Encountered ckp record in log. *)
  DB_VERIFY_FATAL      = (-30888);(* DB->verify cannot proceed. *)

  DB_LOGFILEID_INVALID = -1;

  DB_ASSOC_IMMUTABLE_KEY = $00000001; (* Secondary key is immutable. *)
  DB_ASSOC_CREATE        = $00000002; (* Secondary db populated on open. *)

  DB_OK_BTREE = $01;
  DB_OK_HASH  = $02;
  DB_OK_QUEUE = $04;
  DB_OK_RECNO = $08;

  DB_AM_CHKSUM           = $00000001; (* Checksumming *)
  DB_AM_COMPENSATE       = $00000002; (* Created by compensating txn *)
  DB_AM_COMPRESS         = $00000004; (* Compressed BTree *)
  DB_AM_CREATED          = $00000008; (* Database was created upon open *)
  DB_AM_CREATED_MSTR     = $00000010; (* Encompassing file was created *)
  DB_AM_DBM_ERROR        = $00000020; (* Error in DBM/NDBM database *)
  DB_AM_DELIMITER        = $00000040; (* Variable length delimiter set *)
  DB_AM_DISCARD          = $00000080; (* Discard any cached pages *)
  DB_AM_DUP              = $00000100; (* DB_DUP *)
  DB_AM_DUPSORT          = $00000200; (* DB_DUPSORT *)
  DB_AM_ENCRYPT          = $00000400; (* Encryption *)
  DB_AM_FIXEDLEN         = $00000800; (* Fixed-length records *)
  DB_AM_INMEM            = $00001000; (* In-memory; no sync on close *)
  DB_AM_IN_RENAME        = $00004000; (* file is being renamed *)
  DB_AM_NOT_DURABLE      = $00008000; (* Do not log changes *)
  DB_AM_OPEN_CALLED      = $00010000; (* DB->open called *)
  DB_AM_PAD              = $00020000; (* Fixed-length record pad *)
  DB_AM_PGDEF            = $00040000; (* Page size was defaulted *)
  DB_AM_RDONLY           = $00080000; (* Database is readonly *)
  DB_AM_READ_UNCOMMITTED = $00100000; (* Support degree 1 isolation *)
  DB_AM_RECNUM           = $00200000; (* DB_RECNUM *)
  DB_AM_RECOVER          = $00400000; (* DB opened by recovery routine *)
  DB_AM_RENUMBER         = $00800000; (* DB_RENUMBER *)
  DB_AM_REVSPLITOFF      = $01000000; (* DB_REVSPLITOFF *)
  DB_AM_SECONDARY        = $02000000; (* Database is a secondary index *)
  DB_AM_SNAPSHOT         = $04000000; (* DB_SNAPSHOT *)
  DB_AM_SUBDB            = $08000000; (* Subdatabases supported *)
  DB_AM_SWAP             = $10000000; (* Pages need to be byte-swapped *)
  DB_AM_TXN              = $20000000; (* Opened in a transaction *)
  DB_AM_VERIFYING        = $40000000; (* DB handle is in the verifier *)

  (*
   * DBC_DONTLOCK and DBC_RECOVER are used during recovery and transaction
   * abort.  If a transaction is being aborted or recovered then DBC_RECOVER
   * will be set and locking and logging will be disabled on this cursor.  If
   * we are performing a compensating transaction (e.g. free page processing)
   * then DB_DONTLOCK will be set to inhibit locking, but logging will still
   * be required. DB_DONTLOCK is also used if the whole database is locked.
   *)
  DBC_ACTIVE             = $00001; (* Cursor in use. *)
  DBC_BULK               = $00002; (* Bulk update cursor. *)
  DBC_DONTLOCK           = $00004; (* Don't lock on this cursor. *)
  DBC_DOWNREV            = $00008; (* Down rev replication master. *)
  DBC_DUPLICATE          = $00010; (* Create a duplicate cursor. *)
  DBC_ERROR              = $00020; (* Error in this request. *)
  DBC_FAMILY             = $00040; (* Part of a locker family. *)
  DBC_FROM_DB_GET        = $00080; (* Called from the DB->get() method. *)
  DBC_MULTIPLE           = $00100; (* Return Multiple data. *)
  DBC_MULTIPLE_KEY       = $00200; (* Return Multiple keys and data. *)
  DBC_OPD                = $00400; (* Cursor references off-page dups. *)
  DBC_OWN_LID            = $00800; (* Free lock id on destroy. *)
  DBC_PARTITIONED        = $01000; (* Cursor for a partitioned db. *)
  DBC_READ_COMMITTED     = $02000; (* Cursor has degree 2 isolation. *)
  DBC_READ_UNCOMMITTED   = $04000; (* Cursor has degree 1 isolation. *)
  DBC_RECOVER            = $08000; (* Recovery cursor; don't log/lock. *)
  DBC_RMW                = $10000; (* Acquire write flag in read op. *)
  DBC_TRANSIENT          = $20000; (* Cursor is transient. *)
  DBC_WAS_READ_COMMITTED = $40000; (* Cursor holds a read commited lock. *)
  DBC_WRITECURSOR        = $80000; (* Cursor may be used to write (CDB). *)
  DBC_WRITER             = $100000; (* Cursor immediately writing (CDB). *)

(*******************************************************
 * Environment.
 *******************************************************)
  DB_REGION_MAGIC = $120897;  (* Environment magic number. *)

  DB_ENV_AUTO_COMMIT      = $00000001; (* DB_AUTO_COMMIT *)
  DB_ENV_CDB_ALLDB        = $00000002; (* CDB environment wide locking *)
  DB_ENV_FAILCHK          = $00000004; (* Failchk is running *)
  DB_ENV_DIRECT_DB        = $00000008; (* DB_DIRECT_DB set *)
  DB_ENV_DSYNC_DB         = $00000010; (* DB_DSYNC_DB set *)
  DB_ENV_DATABASE_LOCKING = $00000020; (* Try database-level locking *)
  DB_ENV_MULTIVERSION     = $00000040; (* DB_MULTIVERSION set *)
  DB_ENV_NOLOCKING        = $00000080; (* DB_NOLOCKING set *)
  DB_ENV_NOMMAP           = $00000100; (* DB_NOMMAP set *)
  DB_ENV_NOPANIC          = $00000200; (* Okay if panic set *)
  DB_ENV_OVERWRITE        = $00000400; (* DB_OVERWRITE set *)
  DB_ENV_REGION_INIT      = $00000800; (* DB_REGION_INIT set *)
  DB_ENV_TIME_NOTGRANTED  = $00001000; (* DB_TIME_NOTGRANTED set *)
  DB_ENV_TXN_NOSYNC       = $00002000; (* DB_TXN_NOSYNC set *)
  DB_ENV_TXN_NOWAIT       = $00004000; (* DB_TXN_NOWAIT set *)
  DB_ENV_TXN_SNAPSHOT     = $00008000; (* DB_TXN_SNAPSHOT set *)
  DB_ENV_TXN_WRITE_NOSYNC = $00010000; (* DB_TXN_WRITE_NOSYNC set *)
  DB_ENV_YIELDCPU         = $00020000; (* DB_YIELDCPU set *)
  DB_ENV_HOTBACKUP        = $00040000; (* DB_HOTBACKUP_IN_PROGRESS set *)
  DB_ENV_NOFLUSH          = $00080000; (* DB_NOFLUSH set *)

(*
 *
 *)
  DB_AGGRESSIVE               = $00000001;
  DB_ARCH_ABS                 = $00000001;
  DB_ARCH_DATA                = $00000002;
  DB_ARCH_LOG                 = $00000004;
  DB_ARCH_REMOVE              = $00000008;
  DB_AUTO_COMMIT              = $00000100;
  DB_CDB_ALLDB                = $00000040;
  DB_CHKSUM                   = $00000008;
  DB_CKP_INTERNAL             = $00000002;
  DB_CREATE_                  = $00000001;
  DB_CURSOR_BULK              = $00000001;
  DB_CURSOR_TRANSIENT         = $00000004;
  DB_CXX_NO_EXCEPTIONS        = $00000002;
  DB_DATABASE_LOCKING         = $00000080;
  DB_DIRECT                   = $00000010;
  DB_DIRECT_DB                = $00000200;
  DB_DSYNC_DB                 = $00000400;
  DB_DUP                      = $00000010;
  DB_DUPSORT                  = $00000004;
  DB_DURABLE_UNKNOWN          = $00000020;
  DB_ENCRYPT                  = $00000001;
  DB_ENCRYPT_AES              = $00000001;
  DB_EXCL                     = $00000040;
  DB_EXTENT                   = $00000040;
  DB_FAILCHK                  = $00000020;
  DB_FAST_STAT                = $00000001;
  DB_FCNTL_LOCKING            = $00000800;
  DB_FLUSH                    = $00000001;
  DB_FORCE                    = $00000001;
  DB_FORCESYNC                = $00000001;
  DB_FOREIGN_ABORT            = $00000001;
  DB_FOREIGN_CASCADE          = $00000002;
  DB_FOREIGN_NULLIFY          = $00000004;
  DB_FREELIST_ONLY            = $00000001;
  DB_FREE_SPACE               = $00000002;
  DB_HOTBACKUP_IN_PROGRESS    = $00000800;
  DB_IGNORE_LEASE             = $00001000;
  DB_IMMUTABLE_KEY            = $00000002;
  DB_INIT_CDB                 = $00000040;
  DB_INIT_LOCK                = $00000080;
  DB_INIT_LOG                 = $00000100;
  DB_INIT_MPOOL               = $00000200;
  DB_INIT_REP                 = $00000400;
  DB_INIT_TXN                 = $00000800;
  DB_INORDER                  = $00000020;
  DB_JOIN_NOSORT              = $00000001;
  DB_LOCKDOWN                 = $00001000;
  DB_LOCK_CHECK               = $00000001;
  DB_LOCK_NOWAIT              = $00000002;
  DB_LOCK_RECORD              = $00000004;
  DB_LOCK_SET_TIMEOUT         = $00000008;
  DB_LOCK_SWITCH              = $00000010;
  DB_LOCK_UPGRADE             = $00000020;
  DB_LOG_AUTO_REMOVE          = $00000001;
  DB_LOG_CHKPNT               = $00000002;
  DB_LOG_COMMIT               = $00000004;
  DB_LOG_DIRECT               = $00000002;
  DB_LOG_DSYNC                = $00000004;
  DB_LOG_IN_MEMORY            = $00000008;
  DB_LOG_NOCOPY               = $00000008;
  DB_LOG_NOT_DURABLE          = $00000010;
  DB_LOG_NO_DATA              = $00000004;
  DB_LOG_VERIFY_CAF           = $00000001;
  DB_LOG_VERIFY_DBFILE        = $00000002;
  DB_LOG_VERIFY_ERR           = $00000004;
  DB_LOG_VERIFY_FORWARD       = $00000008;
  DB_LOG_VERIFY_INTERR        = $00000010;
  DB_LOG_VERIFY_PARTIAL       = $00000020;
  DB_LOG_VERIFY_VERBOSE       = $00000040;
  DB_LOG_VERIFY_WARNING       = $00000080;
  DB_LOG_WRNOSYNC             = $00000020;
  DB_LOG_ZERO                 = $00000010;
  DB_MPOOL_CREATE             = $00000001;
  DB_MPOOL_DIRTY              = $00000002;
  DB_MPOOL_DISCARD            = $00000001;
  DB_MPOOL_EDIT               = $00000004;
  DB_MPOOL_FREE               = $00000008;
  DB_MPOOL_LAST               = $00000010;
  DB_MPOOL_NEW                = $00000020;
  DB_MPOOL_NOFILE             = $00000001;
  DB_MPOOL_NOLOCK             = $00000002;
  DB_MPOOL_TRY                = $00000040;
  DB_MPOOL_UNLINK             = $00000002;
  DB_MULTIPLE                 = $00000800;
  DB_MULTIPLE_KEY             = $00004000;
  DB_MULTIVERSION             = $00000004;
  DB_MUTEX_ALLOCATED          = $00000001;
  DB_MUTEX_LOCKED             = $00000002;
  DB_MUTEX_LOGICAL_LOCK       = $00000004;
  DB_MUTEX_PROCESS_ONLY       = $00000008;
  DB_MUTEX_SELF_BLOCK         = $00000010;
  DB_MUTEX_SHARED             = $00000020;
  DB_NOERROR                  = $00001000;
  DB_NOFLUSH                  = $00001000;
  DB_NOLOCKING                = $00002000;
  DB_NOMMAP                   = $00000008;
  DB_NOORDERCHK               = $00000002;
  DB_NOPANIC                  = $00004000;
  DB_NOSYNC                   = $00000001;
  DB_NO_AUTO_COMMIT           = $00002000;
  DB_NO_CHECKPOINT            = $00002000;
  DB_ODDFILESIZE              = $00000080;
  DB_ORDERCHKONLY             = $00000004;
  DB_OVERWRITE                = $00008000;
  DB_PANIC_ENVIRONMENT        = $00010000;
  DB_PRINTABLE                = $00000008;
  DB_PRIVATE                  = $00004000;
  DB_PR_PAGE                  = $00000010;
  DB_PR_RECOVERYTEST          = $00000020;
  DB_RDONLY                   = $00000400;
  DB_RDWRMASTER               = $00004000;
  DB_READ_COMMITTED           = $00000400;
  DB_READ_UNCOMMITTED         = $00000200;
  DB_RECNUM                   = $00000040;
  DB_RECOVER                  = $00000002;
  DB_RECOVER_FATAL            = $00008000;
  DB_REGION_INIT              = $00020000;
  DB_REGISTER                 = $00010000;
  DB_RENUMBER                 = $00000080;
  DB_REPMGR_CONF_2SITE_STRICT = $00000001;
  DB_REPMGR_CONF_ELECTIONS    = $00000002;
  DB_REPMGR_PEER              = $00000001;
  DB_REP_ANYWHERE             = $00000001;
  DB_REP_CLIENT               = $00000001;
  DB_REP_CONF_AUTOINIT        = $00000004;
  DB_REP_CONF_BULK            = $00000008;
  DB_REP_CONF_DELAYCLIENT     = $00000010;
  DB_REP_CONF_INMEM           = $00000020;
  DB_REP_CONF_LEASE           = $00000040;
  DB_REP_CONF_NOWAIT          = $00000080;
  DB_REP_ELECTION             = $00000004;
  DB_REP_MASTER               = $00000002;
  DB_REP_NOBUFFER             = $00000002;
  DB_REP_PERMANENT            = $00000004;
  DB_REP_REREQUEST            = $00000008;
  DB_REVSPLITOFF              = $00000100;
  DB_RMW                      = $00002000;
  DB_SALVAGE                  = $00000040;
  DB_SA_SKIPFIRSTKEY          = $00000080;
  DB_SA_UNKNOWNKEY            = $00000100;
  DB_SEQ_DEC                  = $00000001;
  DB_SEQ_INC                  = $00000002;
  DB_SEQ_RANGE_SET            = $00000004;
  DB_SEQ_WRAP                 = $00000008;
  DB_SEQ_WRAPPED              = $00000010;
  DB_SET_LOCK_TIMEOUT         = $00000001;
  DB_SET_REG_TIMEOUT          = $00000004;
  DB_SET_TXN_NOW              = $00000008;
  DB_SET_TXN_TIMEOUT          = $00000002;
  DB_SHALLOW_DUP              = $00000100;
  DB_SNAPSHOT                 = $00000200;
  DB_STAT_ALL                 = $00000004;
  DB_STAT_CLEAR               = $00000001;
  DB_STAT_LOCK_CONF           = $00000008;
  DB_STAT_LOCK_LOCKERS        = $00000010;
  DB_STAT_LOCK_OBJECTS        = $00000020;
  DB_STAT_LOCK_PARAMS         = $00000040;
  DB_STAT_MEMP_HASH           = $00000008;
  DB_STAT_MEMP_NOERROR        = $00000010;
  DB_STAT_SUBSYSTEM           = $00000002;
  DB_ST_DUPOK                 = $00000200;
  DB_ST_DUPSET                = $00000400;
  DB_ST_DUPSORT               = $00000800;
  DB_ST_IS_RECNO              = $00001000;
  DB_ST_OVFL_LEAF             = $00002000;
  DB_ST_RECNUM                = $00004000;
  DB_ST_RELEN                 = $00008000;
  DB_ST_TOPLEVEL              = $00010000;
  DB_SYSTEM_MEM               = $00020000;
  DB_THREAD                   = $00000010;
  DB_TIME_NOTGRANTED          = $00040000;
  DB_TRUNCATE                 = $00008000;
  DB_TXN_BULK                 = $00000008;
  DB_TXN_FAMILY               = $00000040;
  DB_TXN_NOSYNC               = $00000001;
  DB_TXN_NOT_DURABLE          = $00000002;
  DB_TXN_NOWAIT               = $00000002;
  DB_TXN_SNAPSHOT             = $00000010;
  DB_TXN_SYNC                 = $00000004;
  DB_TXN_WAIT                 = $00000080;
  DB_TXN_WRITE_NOSYNC         = $00000020;
  DB_UNREF                    = $00020000;
  DB_UPGRADE                  = $00000001;
  DB_USE_ENVIRON              = $00000004;
  DB_USE_ENVIRON_ROOT         = $00000008;
  DB_VERB_DEADLOCK            = $00000001;
  DB_VERB_FILEOPS             = $00000002;
  DB_VERB_FILEOPS_ALL         = $00000004;
  DB_VERB_RECOVERY            = $00000008;
  DB_VERB_REGISTER            = $00000010;
  DB_VERB_REPLICATION         = $00000020;
  DB_VERB_REPMGR_CONNFAIL     = $00000040;
  DB_VERB_REPMGR_MISC         = $00000080;
  DB_VERB_REP_ELECT           = $00000100;
  DB_VERB_REP_LEASE           = $00000200;
  DB_VERB_REP_MISC            = $00000400;
  DB_VERB_REP_MSGS            = $00000800;
  DB_VERB_REP_SYNC            = $00001000;
  DB_VERB_REP_SYSTEM          = $00002000;
  DB_VERB_REP_TEST            = $00004000;
  DB_VERB_WAITSFOR            = $00008000;
  DB_VERIFY                   = $00000002;
  DB_VERIFY_PARTITION         = $00040000;
  DB_WRITECURSOR              = $00000008;
  DB_WRITELOCK                = $00000020;
  DB_WRITEOPEN                = $00010000;
  DB_YIELDCPU                 = $00080000;



// TRANSLATION: HELPER TYPES...

type

// Translation: C library types
  size_t = longword;
  time_t = int64;

(*
 * Forward structure declarations, so we can declare pointers and
 * applications can get type checking.
 *)
  PDB                   = ^__db               ; // @@PAS_HELPER(PDB)
  PDB_BTREE_STAT        = ^__db_bt_stat       ; // @@PAS_HELPER(PDB_BTREE_STAT)
  //PDB_CIPHER          = ^__db_cipher        ;
  PDB_COMPACT           = ^__db_compact       ; // @@PAS_HELPER(PDB_COMPACT)
  PDB_DISTAB            = ^__db_distab        ; // @@PAS_HELPER(PDB_DISTAB)
  PDB_ENV               = ^__db_env           ; // @@PAS_HELPER(PDB_ENV)
  PDB_HASH_STAT         = ^__db_h_stat        ; // @@PAS_HELPER(PDB_HASH_STAT)
  PDB_LOCK_ILOCK        = ^__db_ilock         ; // @@PAS_HELPER(PDB_LOCK_ILOCK)
  PDB_LOCK_HSTAT        = ^__db_lock_hstat    ; // @@PAS_HELPER(PDB_LOCK_HSTAT)
  PDB_LOCK_PSTAT        = ^__db_lock_pstat    ; // @@PAS_HELPER(PDB_LOCK_PSTAT)
  PDB_LOCK_STAT         = ^__db_lock_stat     ; // @@PAS_HELPER(PDB_LOCK_STAT)
  //PDB_LOCKER          = ^__db_locker        ;
  PDB_LOCKREQ           = ^__db_lockreq       ; // @@PAS_HELPER(PDB_LOCKREQ)
  //PDB_LOCKTAB         = ^__db_locktab       ;
  //PDB_LOG             = ^__db_log           ;
  PDB_LOGC              = ^__db_log_cursor    ; // @@PAS_HELPER(PDB_LOGC)
  PDB_LOG_STAT          = ^__db_log_stat      ; // @@PAS_HELPER(PDB_LOG_STAT)
  PDB_LSN               = ^__db_lsn           ; // @@PAS_HELPER(PDB_LSN)
  //PDB_MPOOL           = ^__db_mpool         ;
  PDB_MPOOL_FSTAT       = ^__db_mpool_fstat   ; // @@PAS_HELPER(PDB_MPOOL_FSTAT)
  PDB_MPOOL_STAT        = ^__db_mpool_stat    ; // @@PAS_HELPER(PDB_MPOOL_STAT)
  PDB_MPOOLFILE         = ^__db_mpoolfile     ; // @@PAS_HELPER(PDB_MPOOLFILE)
  PDB_MUTEX_STAT        = ^__db_mutex_stat    ; // @@PAS_HELPER(PDB_MUTEX_STAT)
  //PDB_MUTEX           = ^__db_mutex_t       ;
  //PDB_MUTEXMGR        = ^__db_mutexmgr      ;
  PDB_PREPLIST          = ^__db_preplist      ; // @@PAS_HELPER(PDB_PREPLIST)
  PDB_QUEUE_STAT        = ^__db_qam_stat      ; // @@PAS_HELPER(PDB_QUEUE_STAT)
  //PDB_REP             = ^__db_rep           ;
  PDB_REP_STAT          = ^__db_rep_stat      ; // @@PAS_HELPER(PDB_REP_STAT)
  PDB_REPMGR_SITE       = ^__db_repmgr_site   ; // @@PAS_HELPER(PDB_REPMGR_SITE)
  PDB_REPMGR_STAT       = ^__db_repmgr_stat   ; // @@PAS_HELPER(PDB_REPMGR_STAT)
  PDB_SEQ_RECORD        = ^__db_seq_record    ; // @@PAS_HELPER(PDB_SEQ_RECORD)
  PDB_SEQUENCE_STAT     = ^__db_seq_stat      ; // @@PAS_HELPER(PDB_SEQUENCE_STAT)
  PDB_SEQUENCE          = ^__db_sequence      ; // @@PAS_HELPER(PDB_SEQUENCE)
  //PDB_THREAD_INFO     = ^__db_thread_info   ;
  PDB_TXN               = ^__db_txn           ; // @@PAS_HELPER(PDB_TXN)
  PDB_TXN_ACTIVE        = ^__db_txn_active    ; // @@PAS_HELPER(PDB_TXN_ACTIVE)
  PDB_TXN_STAT          = ^__db_txn_stat      ; // @@PAS_HELPER(PDB_TXN_STAT)
  PDB_TXN_TOKEN         = ^__db_txn_token     ; // @@PAS_HELPER(PDB_TXN_TOKEN)
  //PDB_TXNMGR          = ^__db_txnmgr        ;
  PDBC                  = ^__dbc              ; // @@PAS_HELPER(PDBC)
  //PDBC_INTERNAL       = ^__dbc_internal     ;
  //PENV                = ^__env              ;
  //PDB_FH              = ^__fh_t             ;
  //PFNAME              = ^__fname            ;
  PDB_KEY_RANGE         = ^__key_range        ; // @@PAS_HELPER(PDB_KEY_RANGE)
  //PMPOOLFILE          = ^__mpoolfile        ;
  PDB_LOG_VERIFY_CONFIG = ^__db_logvrfy_config; // @@PAS_HELPER(PDB_LOG_VERIFY_CONFIG)
  PDBT                  = ^__db_dbt           ; // @@PAS_HELPER(PDBT)

// Translation: helper types for function parameters
  PDB_CACHE_PRIORITY = ^DB_CACHE_PRIORITY; // @@PAS_HELPER(PDB_CACHE_PRIORITY)
  PDB_LOCK           = ^DB_LOCK          ; // @@PAS_HELPER(PDB_LOCK)
  PDB_LOG_RECSPEC    = ^DB_LOG_RECSPEC   ; // @@PAS_HELPER(PDB_LOG_RECSPEC)
  Pdb_mutex_t        = ^db_mutex_t       ; // @@PAS_HELPER(Pdb_mutex_t)
  Pdb_pgno_t         = ^db_pgno_t        ; // @@PAS_HELPER(Pdb_pgno_t)
  Pdb_recno_t        = ^db_recno_t       ; // @@PAS_HELPER(Pdb_recno_t)
  Pdb_seq_t          = ^db_seq_t         ; // @@PAS_HELPER(Pdb_seq_t)
  //PDB_THREAD_INFO  = ^DB_THREAD_INFO
  PDB_THREAD_INFO    = pointer           ; // @@PAS_HELPER(PDB_THREAD_INFO)
  Pdb_threadid_t     = ^db_threadid_t    ; // @@PAS_HELPER(Pdb_threadid_t)
  Pdb_timeout_t      = ^db_timeout_t     ; // @@PAS_HELPER(Pdb_timeout_t)
  Pint32_t           = ^int32_t          ; // @@PAS_HELPER(Pint32_t)
  Pinteger           = ^integer          ; // @@PAS_HELPER(Pinteger)
  Plongint           = ^longint          ; // @@PAS_HELPER(Plongint)
  PPAnsiChar         = ^PAnsiChar        ; // @@PAS_HELPER(PPAnsiChar)
  PPDB               = ^PDB              ; // @@PAS_HELPER(PPDB)
  PPDB_LOCK_STAT     = ^PDB_LOCK_STAT    ; // @@PAS_HELPER(PPDB_LOCK_STAT)
  PPDB_LOCKREQ       = ^PDB_LOCKREQ      ; // @@PAS_HELPER(PPDB_LOCKREQ)
  PPDB_LOG_STAT      = ^PDB_LOG_STAT     ; // @@PAS_HELPER(PPDB_LOG_STAT)
  PPDB_LOGC          = ^PDB_LOGC         ; // @@PAS_HELPER(PPDB_LOGC)
  PPDB_LSN           = ^PDB_LSN          ; // @@PAS_HELPER(PPDB_LSN)
  PPDB_MPOOL_FSTAT   = ^PDB_MPOOL_FSTAT  ; // @@PAS_HELPER(PPDB_MPOOL_FSTAT)
  PPDB_MPOOL_STAT    = ^PDB_MPOOL_STAT   ; // @@PAS_HELPER(PPDB_MPOOL_STAT)
  PPDB_MPOOLFILE     = ^PDB_MPOOLFILE    ; // @@PAS_HELPER(PPDB_MPOOLFILE)
  PPDB_MUTEX_STAT    = ^PDB_MUTEX_STAT   ; // @@PAS_HELPER(PPDB_MUTEX_STAT)
  PPDB_REP_STAT      = ^PDB_REP_STAT     ; // @@PAS_HELPER(PPDB_REP_STAT)
  PPDB_REPMGR_SITE   = ^PDB_REPMGR_SITE  ; // @@PAS_HELPER(PPDB_REPMGR_SITE)
  PPDB_REPMGR_STAT   = ^PDB_REPMGR_STAT  ; // @@PAS_HELPER(PPDB_REPMGR_STAT)
  PPDB_SEQUENCE_STAT = ^PDB_SEQUENCE_STAT; // @@PAS_HELPER(PPDB_SEQUENCE_STAT)
  PPDB_TXN           = ^PDB_TXN          ; // @@PAS_HELPER(PPDB_TXN)
  PPDB_TXN_STAT      = ^PDB_TXN_STAT     ; // @@PAS_HELPER(PPDB_TXN_STAT)
  PPDBC              = ^PDBC             ; // @@PAS_HELPER(PPDBC)
  PPDBT              = ^PDBT             ; // @@PAS_HELPER(PPDBT)
  Ppid_t             = ^pid_t            ; // @@PAS_HELPER(Ppid_t)
  PPPAnsiChar        = ^PPAnsiChar       ; // @@PAS_HELPER(PPPAnsiChar)
  PPPDB_MPOOL_FSTAT  = ^PPDB_MPOOL_FSTAT ; // @@PAS_HELPER(PPPDB_MPOOL_FSTAT)
  PPu_int8_t         = ^Pu_int8_t        ; // @@PAS_HELPER(PPu_int8_t)
  Psize_t            = ^size_t           ; // @@PAS_HELPER(Psize_t)
  Ptime_t            = ^time_t           ; // @@PAS_HELPER(Ptime_t)
  Pu_int             = ^u_int            ; // @@PAS_HELPER(Pu_int)
  Pu_int32_t         = ^u_int32_t        ; // @@PAS_HELPER(Pu_int32_t)
  Pu_int8_t          = ^u_int8_t         ; // @@PAS_HELPER(Pu_int8_t)


// TRANSLATION: TYPES...

(*
 * !!!
 * Berkeley DB uses specifically sized types.  If they're not provided by
 * the system, type  them here.
 *
 * We protect them against multiple inclusion using __BIT_TYPES_DEFINED__,
 * as does BIND and Kerberos, since we don't know for sure what //include
 * files the user is using.
 *
 * !!!
 * We also provide the standard u_int, u_long etc., if they're not provided
 * by the system.
 *)
  u_int8_t = byte;
  int16_t = smallint;
  u_int16_t = word;
  int32_t = integer;
  u_int32_t = longword;
  int64_t = int64;
  u_int64_t = int64;

  u_char = byte;
  u_int = longword;
  u_long = longword;
  u_short = word;

(*
 * Missing ANSI types.
 *
 * uintmax_t --
 * Largest unsigned type, used to align structures in memory.  We don't store
 * floating point types in structures, so integral types should be sufficient
 * (and we don't have to worry about systems that store floats in other than
 * power-of-2 numbers of bytes).  Additionally this fixes compilers that rewrite
 * structure assignments and ANSI C memcpy calls to be in-line instructions
 * that happen to require alignment.
 *
 * uintptr_t --
 * Unsigned type that's the same size as a pointer.  There are places where
 * DB modifies pointers by discarding the bottom bits to guarantee alignment.
 * We can't use uintmax_t, it may be larger than the pointer, and compilers
 * get upset about that.  So far we haven't run on any machine where there's
 * no unsigned type the same size as a pointer -- here's hoping.
 *)
  uintmax_t = u_int64_t;
{$IFDEF _WIN64}
  uintptr_t = u_int64_t;
{$ELSE}
  uintptr_t = u_int32_t;
{$ENDIF}

(*
 * Windows defines off_t to long (i.e., 32 bits).  We need to pass 64-bit
 * file offsets, so we declare our own.
 *)
  off_t = int64_t;
  pid_t = integer;
{$IFDEF _WIN64}
  ssize_t = int64_t;
{$ELSE}
  ssize_t = int32_t;
{$ENDIF}

(*
 * Sequences are only available on machines with 64-bit integral types.
 *)
  db_seq_t = int64_t;

(* Thread and process identification. *)
  db_threadid_t = u_int32_t;

(* Basic types that are exported or quasi-exported. *)
  db_pgno_t = u_int32_t;
  db_indx_t = u_int16_t;

  db_recno_t = u_int32_t;

  db_timeout_t = u_int32_t;

(*
 * Region offsets are the difference between a pointer in a region and the
 * region's base address.  With private environments, both addresses are the
 * result of calling malloc, and we can't assume anything about what malloc
 * will return, so region offsets have to be able to hold differences between
 * arbitrary pointers.
 *)
  roff_t = uintptr_t;

(* Key/data structure -- a Data-Base Thang. *)
  __db_dbt = record
    data: pointer;      (* Key/data *)
    size: u_int32_t;      (* key/data length *)
    ulen: u_int32_t;      (* RO: length of user buffer. *)
    dlen: u_int32_t;      (* RO: get/put record length. *)
    doff: u_int32_t;      (* RO: get/put record offset. *)
    app_data: pointer;
    flags: u_int32_t;
  end;
  DBT = __db_dbt;

(*******************************************************
 * Mutexes.
 *******************************************************)
  db_mutex_t = u_int32_t;

  __db_mutex_stat = record
    st_mutex_align: u_int32_t;  (* Mutex alignment *)
    st_mutex_tas_spins: u_int32_t;  (* Mutex test-and-set spins *)
    st_mutex_cnt: u_int32_t;    (* Mutex count *)
    st_mutex_free: u_int32_t; (* Available mutexes *)
    st_mutex_inuse: u_int32_t;  (* Mutexes in use *)
    st_mutex_inuse_max: u_int32_t;  (* Maximum mutexes ever in use *)
    (* The following fields are filled-in from other places. *)
{$IFNDEF __TEST_DB_NO_STATISTICS}
    st_region_wait: uintmax_t;  (* Region lock granted after wait. *)
    st_region_nowait: uintmax_t;  (* Region lock granted without wait. *)
    st_regsize: roff_t;   (* Region size. *)
{$ENDIF}
  end;

(*
 * Simple R/W lock modes and for multi-granularity intention locking.
 *
 * !!!
 * These values are NOT random, as they are used as an index into the lock
 * conflicts arrays, i.e., DB_LOCK_IWRITE must be = 3, and DB_LOCK_IREAD
 * must be = 4.
 *)
  db_lockmode_t = (
    DB_LOCK_NG=0,     (* Not granted. *)
    DB_LOCK_READ=1,     (* Shared/read. *)
    DB_LOCK_WRITE=2,    (* Exclusive/write. *)
    DB_LOCK_WAIT=3,     (* Wait for event *)
    DB_LOCK_IWRITE=4,   (* Intent exclusive/write. *)
    DB_LOCK_IREAD=5,    (* Intent to share/read. *)
    DB_LOCK_IWR=6,      (* Intent to read and write. *)
    DB_LOCK_READ_UNCOMMITTED=7, (* Degree 1 isolation. *)
    DB_LOCK_WWRITE=8    (* Was Written. *)
  );

(*
 * Request types.
 *)
  db_lockop_t = (
    DB_LOCK_DUMP=0,     (* Display held locks. *)
    DB_LOCK_GET=1,      (* Get the lock. *)
    DB_LOCK_GET_TIMEOUT=2,    (* Get lock with a timeout. *)
    DB_LOCK_INHERIT=3,    (* Pass locks to parent. *)
    DB_LOCK_PUT=4,      (* Release the lock. *)
    DB_LOCK_PUT_ALL=5,    (* Release locker's locks. *)
    DB_LOCK_PUT_OBJ=6,    (* Release locker's locks on obj. *)
    DB_LOCK_PUT_READ=7,   (* Release locker's read locks. *)
    DB_LOCK_TIMEOUT=8,    (* Force a txn to timeout. *)
    DB_LOCK_TRADE=9,    (* Trade locker ids on a lock. *)
    DB_LOCK_UPGRADE_WRITE=10  (* Upgrade writes for dirty reads. *)
  );

(*
 * Status of a lock.
 *)
  db_status_t = (
    DB_LSTAT_ABORTED=1,   (* Lock belongs to an aborted txn. *)
    DB_LSTAT_EXPIRED=2,   (* Lock has expired. *)
    DB_LSTAT_FREE=3,    (* Lock is unallocated. *)
    DB_LSTAT_HELD=4,    (* Lock is currently held. *)
    DB_LSTAT_PENDING=5,   (* Lock was waiting and has been
             * promoted; waiting for the owner
             * to run and upgrade it to held. *)
    DB_LSTAT_WAITING=6    (* Lock is on the wait queue. *)
  );


(* Lock statistics structure. *)

  __db_lock_stat = record
    st_id: u_int32_t;                           (* Last allocated locker ID. *)
    st_cur_maxid: u_int32_t;                    (* Current maximum unused ID. *)
    st_maxlocks: u_int32_t;                     (* Maximum number of locks in table. *)
    st_maxlockers: u_int32_t;                   (* Maximum num of lockers in table. *)
    st_maxobjects: u_int32_t;                   (* Maximum num of objects in table. *)
    st_partitions: u_int32_t;                   (* number of partitions. *)
    st_nmodes: integer;                         (* Number of lock modes. *)
    st_nlockers: u_int32_t;                     (* Current number of lockers. *)
{$IFNDEF __TEST_DB_NO_STATISTICS}
    st_nlocks: u_int32_t;                       (* Current number of locks. *)
    st_maxnlocks: u_int32_t;                    (* Maximum number of locks so far. *)
    st_maxhlocks: u_int32_t;                    (* Maximum number of locks in any bucket. *)
    st_locksteals: uintmax_t;                   (* Number of lock steals so far. *)
    st_maxlsteals: uintmax_t;                   (* Maximum number steals in any partition. *)
    st_maxnlockers: u_int32_t;                  (* Maximum number of lockers so far. *)
    st_nobjects: u_int32_t;                     (* Current number of objects. *)
    st_maxnobjects: u_int32_t;                  (* Maximum number of objects so far. *)
    st_maxhobjects: u_int32_t;                  (* Maximum number of objectsin any bucket. *)
    st_objectsteals: uintmax_t;                 (* Number of objects steals so far. *)
    st_maxosteals: uintmax_t;                   (* Maximum number of steals in any partition. *)
    st_nrequests: uintmax_t;                    (* Number of lock gets. *)
    st_nreleases: uintmax_t;                    (* Number of lock puts. *)
    st_nupgrade: uintmax_t;                     (* Number of lock upgrades. *)
    st_ndowngrade: uintmax_t;                   (* Number of lock downgrades. *)
    st_lock_wait: uintmax_t;                    (* Lock conflicts w/ subsequent wait *)
    st_lock_nowait: uintmax_t;                  (* Lock conflicts w/o subsequent wait *)
    st_ndeadlocks: uintmax_t;                   (* Number of lock deadlocks. *)
    st_locktimeout: db_timeout_t;               (* Lock timeout. *)
    st_nlocktimeouts: uintmax_t;                (* Number of lock timeouts. *)
    st_txntimeout: db_timeout_t;                (* Transaction timeout. *)
    st_ntxntimeouts: uintmax_t;                 (* Number of transaction timeouts. *)
    st_part_wait: uintmax_t;                    (* Partition lock granted after wait. *)
    st_part_nowait: uintmax_t;                  (* Partition lock granted without wait. *)
    st_part_max_wait: uintmax_t;                (* Max partition lock granted after wait. *)
    st_part_max_nowait: uintmax_t;              (* Max partition lock granted without wait. *)
    st_objs_wait: uintmax_t;                    (* Object lock granted after wait. *)
    st_objs_nowait: uintmax_t;                  (* Object lock granted without wait. *)
    st_lockers_wait: uintmax_t;                 (* Locker lock granted after wait. *)
    st_lockers_nowait: uintmax_t;               (* Locker lock granted without wait. *)
    st_region_wait: uintmax_t;                  (* Region lock granted after wait. *)
    st_region_nowait: uintmax_t;                (* Region lock granted without wait. *)
    st_hash_len: u_int32_t;                     (* Max length of bucket. *)
    st_regsize: roff_t;                         (* Region size. *)
{$ENDIF}
  end;

  __db_lock_hstat = record
    st_nrequests: uintmax_t;        (* Number of lock gets. *)
    st_nreleases: uintmax_t;        (* Number of lock puts. *)
    st_nupgrade: uintmax_t;         (* Number of lock upgrades. *)
    st_ndowngrade: uintmax_t;       (* Number of lock downgrades. *)
    st_nlocks: u_int32_t;           (* Current number of locks. *)
    st_maxnlocks: u_int32_t;        (* Maximum number of locks so far. *)
    st_nobjects: u_int32_t;         (* Current number of objects. *)
    st_maxnobjects: u_int32_t;      (* Maximum number of objects so far. *)
    st_lock_wait: uintmax_t;        (* Lock conflicts w/ subsequent wait *)
    st_lock_nowait: uintmax_t;      (* Lock conflicts w/o subsequent wait *)
    st_nlocktimeouts: uintmax_t;    (* Number of lock timeouts. *)
    st_ntxntimeouts: uintmax_t;     (* Number of transaction timeouts. *)
    st_hash_len: u_int32_t;         (* Max length of bucket. *)
  end;

  __db_lock_pstat = record
    st_nlocks: u_int32_t;           (* Current number of locks. *)
    st_maxnlocks: u_int32_t;        (* Maximum number of locks so far. *)
    st_nobjects: u_int32_t;         (* Current number of objects. *)
    st_maxnobjects: u_int32_t;      (* Maximum number of objects so far. *)
    st_locksteals: uintmax_t;       (* Number of lock steals so far. *)
    st_objectsteals: uintmax_t;     (* Number of objects steals so far. *)
  end;

(*
 * DB_LOCK_ILOCK --
 *  Internal DB access method lock.
 *)
  __db_ilock = record
    pgno: DB_PGNO_T;                                 (* Page being locked. *)
    fileid: array[0..DB_FILE_ID_LEN-1] of u_int8_t;  (* File id. *)
    type_: u_int32_t;                                (* Type of lock. *) // @@PAS_MAPPING(type_=type)
  end;
  DB_LOCK_ILOCK = __db_ilock;

(*
 * DB_LOCK --
 *  The structure is allocated by the caller and filled in during a
 *  lock_get request (or a lock_vec/DB_LOCK_GET).
 *)
  __db_lock_u = record
    off: roff_t;            (* Offset of the lock in the region *)
    ndx: u_int32_t;         (* Index of the object referenced by this lock; used for locking. *)
    gen: u_int32_t;         (* Generation number of this lock. *)
    mode: db_lockmode_t;    (* mode of this lock. *)
  end;
  DB_LOCK = __db_lock_u;

(* Lock request structure. *)
  __db_lockreq = record
    op: db_lockop_t;        (* Operation. *)
    mode: db_lockmode_t;    (* Requested mode. *)
    timeout: db_timeout_t;  (* Time to expire lock. *)
    obj: ^DBT;              (* Object being locked. *)
    lock: DB_LOCK;          (* Lock returned. *)
  end;

(*
 * A DB_LSN has two parts, a fileid which identifies a specific file, and an
 * offset within that file.  The fileid is an Cardinal 4-byte quantity that
 * uniquely identifies a file within the log directory -- currently a simple
 * counter inside the log.  The offset is also an Cardinal 4-byte value.  The
 * log manager guarantees the offset is never more than 4 bytes by switching
 * to a new log file before the maximum length imposed by an Cardinal 4-byte
 * offset is reached.
 *)
  __db_lsn = record
    file_: u_int32_t;   (* file ID. *) // @@PAS_MAPPING(file_=file)
    offset: u_int32_t;    (* file offset. *)
  end;
  DB_LSN = __db_lsn;

(*
 * DB_LOGC --
 *  Log cursor.
 *)
  __db_log_cursor = record
    env: pointer; //^ENV      (* Environment *)
    fhp: pointer; //^DB_FH;     (* file handle. *)
    lsn: DB_LSN;      (* Cursor: LSN *)
    len: u_int32_t;     (* Cursor: record length *)
    prev: u_int32_t;      (* Cursor: previous record's offset *)
    dbt: DBT;     (* Return DBT. *)
    p_lsn: DB_LSN;    (* Persist LSN. *)
    p_version: u_int32_t;   (* Persist version. *)
    bp: ^u_int8_t;      (* Allocated read buffer. *)
    bp_size: u_int32_t;   (* Read buffer length in bytes. *)
    bp_rlen: u_int32_t;   (* Read buffer valid data length. *)
    bp_lsn: DB_LSN;   (* Read buffer first byte LSN. *)
    bp_maxrec: u_int32_t;   (* Max record length in the log file. *)

    (* DB_LOGC PUBLIC HANDLE LIST BEGIN *)
    close  : function(p1: PDB_LOGC; p2: u_int32_t): integer;
    get    : function(p1: PDB_LOGC; p2: PDB_LSN; p3: PDBT; p4: u_int32_t): integer;
    version: function(p1: PDB_LOGC; p2: Pu_int32_t; p3: u_int32_t): integer;
    (* DB_LOGC PUBLIC HANDLE LIST END *)

    flags: u_int32_t;
  end;

(* Log statistics structure. *)
  __db_log_stat = record
    st_magic: u_int32_t;    (* Log file magic number. *)
    st_version: u_int32_t;    (* Log file version number. *)
    st_mode: integer;   (* Log file permissions mode. *)
    st_lg_bsize: u_int32_t;   (* Log buffer size. *)
    st_lg_size: u_int32_t;    (* Log file size. *)
    st_wc_bytes: u_int32_t;   (* Bytes to log since checkpoint. *)
    st_wc_mbytes: u_int32_t;    (* Megabytes to log since checkpoint. *)
{$ifndef __TEST_DB_NO_STATISTICS}
    st_record: uintmax_t;   (* Records entered into the log. *)
    st_w_bytes: u_int32_t;    (* Bytes to log. *)
    st_w_mbytes: u_int32_t;   (* Megabytes to log. *)
    st_wcount: uintmax_t;   (* Total I/O writes to the log. *)
    st_wcount_fill: uintmax_t;  (* Overflow writes to the log. *)
    st_rcount: uintmax_t;   (* Total I/O reads from the log. *)
    st_scount: uintmax_t;   (* Total syncs to the log. *)
    st_region_wait: uintmax_t;  (* Region lock granted after wait. *)
    st_region_nowait: uintmax_t;  (* Region lock granted without wait. *)
    st_cur_file: u_int32_t;   (* Current log file number. *)
    st_cur_offset: u_int32_t; (* Current log file offset. *)
    st_disk_file: u_int32_t;    (* Known on disk log file number. *)
    st_disk_offset: u_int32_t;  (* Known on disk log file offset. *)
    st_maxcommitperflush: u_int32_t;  (* Max number of commits in a flush. *)
    st_mincommitperflush: u_int32_t;  (* Min number of commits in a flush. *)
    st_regsize: roff_t;   (* Region size. *)
{$endif}
  end;

(*
 * We need to record the first log record of a transaction.  For user
 * defined logging this macro returns the place to put that information,
 * if it is need in rlsnp, otherwise it leaves it unchanged.  We also
 * need to track the last record of the transaction, this returns the
 * place to put that info.
 *)
// #define  DB_SET_TXN_LSNP(txn, blsnp, llsnp)    \
//   ((txn)->set_txn_lsnp(txn, blsnp, llsnp))

(*
 * Definition of the structure which specifies marshalling of log records.
 *)
  log_rec_type_t = (
    LOGREC_Done    = 0,
    LOGREC_ARG     = 1,
    LOGREC_HDR     = 2,
    LOGREC_DATA    = 3,
    LOGREC_DB      = 4,
    LOGREC_DBOP    = 5,
    LOGREC_DBT     = 6,
    LOGREC_LOCKS   = 7,
    LOGREC_OP      = 8,
    LOGREC_PGDBT   = 9,
    LOGREC_PGDDBT  = 10,
    LOGREC_PGLIST  = 11,
    LOGREC_POINTER = 12
  );

  __log_rec_spec = record
    type_: log_rec_type_t; // @@PAS_MAPPING(type_=type)
    offset: u_int32_t;
    name: PAnsiChar;
    fmt: array[0..3] of AnsiChar;
  end;
  DB_LOG_RECSPEC = __log_rec_spec;

(*
 * Size of a DBT in a log record.
 *)
// #define  LOG_DBT_SIZE(dbt)           \
//    (sizeof(u_int32_t) + ((dbt) == NULL ? 0 : (dbt)->size))

(*******************************************************
 * Shared buffer cache (mpool).
 *******************************************************)
(* Priority values for DB_MPOOLFILE->{put,set_priority}. *)
  DB_CACHE_PRIORITY = (
    DB_PRIORITY_UNCHANGED=0,
    DB_PRIORITY_VERY_LOW=1,
    DB_PRIORITY_LOW=2,
    DB_PRIORITY_DEFAULT=3,
    DB_PRIORITY_HIGH=4,
    DB_PRIORITY_VERY_HIGH=5
  );

(* Per-process DB_MPOOLFILE information. *)
  __db_mpoolfile = record
    fhp: pointer; //^DB_FH;     (* Underlying file handle. *)

    (*
     * !!!
     * The ref, pinref and q fields are protected by the region lock.
     *)
    ref: u_int32_t;     (* Reference count. *)

    pinref: u_int32_t;    (* Pinned block reference count. *)

    (*
     * !!!
     * Explicit representations of structures from queue.h.
     * TAILQ_ENTRY(__db_mpoolfile) q;
     *)
    q: record
      tqe_next: PDB_MPOOLFILE;
      tqe_prev: ^PDB_MPOOLFILE;
    end; (* Linked list of DB_MPOOLFILE's. *)

    (*
     * !!!
     * The rest of the fields (with the exception of the MP_FLUSH flag)
     * are not thread-protected, even when they may be modified at any
     * time by the application.  The reason is the DB_MPOOLFILE handle
     * is single-threaded from the viewpoint of the application, and so
     * the only fields needing to be thread-protected are those accessed
     * by checkpoint or sync threads when using DB_MPOOLFILE structures
     * to flush buffers from the cache.
     *)
    env: pointer; //^ENV;   (* Environment *)
    mfp: pointer; //^MPOOLFILE    (* Underlying MPOOLFILE. *)

    clear_len: u_int32_t; (* Cleared length on created pages. *)
    fileid: array[0..DB_FILE_ID_LEN-1] of u_int8_t;(* Unique file ID. *)

    ftype: integer;   (* file type. *)
    lsn_offset: int32_t;  (* LSN offset in page. *)
    gbytes, bytes: u_int32_t; (* Maximum file size. *)
    pgcookie: ^DBT; (* Byte-string passed to pgin/pgout. *)
    priority: int32_t;  (* Cache priority. *)

    addr: pointer;    (* Address of mmap'd region. *)
    len: size_t;    (* Length of mmap'd region. *)

    config_flags: u_int32_t;  (* Flags to DB_MPOOLFILE->set_flags. *)

    (* DB_MPOOLFILE PUBLIC HANDLE LIST BEGIN *)
    close         : function(p1: PDB_MPOOLFILE; p2: u_int32_t): integer; cdecl;
    get           : function(p1: PDB_MPOOLFILE; p2: Pdb_pgno_t; p3: PDB_TXN; p4: u_int32_t; p5: pointer): integer; cdecl;
    get_clear_len : function(p1: PDB_MPOOLFILE; p2: Pu_int32_t): integer; cdecl;
    get_fileid    : function(p1: PDB_MPOOLFILE; p2: Pu_int8_t): integer; cdecl;
    get_flags     : function(p1: PDB_MPOOLFILE; p2: Pu_int32_t): integer; cdecl;
    get_ftype     : function(p1: PDB_MPOOLFILE; p2: Pinteger): integer; cdecl;
    get_last_pgno : function(p1: PDB_MPOOLFILE; p2: Pdb_pgno_t): integer; cdecl;
    get_lsn_offset: function(p1: PDB_MPOOLFILE; p2: Pint32_t): integer; cdecl;
    get_maxsize   : function(p1: PDB_MPOOLFILE; p2: Pu_int32_t; p3: Pu_int32_t): integer; cdecl;
    get_pgcookie  : function(p1: PDB_MPOOLFILE; p2: PDBT): integer; cdecl;
    get_priority  : function(p1: PDB_MPOOLFILE; p2: PDB_CACHE_PRIORITY): integer; cdecl;
    open          : function(p1: PDB_MPOOLFILE; p2: PAnsiChar; p3: u_int32_t; p4: integer; p5: size_t): integer; cdecl;
    put           : function(p1: PDB_MPOOLFILE; p2: pointer; p3: DB_CACHE_PRIORITY; p4: u_int32_t): integer; cdecl;
    set_clear_len : function(p1: PDB_MPOOLFILE; p2: u_int32_t): integer; cdecl;
    set_fileid    : function(p1: PDB_MPOOLFILE; p2: Pu_int8_t): integer; cdecl;
    set_flags     : function(p1: PDB_MPOOLFILE; p2: u_int32_t; p3: integer): integer; cdecl;
    set_ftype     : function(p1: PDB_MPOOLFILE; p2: integer): integer; cdecl;
    set_lsn_offset: function(p1: PDB_MPOOLFILE; p2: int32_t): integer; cdecl;
    set_maxsize   : function(p1: PDB_MPOOLFILE; p2: u_int32_t; p3: u_int32_t): integer; cdecl;
    set_pgcookie  : function(p1: PDB_MPOOLFILE; p2: PDBT): integer; cdecl;
    set_priority  : function(p1: PDB_MPOOLFILE; p2: DB_CACHE_PRIORITY): integer; cdecl;
    sync          : function(p1: PDB_MPOOLFILE): integer; cdecl;
    (* DB_MPOOLFILE PUBLIC HANDLE LIST END *)

    flags: u_int32_t;
  end;

(* Mpool statistics structure. *)
  __db_mpool_stat = record
    st_gbytes: u_int32_t;   (* Total cache size: GB. *)
    st_bytes: u_int32_t;    (* Total cache size: B. *)
    st_ncache: u_int32_t;   (* Number of cache regions. *)
    st_max_ncache: u_int32_t; (* Maximum number of regions. *)
    st_mmapsize: size_t;    (* Maximum file size for mmap. *)
    st_maxopenfd: Integer;    (* Maximum number of open fd's. *)
    st_maxwrite: Integer;   (* Maximum buffers to write. *)
    st_maxwrite_sleep: db_timeout_t;  (* Sleep after writing max buffers. *)
    st_pages: u_int32_t;    (* Total number of pages. *)
{$IFNDEF __TEST_DB_NO_STATISTICS}
    st_map: u_int32_t;    (* Pages from mapped files. *)
    st_cache_hit: uintmax_t;  (* Pages found in the cache. *)
    st_cache_miss: uintmax_t; (* Pages not found in the cache. *)
    st_page_create: uintmax_t;  (* Pages created in the cache. *)
    st_page_in: uintmax_t;    (* Pages read in. *)
    st_page_out: uintmax_t;   (* Pages written out. *)
    st_ro_evict: uintmax_t;   (* Clean pages forced from the cache. *)
    st_rw_evict: uintmax_t;   (* Dirty pages forced from the cache. *)
    st_page_trickle: uintmax_t; (* Pages written by memp_trickle. *)
    st_page_clean: u_int32_t; (* Clean pages. *)
    st_page_dirty: u_int32_t; (* Dirty pages. *)
    st_hash_buckets: u_int32_t; (* Number of hash buckets. *)
    st_hash_mutexes: u_int32_t; (* Number of hash bucket mutexes. *)
    st_pagesize: u_int32_t;   (* Assumed page size. *)
    st_hash_searches: u_int32_t;  (* Total hash chain searches. *)
    st_hash_longest: u_int32_t; (* Longest hash chain searched. *)
    st_hash_examined: uintmax_t;  (* Total hash entries searched. *)
    st_hash_nowait: uintmax_t;  (* Hash lock granted with nowait. *)
    st_hash_wait: uintmax_t;    (* Hash lock granted after wait. *)
    st_hash_max_nowait: uintmax_t;  (* Max hash lock granted with nowait. *)
    st_hash_max_wait: uintmax_t;  (* Max hash lock granted after wait. *)
    st_region_nowait: uintmax_t;  (* Region lock granted with nowait. *)
    st_region_wait: uintmax_t;  (* Region lock granted after wait. *)
    st_mvcc_frozen: uintmax_t;  (* Buffers frozen. *)
    st_mvcc_thawed: uintmax_t;  (* Buffers thawed. *)
    st_mvcc_freed: uintmax_t; (* Frozen buffers freed. *)
    st_alloc: uintmax_t;    (* Number of page allocations. *)
    st_alloc_buckets: uintmax_t;  (* Buckets checked during allocation. *)
    st_alloc_max_buckets: uintmax_t;(* Max checked during allocation. *)
    st_alloc_pages: uintmax_t;  (* Pages checked during allocation. *)
    st_alloc_max_pages: uintmax_t;  (* Max checked during allocation. *)
    st_io_wait: uintmax_t;    (* Thread waited on buffer I/O. *)
    st_sync_interrupted: uintmax_t; (* Number of times sync interrupted. *)
    st_regsize: roff_t;   (* Region size. *)
{$ENDIF}
  end;

(* Mpool file statistics structure. *)
  __db_mpool_fstat = record
    file_name: PAnsiChar;   (* file name. *)
    st_pagesize: u_int32_t;   (* Page size. *)
{$IFNDEF __TEST_DB_NO_STATISTICS}
    st_map: u_int32_t;    (* Pages from mapped files. *)
    st_cache_hit: uintmax_t;  (* Pages found in the cache. *)
    st_cache_miss: uintmax_t; (* Pages not found in the cache. *)
    st_page_create: uintmax_t;  (* Pages created in the cache. *)
    st_page_in: uintmax_t;    (* Pages read in. *)
    st_page_out: uintmax_t;   (* Pages written out. *)
{$ENDIF}
  end;

  db_recops = (
    DB_TXN_ABORT=0,     (* Public. *)
    DB_TXN_APPLY=1,     (* Public. *)
    DB_TXN_BACKWARD_ROLL=3,   (* Public. *)
    DB_TXN_FORWARD_ROLL=4,    (* Public. *)
    DB_TXN_OPENFILES=5,   (* Internal. *)
    DB_TXN_POPENFILES=6,    (* Internal. *)
    DB_TXN_PRINT=7,     (* Public. *)
    DB_TXN_LOG_VERIFY=8   (* Internal. *)
  );

(*
 * BACKWARD_ALLOC is used during the forward pass to pick up any aborted
 * allocations for files that were created during the forward pass.
 * The main difference between _ALLOC and _ROLL is that the entry for
 * the file not exist during the rollforward pass.
 *)
// #define  DB_UNDO(op) ((op) == DB_TXN_ABORT || (op) == DB_TXN_BACKWARD_ROLL)
// #define  DB_REDO(op) ((op) == DB_TXN_FORWARD_ROLL || (op) == DB_TXN_APPLY)

  __db_txn = record
    mgrp: pointer; //^DB_TXNMGR (* Pointer to transaction manager. *)
    parent: PDB_TXN;  (* Pointer to transaction's parent. *)
    thread_info: pointer; //^DB_THREAD_INFO (* Pointer to thread information. *)
    txnid: u_int32_t;   (* Unique transaction id. *)
    name: PAnsiChar;    (* Transaction name. *)
    locker: pointer; //^DB_LOCKER (* Locker for this txn. *)
    td: pointer;    (* Detail structure within region. *)
    lock_timeout: db_timeout_t; (* Timeout for locks for this txn. *)
    txn_list: pointer;  (* Undo information for parent. *)

    (*
     * !!!
     * Explicit representations of structures from queue.h.
     * TAILQ_ENTRY(__db_txn) links;
     *)
    links: record
      tqe_next: PDB_TXN;
      tqe_prev: ^PDB_TXN;
    end; (* Links transactions off manager. *)

    (*
     * !!!
     * Explicit representations of structures from queue.h.
     * TAILQ_HEAD(__kids, __db_txn) kids;
     *)
    kids: record
      tqh_first: PDB_TXN;
      tqh_last: ^PDB_TXN;
    end;

    (*
     * !!!
     * Explicit representations of structures from queue.h.
     * TAILQ_HEAD(__events, __txn_event) events;
     *)
    events: record
      tqh_first: pointer; //^__txn_event
      tqh_last: ^pointer; //^^__txn_event
    end;      (* Links deferred events. *)

    (*
     * !!!
     * Explicit representations of structures from queue.h.
     * STAILQ_HEAD(__logrec, __txn_logrec) logs;
     *)
    logs: record
      stqh_first: pointer; //^__txn_logrec
      stqh_last: ^pointer; //^^__txn_logrec
    end;        (* Links in memory log records. *)

    (*
     * !!!
     * Explicit representations of structures from queue.h.
     * TAILQ_ENTRY (__db_txn): ENTRY;klinks;
     *)
    klinks: record
      tqe_next: PDB_TXN;
      tqe_prev: ^PDB_TXN;
    end;

    (*
     * !!!
     * Explicit representations of structures from queue.h.
     * TAILQ_HEAD(__my_cursors, __dbc) my_cursors;
     *)
    my_cursors: record
      tqh_first: PDBC;
      tqh_last: ^PDBC;
    end;

    (*
     * !!!
     * Explicit representations of structures from queue.h.
     * TAILQ_HEAD(__femfs, MPOOLFILE) femfs;
     *
     * These are DBs involved in file extension in this transaction.
     *)
    femfs: record
      tqh_first: PDB;
      tqh_last: ^PDB;
    end;

    token_buffer: PDB_TXN_TOKEN;  (* User's commit token buffer. *)
    api_internal: pointer;    (* C++ API private. *)
    xml_internal: pointer;    (* XML API private. *)

    cursors: u_int32_t; (* Number of cursors open for txn *)

    (* DB_TXN PUBLIC HANDLE LIST BEGIN *)
    abort           : function(p1: PDB_TXN): integer; cdecl;
    commit          : function(p1: PDB_TXN; p2: u_int32_t): integer; cdecl;
    discard         : function(p1: PDB_TXN; p2: u_int32_t): integer; cdecl;
    get_name        : function(p1: PDB_TXN; p2: PPAnsiChar): integer; cdecl;
    get_priority    : function(p1: PDB_TXN; p2: Pu_int32_t): integer; cdecl;
    id              : function(p1: PDB_TXN): u_int32_t; cdecl;
    prepare         : function(p1: PDB_TXN; p2: Pu_int8_t): integer; cdecl;
    set_commit_token: function(p1: PDB_TXN; p2: PDB_TXN_TOKEN): integer; cdecl;
    set_name        : function(p1: PDB_TXN; p2: PAnsiChar): integer; cdecl;
    set_priority    : function(p1: PDB_TXN; p2: u_int32_t): integer; cdecl;
    set_timeout     : function(p1: PDB_TXN; p2: db_timeout_t; p3: u_int32_t): integer; cdecl;
    (* DB_TXN PUBLIC HANDLE LIST END *)

    (* DB_TXN PRIVATE HANDLE LIST BEGIN *)
    set_txn_lsnp    : procedure(txn: PDB_TXN; p2: PPDB_LSN; p3: PPDB_LSN); cdecl;
    (* DB_TXN PRIVATE HANDLE LIST END *)

    flags: u_int32_t;
  end;

  __db_preplist = record
    txn: PDB_TXN;
    gid : array[0..DB_GID_SIZE-1] of u_int8_t;
  end;


(* Transaction statistics structure. *)
  __db_txn_active = record
    txnid: u_int32_t;   (* Transaction ID *)
    parentid: u_int32_t;    (* Transaction ID of parent *)
    pid: pid_t;     (* Process owning txn ID *)
    tid: db_threadid_t;   (* Thread owning txn ID *)
    lsn: DB_LSN;      (* LSN when transaction began *)
    read_lsn: DB_LSN;   (* Read LSN for MVCC *)
    mvcc_ref: u_int32_t;    (* MVCC reference count *)
    priority: u_int32_t;    (* Deadlock resolution priority *)
    status: u_int32_t;    (* Status of the transaction *)
    gid : array[0..DB_GID_SIZE-1] of u_int8_t;  (* Global transaction ID *)
    name : array[0..50] of AnsiChar;    (* 50 bytes of name, nul termination *)
  end;

  __db_txn_stat = record
    st_nrestores: u_int32_t;    (* number of restored transactions after recovery. *)
{$IFNDEF __TEST_DB_NO_STATISTICS}
    st_last_ckp: DB_LSN;    (* lsn of the last checkpoint *)
    st_time_ckp: time_t;    (* time of last checkpoint *)
    st_last_txnid: u_int32_t; (* last transaction id given out *)
    st_maxtxns: u_int32_t;    (* maximum txns possible *)
    st_naborts: uintmax_t;    (* number of aborted transactions *)
    st_nbegins: uintmax_t;    (* number of begun transactions *)
    st_ncommits: uintmax_t;   (* number of committed transactions *)
    st_nactive: u_int32_t;    (* number of active transactions *)
    st_nsnapshot: u_int32_t;    (* number of snapshot transactions *)
    st_maxnactive: u_int32_t; (* maximum active transactions *)
    st_maxnsnapshot: u_int32_t; (* maximum snapshot transactions *)
    st_txnarray: PDB_TXN_ACTIVE;  (* array of active transactions *)
    st_region_wait: uintmax_t;  (* Region lock granted after wait. *)
    st_region_nowait: uintmax_t;  (* Region lock granted without wait. *)
    st_regsize: roff_t;   (* Region size. *)
{$ENDIF}
  end;

  __db_txn_token = record
    buf: array[0..DB_TXN_TOKEN_SIZE-1] of u_int8_t;
  end;

(*******************************************************
 * Replication.
 *******************************************************)

(* Replication Manager site status. *)
  __db_repmgr_site = record
    eid: integer;
    host: PAnsiChar;
    port: u_int;
    status: u_int32_t;
    flags: u_int32_t;
  end;

(* Replication statistics. *)
  __db_rep_stat = record
    (* !!!
     * Many replication statistics fields cannot be protected by a mutex
     * without an unacceptable performance penalty, since most message
     * processing is done without the need to hold a region-wide lock.
     * Fields whose comments end with a '+' may be updated without holding
     * the replication or log mutexes (as appropriate), and thus may be
     * off somewhat (or, on unreasonable architectures under unlucky
     * circumstances, garbaged).
     *)
    st_startup_complete: u_int32_t; (* Site completed client sync-up. *)
{$IFNDEF __TEST_DB_NO_STATISTICS}
    st_log_queued: uintmax_t; (* Log records currently queued.+ *)
    st_status: u_int32_t;   (* Current replication status. *)
    st_next_lsn: DB_LSN;    (* Next LSN to use or expect. *)
    st_waiting_lsn: DB_LSN;   (* LSN we're awaiting, if any. *)
    st_max_perm_lsn: DB_LSN;    (* Maximum permanent LSN. *)
    st_next_pg: db_pgno_t;    (* Next pg we expect. *)
    st_waiting_pg: db_pgno_t; (* pg we're awaiting, if any. *)
    st_dupmasters: u_int32_t; (* # of times a duplicate master condition was detected.+ *)
    st_env_id: Integer;     (* Current environment ID. *)
    st_env_priority: u_int32_t; (* Current environment priority. *)
    st_bulk_fills: uintmax_t; (* Bulk buffer fills. *)
    st_bulk_overflows: uintmax_t; (* Bulk buffer overflows. *)
    st_bulk_records: uintmax_t; (* Bulk records stored. *)
    st_bulk_transfers: uintmax_t; (* Transfers of bulk buffers. *)
    st_client_rerequests: uintmax_t;(* Number of forced rerequests. *)
    st_client_svc_req: uintmax_t; (* Number of client service requests received by this client. *)
    st_client_svc_miss: uintmax_t;  (* Number of client service requests missing on this client. *)
    st_gen: u_int32_t;    (* Current generation number. *)
    st_egen: u_int32_t;   (* Current election gen number. *)
    st_log_duplicated: uintmax_t; (* Log records received multiply.+ *)
    st_log_queued_max: uintmax_t; (* Max. log records queued at once.+ *)
    st_log_queued_total: uintmax_t; (* Total # of log recs. ever queued.+ *)
    st_log_records: uintmax_t;  (* Log records received and put.+ *)
    st_log_requested: uintmax_t;  (* Log recs. missed and requested.+ *)
    st_master: integer;     (* Env. ID of the current master. *)
    st_master_changes: uintmax_t; (* # of times we've switched masters. *)
    st_msgs_badgen: uintmax_t;  (* Messages with a bad generation #.+ *)
    st_msgs_processed: uintmax_t; (* Messages received and processed.+ *)
    st_msgs_recover: uintmax_t; (* Messages ignored because this site was a client in recovery.+ *)
    st_msgs_send_failures: uintmax_t;(* # of failed message sends.+ *)
    st_msgs_sent: uintmax_t;  (* # of successful message sends.+ *)
    st_newsites: uintmax_t;   (* # of NEWSITE msgs. received.+ *)
    st_nsites: u_int32_t;   (* Current number of sites we will assume during elections. *)
    st_nthrottles: uintmax_t; (* # of times we were throttled. *)
    st_outdated: uintmax_t;   (* # of times we detected and returned an OUTDATED condition.+ *)
    st_pg_duplicated: uintmax_t;  (* Pages received multiply.+ *)
    st_pg_records: uintmax_t; (* Pages received and stored.+ *)
    st_pg_requested: uintmax_t; (* Pages missed and requested.+ *)
    st_txns_applied: uintmax_t; (* # of transactions applied.+ *)
    st_startsync_delayed: uintmax_t;(* # of STARTSYNC msgs delayed.+ *)
    st_elections: uintmax_t;  (* # of elections held.+ *)
    st_elections_won: uintmax_t;  (* # of elections won by this site.+ *)
    st_election_cur_winner: Integer;  (* Current front-runner. *)
    st_election_gen: u_int32_t; (* Election generation number. *)
    st_election_lsn: DB_LSN;    (* Max. LSN of current winner. *)
    st_election_nsites: u_int32_t;  (* # of "registered voters". *)
    st_election_nvotes: u_int32_t;  (* # of "registered voters" needed. *)
    st_election_priority: u_int32_t;  (* Current election priority. *)
    st_election_status: Integer;    (* Current election status. *)
    st_election_tiebreaker: u_int32_t;(* Election tiebreaker value. *)
    st_election_votes: u_int32_t; (* Votes received in this round. *)
    st_election_sec: u_int32_t; (* Last election time seconds. *)
    st_election_usec: u_int32_t;  (* Last election time useconds. *)
    st_max_lease_sec: u_int32_t;  (* Maximum lease timestamp seconds. *)
    st_max_lease_usec: u_int32_t; (* Maximum lease timestamp useconds. *)
    (* Undocumented statistics only used by the test system. *)
{$IFDEF CONFIG_TEST}
    st_filefail_cleanups: u_int32_t;  (* # of FILE_FAIL cleanups done. *)
{$ENDIF}
{$ENDIF}
  end;

(* Replication Manager statistics. *)
  __db_repmgr_stat = record
    st_perm_failed: uintmax_t;  (* # of insufficiently ack'ed msgs. *)
    st_msgs_queued: uintmax_t;  (* # msgs queued for network delay. *)
    st_msgs_dropped: uintmax_t; (* # msgs discarded due to excessive queue length. *)
    st_connection_drop: uintmax_t;  (* Existing connections dropped. *)
    st_connect_fail: uintmax_t; (* Failed new connection attempts. *)
    st_elect_threads: uintmax_t;  (* # of active election threads. *)
    st_max_elect_threads: uintmax_t;  (* Max concurrent e-threads ever. *)
  end;

(*******************************************************
 * Sequences.
 *******************************************************)
(*
 * The storage record for a sequence.
 *)
  __db_seq_record = record
    seq_version: u_int32_t; (* Version size/number. *)
    flags: u_int32_t;   (* DB_SEQ_XXX Flags. *)
    seq_value: db_seq_t;  (* Current value. *)
    seq_max: db_seq_t;  (* Max permitted. *)
    seq_min: db_seq_t;  (* Min permitted. *)
  end;
  DB_SEQ_RECORD = __db_seq_record;

(*
 * Handle for a sequence object.
 *)
  __db_sequence = record
    seq_dbp: PDB;  (* DB handle for this sequence. *)
    mtx_seq: db_mutex_t;  (* Mutex if sequence is threaded. *)
    seq_rp: PDB_SEQ_RECORD; (* Pointer to current data. *)
    seq_record: DB_SEQ_RECORD;  (* Data from DB_SEQUENCE. *)
    seq_cache_size: int32_t; (* Number of values cached. *)
    seq_last_value: db_seq_t; (* Last value cached. *)
    seq_key: DBT; (* DBT pointing to sequence key. *)
    seq_data: DBT;  (* DBT pointing to seq_record. *)

    (* API-private structure: used by C++ and Java. *)
    api_internal: pointer;

    (* DB_SEQUENCE PUBLIC HANDLE LIST BEGIN *)
    close        : function(p1: PDB_SEQUENCE; p2: u_int32_t): integer; cdecl;
    get          : function(p1: PDB_SEQUENCE; p2: PDB_TXN; p3: int32_t; p4: Pdb_seq_t; p5: u_int32_t): integer; cdecl;
    get_cachesize: function(p1: PDB_SEQUENCE; p2: Pint32_t): integer; cdecl;
    get_db       : function(p1: PDB_SEQUENCE; p2: PPDB): integer; cdecl;
    get_flags    : function(p1: PDB_SEQUENCE; p2: Pu_int32_t): integer; cdecl;
    get_key      : function(p1: PDB_SEQUENCE; p2: PDBT): integer; cdecl;
    get_range    : function(p1: PDB_SEQUENCE; p2: Pdb_seq_t; p3: Pdb_seq_t): integer; cdecl;
    initial_value: function(p1: PDB_SEQUENCE; p2: db_seq_t): integer; cdecl;
    open         : function(p1: PDB_SEQUENCE; p2: PDB_TXN; p3: PDBT; p4: u_int32_t): integer; cdecl;
    remove       : function(p1: PDB_SEQUENCE; p2: PDB_TXN; p3: u_int32_t): integer; cdecl;
    set_cachesize: function(p1: PDB_SEQUENCE; p2: int32_t): integer; cdecl;
    set_flags    : function(p1: PDB_SEQUENCE; p2: u_int32_t): integer; cdecl;
    set_range    : function(p1: PDB_SEQUENCE; p2: db_seq_t; p3: db_seq_t): integer; cdecl;
    stat         : function(p1: PDB_SEQUENCE; p2: PPDB_SEQUENCE_STAT; p3: u_int32_t): integer; cdecl;
    stat_print   : function(p1: PDB_SEQUENCE; p2: u_int32_t): integer; cdecl;
    (* DB_SEQUENCE PUBLIC HANDLE LIST END *)
  end;

  __db_seq_stat = record
    st_wait: uintmax_t;   (* Sequence lock granted w/o wait. *)
    st_nowait: uintmax_t;   (* Sequence lock granted after wait. *)
    st_current: db_seq_t;   (* Current value in db. *)
    st_value: db_seq_t;   (* Current cached value. *)
    st_last_value: db_seq_t;  (* Last cached value. *)
    st_min: db_seq_t;   (* Minimum value. *)
    st_max: db_seq_t;   (* Maximum value. *)
    st_cache_size: int32_t; (* Cache size. *)
    st_flags: u_int32_t;    (* Flag value. *)
  end;

(*******************************************************
 * Access methods.
 *******************************************************)
  DBTYPE = (
    DB_BTREE=1,
    DB_HASH=2,
    DB_RECNO=3,
    DB_QUEUE=4,
    DB_UNKNOWN=5      (* Figure it out on open. *)
  );

(* Database handle. *)

// Translation: helper types for function parameters
  P___db_get_alloc_P2              = ^T___db_get_alloc_P2             ; // @@PAS_HELPER(P___db_get_alloc_P2)
  P___db_get_alloc_P3              = ^T___db_get_alloc_P3             ; // @@PAS_HELPER(P___db_get_alloc_P3)
  P___db_get_alloc_P4              = ^T___db_get_alloc_P4             ; // @@PAS_HELPER(P___db_get_alloc_P4)
  P___db_get_append_recno_P2       = ^T___db_get_append_recno_P2      ; // @@PAS_HELPER(P___db_get_append_recno_P2)
  P___db_get_bt_compare_P2         = ^T___db_get_bt_compare_P2        ; // @@PAS_HELPER(P___db_get_bt_compare_P2)
  P___db_get_bt_compress_P2        = ^T___db_get_bt_compress_P2       ; // @@PAS_HELPER(P___db_get_bt_compress_P2)
  P___db_get_bt_compress_P3        = ^T___db_get_bt_compress_P3       ; // @@PAS_HELPER(P___db_get_bt_compress_P3)
  P___db_get_bt_prefix_P2          = ^T___db_get_bt_prefix_P2         ; // @@PAS_HELPER(P___db_get_bt_prefix_P2)
  P___db_get_dup_compare_P2        = ^T___db_get_dup_compare_P2       ; // @@PAS_HELPER(P___db_get_dup_compare_P2)
  P___db_get_errcall_P2            = ^T___db_get_errcall_P2           ; // @@PAS_HELPER(P___db_get_errcall_P2)
  P___db_get_feedback_P2           = ^T___db_get_feedback_P2          ; // @@PAS_HELPER(P___db_get_feedback_P2)
  P___db_get_h_compare_P2          = ^T___db_get_h_compare_P2         ; // @@PAS_HELPER(P___db_get_h_compare_P2)
  P___db_get_h_hash_P2             = ^T___db_get_h_hash_P2            ; // @@PAS_HELPER(P___db_get_h_hash_P2)
  P___db_get_msgcall_P2            = ^T___db_get_msgcall_P2           ; // @@PAS_HELPER(P___db_get_msgcall_P2)
  P___db_get_partition_callback_P3 = ^T___db_get_partition_callback_P3; // @@PAS_HELPER(P___db_get_partition_callback_P3)
  T___db_associate_foreign_P3      = function (p1: PDB; p2: PDBT; p3: PDBT; p4: PDBT; p5: Pinteger      ): integer  ; cdecl; // @@PAS_HELPER(T___db_associate_foreign_P3)
  T___db_associate_P4              = function (p1: PDB; p2: PDBT; p3: PDBT; p4: PDBT                    ): integer  ; cdecl; // @@PAS_HELPER(T___db_associate_P4)
  T___db_dump_P3                   = function (p1: pointer; p2: pointer                                 ): integer  ; cdecl; // @@PAS_HELPER(T___db_dump_P3)
  T___db_get_alloc_P2              = function (p1: size_t                                               ): pointer  ; cdecl; // @@PAS_HELPER(T___db_get_alloc_P2)
  T___db_get_alloc_P3              = function (p1: pointer; p2: size_t                                  ): pointer  ; cdecl; // @@PAS_HELPER(T___db_get_alloc_P3)
  T___db_get_alloc_P4              = procedure(p1: pointer                                              )           ; cdecl; // @@PAS_HELPER(T___db_get_alloc_P4)
  T___db_get_append_recno_P2       = function (p1: PDB; p2: PDBT; p3: db_recno_t                        ): integer  ; cdecl; // @@PAS_HELPER(T___db_get_append_recno_P2)
  T___db_get_bt_compare_P2         = function (p1: PDB; p2: PDBT; p3: PDBT                              ): integer  ; cdecl; // @@PAS_HELPER(T___db_get_bt_compare_P2)
  T___db_get_bt_compress_P2        = function (p1: PDB; p2: PDBT; p3: PDBT; p4: PDBT; p5: PDBT; p6: PDBT): integer  ; cdecl; // @@PAS_HELPER(T___db_get_bt_compress_P2)
  T___db_get_bt_compress_P3        = function (p1: PDB; p2: PDBT; p3: PDBT; p4: PDBT; p5: PDBT; p6: PDBT): integer  ; cdecl; // @@PAS_HELPER(T___db_get_bt_compress_P3)
  T___db_get_bt_prefix_P2          = function (p1: PDB; p2: PDBT; p3: PDBT                              ): size_t   ; cdecl; // @@PAS_HELPER(T___db_get_bt_prefix_P2)
  T___db_get_dup_compare_P2        = function (p1: PDB; p2: PDBT; p3: PDBT                              ): integer  ; cdecl; // @@PAS_HELPER(T___db_get_dup_compare_P2)
  T___db_get_errcall_P2            = procedure(p1: PDB_ENV; p2: PAnsiChar; p3: PAnsiChar                )           ; cdecl; // @@PAS_HELPER(T___db_get_errcall_P2)
  T___db_get_feedback_P2           = procedure(p1: PDB; p2: integer; p3: integer                        )           ; cdecl; // @@PAS_HELPER(T___db_get_feedback_P2)
  T___db_get_h_compare_P2          = function (p1: PDB; p2: PDBT; p3: PDBT                              ): integer  ; cdecl; // @@PAS_HELPER(T___db_get_h_compare_P2)
  T___db_get_h_hash_P2             = function (p1: PDB; p2: pointer; p3: u_int32_t                      ): u_int32_t; cdecl; // @@PAS_HELPER(T___db_get_h_hash_P2)
  T___db_get_msgcall_P2            = procedure(p1: PDB_ENV; p2: PAnsiChar                               )           ; cdecl; // @@PAS_HELPER(T___db_get_msgcall_P2)
  T___db_get_partition_callback_P3 = function (p1: PDB; key: PDBT                                       ): u_int32_t; cdecl; // @@PAS_HELPER(T___db_get_partition_callback_P3)
  T___db_set_alloc_P2              = function (p1: size_t                                               ): pointer  ; cdecl; // @@PAS_HELPER(T___db_set_alloc_P2)
  T___db_set_alloc_P3              = function (p1: pointer; p2: size_t                                  ): pointer  ; cdecl; // @@PAS_HELPER(T___db_set_alloc_P3)
  T___db_set_alloc_P4              = procedure(p1: pointer                                              )           ; cdecl; // @@PAS_HELPER(T___db_set_alloc_P4)
  T___db_set_append_recno_P2       = function (p1: PDB; p2: PDBT; p3: db_recno_t                        ): integer  ; cdecl; // @@PAS_HELPER(T___db_set_append_recno_P2)
  T___db_set_bt_compare_P2         = function (p1: PDB; p2: PDBT; p3: PDBT                              ): integer  ; cdecl; // @@PAS_HELPER(T___db_set_bt_compare_P2)
  T___db_set_bt_compress_P2        = function (p1: PDB; p2: PDBT; p3: PDBT; p4: PDBT; p5: PDBT; p6: PDBT): integer  ; cdecl; // @@PAS_HELPER(T___db_set_bt_compress_P2)
  T___db_set_bt_compress_P3        = function (p1: PDB; p2: PDBT; p3: PDBT; p4: PDBT; p5: PDBT; p6: PDBT): integer  ; cdecl; // @@PAS_HELPER(T___db_set_bt_compress_P3)
  T___db_set_bt_prefix_P2          = function (p1: PDB; p2: PDBT; p3: PDBT                              ): size_t   ; cdecl; // @@PAS_HELPER(T___db_set_bt_prefix_P2)
  T___db_set_dup_compare_P2        = function (p1: PDB; p2: PDBT; p3: PDBT                              ): integer  ; cdecl; // @@PAS_HELPER(T___db_set_dup_compare_P2)
  T___db_set_errcall_P2            = procedure(env: PDB_ENV; errpfx, msg: PAnsiChar                     )           ; cdecl; // @@PAS_HELPER(T___db_set_errcall_P2)
  T___db_set_feedback_P2           = procedure(p1: PDB; p2: integer; p3: integer                        )           ; cdecl; // @@PAS_HELPER(T___db_set_feedback_P2)
  T___db_set_h_compare_P2          = function (p1: PDB; p2: PDBT; p3: PDBT                              ): integer  ; cdecl; // @@PAS_HELPER(T___db_set_h_compare_P2)
  T___db_set_h_hash_P2             = function (p1: PDB; p2: pointer; p3: u_int32_t                      ): u_int32_t; cdecl; // @@PAS_HELPER(T___db_set_h_hash_P2)
  T___db_set_msgcall_P2            = procedure(p1: PDB_ENV; p2: PAnsiChar                               )           ; cdecl; // @@PAS_HELPER(T___db_set_msgcall_P2)
  T___db_set_paniccall_P2          = procedure(p1: PDB_ENV; p2: integer                                 )           ; cdecl; // @@PAS_HELPER(T___db_set_paniccall_P2)
  T___db_set_partition_P4          = function (p1: PDB; key: PDBT                                       ): u_int32_t; cdecl; // @@PAS_HELPER(T___db_set_partition_P4)

  __db = record
    pgsize: u_int32_t;    (* Database logical page size. *)
    priority: DB_CACHE_PRIORITY;  (* Database priority in cache. *)

    db_append_recno: function (p1: PDB; p2: PDBT; p3: db_recno_t): integer; cdecl;
    db_feedback    : procedure(p1: PDB; p2: integer; p3: integer); cdecl;
    dup_compare    : function (p1: PDB; p2: PDBT; p3: PDBT): integer; cdecl;

    app_private: pointer;   (* Application-private handle. *)

    (*******************************************************
     * Private: owned by DB.
     *******************************************************)
    dbenv: PDB_ENV;     (* Backing public environment. *)
    env: pointer; //^ENV      (* Backing private environment. *)

    type_: DBTYPE;      (* DB access method type. *) // @@PAS_MAPPING(type_=type)

    mpf: PDB_MPOOLFILE;   (* Backing buffer pool. *)

    mutex: db_mutex_t;    (* Synchronization for free threading *)

    fname, dname: PAnsiChar;    (* file/database passed to DB->open. *)
    dirname: PAnsiChar;   (* Directory of DB file. *)
    open_flags: u_int32_t;    (* Flags passed to DB->open. *)

    fileid: array[0..DB_FILE_ID_LEN-1] of u_int8_t;(* File's unique ID for locking. *)

    adj_fileid: u_int32_t;    (* File's unique ID for curs. adj. *)

    log_filename: pointer; //^FNAME   (* File's naming info for logging. *)

    meta_pgno: db_pgno_t ;    (* Meta page number *)
    locker: pointer; //^DB_LOCKER;    (* Locker for handle locking. *)
    cur_locker: pointer; //^DB_LOCKER;    (* Current handle lock holder. *)
    cur_txn: PDB_TXN   ;    (* Opening transaction. *)
    associate_locker: pointer; //^DB_LOCKER;  (* Locker for DB->associate call. *)
    handle_lock: DB_LOCK    ;   (* Lock held on this handle. *)

    timestamp: time_t;    (* Handle timestamp for replication. *)
    fid_gen: u_int32_t;   (* Rep generation number for fids. *)

    (*
     * Returned data memory for DB->get() and friends.
     *)
    my_rskey: DBT;    (* Secondary key. *)
    my_rkey: DBT;   (* [Primary] key. *)
    my_rdata: DBT;    (* Data. *)

    (*
     * !!!
     * Some applications use DB but implement their own locking outside of
     * DB.  If they're using fcntl(2) locking on the underlying database
     * file, and we open and close a file descriptor for that file, we will
     * discard their locks.  The DB_FCNTL_LOCKING flag to DB->open is an
     * undocumented interface to support this usage which leaves any file
     * descriptors we open until DB->close.  This will only work with the
     * DB->open interface and simple caches, e.g., creating a transaction
     * thread may open/close file descriptors this flag doesn't protect.
     * Locking with fcntl(2) on a file that you don't own is a very, very
     * unsafe thing to do.  'Nuff said.
     *)
    saved_open_fhp: pointer; //^DB_FH;  (* Saved file handle. *)

    (*
     * Linked list of DBP's, linked from the ENV, used to keep track
     * of all open db handles for cursor adjustment.
     *
     * !!!
     * Explicit representations of structures from queue.h.
     * TAILQ_ENTRY (__db): ENTRY;dblistlinks;
     *)
    dblistlinks: record
      tqe_next: PDB;
      tqe_prev: ^PDB;
    end;


    (*
     * Cursor queues.
     *
     * !!!
     * Explicit representations of structures from queue.h.
     * TAILQ_HEAD(__cq_fq, __dbc) free_queue;
     * TAILQ_HEAD(__cq_aq, __dbc) active_queue;
     * TAILQ_HEAD(__cq_jq, __dbc) join_queue;
     *)
    free_queue: record
      tqh_first: PDBC;
      tqh_last: ^PDBC;
    end;
    active_queue: record
      tqh_first: PDBC;
      tqh_last: ^PDBC;
    end;
    join_queue: record
      tqh_first: PDBC;
      tqh_last: ^PDBC;
    end;

    (*
     * Secondary index support.
     *
     * Linked list of secondary indices -- set in the primary.
     *
     * !!!
     * Explicit representations of structures from queue.h.
     * LIST_HEAD(s_secondaries, __dbend;
     *)
    s_secondaries: record
      lh_first: PDB;
    end;

    (*
     * List entries for secondaries, and reference count of how many
     * threads are updating this secondary (see Dbc.put).
     *
     * !!!
     * Note that these are synchronized by the primary's mutex, but
     * filled in in the secondaries.
     *
     * !!!
     * Explicit representations of structures from queue.h.
     * LIST_function (__db): ENTRY;s_links;
     *)
    s_links: record
      le_next: PDB;
      le_prev: ^PDB;
    end;
    s_refcnt: u_int32_t;

    (* Secondary callback and free functions -- set in the secondary. *)
    s_callback: function(p1: PDB; p2: PDBT; p3: PDBT; p4: PDBT): integer; cdecl;

    (* Reference to primary -- set in the secondary. *)
    s_primary: PDB;

    (* Flags passed to associate -- set in the secondary. *)
    s_assoc_flags: u_int32_t;

    (*
     * Foreign key support.
     *
     * Linked list of primary dbs -- set in the foreign db
     *
     * !!!
     * Explicit representations of structures from queue.h.
     * LIST_HEAD(f_primaries, __db);
     *)
    f_primaries: record
      lh_first: pointer; //^__db_foreign_info
    end;

    (*
     * !!!
     * Explicit representations of structures from queue.h.
     * TAILQ_ENTRY (__db): ENTRY;felink;
     *
     * Links in a list of DBs involved in file extension
     * during a transaction.  These are to be used only while the
     * metadata is locked.
     *)
    felink: record
      tqe_next: PDB;
      tqe_prev: ^PDB;
    end;

    (* Reference to foreign -- set in the secondary. *)
    s_foreign: PDB;

    (* API-private structure: used by DB 1.85, C++, Java, Perl and Tcl *)
    api_internal: pointer;

    (* Subsystem-private structure. *)
    bt_internal: pointer;   (* Btree/Recno access method. *)
    h_internal: pointer;    (* Hash access method. *)
    p_internal: pointer;    (* Partition informaiton. *)
    q_internal: pointer;    (* Queue access method. *)

    (* DB PUBLIC HANDLE LIST BEGIN *)
    associate             : function (p1: PDB; p2: PDB_TXN; p3: PDB; p4: T___db_associate_P4; p5: u_int32_t): integer; cdecl;
    associate_foreign     : function (p1: PDB; p2: PDB; p3: T___db_associate_foreign_P3; p4: u_int32_t): integer; cdecl;
    close                 : function (db: PDB; flags: u_int32_t): integer; cdecl;
    compact               : function (p1: PDB; p2: PDB_TXN; p3: PDBT; p4: PDBT; p5: PDB_COMPACT; p6: u_int32_t; p7: PDBT): integer; cdecl;
    cursor                : function (p1: PDB; p2: PDB_TXN; p3: PPDBC; p4: u_int32_t): integer; cdecl;
    del                   : function (p1: PDB; p2: PDB_TXN; p3: PDBT; p4: u_int32_t): integer; cdecl;
    err                   : procedure(p1: PDB; p2: integer; p3: PAnsiChar { ... })         ; cdecl;
    errx                  : procedure(p1: PDB; p2: PAnsiChar { ... })         ; cdecl;
    exists                : function (p1: PDB; p2: PDB_TXN; p3: PDBT; p4: u_int32_t): integer; cdecl;
    fd                    : function (p1: PDB; p2: Pinteger): integer; cdecl;
    get                   : function (p1: PDB; p2: PDB_TXN; p3: PDBT; p4: PDBT; p5: u_int32_t): integer; cdecl;
    get_alloc             : function (p1: PDB; p2: P___db_get_alloc_P2; p3: P___db_get_alloc_P3; p4: P___db_get_alloc_P4): integer; cdecl;
    get_append_recno      : function (p1: PDB; p2: P___db_get_append_recno_P2): integer; cdecl;
    get_assoc_flags       : function (p1: PDB; p2: Pu_int32_t): integer; cdecl;
    get_bt_compare        : function (p1: PDB; p2: P___db_get_bt_compare_P2): integer; cdecl;
    get_bt_compress       : function (p1: PDB; p2: P___db_get_bt_compress_P2; p3: P___db_get_bt_compress_P3): integer; cdecl;
    get_bt_minkey         : function (p1: PDB; p2: Pu_int32_t): integer; cdecl;
    get_bt_prefix         : function (p1: PDB; p2: P___db_get_bt_prefix_P2): integer; cdecl;
    get_byteswapped       : function (p1: PDB; p2: Pinteger): integer; cdecl;
    get_cachesize         : function (p1: PDB; p2: Pu_int32_t; p3: Pu_int32_t; p4: Pinteger): integer; cdecl;
    get_create_dir        : function (p1: PDB; p2: PPAnsiChar): integer; cdecl;
    get_dbname            : function (p1: PDB; p2: PPAnsiChar; p3: PPAnsiChar): integer; cdecl;
    get_dup_compare       : function (p1: PDB; p2: P___db_get_dup_compare_P2): integer; cdecl;
    get_encrypt_flags     : function (p1: PDB; p2: Pu_int32_t): integer; cdecl;
    get_env               : function (p1: PDB): PDB_ENV; cdecl;
    get_errcall           : procedure(p1: PDB; p2: P___db_get_errcall_P2)         ; cdecl;
    get_errfile           : procedure(p1: PDB; p2: pointer)         ; cdecl;
    get_errpfx            : procedure(p1: PDB; p2: PPAnsiChar)         ; cdecl;
    get_feedback          : function (p1: PDB; p2: P___db_get_feedback_P2): integer; cdecl;
    get_flags             : function (p1: PDB; p2: Pu_int32_t): integer; cdecl;
    get_h_compare         : function (p1: PDB; p2: P___db_get_h_compare_P2): integer; cdecl;
    get_h_ffactor         : function (p1: PDB; p2: Pu_int32_t): integer; cdecl;
    get_h_hash            : function (p1: PDB; p2: P___db_get_h_hash_P2): integer; cdecl;
    get_h_nelem           : function (p1: PDB; p2: Pu_int32_t): integer; cdecl;
    get_lorder            : function (p1: PDB; p2: Pinteger): integer; cdecl;
    get_mpf               : function (p1: PDB): PDB_MPOOLFILE; cdecl;
    get_msgcall           : procedure(p1: PDB; p2: P___db_get_msgcall_P2)         ; cdecl;
    get_msgfile           : procedure(p1: PDB; p2: pointer)         ; cdecl;
    get_multiple          : function (p1: PDB): integer; cdecl;
    get_open_flags        : function (p1: PDB; p2: Pu_int32_t): integer; cdecl;
    get_pagesize          : function (p1: PDB; p2: Pu_int32_t): integer; cdecl;
    get_partition_callback: function (p1: PDB; p2: Pu_int32_t; p3: P___db_get_partition_callback_P3): integer; cdecl;
    get_partition_dirs    : function (p1: PDB; p2: PPPAnsiChar): integer; cdecl;
    get_partition_keys    : function (p1: PDB; p2: Pu_int32_t; p3: PPDBT): integer; cdecl;
    get_priority          : function (p1: PDB; p2: PDB_CACHE_PRIORITY): integer; cdecl;
    get_q_extentsize      : function (p1: PDB; p2: Pu_int32_t): integer; cdecl;
    get_re_delim          : function (p1: PDB; p2: Pinteger): integer; cdecl;
    get_re_len            : function (p1: PDB; p2: Pu_int32_t): integer; cdecl;
    get_re_pad            : function (p1: PDB; p2: Pinteger): integer; cdecl;
    get_re_source         : function (p1: PDB; p2: PPAnsiChar): integer; cdecl;
    get_transactional     : function (p1: PDB): integer; cdecl;
    get_type              : function (p1: PDB; YPE: PDBT): integer; cdecl;
    join                  : function (p1: PDB; p2: PPDBC; p3: PPDBC; p4: u_int32_t): integer; cdecl;
    key_range             : function (p1: PDB; p2: PDB_TXN; p3: PDBT; p4: PDB_KEY_RANGE; p5: u_int32_t): integer; cdecl;
    open                  : function (db: PDB; txnid: PDB_TXN; file_, database: PAnsiChar; type_: DBTYPE; flags: u_int32_t; mode: integer): integer; cdecl; // @@PAS_MAPPING(type_=type;file_=file)
    pget                  : function (p1: PDB; p2: PDB_TXN; p3: PDBT; p4: PDBT; p5: PDBT; p6: u_int32_t): integer; cdecl;
    put                   : function (p1: PDB; p2: PDB_TXN; p3: PDBT; p4: PDBT; p5: u_int32_t): integer; cdecl;
    remove                : function (db: PDB; file_, database: PAnsiChar; flags: u_int32_t): integer; cdecl; // @@PAS_MAPPING(type_=type)
    rename                : function (p1: PDB; p2: PAnsiChar; p3: PAnsiChar; p4: PAnsiChar; p5: u_int32_t): integer; cdecl;
    set_alloc             : function (p1: PDB; p2: T___db_set_alloc_P2; p3: T___db_set_alloc_P3; p4: T___db_set_alloc_P4): integer; cdecl;
    set_append_recno      : function (p1: PDB; p2: T___db_set_append_recno_P2): integer; cdecl;
    set_bt_compare        : function (p1: PDB; p2: T___db_set_bt_compare_P2): integer; cdecl;
    set_bt_compress       : function (p1: PDB; p2: T___db_set_bt_compress_P2; p3: T___db_set_bt_compress_P3): integer; cdecl;
    set_bt_minkey         : function (p1: PDB; p2: u_int32_t): integer; cdecl;
    set_bt_prefix         : function (p1: PDB; p2: T___db_set_bt_prefix_P2): integer; cdecl;
    set_cachesize         : function (p1: PDB; p2: u_int32_t; p3: u_int32_t; p4: integer): integer; cdecl;
    set_create_dir        : function (p1: PDB; p2: PAnsiChar): integer; cdecl;
    set_dup_compare       : function (p1: PDB; p2: T___db_set_dup_compare_P2): integer; cdecl;
    set_encrypt           : function (p1: PDB; p2: PAnsiChar; p3: u_int32_t): integer; cdecl;
    set_errcall           : procedure(db: PDB; db_errcall_fcn: T___db_set_errcall_P2); cdecl;
    set_errfile           : procedure(p1: PDB; p2: pointer)         ; cdecl;
    set_errpfx            : procedure(db: PDB; errpfx: PAnsiChar); cdecl;
    set_feedback          : function (p1: PDB; p2: T___db_set_feedback_P2): integer; cdecl;
    set_flags             : function (p1: PDB; p2: u_int32_t): integer; cdecl;
    set_h_compare         : function (p1: PDB; p2: T___db_set_h_compare_P2): integer; cdecl;
    set_h_ffactor         : function (p1: PDB; p2: u_int32_t): integer; cdecl;
    set_h_hash            : function (p1: PDB; p2: T___db_set_h_hash_P2): integer; cdecl;
    set_h_nelem           : function (p1: PDB; p2: u_int32_t): integer; cdecl;
    set_lorder            : function (p1: PDB; p2: integer): integer; cdecl;
    set_msgcall           : procedure(p1: PDB; p2: T___db_set_msgcall_P2)         ; cdecl;
    set_msgfile           : procedure(p1: PDB; p2: pointer)         ; cdecl;
    set_pagesize          : function (p1: PDB; p2: u_int32_t): integer; cdecl;
    set_paniccall         : function (p1: PDB; p2: T___db_set_paniccall_P2): integer; cdecl;
    set_partition         : function (p1: PDB; p2: u_int32_t; p3: PDBT; p4: T___db_set_partition_P4): integer; cdecl;
    set_partition_dirs    : function (p1: PDB; p2: PPAnsiChar): integer; cdecl;
    set_priority          : function (p1: PDB; p2: DB_CACHE_PRIORITY): integer; cdecl;
    set_q_extentsize      : function (p1: PDB; p2: u_int32_t): integer; cdecl;
    set_re_delim          : function (p1: PDB; p2: integer): integer; cdecl;
    set_re_len            : function (p1: PDB; p2: u_int32_t): integer; cdecl;
    set_re_pad            : function (p1: PDB; p2: integer): integer; cdecl;
    set_re_source         : function (p1: PDB; p2: PAnsiChar): integer; cdecl;
    sort_multiple         : function (p1: PDB; p2: PDBT; p3: PDBT; p4: u_int32_t): integer; cdecl;
    stat                  : function (p1: PDB; p2: PDB_TXN; p3: pointer; p4: u_int32_t): integer; cdecl;
    stat_print            : function (p1: PDB; p2: u_int32_t): integer; cdecl;
    sync                  : function (p1: PDB; p2: u_int32_t): integer; cdecl;
    truncate              : function (p1: PDB; p2: PDB_TXN; p3: Pu_int32_t; p4: u_int32_t): integer; cdecl;
    upgrade               : function (p1: PDB; p2: PAnsiChar; p3: u_int32_t): integer; cdecl;
    verify                : function (p1: PDB; p2: PAnsiChar; p3: PAnsiChar; p4: pointer; p5: u_int32_t): integer; cdecl;
    (* DB PUBLIC HANDLE LIST END *)

    (* DB PRIVATE HANDLE LIST BEGIN *)
    dump                  : function (p1: PDB; p2: PAnsiChar; p3: T___db_dump_P3; p4: pointer; p5: integer; p6: integer): integer; cdecl;
    db_am_remove          : function (p1: PDB; p2: PDB_THREAD_INFO; p3: PDB_TXN; p4: PAnsiChar; p5: PAnsiChar; p6: u_int32_t): integer; cdecl;
    db_am_rename          : function (p1: PDB; p2: PDB_THREAD_INFO; p3: PDB_TXN; p4: PAnsiChar; p5: PAnsiChar; p6: PAnsiChar): integer; cdecl;
    (* DB PRIVATE HANDLE LIST END *)

    (*
     * Never called; these are a place to save function pointers
     * so that we can undo an associate.
     *)
    stored_get  : function(p1: PDB; p2: PDB_TXN; p3: PDBT; p4: PDBT; p5: u_int32_t): integer; cdecl;
    stored_close: function(p1: PDB; p2: u_int32_t): integer; cdecl;

    (* Alternative handle close function, used by C++ API. *)
    alt_close: function(p1: PDB; p2: u_int32_t): integer; cdecl;

    am_ok: u_int32_t;   (* Legal AM choices. *)

    (*
     * This field really ought to be an AM_FLAG, but we have
     * have run out of bits.  If/when we decide to split up
     * the flags, we can incorporate it.
     *)
    preserve_fid: integer;    (* Do not free fileid on close. *)

    orig_flags: u_int32_t;       (* Flags at  open, for refresh *)
    flags: u_int32_t;
  end;



(*******************************************************
 * Access method cursors.
 *******************************************************)
  __dbc = record
    dbp: PDB;      (* Backing database *)
    dbenv: PDB_ENV;     (* Backing environment *)
    env: pointer; //^ENV;     (* Backing environment *)
    thread_info: pointer; //^DB_THREAD_INFO;  (* Thread that owns this cursor. *)
    txn: PDB_TXN;     (* Associated transaction. *)
    priority: DB_CACHE_PRIORITY;  (* Priority in cache. *)

    (*
     * Active/free cursor queues.
     *
     * !!!
     * Explicit representations of structures from queue.h.
     * TAILQ_ENTRY(__dbc) links;
     *)
    links: record
      tqe_next: PDBC;
      tqe_prev: ^PDBC;
    end;

    (*
     * Cursor queue of the owning transaction.
     *
     * !!!
     * Explicit representations of structures from queue.h.
     * TAILQ_function (__dbc): ENTRY;txn_cursors;
     *)
    txn_cursors: record
      tqe_next: PDBC; (* next element *)
      tqe_prev: ^PDBC;  (* address of previous next element *)
    end;

    (*
     * The DBT *'s below are used by the cursor routines to return
     * data to the user when DBT flags indicate that DB should manage
     * the returned memory.  They point at a DBT containing the buffer
     * and length that will be used, and 'belonging' to the handle that
     * should 'own' this memory.  This may be a 'my_*' field of this
     * cursor--the default--or it may be the corresponding field of
     * another cursor, a DB handle, a join cursor, etc.  In general, it
     * will be whatever handle the user originally used for the current
     * DB interface call.
     *)
    rskey: ^DBT;    (* Returned secondary key. *)
    rkey: ^DBT;     (* Returned [primary] key. *)
    rdata: ^DBT;    (* Returned data. *)

    my_rskey: DBT;    (* Space for returned secondary key. *)
    my_rkey: DBT;   (* Space for returned [primary] key. *)
    my_rdata: DBT;    (* Space for returned data. *)

    lref: pointer; //^DB_LOCKER;    (* Reference to default locker. *)
    locker: pointer; //^DB_LOCKER;    (* Locker for this operation. *)
    lock_dbt: DBT;    (* DBT referencing lock. *)
    lock: DB_LOCK_ILOCK;    (* Object to be locked. *)
    mylock: DB_LOCK;    (* CDB lock held on this cursor. *)

    dbtype: DBTYPE;   (* Cursor type. *)

    internal: pointer; //^DBC_INTERNAL;   (* Access method private. *)

    (* DBC PUBLIC HANDLE LIST BEGIN *)
    close       : function(p1: PDBC): integer; cdecl;
    cmp         : function(p1: PDBC; p2: PDBC; p3: Pinteger; p4: u_int32_t): integer; cdecl;
    count       : function(p1: PDBC; p2: Pdb_recno_t; p3: u_int32_t): integer; cdecl;
    del         : function(p1: PDBC; p2: u_int32_t): integer; cdecl;
    dup         : function(p1: PDBC; p2: PPDBC; p3: u_int32_t): integer; cdecl;
    get         : function(p1: PDBC; p2: PDBT; p3: PDBT; p4: u_int32_t): integer; cdecl;
    get_priority: function(p1: PDBC; p2: PDB_CACHE_PRIORITY): integer; cdecl;
    pget        : function(p1: PDBC; p2: PDBT; p3: PDBT; p4: PDBT; p5: u_int32_t): integer; cdecl;
    put         : function(p1: PDBC; p2: PDBT; p3: PDBT; p4: u_int32_t): integer; cdecl;
    set_priority: function(p1: PDBC; p2: DB_CACHE_PRIORITY): integer; cdecl;
    (* DBC PUBLIC HANDLE LIST END *)

    (* The following are the method names deprecated in the 4.6 release. *)
    c_close     : function(p1: PDBC): integer; cdecl;
    c_count     : function(p1: PDBC; p2: Pdb_recno_t; p3: u_int32_t): integer; cdecl;
    c_del       : function(p1: PDBC; p2: u_int32_t): integer; cdecl;
    c_dup       : function(p1: PDBC; p2: PPDBC; p3: u_int32_t): integer; cdecl;
    c_get       : function(p1: PDBC; p2: PDBT; p3: PDBT; p4: u_int32_t): integer; cdecl;
    c_pget      : function(p1: PDBC; p2: PDBT; p3: PDBT; p4: PDBT; p5: u_int32_t): integer; cdecl;
    c_put       : function(p1: PDBC; p2: PDBT; p3: PDBT; p4: u_int32_t): integer; cdecl;

    (* DBC PRIVATE HANDLE LIST BEGIN *)
    am_bulk     : function(p1: PDBC; p2: PDBT; p3: u_int32_t): integer; cdecl;
    am_close    : function(p1: PDBC; p2: db_pgno_t; p3: Pinteger): integer; cdecl;
    am_del      : function(p1: PDBC; p2: u_int32_t): integer; cdecl;
    am_destroy  : function(p1: PDBC): integer; cdecl;
    am_get      : function(p1: PDBC; p2: PDBT; p3: PDBT; p4: u_int32_t; p5: Pdb_pgno_t): integer; cdecl;
    am_put      : function(p1: PDBC; p2: PDBT; p3: PDBT; p4: u_int32_t; p5: Pdb_pgno_t): integer; cdecl;
    am_writelock: function(p1: PDBC): integer; cdecl;
    (* DBC PRIVATE HANDLE LIST END *)

    flags: u_int32_t;
  end;

(* Key range statistics structure *)
  __key_range = record
    less: double;
    equal: double;
    greater: double;
  end;

(* Btree/Recno statistics structure. *)
  __db_bt_stat = record
    bt_magic: u_int32_t;    (* Magic number. *)
    bt_version: u_int32_t;    (* Version number. *)
    bt_metaflags: u_int32_t;    (* Metadata flags. *)
    bt_nkeys: u_int32_t;    (* Number of unique keys. *)
    bt_ndata: u_int32_t;    (* Number of data items. *)
    bt_pagecnt: u_int32_t;    (* Page count. *)
    bt_pagesize: u_int32_t;   (* Page size. *)
    bt_minkey: u_int32_t;   (* Minkey value. *)
    bt_re_len: u_int32_t;   (* Fixed-length record length. *)
    bt_re_pad: u_int32_t;   (* Fixed-length record pad. *)
    bt_levels: u_int32_t;   (* Tree levels. *)
    bt_int_pg: u_int32_t;   (* Internal pages. *)
    bt_leaf_pg: u_int32_t;    (* Leaf pages. *)
    bt_dup_pg: u_int32_t;   (* Duplicate pages. *)
    bt_over_pg: u_int32_t;    (* Overflow pages. *)
    bt_empty_pg: u_int32_t;   (* Empty pages. *)
    bt_free: u_int32_t;   (* Pages on the free list. *)
    bt_int_pgfree: uintmax_t; (* Bytes free in internal pages. *)
    bt_leaf_pgfree: uintmax_t;  (* Bytes free in leaf pages. *)
    bt_dup_pgfree: uintmax_t; (* Bytes free in duplicate pages. *)
    bt_over_pgfree: uintmax_t;  (* Bytes free in overflow pages. *)
  end;

  __db_compact = record
    compact_fillpercent: u_int32_t; (* Desired fillfactor: 1-100 *)
    compact_timeout: db_timeout_t;  (* Lock timeout. *)
    compact_pages: u_int32_t;   (* Max pages to process. *)
    compact_empty_buckets: u_int32_t; (* Empty hash buckets found. *)
    compact_pages_free: u_int32_t;  (* Number of pages freed. *)
    compact_pages_examine: u_int32_t; (* Number of pages examine. *)
    compact_levels: u_int32_t;    (* Number of levels removed. *)
    compact_deadlock: u_int32_t;  (* Number of deadlocks. *)
    compact_pages_truncated: db_pgno_t; (* Pages truncated to OS. *)
    compact_truncate: db_pgno_t;  (* Page number for truncation *)
  end;

(* Hash statistics structure. *)
  __db_h_stat = record
    hash_magic: u_int32_t;    (* Magic number. *)
    hash_version: u_int32_t;    (* Version number. *)
    hash_metaflags: u_int32_t;  (* Metadata flags. *)
    hash_nkeys: u_int32_t;    (* Number of unique keys. *)
    hash_ndata: u_int32_t;    (* Number of data items. *)
    hash_pagecnt: u_int32_t;    (* Page count. *)
    hash_pagesize: u_int32_t; (* Page size. *)
    hash_ffactor: u_int32_t;    (* Fill factor specified at create. *)
    hash_buckets: u_int32_t;    (* Number of hash buckets. *)
    hash_free: u_int32_t;   (* Pages on the free list. *)
    hash_bfree: uintmax_t;    (* Bytes free on bucket pages. *)
    hash_bigpages: u_int32_t; (* Number of big key/data pages. *)
    hash_big_bfree: uintmax_t;  (* Bytes free on big item pages. *)
    hash_overflows: u_int32_t;  (* Number of overflow pages. *)
    hash_ovfl_free: uintmax_t;  (* Bytes free on ovfl pages. *)
    hash_dup: u_int32_t;    (* Number of dup pages. *)
    hash_dup_free: uintmax_t; (* Bytes free on duplicate pages. *)
  end;

(* Queue statistics structure. *)
  __db_qam_stat = record
    qs_magic: u_int32_t;    (* Magic number. *)
    qs_version: u_int32_t;    (* Version number. *)
    qs_metaflags: u_int32_t;    (* Metadata flags. *)
    qs_nkeys: u_int32_t;    (* Number of unique keys. *)
    qs_ndata: u_int32_t;    (* Number of data items. *)
    qs_pagesize: u_int32_t;   (* Page size. *)
    qs_extentsize: u_int32_t; (* Pages per extent. *)
    qs_pages: u_int32_t;    (* Data pages. *)
    qs_re_len: u_int32_t;   (* Fixed-length record length. *)
    qs_re_pad: u_int32_t;   (* Fixed-length record pad. *)
    qs_pgfree: u_int32_t;   (* Bytes free in data pages. *)
    qs_first_recno: u_int32_t;  (* First not deleted record. *)
    qs_cur_recno: u_int32_t;    (* Next available record number. *)
  end;

(*******************************************************
 * Environment.
 *******************************************************)

(*
 * Database environment structure.
 *
 * This is the public database environment handle.  The private environment
 * handle is the ENV structure.   The user owns this structure, the library
 * owns the ENV structure.  The reason there are two structures is because
 * the user's configuration outlives any particular DB_ENV^.open call, and
 * separate structures allows us to easily discard internal information without
 * discarding the user's configuration.
 *
 * Fields in the DB_ENV structure should normally be set only by application
 * DB_ENV handle methods.
 *)

// Translation: helper types for function parameters
  P___db_env_get_alloc_P2               = ^T___db_env_get_alloc_P2              ; // @@PAS_HELPER(P___db_env_get_alloc_P2)
  P___db_env_get_alloc_P3               = ^T___db_env_get_alloc_P3              ; // @@PAS_HELPER(P___db_env_get_alloc_P3)
  P___db_env_get_alloc_P4               = ^T___db_env_get_alloc_P4              ; // @@PAS_HELPER(P___db_env_get_alloc_P4)
  P___db_env_get_app_dispatch_P2        = ^T___db_env_get_app_dispatch_P2       ; // @@PAS_HELPER(P___db_env_get_app_dispatch_P2)
  P___db_env_get_errcall_P2             = ^T___db_env_get_errcall_P2            ; // @@PAS_HELPER(P___db_env_get_errcall_P2)
  P___db_env_get_feedback_P2            = ^T___db_env_get_feedback_P2           ; // @@PAS_HELPER(P___db_env_get_feedback_P2)
  P___db_env_get_isalive_P2             = ^T___db_env_get_isalive_P2            ; // @@PAS_HELPER(P___db_env_get_isalive_P2)
  P___db_env_get_msgcall_P2             = ^T___db_env_get_msgcall_P2            ; // @@PAS_HELPER(P___db_env_get_msgcall_P2)
  P___db_env_get_thread_id_fn_P2        = ^T___db_env_get_thread_id_fn_P2       ; // @@PAS_HELPER(P___db_env_get_thread_id_fn_P2)
  P___db_env_get_thread_id_string_fn_P2 = ^T___db_env_get_thread_id_string_fn_P2; // @@PAS_HELPER(P___db_env_get_thread_id_string_fn_P2)
  T___db_env_get_alloc_P2               = function (p1: size_t                                                              ): pointer  ; cdecl; // @@PAS_HELPER(T___db_env_get_alloc_P2)
  T___db_env_get_alloc_P3               = function (p1: pointer; p2: size_t                                                 ): pointer  ; cdecl; // @@PAS_HELPER(T___db_env_get_alloc_P3)
  T___db_env_get_alloc_P4               = procedure(p1: pointer                                                             )           ; cdecl; // @@PAS_HELPER(T___db_env_get_alloc_P4)
  T___db_env_get_app_dispatch_P2        = function (p1: PDB_ENV; p2: PDBT; p3: PDB_LSN; p4: db_recops                       ): integer  ; cdecl; // @@PAS_HELPER(T___db_env_get_app_dispatch_P2)
  T___db_env_get_errcall_P2             = procedure(p1: PDB_ENV; p2: PAnsiChar; p3: PAnsiChar                               )           ; cdecl; // @@PAS_HELPER(T___db_env_get_errcall_P2)
  T___db_env_get_feedback_P2            = procedure(p1: PDB_ENV; p2: integer; p3: integer                                   )           ; cdecl; // @@PAS_HELPER(T___db_env_get_feedback_P2)
  T___db_env_get_isalive_P2             = function (p1: PDB_ENV; p2: pid_t; p3: db_threadid_t; p4: u_int32_t                ): integer  ; cdecl; // @@PAS_HELPER(T___db_env_get_isalive_P2)
  T___db_env_get_msgcall_P2             = procedure(p1: PDB_ENV; p2: PAnsiChar                                              )           ; cdecl; // @@PAS_HELPER(T___db_env_get_msgcall_P2)
  T___db_env_get_thread_id_fn_P2        = procedure(p1: PDB_ENV; p2: Ppid_t; p3: Pdb_threadid_t                             )           ; cdecl; // @@PAS_HELPER(T___db_env_get_thread_id_fn_P2)
  T___db_env_get_thread_id_string_fn_P2 = function (p1: PDB_ENV; p2: pid_t; p3: db_threadid_t; p4: PAnsiChar                ): PAnsiChar; cdecl; // @@PAS_HELPER(T___db_env_get_thread_id_string_fn_P2)
  T___db_env_memp_register_P3           = function (p1: PDB_ENV; p2: db_pgno_t; p3: pointer; p4: PDBT                       ): integer  ; cdecl; // @@PAS_HELPER(T___db_env_memp_register_P3)
  T___db_env_memp_register_P4           = function (p1: PDB_ENV; p2: db_pgno_t; p3: pointer; p4: PDBT                       ): integer  ; cdecl; // @@PAS_HELPER(T___db_env_memp_register_P4)
  T___db_env_prdbt_P5                   = function (p1: pointer; p2: pointer                                                ): integer  ; cdecl; // @@PAS_HELPER(T___db_env_prdbt_P5)
  T___db_env_rep_set_transport_P3       = function (p1: PDB_ENV; p2: PDBT; p3: PDBT; p4: PDB_LSN; p5: integer; p6: u_int32_t): integer  ; cdecl; // @@PAS_HELPER(T___db_env_rep_set_transport_P3)
  T___db_env_set_alloc_P2               = function (p1: size_t                                                              ): pointer  ; cdecl; // @@PAS_HELPER(T___db_env_set_alloc_P2)
  T___db_env_set_alloc_P3               = function (p1: pointer; p2: size_t                                                 ): pointer  ; cdecl; // @@PAS_HELPER(T___db_env_set_alloc_P3)
  T___db_env_set_alloc_P4               = procedure(p1: pointer                                                             )           ; cdecl; // @@PAS_HELPER(T___db_env_set_alloc_P4)
  T___db_env_set_app_dispatch_P2        = function (p1: PDB_ENV; p2: PDBT; p3: PDB_LSN; p4: db_recops                       ): integer  ; cdecl; // @@PAS_HELPER(T___db_env_set_app_dispatch_P2)
  T___db_env_set_event_notify_P2        = procedure(p1: PDB_ENV; p2: u_int32_t; p3: pointer                                 )           ; cdecl; // @@PAS_HELPER(T___db_env_set_event_notify_P2)
  T___db_env_set_errcall_P2             = procedure(env: PDB_ENV; errpfx, msg: PAnsiChar                                    )           ; cdecl; // @@PAS_HELPER(T___db_env_set_errcall_P2)
  T___db_env_set_feedback_P2            = procedure(p1: PDB_ENV; p2: integer; p3: integer                                   )           ; cdecl; // @@PAS_HELPER(T___db_env_set_feedback_P2)
  T___db_env_set_isalive_P2             = function (p1: PDB_ENV; p2: pid_t; p3: db_threadid_t; p4: u_int32_t                ): integer  ; cdecl; // @@PAS_HELPER(T___db_env_set_isalive_P2)
  T___db_env_set_msgcall_P2             = procedure(p1: PDB_ENV; p2: PAnsiChar                                              )           ; cdecl; // @@PAS_HELPER(T___db_env_set_msgcall_P2)
  T___db_env_set_paniccall_P2           = procedure(p1: PDB_ENV; p2: integer                                                )           ; cdecl; // @@PAS_HELPER(T___db_env_set_paniccall_P2)
  T___db_env_set_thread_id_P2           = procedure(p1: PDB_ENV; p2: Ppid_t; p3: Pdb_threadid_t                             )           ; cdecl; // @@PAS_HELPER(T___db_env_set_thread_id_P2)
  T___db_env_set_thread_id_string_P2    = function (p1: PDB_ENV; p2: pid_t; p3: db_threadid_t; p4: PAnsiChar                ): PAnsiChar; cdecl; // @@PAS_HELPER(T___db_env_set_thread_id_string_P2)

  __db_env = record
    env: pointer; //^ENV;     (* Linked ENV structure *)

    (*
     * The DB_ENV structure can be used concurrently, so field access is
     * protected.
     *)
    mtx_db_env: db_mutex_t;   (* DB_ENV structure mutex *)

        (* Error message callback *)
    db_errcall: procedure(p1: PDB_ENV; p2: PAnsiChar; p3: PAnsiChar); cdecl;
    db_errfile: pointer; //FILE* (* Error message file stream *)
    db_errpfx: PAnsiChar; (* Error message prefix *)

        (* Other message callback *)
    db_msgcall: procedure(p1: PDB_ENV; p2: PAnsiChar); cdecl;
    db_msgfile: pointer; //FILE* (* Other message file stream *)

    (* Other application callback functions *)
    app_dispatch    : function (p1: PDB_ENV; p2: PDBT; p3: PDB_LSN; p4: db_recops): integer; cdecl;
    db_event_func   : procedure(p1: PDB_ENV; p2: u_int32_t; p3: pointer); cdecl;
    db_feedback     : procedure(p1: PDB_ENV; p2: integer; p3: integer); cdecl;
    db_free         : procedure(p1: pointer); cdecl;
    db_paniccall    : procedure(p1: PDB_ENV; p2: integer); cdecl;
    db_malloc       : function (p1: size_t): pointer; cdecl;
    db_realloc      : function (p1: pointer; p2: size_t): pointer; cdecl;
    is_alive        : function (p1: PDB_ENV; p2: pid_t; p3: db_threadid_t; p4: u_int32_t): integer; cdecl;
    thread_id       : procedure(p1: PDB_ENV; p2: Ppid_t; p3: Pdb_threadid_t); cdecl;
    thread_id_string: function (p1: PDB_ENV; p2: pid_t; p3: db_threadid_t; p4: PAnsiChar): PAnsiChar; cdecl;

    (* Application specified paths *)
    db_log_dir: PAnsiChar;    (* Database log file directory *)
    db_tmp_dir: PAnsiChar;    (* Database tmp file directory *)

    db_create_dir: PAnsiChar;   (* Create directory for data files *)
    db_data_dir: ^PAnsiChar;    (* Database data file directories *)
    data_cnt: Integer;    (* Database data file slots *)
    data_next: Integer;   (* Next database data file slot *)

    intermediate_dir_mode: PAnsiChar; (* Intermediate directory perms *)

    shm_key: LongInt;   (* shmget key *)

    passwd: PAnsiChar;    (* Cryptography support *)
    passwd_len: size_t;

    (* Private handle references *)
    app_private: pointer;   (* Application-private handle *)
    api1_internal: pointer;   (* C++, Perl API private *)
    api2_internal: pointer;   (* Java API private *)

    verbose: u_int32_t; (* DB_VERB_XXX flags *)

    (* Mutex configuration *)
    mutex_align: u_int32_t; (* Mutex alignment *)
    mutex_cnt: u_int32_t; (* Number of mutexes to configure *)
    mutex_inc: u_int32_t; (* Number of mutexes to add *)
    mutex_tas_spins: u_int32_t;(* Test-and-set spin count *)

    (* Locking configuration *)
    lk_conflicts: ^u_int8_t;  (* Two dimensional conflict matrix *)
    lk_modes: Integer;  (* Number of lock modes in table *)
    lk_detect: u_int32_t; (* Deadlock detect on all conflicts *)
    lk_max: u_int32_t;  (* Maximum number of locks *)
    lk_max_lockers: u_int32_t;(* Maximum number of lockers *)
    lk_max_objects: u_int32_t;(* Maximum number of locked objects *)
    lk_partitions: u_int32_t;(* Number of object partitions *)
    lk_timeout: db_timeout_t; (* Lock timeout period *)

    (* Logging configuration *)
    lg_bsize: u_int32_t;  (* Buffer size *)
    lg_filemode: Integer; (* Log file permission mode *)
    lg_regionmax: u_int32_t;  (* Region size *)
    lg_size: u_int32_t; (* Log file size *)
    lg_flags: u_int32_t;  (* Log configuration *)

    (* Memory pool configuration *)
    mp_gbytes: u_int32_t; (* Cache size: GB *)
    mp_bytes: u_int32_t;  (* Cache size: bytes *)
    mp_max_gbytes: u_int32_t; (* Maximum cache size: GB *)
    mp_max_bytes: u_int32_t;  (* Maximum cache size: bytes *)
    mp_mmapsize: size_t;  (* Maximum file size for mmap *)
    mp_maxopenfd: Integer;  (* Maximum open file descriptors *)
    mp_maxwrite: Integer; (* Maximum buffers to write *)
    mp_ncache: u_int; (* Initial number of cache regions *)
    mp_pagesize: u_int32_t; (* Average page size *)
    mp_tablesize: u_int32_t;  (* Approximate hash table size *)
    mp_mtxcount: u_int32_t; (* Number of mutexs *)
          (* Sleep after writing max buffers *)
    mp_maxwrite_sleep: db_timeout_t;

    (* Transaction configuration *)
    tx_max: u_int32_t;    (* Maximum number of transactions *)
    tx_timestamp: time_t; (* Recover to specific timestamp *)
    tx_timeout: db_timeout_t; (* Timeout for transactions *)

    (* Thread tracking configuration *)
    thr_max: u_int32_t; (* Thread count *)

    (*
     * The following fields are not strictly user-owned, but they outlive
     * the ENV structure, and so are stored here.
     *)
    registry: pointer; //^DB_FH;  (* DB_REGISTER file handle *)
    registry_off: u_int32_t;  (*
           * Offset of our slot.  We can't use
           * off_t because its size depends on
           * build settings.
           *)
    envreg_timeout: db_timeout_t; (* DB_REGISTER wait timeout *)

    flags: u_int32_t;

    (* DB_ENV PUBLIC HANDLE LIST BEGIN *)
    add_data_dir             : function (p1: PDB_ENV; p2: PAnsiChar): integer; cdecl;
    cdsgroup_begin           : function (p1: PDB_ENV; p2: PPDB_TXN): integer; cdecl;
    close                    : function (dbenv: PDB_ENV; flags: u_int32_t): integer; cdecl;
    dbremove                 : function (p1: PDB_ENV; p2: PDB_TXN; p3: PAnsiChar; p4: PAnsiChar; p5: u_int32_t): integer; cdecl;
    dbrename                 : function (p1: PDB_ENV; p2: PDB_TXN; p3: PAnsiChar; p4: PAnsiChar; p5: PAnsiChar; p6: u_int32_t): integer; cdecl;
    err                      : procedure(p1: PDB_ENV; p2: integer; p3: PAnsiChar { ... }); cdecl;
    errx                     : procedure(p1: PDB_ENV; p2: PAnsiChar { ... }); cdecl;
    failchk                  : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    fileid_reset             : function (p1: PDB_ENV; p2: PAnsiChar; p3: u_int32_t): integer; cdecl;
    get_alloc                : function (p1: PDB_ENV; p2: P___db_env_get_alloc_P2; p3: P___db_env_get_alloc_P3; p4: P___db_env_get_alloc_P4): integer; cdecl;
    get_app_dispatch         : function (p1: PDB_ENV; p2: P___db_env_get_app_dispatch_P2): integer; cdecl;
    get_cache_max            : function (p1: PDB_ENV; p2: Pu_int32_t; p3: Pu_int32_t): integer; cdecl;
    get_cachesize            : function (p1: PDB_ENV; p2: Pu_int32_t; p3: Pu_int32_t; p4: Pinteger): integer; cdecl;
    get_create_dir           : function (p1: PDB_ENV; p2: PPAnsiChar): integer; cdecl;
    get_data_dirs            : function (p1: PDB_ENV; p2: PPPAnsiChar): integer; cdecl;
    get_data_len             : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_encrypt_flags        : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_errcall              : procedure(p1: PDB_ENV; p2: P___db_env_get_errcall_P2); cdecl;
    get_errfile              : procedure(p1: PDB_ENV; p2: pointer); cdecl;
    get_errpfx               : procedure(p1: PDB_ENV; p2: PPAnsiChar); cdecl;
    get_flags                : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_feedback             : function (p1: PDB_ENV; p2: P___db_env_get_feedback_P2): integer; cdecl;
    get_home                 : function (p1: PDB_ENV; p2: PPAnsiChar): integer; cdecl;
    get_intermediate_dir_mode: function (p1: PDB_ENV; p2: PPAnsiChar): integer; cdecl;
    get_isalive              : function (p1: PDB_ENV; p2: P___db_env_get_isalive_P2): integer; cdecl;
    get_lg_bsize             : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_lg_dir               : function (p1: PDB_ENV; p2: PPAnsiChar): integer; cdecl;
    get_lg_filemode          : function (p1: PDB_ENV; p2: Pinteger): integer; cdecl;
    get_lg_max               : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_lg_regionmax         : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_lk_conflicts         : function (p1: PDB_ENV; p2: PPu_int8_t; p3: Pinteger): integer; cdecl;
    get_lk_detect            : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_lk_max_lockers       : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_lk_max_locks         : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_lk_max_objects       : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_lk_partitions        : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_lk_priority          : function (p1: PDB_ENV; p2: u_int32_t; p3: Pu_int32_t): integer; cdecl;
    get_mp_max_openfd        : function (p1: PDB_ENV; p2: Pinteger): integer; cdecl;
    get_mp_max_write         : function (p1: PDB_ENV; p2: Pinteger; p3: Pdb_timeout_t): integer; cdecl;
    get_mp_mmapsize          : function (p1: PDB_ENV; p2: Psize_t): integer; cdecl;
    get_mp_mtxcount          : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_mp_pagesize          : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_mp_tablesize         : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_msgcall              : procedure(p1: PDB_ENV; p2: P___db_env_get_msgcall_P2); cdecl;
    get_msgfile              : procedure(p1: PDB_ENV; p2: pointer); cdecl;
    get_open_flags           : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_shm_key              : function (p1: PDB_ENV; p2: Plongint): integer; cdecl;
    get_thread_count         : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_thread_id_fn         : function (p1: PDB_ENV; p2: P___db_env_get_thread_id_fn_P2): integer; cdecl;
    get_thread_id_string_fn  : function (p1: PDB_ENV; p2: P___db_env_get_thread_id_string_fn_P2): integer; cdecl;
    get_timeout              : function (p1: PDB_ENV; p2: Pdb_timeout_t; p3: u_int32_t): integer; cdecl;
    get_tmp_dir              : function (p1: PDB_ENV; p2: PPAnsiChar): integer; cdecl;
    get_tx_max               : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    get_tx_timestamp         : function (p1: PDB_ENV; p2: Ptime_t): integer; cdecl;
    get_verbose              : function (p1: PDB_ENV; p2: u_int32_t; p3: Pinteger): integer; cdecl;
    is_bigendian             : function (): integer; cdecl;
    lock_detect              : function (p1: PDB_ENV; p2: u_int32_t; p3: u_int32_t; p4: Pinteger): integer; cdecl;
    lock_get                 : function (p1: PDB_ENV; p2: u_int32_t; p3: u_int32_t; p4: PDBT; p5: db_lockmode_t; p6: PDB_LOCK): integer; cdecl;
    lock_id                  : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    lock_id_free             : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    lock_put                 : function (p1: PDB_ENV; p2: PDB_LOCK): integer; cdecl;
    lock_stat                : function (p1: PDB_ENV; p2: PPDB_LOCK_STAT; p3: u_int32_t): integer; cdecl;
    lock_stat_print          : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    lock_vec                 : function (p1: PDB_ENV; p2: u_int32_t; p3: u_int32_t; p4: PDB_LOCKREQ; p5: integer; p6: PPDB_LOCKREQ): integer; cdecl;
    log_archive              : function (p1: PDB_ENV; p2: PPPAnsiChar; p3: u_int32_t): integer; cdecl;
    log_cursor               : function (p1: PDB_ENV; p2: PPDB_LOGC; p3: u_int32_t): integer; cdecl;
    log_file                 : function (p1: PDB_ENV; p2: PDB_LSN; p3: PAnsiChar; p4: size_t): integer; cdecl;
    log_flush                : function (p1: PDB_ENV; p2: PDB_LSN): integer; cdecl;
    log_get_config           : function (p1: PDB_ENV; p2: u_int32_t; p3: Pinteger): integer; cdecl;
    log_printf               : function (p1: PDB_ENV; p2: PDB_TXN; p3: PAnsiChar { ... }): integer; cdecl;
    log_put                  : function (p1: PDB_ENV; p2: PDB_LSN; p3: PDBT; p4: u_int32_t): integer; cdecl;
    log_put_record           : function (p1: PDB_ENV; p2: PDB; p3: PDB_TXN; p4: PDB_LSN; p5: u_int32_t; p6: u_int32_t; p7: u_int32_t; p8: u_int32_t; p9: PDB_LOG_RECSPEC { ... }): integer; cdecl;
    log_read_record          : function (p1: PDB_ENV; p2: PPDB; p3: pointer; p4: pointer; p5: PDB_LOG_RECSPEC; p6: u_int32_t; p7: Ppointer): integer; cdecl;
    log_set_config           : function (p1: PDB_ENV; p2: u_int32_t; p3: integer): integer; cdecl;
    log_stat                 : function (p1: PDB_ENV; p2: PPDB_LOG_STAT; p3: u_int32_t): integer; cdecl;
    log_stat_print           : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    log_verify               : function (p1: PDB_ENV; p2: PDB_LOG_VERIFY_CONFIG): integer; cdecl;
    lsn_reset                : function (p1: PDB_ENV; p2: PAnsiChar; p3: u_int32_t): integer; cdecl;
    memp_fcreate             : function (p1: PDB_ENV; p2: PPDB_MPOOLFILE; p3: u_int32_t): integer; cdecl;
    memp_register            : function (p1: PDB_ENV; p2: integer; p3: T___db_env_memp_register_P3; p4: T___db_env_memp_register_P4): integer; cdecl;
    memp_stat                : function (p1: PDB_ENV; p2: PPDB_MPOOL_STAT; p3: PPPDB_MPOOL_FSTAT; p4: u_int32_t): integer; cdecl;
    memp_stat_print          : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    memp_sync                : function (p1: PDB_ENV; p2: PDB_LSN): integer; cdecl;
    memp_trickle             : function (p1: PDB_ENV; p2: integer; p3: Pinteger): integer; cdecl;
    mutex_alloc              : function (p1: PDB_ENV; p2: u_int32_t; p3: Pdb_mutex_t): integer; cdecl;
    mutex_free               : function (p1: PDB_ENV; p2: db_mutex_t): integer; cdecl;
    mutex_get_align          : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    mutex_get_increment      : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    mutex_get_max            : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    mutex_get_tas_spins      : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    mutex_lock               : function (p1: PDB_ENV; p2: db_mutex_t): integer; cdecl;
    mutex_set_align          : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    mutex_set_increment      : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    mutex_set_max            : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    mutex_set_tas_spins      : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    mutex_stat               : function (p1: PDB_ENV; p2: PPDB_MUTEX_STAT; p3: u_int32_t): integer; cdecl;
    mutex_stat_print         : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    mutex_unlock             : function (p1: PDB_ENV; p2: db_mutex_t): integer; cdecl;
    open                     : function (dbenv: PDB_ENV; const db_home: PAnsiChar; flags: u_int32_t; mode: integer): integer; cdecl;
    remove                   : function (p1: PDB_ENV; p2: PAnsiChar; p3: u_int32_t): integer; cdecl;
    rep_elect                : function (p1: PDB_ENV; p2: u_int32_t; p3: u_int32_t; p4: u_int32_t): integer; cdecl;
    rep_flush                : function (p1: PDB_ENV): integer; cdecl;
    rep_get_clockskew        : function (p1: PDB_ENV; p2: Pu_int32_t; p3: Pu_int32_t): integer; cdecl;
    rep_get_config           : function (p1: PDB_ENV; p2: u_int32_t; p3: Pinteger): integer; cdecl;
    rep_get_limit            : function (p1: PDB_ENV; p2: Pu_int32_t; p3: Pu_int32_t): integer; cdecl;
    rep_get_nsites           : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    rep_get_priority         : function (p1: PDB_ENV; p2: Pu_int32_t): integer; cdecl;
    rep_get_request          : function (p1: PDB_ENV; p2: Pu_int32_t; p3: Pu_int32_t): integer; cdecl;
    rep_get_timeout          : function (p1: PDB_ENV; p2: integer; p3: Pu_int32_t): integer; cdecl;
    rep_process_message      : function (p1: PDB_ENV; p2: PDBT; p3: PDBT; p4: integer; p5: PDB_LSN): integer; cdecl;
    rep_set_clockskew        : function (p1: PDB_ENV; p2: u_int32_t; p3: u_int32_t): integer; cdecl;
    rep_set_config           : function (p1: PDB_ENV; p2: u_int32_t; p3: integer): integer; cdecl;
    rep_set_limit            : function (p1: PDB_ENV; p2: u_int32_t; p3: u_int32_t): integer; cdecl;
    rep_set_nsites           : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    rep_set_priority         : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    rep_set_request          : function (p1: PDB_ENV; p2: u_int32_t; p3: u_int32_t): integer; cdecl;
    rep_set_timeout          : function (p1: PDB_ENV; p2: integer; p3: db_timeout_t): integer; cdecl;
    rep_set_transport        : function (p1: PDB_ENV; p2: integer; p3: T___db_env_rep_set_transport_P3): integer; cdecl;
    rep_start                : function (p1: PDB_ENV; p2: PDBT; p3: u_int32_t): integer; cdecl;
    rep_stat                 : function (p1: PDB_ENV; p2: PPDB_REP_STAT; p3: u_int32_t): integer; cdecl;
    rep_stat_print           : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    rep_sync                 : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    repmgr_add_remote_site   : function (p1: PDB_ENV; p2: PAnsiChar; p3: u_int; p4: Pinteger; p5: u_int32_t): integer; cdecl;
    repmgr_get_ack_policy    : function (p1: PDB_ENV; p2: Pinteger): integer; cdecl;
    repmgr_get_local_site    : function (p1: PDB_ENV; p2: PPAnsiChar; p3: Pu_int): integer; cdecl;
    repmgr_set_ack_policy    : function (p1: PDB_ENV; p2: integer): integer; cdecl;
    repmgr_set_local_site    : function (p1: PDB_ENV; p2: PAnsiChar; p3: u_int; p4: u_int32_t): integer; cdecl;
    repmgr_site_list         : function (p1: PDB_ENV; p2: Pu_int; p3: PPDB_REPMGR_SITE): integer; cdecl;
    repmgr_start             : function (p1: PDB_ENV; p2: integer; p3: u_int32_t): integer; cdecl;
    repmgr_stat              : function (p1: PDB_ENV; p2: PPDB_REPMGR_STAT; p3: u_int32_t): integer; cdecl;
    repmgr_stat_print        : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    set_alloc                : function (p1: PDB_ENV; p2: T___db_env_set_alloc_P2; p3: T___db_env_set_alloc_P3; p4: T___db_env_set_alloc_P4): integer; cdecl;
    set_app_dispatch         : function (p1: PDB_ENV; p2: T___db_env_set_app_dispatch_P2): integer; cdecl;
    set_cache_max            : function (p1: PDB_ENV; p2: u_int32_t; p3: u_int32_t): integer; cdecl;
    set_cachesize            : function (p1: PDB_ENV; p2: u_int32_t; p3: u_int32_t; p4: integer): integer; cdecl;
    set_create_dir           : function (p1: PDB_ENV; p2: PAnsiChar): integer; cdecl;
    set_data_dir             : function (p1: PDB_ENV; p2: PAnsiChar): integer; cdecl;
    set_data_len             : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    set_encrypt              : function (p1: PDB_ENV; p2: PAnsiChar; p3: u_int32_t): integer; cdecl;
    set_errcall              : procedure(env: PDB_ENV; p2: T___db_env_set_errcall_P2); cdecl;
    set_errfile              : procedure(p1: PDB_ENV; p2: pointer); cdecl;
    set_errpfx               : procedure(p1: PDB_ENV; p2: PAnsiChar); cdecl;
    set_event_notify         : function (p1: PDB_ENV; p2: T___db_env_set_event_notify_P2): integer; cdecl;
    set_feedback             : function (p1: PDB_ENV; p2: T___db_env_set_feedback_P2): integer; cdecl;
    set_flags                : function (p1: PDB_ENV; p2: u_int32_t; p3: integer): integer; cdecl;
    set_intermediate_dir_mode: function (p1: PDB_ENV; p2: PAnsiChar): integer; cdecl;
    set_isalive              : function (p1: PDB_ENV; p2: T___db_env_set_isalive_P2): integer; cdecl;
    set_lg_bsize             : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    set_lg_dir               : function (p1: PDB_ENV; p2: PAnsiChar): integer; cdecl;
    set_lg_filemode          : function (p1: PDB_ENV; p2: integer): integer; cdecl;
    set_lg_max               : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    set_lg_regionmax         : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    set_lk_conflicts         : function (p1: PDB_ENV; p2: Pu_int8_t; p3: integer): integer; cdecl;
    set_lk_detect            : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    set_lk_max_lockers       : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    set_lk_max_locks         : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    set_lk_max_objects       : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    set_lk_partitions        : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    set_lk_priority          : function (p1: PDB_ENV; p2: u_int32_t; p3: u_int32_t): integer; cdecl;
    set_mp_max_openfd        : function (p1: PDB_ENV; p2: integer): integer; cdecl;
    set_mp_max_write         : function (p1: PDB_ENV; p2: integer; p3: db_timeout_t): integer; cdecl;
    set_mp_mmapsize          : function (p1: PDB_ENV; p2: size_t): integer; cdecl;
    set_mp_mtxcount          : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    set_mp_pagesize          : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    set_mp_tablesize         : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    set_msgcall              : procedure(p1: PDB_ENV; p2: T___db_env_set_msgcall_P2); cdecl;
    set_msgfile              : procedure(p1: PDB_ENV; p2: pointer); cdecl;
    set_paniccall            : function (p1: PDB_ENV; p2: T___db_env_set_paniccall_P2): integer; cdecl;
    set_shm_key              : function (p1: PDB_ENV; p2: longint): integer; cdecl;
    set_thread_count         : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    set_thread_id            : function (p1: PDB_ENV; p2: T___db_env_set_thread_id_P2): integer; cdecl;
    set_thread_id_string     : function (p1: PDB_ENV; p2: T___db_env_set_thread_id_string_P2): integer; cdecl;
    set_timeout              : function (p1: PDB_ENV; p2: db_timeout_t; p3: u_int32_t): integer; cdecl;
    set_tmp_dir              : function (p1: PDB_ENV; p2: PAnsiChar): integer; cdecl;
    set_tx_max               : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    set_tx_timestamp         : function (p1: PDB_ENV; p2: Ptime_t): integer; cdecl;
    set_verbose              : function (p1: PDB_ENV; p2: u_int32_t; p3: integer): integer; cdecl;
    txn_applied              : function (p1: PDB_ENV; p2: PDB_TXN_TOKEN; p3: db_timeout_t; p4: u_int32_t): integer; cdecl;
    stat_print               : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    txn_begin                : function (p1: PDB_ENV; p2: PDB_TXN; p3: PPDB_TXN; p4: u_int32_t): integer; cdecl;
    txn_checkpoint           : function (p1: PDB_ENV; p2: u_int32_t; p3: u_int32_t; p4: u_int32_t): integer; cdecl;
    txn_recover              : function (p1: PDB_ENV; p2: PDB_PREPLIST; p3: u_int32_t; p4: Pu_int32_t; p5: u_int32_t): integer; cdecl;
    txn_stat                 : function (p1: PDB_ENV; p2: PPDB_TXN_STAT; p3: u_int32_t): integer; cdecl;
    txn_stat_print           : function (p1: PDB_ENV; p2: u_int32_t): integer; cdecl;
    (* DB_ENV PUBLIC HANDLE LIST END *)

    (* DB_ENV PRIVATE HANDLE LIST BEGIN *)
    prdbt                    : function (p1: PDBT; p2: integer; p3: PAnsiChar; p4: pointer; p5: T___db_env_prdbt_P5; p6: integer): integer; cdecl;
    (* DB_ENV PRIVATE HANDLE LIST END *)
  end;

(*
 * Dispatch structure for recovery, log verification and print routines. Since
 * internal and external routines take different arguments (ENV versus DB_ENV),
 * we need something more elaborate than a single pointer and size.
 *)
  __db_distab = record
    int_dispatch: pointer; // int (**int_dispatch) (ENV *, DBT *, DB_LSN *, db_recops, void *); cdecl;
    int_size: size_t;
    ext_dispatch: pointer; // int (**ext_dispatch) (DB_ENV *, DBT *, DB_LSN *, db_recops); cdecl;
    ext_size: size_t;
  end;

(*
 * Log verification configuration structure.
 *)
  __db_logvrfy_config = record
    continue_after_fail, verbose: integer;
    cachesize: u_int32_t;
    temp_envhome: PAnsiChar;
    dbfile, dbname: PAnsiChar;
    start_lsn, end_lsn: DB_LSN;
    start_time, end_time: time_t;
  end;

// TRANSLATION: STRUCTURE (RECORD) RENAMING...

type
  DB_DB                = __db               ; // @@PAS_MAPPING(DB_DB=DB)
  DB_BTREE_STAT        = __db_bt_stat       ;
  //DB_CIPHER          = __db_cipher        ;
  DB_COMPACT           = __db_compact       ;
  DB_DISTAB            = __db_distab        ;
  DB_ENV               = __db_env           ;
  DB_HASH_STAT         = __db_h_stat        ;
  DB_LOCK_HSTAT        = __db_lock_hstat    ;
  DB_LOCK_PSTAT        = __db_lock_pstat    ;
  DB_LOCK_STAT         = __db_lock_stat     ;
  //DB_LOCKER          = __db_locker        ;
  DB_LOCKREQ           = __db_lockreq       ;
  //DB_LOCKTAB         = __db_locktab       ;
  //DB_LOG             = __db_log           ;
  DB_LOGC              = __db_log_cursor    ;
  DB_LOG_STAT          = __db_log_stat      ;
  //DB_MPOOL           = __db_mpool         ;
  DB_MPOOL_FSTAT       = __db_mpool_fstat   ;
  DB_MPOOL_STAT        = __db_mpool_stat    ;
  DB_MPOOLFILE         = __db_mpoolfile     ;
  DB_MUTEX_STAT        = __db_mutex_stat    ;
  //DB_MUTEX           = __db_mutex_t       ;
  //DB_MUTEXMGR        = __db_mutexmgr      ;
  DB_PREPLIST          = __db_preplist      ;
  DB_QUEUE_STAT        = __db_qam_stat      ;
  //DB_REP             = __db_rep           ;
  DB_REP_STAT          = __db_rep_stat      ;
  DB_REPMGR_SITE       = __db_repmgr_site   ;
  DB_REPMGR_STAT       = __db_repmgr_stat   ;
  DB_SEQUENCE_STAT     = __db_seq_stat      ;
  DB_SEQUENCE          = __db_sequence      ;
  //DB_THREAD_INFO     = __db_thread_info   ;
  DB_TXN               = __db_txn           ;
  DB_TXN_ACTIVE        = __db_txn_active    ;
  DB_TXN_STAT          = __db_txn_stat      ;
  DB_TXN_TOKEN         = __db_txn_token     ;
  //DB_TXNMGR          = __db_txnmgr        ;
  DBC                  = __dbc              ;
  //DBC_INTERNAL       = __dbc_internal     ;
  //ENV                = __env              ;
  //DB_FH              = __fh_t             ;
  //FNAME              = __fname            ;
  DB_KEY_RANGE         = __key_range        ;
  //MPOOLFILE          = __mpoolfile        ;
  DB_LOG_VERIFY_CONFIG = __db_logvrfy_config;

// Translation: helper types for function parameters
  __db_off_t = int64_t; // @@PAS_HELPER(__db_off_t)
  T_db_env_set_func_close_P1      = function (p1: integer                                                               ): integer; cdecl; // @@PAS_HELPER(T_db_env_set_func_close_P1)
  T_db_env_set_func_dirfree_P1    = procedure(p1: PPAnsiChar; p2: integer                                               )         ; cdecl; // @@PAS_HELPER(T_db_env_set_func_dirfree_P1)
  T_db_env_set_func_dirlist_P1    = function (p1: PAnsiChar; p2: PPPAnsiChar; p3: Pinteger                              ): integer; cdecl; // @@PAS_HELPER(T_db_env_set_func_dirlist_P1)
  T_db_env_set_func_exists_P1     = function (p1: PAnsiChar; p2: Pinteger                                               ): integer; cdecl; // @@PAS_HELPER(T_db_env_set_func_exists_P1)
  T_db_env_set_func_file_map_P1   = function (p1: PDB_ENV; p2: PAnsiChar; p3: size_t; p4: integer; p5: Ppointer         ): integer; cdecl; // @@PAS_HELPER(T_db_env_set_func_file_map_P1)
  T_db_env_set_func_file_map_P2   = function (p1: PDB_ENV; p2: pointer                                                  ): integer; cdecl; // @@PAS_HELPER(T_db_env_set_func_file_map_P2)
  T_db_env_set_func_free_P1       = procedure(p1: pointer                                                               )         ; cdecl; // @@PAS_HELPER(T_db_env_set_func_free_P1)
  T_db_env_set_func_fsync_P1      = function (p1: integer                                                               ): integer; cdecl; // @@PAS_HELPER(T_db_env_set_func_fsync_P1)
  T_db_env_set_func_ftruncate_P1  = function (p1: integer; p2: __db_off_t                                               ): integer; cdecl; // @@PAS_HELPER(T_db_env_set_func_ftruncate_P1)
  T_db_env_set_func_ioinfo_P1     = function (p1: PAnsiChar; p2: integer; p3: Pu_int32_t; p4: Pu_int32_t; p5: Pu_int32_t): integer; cdecl; // @@PAS_HELPER(T_db_env_set_func_ioinfo_P1)
  T_db_env_set_func_malloc_P1     = function (p1: size_t                                                                ): pointer; cdecl; // @@PAS_HELPER(T_db_env_set_func_malloc_P1)
  T_db_env_set_func_open_P1       = function (p1: PAnsiChar; p2: integer { ... }                                        ): integer; cdecl; // @@PAS_HELPER(T_db_env_set_func_open_P1)
  T_db_env_set_func_pread_P1      = function (p1: integer; p2: pointer; p3: size_t; p4: __db_off_t                      ): ssize_t; cdecl; // @@PAS_HELPER(T_db_env_set_func_pread_P1)
  T_db_env_set_func_pwrite_P1     = function (p1: integer; p2: pointer; p3: size_t; p4: __db_off_t                      ): ssize_t; cdecl; // @@PAS_HELPER(T_db_env_set_func_pwrite_P1)
  T_db_env_set_func_read_P1       = function (p1: integer; p2: pointer; p3: size_t                                      ): ssize_t; cdecl; // @@PAS_HELPER(T_db_env_set_func_read_P1)
  T_db_env_set_func_realloc_P1    = function (p1: pointer; p2: size_t                                                   ): pointer; cdecl; // @@PAS_HELPER(T_db_env_set_func_realloc_P1)
  T_db_env_set_func_region_map_P1 = function (p1: PDB_ENV; p2: PAnsiChar; p3: size_t; p4: Pinteger; p5: Ppointer        ): integer; cdecl; // @@PAS_HELPER(T_db_env_set_func_region_map_P1)
  T_db_env_set_func_region_map_P2 = function (p1: PDB_ENV; p2: pointer                                                  ): integer; cdecl; // @@PAS_HELPER(T_db_env_set_func_region_map_P2)
  T_db_env_set_func_rename_P1     = function (p1: PAnsiChar; p2: PAnsiChar                                              ): integer; cdecl; // @@PAS_HELPER(T_db_env_set_func_rename_P1)
  T_db_env_set_func_seek_P1       = function (p1: integer; p2: __db_off_t; p3: integer                                  ): integer; cdecl; // @@PAS_HELPER(T_db_env_set_func_seek_P1)
  T_db_env_set_func_unlink_P1     = function (p1: PAnsiChar                                                             ): integer; cdecl; // @@PAS_HELPER(T_db_env_set_func_unlink_P1)
  T_db_env_set_func_write_P1      = function (p1: integer; p2: pointer; p3: size_t                                      ): ssize_t; cdecl; // @@PAS_HELPER(T_db_env_set_func_write_P1)
  T_db_env_set_func_yield_P1      = function (p1: u_long; p2: u_long                                                    ): integer; cdecl; // @@PAS_HELPER(T_db_env_set_func_yield_P1)

// TRANSLATION: GLOBAL FUNCTIONS...

var
  db_create                 : function(out dbp: PDB; dbenv: PDB_ENV; flags: u_int32_t                          ): integer  ; cdecl;
  db_strerror               : function(error: integer                                                          ): PAnsiChar; cdecl;
  db_env_create             : function(out dbenvp: PDB_ENV; flags: u_int32_t                                   ): integer  ; cdecl;
  db_version                : function(major, minor, patch: pinteger                                           ): PAnsiChar; cdecl;
  db_full_version           : function(family, release, major, minor, patch: pinteger                          ): PAnsiChar; cdecl;
  log_compare               : function(lsn0, lsn1: PDB_LSN                                                     ): integer  ; cdecl;
  db_sequence_create        : function(out seq: PDB_SEQUENCE; db: PDB; flags: u_int32_t                        ): integer  ; cdecl;

  //db_env_set_func_close     : function(p1: T_db_env_set_func_close_P1                                          ): integer  ; cdecl;
  //db_env_set_func_dirfree   : function(p1: T_db_env_set_func_dirfree_P1                                        ): integer  ; cdecl;
  //db_env_set_func_dirlist   : function(p1: T_db_env_set_func_dirlist_P1                                        ): integer  ; cdecl;
  //db_env_set_func_exists    : function(p1: T_db_env_set_func_exists_P1                                         ): integer  ; cdecl;
  db_env_set_func_free      : function(p1: T_db_env_set_func_free_P1                                           ): integer  ; cdecl;
  //db_env_set_func_fsync     : function(p1: T_db_env_set_func_fsync_P1                                          ): integer  ; cdecl;
  //db_env_set_func_ftruncate : function(p1: T_db_env_set_func_ftruncate_P1                                      ): integer  ; cdecl;
  //db_env_set_func_ioinfo    : function(p1: T_db_env_set_func_ioinfo_P1                                         ): integer  ; cdecl;
  db_env_set_func_malloc    : function(p1: T_db_env_set_func_malloc_P1                                         ): integer  ; cdecl;
  //db_env_set_func_file_map  : function(p1: T_db_env_set_func_file_map_P1  ; p2: T_db_env_set_func_file_map_P2  ): integer  ; cdecl;
  //db_env_set_func_region_map: function(p1: T_db_env_set_func_region_map_P1; p2: T_db_env_set_func_region_map_P2): integer  ; cdecl;
  //db_env_set_func_pread     : function(p1: T_db_env_set_func_pread_P1                                          ): integer  ; cdecl;
  //db_env_set_func_pwrite    : function(p1: T_db_env_set_func_pwrite_P1                                         ): integer  ; cdecl;
  //db_env_set_func_open      : function(p1: T_db_env_set_func_open_P1                                           ): integer  ; cdecl;
  //db_env_set_func_read      : function(p1: T_db_env_set_func_read_P1                                           ): integer  ; cdecl;
  db_env_set_func_realloc   : function(p1: T_db_env_set_func_realloc_P1                                        ): integer  ; cdecl;
  //db_env_set_func_rename    : function(p1: T_db_env_set_func_rename_P1                                         ): integer  ; cdecl;
  //db_env_set_func_seek      : function(p1: T_db_env_set_func_seek_P1                                           ): integer  ; cdecl;
  //db_env_set_func_unlink    : function(p1: T_db_env_set_func_unlink_P1                                         ): integer  ; cdecl;
  //db_env_set_func_write     : function(p1: T_db_env_set_func_write_P1                                          ): integer  ; cdecl;
  //db_env_set_func_yield     : function(p1: T_db_env_set_func_yield_P1                                          ): integer  ; cdecl;

// TRANSLATION: ROUTINES

function InitBerkeleyDB: Boolean; // INITIALIZE DELPHI LIBRARY (dynamic binding)

procedure CheckBDB            (resultCode: integer);                   // check result from api functions and raise exception if error
function  CheckAndFoundBDB    (resultCode: integer): boolean;          // check result, return true if ok, return false if not found, raise exception if error
function  CheckAndNotExistsBDB(resultCode: integer): boolean;          // check result, return true if ok, return false if already exists, raise exception if error
procedure CheckBDBandNil      (resultCode: integer; var ptr);          // set the 'ptr' pointer to nil and call CheckBDB()
function  CallBDBandNil       (resultCode: integer; var ptr): integer; // set the 'ptr' pointer to nil and return the 'resultCode'

implementation

uses Windows, SyncObjs;

var
  DllHandle: THandle = 0;
  DllInitError: Boolean = False;
  LibInitCS: TCriticalSection = nil;

const
  DllName = 'libdb51.dll';

function InitBerkeleyDB: Boolean;

  procedure LoadProc(var proc; const procName: string);
  begin
    pointer(proc) := GetProcAddress(DllHandle, pchar(procName));
    if pointer(proc) = nil then begin
      raise EBerkeleyDBExeption.Create(
        'Function ''' + procName + ''' not found in ' + DllName
      );
    end;
  end;

begin
  LibInitCS.Acquire;
  try
    Result := DllHandle <> 0;

    if (DllHandle = 0) and not DllInitError then begin

      DllHandle := LoadLibrary(DllName);

      if DllHandle = 0 then begin
        DllInitError := True;
        raise EBerkeleyDBExeption.Create(
          'Berkeley DB not found (' + DllName + ')' + #13#10 + SysErrorMessage(GetLastError)
        );
      end;

      LoadProc(db_create              , 'db_create'              );
      LoadProc(db_strerror            , 'db_strerror'            );
      LoadProc(db_env_create          , 'db_env_create'          );
      LoadProc(db_version             , 'db_version'             );
      LoadProc(db_full_version        , 'db_full_version'        );
      LoadProc(log_compare            , 'log_compare'            );
      LoadProc(db_sequence_create     , 'db_sequence_create'     );

      LoadProc(db_env_set_func_free   , 'db_env_set_func_free'   );
      LoadProc(db_env_set_func_malloc , 'db_env_set_func_malloc' );
      LoadProc(db_env_set_func_realloc, 'db_env_set_func_realloc');

      //NOT AVAILABLE IN BERKELEY DB FOR WINDOWS:
      //LoadProc(db_env_set_func_close     , 'db_env_set_func_close'     );
      //LoadProc(db_env_set_func_dirfree   , 'db_env_set_func_dirfree'   );
      //LoadProc(db_env_set_func_dirlist   , 'db_env_set_func_dirlist'   );
      //LoadProc(db_env_set_func_exists    , 'db_env_set_func_exists'    );
      //LoadProc(db_env_set_func_fsync     , 'db_env_set_func_fsync'     );
      //LoadProc(db_env_set_func_ftruncate , 'db_env_set_func_ftruncate' );
      //LoadProc(db_env_set_func_ioinfo    , 'db_env_set_func_ioinfo'    );
      //LoadProc(db_env_set_func_file_map  , 'db_env_set_func_file_map'  );
      //LoadProc(db_env_set_func_region_map, 'db_env_set_func_region_map');
      //LoadProc(db_env_set_func_pread     , 'db_env_set_func_pread'     );
      //LoadProc(db_env_set_func_pwrite    , 'db_env_set_func_pwrite'    );
      //LoadProc(db_env_set_func_open      , 'db_env_set_func_open'      );
      //LoadProc(db_env_set_func_read      , 'db_env_set_func_read'      );
      //LoadProc(db_env_set_func_rename    , 'db_env_set_func_rename'    );
      //LoadProc(db_env_set_func_seek      , 'db_env_set_func_seek'      );
      //LoadProc(db_env_set_func_unlink    , 'db_env_set_func_unlink'    );
      //LoadProc(db_env_set_func_write     , 'db_env_set_func_write'     );
      //LoadProc(db_env_set_func_yield     , 'db_env_set_func_yield'     );

      Result := True;
    end;
  finally
    LibInitCS.Release;
  end;
end;

procedure CheckBDB(resultCode: integer);
begin
  if resultCode <> 0 then begin
    raise EBerkeleyDBExeption.Create(
      'Error #' + IntToStr(resultCode) + ': ' + string(AnsiString(db_strerror(resultCode)))
    );
  end;
end;

function CheckAndFoundBDB(resultCode: integer): boolean;
begin
  if (resultCode=DB_NOTFOUND) or (resultCode=DB_KEYEMPTY) then begin
    Result := false
  end else begin
    CheckBDB(resultCode);
    Result := true;
  end;
end;

function CheckAndNotExistsBDB(resultCode: integer): boolean;
begin
  if resultCode = DB_KEYEXIST then begin
    Result := false
  end else begin
    CheckBDB(resultCode);
    Result := true;
  end;
end;

procedure CheckBDBandNil(resultCode: integer; var ptr);
begin
  pointer(ptr) := nil;
  CheckBDB(resultCode);
end;

function CallBDBandNil(resultCode: integer; var ptr): integer;
begin
  pointer(ptr) := nil;
  Result := resultCode;
end;

initialization
  LibInitCS := TCriticalSection.Create;

finalization
  if DllHandle<>0 then begin
    FreeLibrary(DllHandle);
  end;
  FreeAndNil(LibInitCS);

end.

