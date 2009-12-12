{                                                                              }
{ Conditional defines defined here:                                            }
{   DELPHI       - Delphi compiler.                                            }
{     DELPHI1    - Delphi 1.                                                   }
{     DELPHI2    - Delphi 2.                                                   }
{     DELPHI3    - Delphi 3.                                                   }
{     DELPHI4    - Delphi 4.                                                   }
{     DELPHI5    - Delphi 5.                                                   }
{     DELPHI6    - Delphi 6.                                                   }
{     DELPHI7    - Delphi 7.                                                   }
{     DELPHI3_UP - Delphi 3 or later.                                          }
{     DELPHI4_UP - Delphi 4 or later.                                          }
{     DELPHI5_UP - Delphi 5 or later.                                          }
{     DELPHI6_UP - Delphi 6 or later.                                          }
{     DELPHI7_UP - Delphi 7 or later.                                          }
{   KYLIX        - Kylix compiler.                                             }
{   BCB          - Borland C++ Builder compiler.                               }
{   FREEPASCAL   - Free Pascal compiler.                                       }
{   OS_UNIX      - Unix type operating system.                                 }
{   OS_BSD       - BSD type operating system.                                  }
{     OS_LINUX   - Linux operating system.                                     }
{     OS_FREEBSD - FreeBSD operating system.                                   }
{   OS_MSWIN     - MS Windows type operating system.                           }
{     OS_WIN16   - Win16 operating system.                                     }
{     OS_WIN32   - Win32 operating system.                                     }
{     OS_WIN64   - Win64 operating system.                                     }
{   CPU_INTEL386 - Intel 386 compatible CPU.                                   }
{   WINTEL       - OS_WIN32 and CPU_INTEL386.                                  }
{   LINTEL       - OS_LINUX and CPU_INTEL386.                                  }
{                                                                              }



{                                                                              }
{ Delphi 1                                                                     }
{                                                                              }
{$IFDEF VER80}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI1}
  {$DEFINE WIN16}
{$ENDIF}



{                                                                              }
{ Delphi 2                                                                     }
{                                                                              }
{$IFDEF VER90}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2}
  {$DEFINE WIN32}
  {$DEFINE CPU386}
{$ENDIF}
{$IFDEF VER93}
  {$DEFINE BCB}
  {$DEFINE BCB1}
{$ENDIF}



{                                                                              }
{ Delphi 3                                                                     }
{                                                                              }
{$IFDEF VER100}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI3}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI3_DOWN}
  {$DEFINE DELPHI4_DOWN}
  {$DEFINE DELPHI5_DOWN}
  {$DEFINE WIN32}
  {$DEFINE CPU386}
{$ENDIF}
{$IFDEF VER110}
  {$DEFINE BCB}
  {$DEFINE BCB3}
{$ENDIF}



{                                                                              }
{ Delphi 4                                                                     }
{                                                                              }
{$IFDEF VER120}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI4_DOWN}
  {$DEFINE DELPHI5_DOWN}
  {$DEFINE WIN32}
  {$DEFINE CPU386}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE BCB}
  {$DEFINE BCB4}
{$ENDIF}



{                                                                              }
{ Delphi 5                                                                     }
{                                                                              }
{$IFDEF VER130}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI5_DOWN}
  {$DEFINE WIN32}
  {$DEFINE CPU386}
{$ENDIF}
{$IFDEF VER135}
  {$DEFINE BCB}
  {$DEFINE BCB5}
{$ENDIF}



{                                                                              }
{ Delphi 6                                                                     }
{                                                                              }
{$IFDEF VER140}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI6_DOWN}
  {$IFDEF LINUX}
    {$DEFINE KYLIX}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Delphi 7                                                                     }
{                                                                              }
{$IFDEF VER150}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$IFDEF LINUX}
    {$DEFINE KYLIX}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Delphi features                                                              }
{                                                                              }
{$IFDEF DELPHI4_UP}
  {$DEFINE SupportCardinal}
{$ENDIF}
{$IFDEF DELPHI5_UP}
  {$DEFINE SupportDynArray}
{$ENDIF}



{                                                                              }
{ Free Pascal                                                                  }
{                                                                              }
{$IFDEF FPC}
  {$DEFINE FREEPASCAL}
  {$MODE DELPHI}
  {$IFDEF CPUI386}
    {$DEFINE CPU386}
  {$ENDIF}
  {$DEFINE SupportCardinal}
  {$DEFINE SupportDynArray}
{$ENDIF}



{                                                                              }
{ CPU type                                                                     }
{                                                                              }
{$IFDEF CPU386}
  {$DEFINE CPU_INTEL386}
{$ENDIF}



{                                                                              }
{ Operating system type                                                        }
{                                                                              }
{$IFDEF WIN16}
  {$DEFINE OS_WIN16}
{$ENDIF}
{$IFDEF WIN32}
  {$DEFINE OS_WIN32}
{$ENDIF}
{$IFDEF WIN64}
  {$DEFINE OS_WIN64}
{$ENDIF}
{$IFDEF OS_WIN16}
  {$DEFINE OS_MSWIN}
{$ENDIF}
{$IFDEF OS_WIN32}
  {$DEFINE OS_MSWIN}
{$ENDIF}
{$IFDEF OS_WIN64}
  {$DEFINE OS_MSWIN}
{$ENDIF}
{$IFDEF MSWINDOWS}
  {$DEFINE OS_MSWIN}
{$ENDIF}
{$IFDEF LINUX}
  {$DEFINE UNIX}
  {$DEFINE OS_LINUX}
{$ENDIF}
{$IFDEF FREEBSD}
  {$DEFINE BSD}
  {$DEFINE OS_FREEBSD}
{$ENDIF}
{$IFDEF BSD}
  {$DEFINE UNIX}
  {$DEFINE OS_BSD}
{$ENDIF}
{$IFDEF UNIX}
  {$DEFINE OS_UNIX}
{$ENDIF}



{                                                                              }
{ OS / CPU combination                                                         }
{                                                                              }
{$IFDEF CPU_INTEL386}
  {$IFDEF OS_MSWIN}
    {$DEFINE WINTEL}
  {$ENDIF}
  {$IFDEF OS_LINUX}
    {$DEFINE LINTEL}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Standard compiler directives                                                 }
{                                                                              }
{$EXTENDEDSYNTAX ON}
{$IOCHECKS ON}
{$LONGSTRINGS ON}
{$BOOLEVAL OFF}
{$OPTIMIZATION OFF}
{$WRITEABLECONST OFF}
{$IFDEF DEBUG}
  {$ASSERTIONS ON}
  {$DEBUGINFO ON}
  {$OVERFLOWCHECKS ON}
  {$RANGECHECKS ON}
  {$WARNINGS ON}
  {$HINTS ON}
{$ELSE}
  {$ASSERTIONS OFF}
  {$DEBUGINFO OFF}
  {$OVERFLOWCHECKS OFF}
  {$RANGECHECKS OFF}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}

{$IFNDEF DEBUG}
  {$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM OFF}
    {$WARN UNIT_PLATFORM OFF}
    {$WARN UNIT_DEPRECATED OFF}
  {$ENDIF}
{$ENDIF}

{$IFDEF DELPHI7_UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

