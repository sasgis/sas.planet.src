{******************************************************************************}
{                                                                              }
{   File name:        cDefines.inc                                             }
{   File updated:     2011/09/27                                               }
{                                                                              }
{   Library:          Fundamentals 4.00                                        }
{   Home page:        http://fundementals.sourceforge.net                      }
{   Copyright:        Copyright © 1998-2011, David J Butler                    }
{                     All rights reserved.                                     }
{                                                                              }
{ Conditionals defined elsewhere:                                              }
{                                                                              }
{   DEBUG             - Enable debug mode.                                     }
{                       Sets compiler options for debug information,           }
{                       range/overflow checking, assertions, etc.              }
{                       If not defined, set compiler options for release.      }
{   PurePascal        - Don't use assembler code.                              }
{                       All code is compiled from pure Pascal for maximum      }
{                       portability.                                           }
{                                                                              }
{ Conditionals defined here:                                                   }
{                                                                              }
{   DELPHI            - Delphi compiler.                                       }
{     DELPHI1         - Delphi 1.                                              }
{     DELPHI2         - Delphi 2.                                              }
{     DELPHI3         - Delphi 3.                                              }
{     DELPHI4         - Delphi 4.                                              }
{     DELPHI5         - Delphi 5.                                              }
{     DELPHI6         - Delphi 6.                                              }
{     DELPHI7         - Delphi 7.                                              }
{     DELPHI8         - Delphi 8.                                              }
{     DELPHI2005      - Delphi 2005.                                           }
{     DELPHI2006      - Delphi 2006.                                           }
{     DELPHI2007      - Delphi 2007.                                           }
{     DELPHI2009      - Delphi 2009.                                           }
{     DELPHI2010      - Delphi 2010.                                           }
{     DELPHIXE        - Delphi XE.                                             }
{     DELPHIXE2       - Delphi XE2.                                            }
{                                                                              }
{     DELPHI3_UP      - Delphi 3 or later.                                     }
{     DELPHI4_UP      - Delphi 4 or later.                                     }
{     DELPHI5_UP      - Delphi 5 or later.                                     }
{     DELPHI6_UP      - Delphi 6 or later.                                     }
{     DELPHI7_UP      - Delphi 7 or later.                                     }
{     DELPHI8_UP      - Delphi 8 or later.                                     }
{     DELPHI2005_UP   - Delphi 2005 or later.                                  }
{     DELPHI2006_UP   - Delphi 2006 or later.                                  }
{     DELPHI2007_UP   - Delphi 2007 or later.                                  }
{     DELPHI2009_UP   - Delphi 2009 or later.                                  }
{     DELPHI2010_UP   - Delphi 2010 or later.                                  }
{     DELPHIXE_UP     - Delphi XE or later.                                    }
{     DELPHIXE2_UP    - Delphi XE2 or later.                                   }
{                                                                              }
{     DELPHI3_DOWN    - Delphi 3 or earlier.                                   }
{     DELPHI4_DOWN    - Delphi 4 or earlier.                                   }
{     DELPHI5_DOWN    - Delphi 5 or earlier.                                   }
{     DELPHI6_DOWN    - Delphi 6 or earlier.                                   }
{     DELPHI7_DOWN    - Delphi 7 or earlier.                                   }
{                                                                              }
{   KYLIX             - Kylix compiler.                                        }
{   BCB               - Borland C++ Builder compiler.                          }
{                                                                              }
{   FREEPASCAL        - Free Pascal compiler.                                  }
{     FREEPASCAL1     - FreePascal 1.x                                         }
{     FREEPASCAL2     - FreePascal 2.x                                         }
{     FREEPASCAL2_UP  - FreePascal 2.x or later.                               }
{     FREEPASCAL2_0   - FreePascal 2.0                                         }
{     FREEPASCAL2_2   - FreePascal 2.2                                         }
{     FREEPASCAL2_4   - FreePascal 2.4                                         }
{                                                                              }
{   SupportCardinal            - Compiler supports Cardinal type.              }
{   SupportDynArray            - Compiler supports dynamic arrays.             }
{   SupportInt64               - Compiler supports Int64 type.                 }
{   SupportWideChar            - Compiler supports WideChar type.              }
{   SupportCurrency            - Compiler supports Currency type.              }
{   SupportVariant             - Compiler supports Variant type.               }
{   SupportAssert              - Compiler supports Assert statement.           }
{   SupportOverload            - Compiler supports method overloading.         }
{   SupportUTF8String          - Compiler supports UTF8String type.            }
{   SupportUCS4String          - Cimpiler supports UCS4String type.            }
{   SupportInline              - Compiler supports inline functions.           }
{   SupportForIn               - Compiler supports for-in loop.                }
{   SupportGenerics            - Compiler supports generics.                   }
{   SupportOperatorOverloading - Compiler supports overator overloading.       }
{   SupportAnonymousMethods    - Compiler supports anonymous methods.          }
{   SupportUnicodeString       - Compiler supports UnicodeString type.         }
{   SupportRawByteString       - Compiler supports RawByteString type.         }
{   SupportEnumValue           - Compiler supports enumeration values.         }
{                                                                              }
{   CharIsAnsi      - Default Char type is AnsiChar (1 byte).                  }
{   CharIsWide      - Default Char type is WideChar (2 bytes).                 }
{   StringIsAnsi    - Default String type is AnsiString.                       }
{   StringIsUnicode - Default String type is UnicodeString.                    }
{                                                                              }
{   UseInline         - SupportInline and not PurePascal.                      }
{                                                                              }
{   OS_UNIX           - Unix type operating system.                            }
{     OS_BSD          - BSD type operating system.                             }
{       OS_FREEBSD    - FreeBSD operating system.                              }
{       OS_NETBSD     - NetBSD operating system.                               }
{       OS_OPENBSD    - OpenBSD operating system.                              }
{     OS_LINUX        - Linux operating system.                                }
{     OS_MACOSX       - MacOSX operating system.                               }
{     OS_SOLARIS      - Sun Solaris operating system.                          }
{   OS_MSWIN          - MS Windows type operating system.                      }
{     OS_WIN16        - Win16 operating system.                                }
{     OS_WIN32        - Win32 operating system.                                }
{     OS_WIN64        - Win64 operating system.                                }
{   OS_OS2            - OS/2 operating system.                                 }
{   OS_DOS            - DOS operating system.                                  }
{     OS_DOS32        - DOS operating system with 32-bit extender.             }
{                                                                              }
{   ManagedCode       - Compiles managed code.                                 }
{     DOT_NET         - Compiles .NET code.                                    }
{                                                                              }
{   NativeCode        - Compiles native code.                                  }
{     CPU_386         - Intel 386 32-bit compatible CPU.                       }
{     CPU_X86_64      - x86 64-bit compatible CPU.                             }
{     CPU_68K         - Motorola 68000 compatible CPU.                         }
{     CPU_PPC32       - PowerPC 32-bit CPU.                                    }
{     CPU_PPC64       - PowerPC 64-bit CPU.                                    }
{     CPU_ARM         - ARM based CPU.                                         }
{                                                                              }
{   WindowsPlatform   - Win32/Win64/.NET code on a Windows platform.           }
{                                                                              }
{   ASM386_DELPHI     - Compiler supports 386 assembler (Delphi style)         }
{   ASM386_FREEPASCAL - Compiler supports 386 assembler (FreePascal style)     }
{   ASM386            - Compiler supports 386 assembler (Generic)              }
{   ASMX86_64         - Compiler supports x86-64 assembler (Generic)           }
{                                                                              }
{******************************************************************************}



{                                                                              }
{ Delphi 1                                                                     }
{                                                                              }
{$IFDEF VER80}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI1}
  {$DEFINE DELPHI3_DOWN}
  {$DEFINE DELPHI4_DOWN}
  {$DEFINE DELPHI5_DOWN}
  {$DEFINE DELPHI6_DOWN}
  {$DEFINE DELPHI7_DOWN}
{$ENDIF}



{                                                                              }
{ Delphi 2                                                                     }
{                                                                              }
{$IFDEF VER90}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2}
  {$DEFINE DELPHI3_DOWN}
  {$DEFINE DELPHI4_DOWN}
  {$DEFINE DELPHI5_DOWN}
  {$DEFINE DELPHI6_DOWN}
  {$DEFINE DELPHI7_DOWN}
{$ENDIF}
{$IFDEF VER93}
  {$DEFINE BCB}
  {$DEFINE BCB1}
{$ENDIF}



{                                                                              }
{ Delphi 3                                                                     }
{ C Builder 3                                                                  }
{                                                                              }
{$IFDEF VER100}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI3}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI3_DOWN}
  {$DEFINE DELPHI4_DOWN}
  {$DEFINE DELPHI5_DOWN}
  {$DEFINE DELPHI6_DOWN}
  {$DEFINE DELPHI7_DOWN}
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
  {$DEFINE DELPHI6_DOWN}
  {$DEFINE DELPHI7_DOWN}
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
  {$DEFINE DELPHI6_DOWN}
  {$DEFINE DELPHI7_DOWN}
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
  {$DEFINE DELPHI7_DOWN}
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
  {$DEFINE DELPHI7_DOWN}
  {$IFDEF LINUX}
    {$DEFINE KYLIX}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Delphi 8 for .NET                                                            }
{                                                                              }
{$IFDEF VER160}
  {$DEFINE DOT_NET}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI8}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
{$ENDIF}



{                                                                              }
{ Delphi 2005                                                                  }
{                                                                              }
{$IFDEF VER170}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2005}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI2005_UP}
{$ENDIF}



{                                                                              }
{ Delphi 2006 (or Delphi 2007)                                                 }
{ Delphi 2006 and some releases of Delphi 2007 used VER180.                    }
{ This file treats VER180 as Delphi 2006.                                      }
{                                                                              }
{$IFDEF VER180}
{$IFNDEF VER185}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2006}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI2005_UP}
  {$DEFINE DELPHI2006_UP}
{$ENDIF}
{$ENDIF}


{                                                                              }
{ Delphi 2007                                                                  }
{                                                                              }
{$IFDEF VER185}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2007}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI2005_UP}
  {$DEFINE DELPHI2006_UP}
  {$DEFINE DELPHI2007_UP}
{$ENDIF}



{                                                                              }
{ Delphi 2007 for .NET                                                         }
{                                                                              }
{$IFDEF VER190}
  {$DEFINE DOT_NET}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2007}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI2005_UP}
  {$DEFINE DELPHI2006_UP}
  {$DEFINE DELPHI2007_UP}
{$ENDIF}



{                                                                              }
{ Delphi 2009                                                                  }
{                                                                              }
{$IFDEF VER200}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2009}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI2005_UP}
  {$DEFINE DELPHI2006_UP}
  {$DEFINE DELPHI2007_UP}
  {$DEFINE DELPHI2009_UP}
{$ENDIF}



{                                                                              }
{ Delphi 2010                                                                  }
{                                                                              }
{$IFDEF VER210}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2010}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI2005_UP}
  {$DEFINE DELPHI2006_UP}
  {$DEFINE DELPHI2007_UP}
  {$DEFINE DELPHI2009_UP}
  {$DEFINE DELPHI2010_UP}
{$ENDIF}


{                                                                              }
{ Delphi XE                                                                    }
{                                                                              }
{$IFDEF VER220}
  {$DEFINE DELPHI}
  {$DEFINE DELPHIXE}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI2005_UP}
  {$DEFINE DELPHI2006_UP}
  {$DEFINE DELPHI2007_UP}
  {$DEFINE DELPHI2009_UP}
  {$DEFINE DELPHI2010_UP}
  {$DEFINE DELPHIXE_UP}
{$ENDIF}



{                                                                              }
{ Delphi XE2                                                                   }
{                                                                              }
{$IFDEF VER230}
  {$DEFINE DELPHI}
  {$DEFINE DELPHIXE2}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI2005_UP}
  {$DEFINE DELPHI2006_UP}
  {$DEFINE DELPHI2007_UP}
  {$DEFINE DELPHI2009_UP}
  {$DEFINE DELPHI2010_UP}
  {$DEFINE DELPHIXE_UP}
  {$DEFINE DELPHIXE2_UP}
{$ENDIF}



{                                                                              }
{ Future Delphi versions                                                       }
{                                                                              }
{$IFDEF VER235}
  {$DEFINE DELPHI_Future}
{$ENDIF}
{$IFDEF VER240}
  {$DEFINE DELPHI_Future}
{$ENDIF}
{$IFDEF VER245}
  {$DEFINE DELPHI_Future}
{$ENDIF}
{$IFDEF VER250}
  {$DEFINE DELPHI_Future}
{$ENDIF}

{$IFDEF DELPHI_Future}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI2005_UP}
  {$DEFINE DELPHI2006_UP}
  {$DEFINE DELPHI2007_UP}
  {$DEFINE DELPHI2009_UP}
  {$DEFINE DELPHI2010_UP}
  {$DEFINE DELPHIXE_UP}
  {$DEFINE DELPHIXE2_UP}
{$ENDIF}



{                                                                              }
{ Delphi: Operating System and CPU                                             }
{                                                                              }
{$IFDEF DELPHI}
  {$IFDEF DELPHI1}
    {$DEFINE OS_WIN16}
    {$DEFINE CPU_X86}
  {$ENDIF}
  {$IFDEF DELPHI2}
    {$DEFINE OS_WIN32}
    {$DEFINE CPU_386}
  {$ENDIF}
  {$IFDEF DELPHI3}
    {$DEFINE OS_WIN32}
    {$DEFINE CPU_386}
  {$ENDIF}
  {$IFDEF DELPHI4}
    {$DEFINE OS_WIN32}
    {$DEFINE CPU_386}
  {$ENDIF}
  {$IFDEF DELPHI5}
    {$DEFINE OS_WIN32}
    {$DEFINE CPU_386}
  {$ENDIF}
  {$IFDEF DELPHI6}
    {$IFDEF LINUX}
      {$DEFINE OS_LINUX}
    {$ELSE}
      {$DEFINE OS_WIN32}
    {$ENDIF}
    {$DEFINE CPU_386}
  {$ENDIF}
  {$IFDEF DELPHI7}
    {$IFDEF LINUX}
      {$DEFINE OS_LINUX}
    {$ELSE}
      {$DEFINE OS_WIN32}
    {$ENDIF}
    {$DEFINE CPU_386}
  {$ENDIF}
  {$IFDEF DELPHI8}
    {$DEFINE OS_WIN32}
  {$ENDIF}
  {$IFDEF DELPHI2005_UP}
    {$IFDEF WIN64}
      {$DEFINE OS_WIN64}
      {$DEFINE CPU_X86_64}
    {$ELSE}
      {$DEFINE OS_WIN32}
      {$DEFINE CPU_386}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Delphi: Supported language features                                          }
{                                                                              }
{$IFDEF DELPHI3_UP}
  {$DEFINE SupportCardinal}
  {$DEFINE SupportCurrency}
  {$DEFINE SupportVariant}
{$ENDIF}

{$IFDEF DELPHI4_UP}
  {$DEFINE SupportInt64}
  {$DEFINE SupportWideChar}
  {$DEFINE SupportDynArray}
  {$DEFINE SupportAssert}
{$ENDIF}

{$IFDEF DELPHI5_UP}
  {$DEFINE SupportOverload}
{$ENDIF}

{$IFDEF DELPHI6_UP}
  {$DEFINE SupportUTF8String}
  {$DEFINE SupportUCS4String}
  {$IFDEF DELPHI2009_UP}{$IFDEF ManagedCode}
    {$UNDEF SupportUCS4String}
  {$ENDIF}{$ENDIF}
  {$DEFINE SupportEnumValue}
{$ENDIF}

{$IFDEF DELPHI2005_UP}
  {$DEFINE SupportInline}
  {$DEFINE SupportOperatorOverloading}
{$ENDIF}

{$IFDEF DELPHI2005}
  {$DEFINE SupportInlineIsBuggy}
{$ENDIF}

{$IFDEF DELPHI2006}
  {$DEFINE SupportInlineIsBuggy}
{$ENDIF}

{$IFDEF DELPHI2007_UP}
  {$DEFINE SupportForIn}
{$ENDIF}

{$IFDEF DELPHI2009_UP}
  {$DEFINE SupportAnonymousMethods}
  {$DEFINE SupportGenerics}
  {$DEFINE SupportUnicodeString}
  {$DEFINE SupportRawByteString}
  {$IFDEF ManagedCode}
    {$UNDEF SupportRawByteString}
    {$UNDEF SupportUTF8String}
  {$ENDIF}
{$ENDIF}

{$IFDEF DELPHI}
  {$IFDEF DELPHI2009_UP}
    {$DEFINE CharIsWide}
    {$DEFINE StringIsUnicode}
  {$ELSE}
    {$DEFINE CharIsAnsi}
    {$DEFINE StringIsAnsi}
  {$ENDIF}
  {$DEFINE SetSizeAlignIsByte}
{$ENDIF}



{                                                                              }
{ Free Pascal                                                                  }
{                                                                              }
{$IFDEF FPC}
  {$DEFINE FREEPASCAL}
  {$IFDEF VER1}
    {$DEFINE FREEPASCAL1}
  {$ELSE}
    {$DEFINE FREEPASCAL2_UP}
    {$IFDEF VER2}
      {$DEFINE FREEPASCAL2}
    {$ENDIF}
    {$IFDEF VER2_0}
      {$DEFINE FREEPASCAL2_0}
    {$ENDIF}
    {$IFDEF VER2_2}
      {$DEFINE FREEPASCAL2_2}
    {$ENDIF}
    {$IFDEF VER2_4}
      {$DEFINE FREEPASCAL2_4}
    {$ENDIF}
  {$ENDIF}
  {$MODE DELPHI}
{$ENDIF}



{                                                                              }
{ Free Pascal: Operating System and CPU                                        }
{                                                                              }
{$IFDEF FREEPASCAL}
  {$IFDEF LINUX}   {$DEFINE OS_LINUX}   {$ENDIF}
  {$IFDEF FREEBSD} {$DEFINE OS_FREEBSD} {$ENDIF}
  {$IFDEF NETBSD}  {$DEFINE OS_NETBSD}  {$ENDIF}
  {$IFDEF SOLARIS} {$DEFINE OS_SOLARIS} {$ENDIF}
  {$IFDEF GO32V2}  {$DEFINE OS_DOS32}   {$ENDIF}
  {$IFDEF OS2}     {$DEFINE OS_OS2}     {$ENDIF}
  {$IFDEF WINDOWS} {$DEFINE OS_MSWIN}   {$ENDIF}
  {$IFDEF WIN32}   {$DEFINE OS_WIN32}   {$ENDIF}
  {$IFDEF WIN64}   {$DEFINE OS_WIN64}   {$ENDIF}
  {$IFDEF WINCE}   {$DEFINE OS_WINCE}   {$ENDIF}
  {$IFDEF DARWIN}  {$DEFINE OS_MACOSX}  {$ENDIF}

  {$IFDEF CPU386}       {$DEFINE CPU_386}    {$ENDIF}
  {$IFDEF CPUI386}      {$DEFINE CPU_386}    {$ENDIF}
  {$IFDEF CPUX86_64}    {$DEFINE CPU_X86_64} {$ENDIF}
  {$IFDEF CPUAMD64}     {$DEFINE CPU_X86_64} {$ENDIF}
  {$IFDEF ARM}          {$DEFINE CPU_ARM}    {$ENDIF}
  {$IFDEF CPU68}        {$DEFINE CPU_68K}    {$ENDIF}
  {$IFDEF CPUM68K}      {$DEFINE CPU_68K}    {$ENDIF}
  {$IFDEF CPUPOWERPC32} {$DEFINE CPU_PPC32}  {$ENDIF}
  {$IFDEF CPUPOWERPC64} {$DEFINE CPU_PPC64}  {$ENDIF}
{$ENDIF}



{                                                                              }
{ FreePascal: Supported language features                                      }
{                                                                              }
{$IFDEF FREEPASCAL2_UP}
  {$DEFINE SupportCardinal}
  {$DEFINE SupportCurrency}
  {$DEFINE SupportInt64}
  {$DEFINE SupportWideChar}
  {$DEFINE SupportDynArray}
  {$DEFINE SupportAssert}
  {$DEFINE SupportOverload}
  {$DEFINE SupportUTF8String}
  {$DEFINE SupportUCS4String}
  {$DEFINE CharIsAnsi}
  {$DEFINE StringIsAnsi}
  {$DEFINE SetSizeAlignIsLongWord}
  {$DEFINE SupportEnumValue}
{$ENDIF}



{                                                                              }
{ Operating System                                                             }
{                                                                              }

{ Windows }
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
{$IFDEF OS_MSWIN}
  {$DEFINE MSWIN}
{$ENDIF}

{ Unix-family }
{$IFDEF LINUX}
  {$DEFINE UNIX}
  {$DEFINE OS_LINUX}
{$ENDIF}
{$IFDEF FREEBSD}
  {$DEFINE BSD}
  {$DEFINE OS_FREEBSD}
{$ENDIF}
{$IFDEF NETBSD}
  {$DEFINE BSD}
  {$DEFINE OS_NETBSD}
{$ENDIF}
{$IFDEF OPENBSD}
  {$DEFINE BSD}
  {$DEFINE OS_OPENBSD}
{$ENDIF}
{$IFDEF BSD}
  {$DEFINE UNIX}
  {$DEFINE OS_BSD}
{$ENDIF}
{$IFDEF MACOSX}
  {$DEFINE UNIX}
  {$DEFINE OS_MACOSX}
{$ENDIF}
{$IFDEF UNIX}
  {$DEFINE OS_UNIX}
{$ENDIF}

{ Other }
{$IFDEF OS2}
  {$DEFINE OS_OS2}
{$ENDIF}
{$IFDEF DOS32}
  {$DEFINE DOS}
  {$DEFINE OS_DOS32}
{$ENDIF}
{$IFDEF DOS}
  {$DEFINE OS_DOS}
{$ENDIF}



{                                                                              }
{ .NET                                                                         }
{                                                                              }
{$IFDEF CLR}
  {$DEFINE DOT_NET}
{$ENDIF}
{$IFDEF DOT_NET}
  {$DEFINE ManagedCode}
{$ENDIF}



{                                                                              }
{ Windows platform                                                             }
{                                                                              }
{$IFDEF DOT_NET}
  {$DEFINE WindowsPlatform}
{$ENDIF}
{$IFDEF OS_WIN32}
  {$DEFINE WindowsPlatform}
{$ENDIF}
{$IFDEF OS_WIN64}
  {$DEFINE WindowsPlatform}
{$ENDIF}



{                                                                              }
{ CPU type                                                                     }
{                                                                              }
{$IFNDEF ManagedCode}
  {$DEFINE NativeCode}
{$ENDIF}
{$IFDEF CPU386}
  {$DEFINE INTEL386}
  {$DEFINE CPU_INTEL386}
{$ENDIF}
{$IFDEF CPUX64}
  {$DEFINE CPU_X86_64}
{$ENDIF}
{$IFDEF CPU86_64}
  {$DEFINE CPU_X86_64}
{$ENDIF}
{$IFDEF CPU68K}
  {$DEFINE CPU_68K}
{$ENDIF}
{$IFDEF CPUPPC}
  {$DEFINE CPU_POWERPC}
{$ENDIF}
{$IFDEF CPUPPC64}
  {$DEFINE CPU_POWERPC64}
{$ENDIF}
{$IFDEF CPUARM}
  {$DEFINE CPU_ARM}
{$ENDIF}



{                                                                              }
{ Assembler style                                                              }
{                                                                              }
{$IFNDEF PurePascal}
  {$IFNDEF ManagedCode}
    {$IFDEF CPU_X86_64}
      {$DEFINE ASMX86_64}
    {$ENDIF}
    {$IFDEF CPU_INTEL386}
      {$DEFINE ASM386}
      {$IFDEF DELPHI}{$IFDEF OS_WIN32}
        {$DEFINE ASM386_DELPHI}
        {$IFNDEF UseInline} {$DEFINE ASM386_DELPHI_INLINE_OFF} {$ENDIF}
      {$ENDIF}{$ENDIF}
      {$IFDEF FREEPASCAL2_UP}
        {$DEFINE ASM386_FREEPASCAL}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Function inlining                                                            }
{                                                                              }
{$IFDEF SupportInline}
  {$IFNDEF SupportInlineIsBuggy}
  {$IFNDEF PurePascal}
    {$DEFINE UseInline}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Standard compiler directives                                                 }
{                                                                              }
{$EXTENDEDSYNTAX ON}
{$IOCHECKS ON}
{$LONGSTRINGS ON}
{$BOOLEVAL OFF}
{$WRITEABLECONST OFF}
{$MINENUMSIZE 1}
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
{$IFDEF CLR}
  {$UNSAFECODE OFF}
{$ENDIF}
{$IFDEF DELPHI}
  {$OPTIMIZATION ON}
{$ENDIF}
{$IFDEF DELPHI2005_UP}
  {$INLINE ON}
{$ENDIF}
{$IFDEF DELPHI2009_UP}
  {$HIGHCHARUNICODE OFF}
{$ENDIF}



{                                                                              }
{ Compiler warnings                                                            }
{                                                                              }
{$IFDEF DELPHI7}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFDEF DELPHI2007}
{$IFNDEF DOT_NET}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}
{$ENDIF}

{$IFDEF DOT_NET}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

{$IFNDEF DEBUG}
  {$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM OFF}
    {$WARN UNIT_PLATFORM OFF}
    {$WARN UNIT_DEPRECATED OFF}
  {$ENDIF}
{$ENDIF}

