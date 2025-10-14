//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//

// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// this software; see the file COPYING. If not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
// visit the web site http://www.gnu.org/).
//
// As a special exception, you have permission for additional uses of the text
// contained in this release of Harbour Minigui.
//
// The exception is that, if you link the Harbour Minigui library with other
// files to produce an executable, this does not by itself cause the resulting
// executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of linking the
// Harbour-Minigui library code into it.

// Parts of this project are based upon:
//
// "Harbour GUI framework for Win32"
// Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
// Copyright 2001 Antonio Linares <alinares@fivetech.com>
// www - https://harbour.github.io/
//
// "Harbour Project"
// Copyright 1999-2022, https://harbour.github.io/
//
// "WHAT32"
// Copyright 2002 AJ Wos <andrwos@aust1.net>
//
// "HWGUI"
// Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

// File:        MyCrypt.c
// Author:      Grigory Filatov
// Description: Crypto Library for MiniGUI
// Status:      Public Domain
// Notes:       This is very simple crypt algorithm based on XOR encryption.

#if !defined(__MINGW32__) // TODO: if the compiler is GCC/MinGW... ?

#include <hbapi.hpp>

HB_FUNC(HMG_CHARXOR)
{
  unsigned int nl1, nl2;

  auto Str1 = const_cast<char *>(hb_parc(1));
  auto len1 = static_cast<unsigned int>(hb_parclen(1));
  auto Str2 = const_cast<char *>(hb_parc(2));
  auto len2 = static_cast<unsigned int>(hb_parclen(2));
  if (!len1) {
    hb_retclen("", 0);
  } else {
    auto Res = static_cast<char *>(hb_xgrab(len1));
    for (nl1 = nl2 = 0; nl1 < len1; nl1++)
    {
      Res[nl1] = Str1[nl1] ^ Str2[nl2];
      if ((++nl2) >= len2) {
        nl2 = 0;
      }
    }

    hb_retclen(Res, len1);
    hb_xfree(Res);
  }
}

#ifndef HMG_NO_DEPRECATED_FUNCTIONS
HB_FUNC_TRANSLATE(CHARXOR, HMG_CHARXOR)
#endif

#endif
