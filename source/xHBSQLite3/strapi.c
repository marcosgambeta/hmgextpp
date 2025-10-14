//
// $Id: strapi.c 15018 2010-07-06 19:02:19Z vszakats $
//

//
// Harbour Project source code:
//    string API functions
//
// Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
// www - http://harbour-project.org
//

// $BEGIN_LICENSE$
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this software; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
// Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
//
// As a special exception, the Harbour Project gives permission for
// additional uses of the text contained in its release of Harbour.
//
// The exception is that, if you link the Harbour libraries with other
// files to produce an executable, this does not by itself cause the
// resulting executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of
// linking the Harbour library code into it.
//
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.
//
// This exception applies only to the code released by the Harbour
// Project under the name Harbour.  If you copy code from other
// Harbour Project or Free Software Foundation releases into a copy of
// Harbour, as the General Public License permits, the exception does
// not apply to the code that you add in this way.  To avoid misleading
// anyone as to the status of such modified files, you must delete
// this exception notice from them.
//
// If you write modifications of your own for Harbour, it is your choice
// whether to permit this exception to apply to your modifications.
// If you do not wish that, delete this exception notice.
// $END_LICENSE$

// Used parts from Harbour for use with XHarbour SQLite3 lib
// November 18, 2011 by R.Visscher <richard@irvis.com>

#include <hbvmopt.hpp>
#include <hbapistr.hpp>
#include <hbapiitm.hpp>
#include <hbstack.hpp>

/*----------------------------------------------------------------------------------
 String functions
----------------------------------------------------------------------------------*/
static const wchar_t s_szConstStr[1] = {0};

const char *hb_strnull(const char *str)
{
  HB_TRACE(HB_TR_DEBUG, ("hb_strnull(%p)", str));

  return str ? str : "";
}

void hb_retstrlen_utf8(const char *szText, ULONG nLen)
{
  HB_THREAD_STUB

  hb_itemPutStrLenUTF8(hb_stackReturnItem(), szText, nLen);
}

const char *hb_parstr_utf8(int iParam, void **phString, ULONG *pnLen)
{
  HB_THREAD_STUB

  HB_TRACE(HB_TR_DEBUG, ("hb_parstr_utf8(%d,%p,%p)", iParam, phString, pnLen));

  if (iParam >= -1 && iParam <= hb_pcount()) {
    PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

    if (HB_IS_BYREF(pItem))
      pItem = hb_itemUnRef(pItem);

    return hb_itemGetStrUTF8(pItem, phString, pnLen);
  }

  if (pnLen)
    *pnLen = 0;
  *phString = NULL;

  return NULL;
}

void hb_strfree(void *hString)
{
  HB_TRACE(HB_TR_DEBUG, ("hb_strfree(%p)", hString));

  if (hString && hString != static_cast<void *>(s_szConstStr))
    hb_xRefFree(hString);
}

void hb_retstr_utf8(const char *szText)
{
  HB_THREAD_STUB

  HB_TRACE(HB_TR_DEBUG, ("hb_retstr_utf8(%s)", szText));

  hb_itemPutStrLenUTF8(hb_stackReturnItem(), szText, szText ? strlen(szText) : 0);
}

PHB_ITEM hb_itemPutStrLenUTF8(PHB_ITEM pItem, const char *pStr, ULONG nLen)
{
  PHB_CODEPAGE cdp;
  ULONG nDest;

  HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStrLenUTF8(%p,%p,%" HB_PFS "u)", pItem, pStr, nLen));

  if (nLen == 0)
    return hb_itemPutC(pItem, NULL);

  cdp = hb_cdppage();
  nDest = hb_cdpStringInUTF8Length(cdp, FALSE, pStr, nLen);
  auto pszDest = static_cast<char *>(hb_xgrab(nDest + 1));
  hb_cdpStrnToUTF8n(cdp, FALSE, pStr, nLen, pszDest, nDest + 1);

  return hb_itemPutCLPtr(pItem, pszDest, nDest);
}

const char *hb_itemGetStrUTF8(PHB_ITEM pItem, void **phString, ULONG *pnLen)
{
  HB_TRACE(HB_TR_DEBUG, ("hb_itemGetStrUTF8(%p,%p,%p)", pItem, phString, pnLen));

  if (pItem && HB_IS_STRING(pItem)) {
    PHB_CODEPAGE cdp = hb_cdppage();

    ULONG nLen = hb_cdpStringInUTF8Length(cdp, FALSE, pItem->item.asString.value, pItem->item.asString.length);
    if (pnLen)
      *pnLen = nLen;

    if (nLen != pItem->item.asString.length) {
      auto pszUtf8 = static_cast<char *>(hb_xgrab(nLen + 1));
      hb_cdpStrnToUTF8n(cdp, FALSE, pItem->item.asString.value, pItem->item.asString.length, pszUtf8, nLen + 1);
      *phString = static_cast<void *>(pszUtf8);
      return pszUtf8;
    }

    if (pItem->item.asString.allocated != 0) {
      *phString = static_cast<void *>(pItem->item.asString.value);
      hb_xRefInc(pItem->item.asString.value);
    }
    else
      *phString = static_cast<void *>(s_szConstStr);
    return pItem->item.asString.value;
  }

  if (pnLen)
    *pnLen = 0;
  *phString = NULL;

  return NULL;
}

PHB_ITEM hb_itemPutStrUTF8(PHB_ITEM pItem, const char *pStr)
{
  PHB_CODEPAGE cdp;
  ULONG nDest, nLen;

  HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStrUTF8(%p,%p)", pItem, pStr));

  if (pStr == NULL)
    return hb_itemPutC(pItem, NULL);

  cdp = hb_cdppage();
  nLen = strlen(pStr);
  nDest = hb_cdpStringInUTF8Length(cdp, FALSE, pStr, nLen);
  auto pszDest = static_cast<char *>(hb_xgrab(nDest + 1));
  hb_cdpStrnToUTF8n(cdp, FALSE, pStr, nLen, pszDest, nDest + 1);

  return hb_itemPutCLPtr(pItem, pszDest, nDest);
}

BOOL hb_arraySetStrUTF8(PHB_ITEM pArray, ULONG nIndex, const char *pStr)
{
  HB_TRACE(HB_TR_DEBUG, ("hb_arraySetStrUTF8(%p, %lu, %p)", pArray, nIndex, pStr));

  if (HB_IS_ARRAY(pArray) && nIndex > 0 && nIndex <= pArray->item.asArray.value->ulLen) {
    hb_itemPutStrUTF8(pArray->item.asArray.value->pItems + nIndex - 1, pStr);
    return TRUE;
  }
  else
    return FALSE;
}