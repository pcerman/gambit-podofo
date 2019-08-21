/***********************************************************************
 * Copyright (c) 2019 Peter Cerman (https://github.com/pcerman)        *
 *                                                                     *
 * gambit-podofo: This is gambit scheme (http://gambitscheme.org)      *
 * binding to the podofo library (http://podofo.sourceforge.net).      *
 *                                                                     *
 * Original source code if this library is published on github         *
 * resository (https://github.com/pcerman/gambit-podofo).              *
 *                                                                     *
 * It is released under GNU General Public License v3.0                *
 ***********************************************************************/

#pragma once

int gReleaseCount = 0;

___SCMOBJ ffi_ptr_release(void * ptr)
{
  if (ptr != NULL)
  {
    delete_pdf_ptr(ptr);
    gReleaseCount++;
  }

  return ___FIX(___NO_ERR);
}
