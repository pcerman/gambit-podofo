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

#include <podofo/podofo.h>

extern "C"
{
  #include "ffi-pdf.h"
}


PDF_PTR * new_pdf_ptr(int, const void *, PDF_PTR *);

PoDoFo::PdfVecObjects * find_cos_owner(PDF_COBJ *);

char * set_name(const char *);
char * set_reference(unsigned, unsigned);
char * set_string(const char *);

int get_hex_digit(char);
