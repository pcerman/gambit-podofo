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

#include <cstdlib>
#include <podofo/podofo.h>

#include "utils.h"

extern "C"
{
  #include "ffi-pdf.h"
}

// ----- Object --------------------------------------------------------

int ffi_obj_is_bool(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;

  return cos->IsBool();
}

int ffi_obj_is_number(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;

  return cos->IsNumber();
}

int ffi_obj_is_real(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;

  return cos->IsReal();
}

int ffi_obj_is_string(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;

  return cos->IsString();
}

int ffi_obj_is_hexstring(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;

  return cos->IsHexString();
}

int ffi_obj_is_name(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;

  return cos->IsName();
}

int ffi_obj_is_array(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;

  return cos->IsArray();
}

int ffi_obj_is_dictionary(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;

  return cos->IsDictionary();
}

int ffi_obj_is_rawdata(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;

  return cos->IsRawData();
}

int ffi_obj_is_null(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;

  return cos->IsNull();
}

int ffi_obj_is_reference(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;

  return cos->IsReference();
}

int ffi_obj_has_stream(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;

  bool result = false;

  try
  {
    result = cos->HasStream();
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    result = false;
  }

  return result;
}

int ffi_obj_bool(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsBool())
    return false;

  int result = 0;

  try
  {
    result = cos->GetBool();
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    result = 0;
  }

  return result;
}

long long ffi_obj_number(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return 0;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsNumber())
    return 0;

  int64_t result = 0;

  try
  {
    result = cos->GetNumber();
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    result = 0;
  }

  return result;
}

double ffi_obj_real(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return 0;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsReal())
    return 0;

  double result = 0;

  try
  {
    result = cos->GetReal();
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    result = 0;
  }

  return result;
}

char * ffi_obj_string(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsString())
    return nullptr;

  char * str = nullptr;

  try
  {
    auto & string = cos->GetString();

    if (string.IsValid() && !string.IsHex())
    {
      auto & u8string = string.GetStringUtf8();

      str = (char *)malloc(u8string.size() + 1);
      str = strcpy(str, u8string.c_str());
    }
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    if (str != nullptr)
      free(str);

    str = nullptr;
  }

  return str;
}

char to_hex(uint8_t b)
{
  if (b < 10)
    return '0' + b;
  if (b < 16)
    return 'A' + (b - 10);

  return 'X';
}

char * ffi_obj_hexstring(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsHexString())
    return nullptr;

  char * str = nullptr;

  try
  {
    auto & string = cos->GetString();

    if (string.IsValid() && string.IsHex())
    {
      const char * _str = string.GetString();
      int len = string.GetLength();

      str = (char *)malloc(2 * len + 3);

      char * ptr = str;
      *ptr++ = '<';

      for (int i=0; i < len; i++)
      {
        auto ch = *_str++;
        *ptr++ = to_hex((ch >> 4) & 0x0f);
        *ptr++ = to_hex(ch & 0x0f);
      }
      *ptr++ = '>';
      *ptr = '\0';
    }
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    if (str != nullptr)
      free(str);

    str = nullptr;
  }

  return str;
}

char * ffi_obj_name(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsName())
    return nullptr;

  const PoDoFo::PdfName * name = nullptr;

  try
  {
    name = &cos->GetName();
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    return nullptr;
  }

  return set_name(name->GetName().c_str());
}

// ----- Reference -----------------------------------------------------

char * ffi_ref_id(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;

  const PoDoFo::PdfReference * ref = nullptr;

  try
  {
    if (!cos->IsReference())
    {
      ref = &cos->Reference();
      if (!ref->IsIndirect())
        ref = nullptr;
    }
    else
    {
      ref = &cos->GetReference();
    }
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    ref = nullptr;
  }

  if (ref == nullptr)
    return nullptr;

  return set_reference(ref->ObjectNumber(), ref->GenerationNumber());
}

PDF_COBJ * ffi_ref_indirect(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsReference())
    return nullptr;

  auto owner = find_cos_owner(pdf);
  if (owner == nullptr)
    return nullptr;

  PoDoFo::PdfObject * obj = nullptr;

  try
  {
    obj = owner->GetObject( cos->GetReference() );
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    obj = nullptr;
  }

  return new_pdf_ptr(PDF_TYPE_COBJ, obj, pdf);
}
