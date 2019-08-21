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

#include <podofo/podofo.h>

#include "stream.h"
#include "utils.h"

extern "C"
{
  #include "ffi-pdf.h"
}


char gBuffer[129];

void ffi_init()
{
  static bool is_init = false;

  if (is_init)
    return;

  PoDoFo::PdfError::EnableDebug(false);
  PoDoFo::PdfError::EnableLogging(false);

  is_init = true;
}

char * podofo_version()
{
  strcpy(gBuffer, PODOFO_VERSION_STRING);

  return gBuffer;
}

char * ffi_version()
{
  sprintf(gBuffer, "%d.%d.%d", FFI_MAJOR_VERSION, FFI_MINOR_VERSION, FFI_RELEASE);

  return gBuffer;
}

int is_pdf_ptr_valid(PDF_PTR * pdf)
{
  if (pdf == nullptr || pdf->ptr == nullptr)
    return false;

  if (pdf->owner == nullptr)
    return true;

  return is_pdf_ptr_valid(pdf->owner);
}

PoDoFo::PdfVecObjects * find_cos_owner(PDF_COBJ * obj)
{
  if (obj == nullptr)
    return nullptr;

  //obj = obj->owner;

  while (obj != nullptr)
  {
    if (obj->ptr == nullptr)
      break;

    switch (obj->type)
    {
      case PDF_TYPE_COBJ:
      {
        PoDoFo::PdfObject * cos = (PoDoFo::PdfObject *)obj->ptr;
        try
        {
          PoDoFo::PdfVecObjects * owner = cos->GetOwner();
          if (owner != nullptr)
            return owner;
        }
        catch (PoDoFo::PdfError & err)
        {
          err.PrintErrorMsg();
        }

        break;
      }

      case PDF_TYPE_MEM_DOC:
      {
        PoDoFo::PdfDocument * doc = (PoDoFo::PdfDocument *)obj->ptr;
        PoDoFo::PdfVecObjects * owner = doc->GetObjects();
        if (owner != nullptr)
          return owner;
        break;
      }
    }

    obj = obj->owner;
  }

  return nullptr;
}

PDF_PTR * new_pdf_ptr(int type, const void * obj, PDF_PTR * owner)
{
  if (obj == nullptr)
    return nullptr;

  if (owner != nullptr)
    owner->count++;

  return new PDF_PTR { 1, type, (void *)obj, owner };
}

void delete_pdf_ptr(PDF_PTR * pdf)
{
  if (pdf == nullptr)
    return;

  pdf->count--;

  if (pdf->count > 0)
    return;

  ffi_dispose(pdf);

  auto owner = pdf->owner;

  pdf->ptr = nullptr;
  pdf->owner = nullptr;

  delete_pdf_ptr(owner);

  delete pdf;
}

char * set_name(const char * str)
{
  int len = strlen(str);

  gBuffer[0] = '/';

  if (len + 2 < sizeof(gBuffer))
    strcpy(gBuffer + 1, str);
  else
  {
    strncpy(gBuffer + 1, str, sizeof(gBuffer) - 2);
    gBuffer[sizeof(gBuffer) - 1] = '\0';
  }

  return gBuffer;
}

char * set_reference(unsigned obj_num, unsigned gen_num)
{
  sprintf(gBuffer, "R-%d-%d", obj_num, gen_num);

  return gBuffer;
}

char * set_string(const char * str)
{
  strncpy(gBuffer, str, sizeof(gBuffer) - 1);
  gBuffer[sizeof(gBuffer)-1] = '\0';

  return gBuffer;
}

int get_hex_digit(char ch)
{
  if (ch >= '0' && ch <= '9')
    return ch - '0';

  if (ch >= 'A' && ch <= 'F')
    return ch - ('A' - 10);

  if (ch >= 'a' && ch <= 'f')
    return ch - ('a' - 10);

  return -1;
}

// ----- ffi -----------------------------------------------------------

void ffi_dispose(PDF_PTR * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return;

  try
  {
    switch (pdf->type)
    {
      case PDF_TYPE_MEM_DOC:
        delete (PoDoFo::PdfMemDocument *)pdf->ptr;
        break;

      case PDF_TYPE_STREAM:
        delete (Stream *)pdf->ptr;
        break;
    }
  }
  catch (PoDoFo::PdfError err)
  {
    err.PrintErrorMsg();
  }

  pdf->ptr = nullptr;
}

char * ffi_type_of(PDF_PTR * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  const char * type = nullptr;

  switch(pdf->type)
  {
    case PDF_TYPE_NONE:       type = "<ffi-none>";       break;
    case PDF_TYPE_MEM_DOC:    type = "<ffi-mem-doc>";    break;
    case PDF_TYPE_COBJ:       type = "<ffi-cos>";        break;
    case PDF_TYPE_STREAM:     type = "<ffi-stream>";     break;

    default:                  type = "<ffi-unknown>";    break;
  }

  return (char *)type;
}

char * ffi_cos_type_of(PDF_PTR * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  if (pdf->type != PDF_TYPE_COBJ)
    return ffi_type_of(pdf);

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;

  const char * type = nullptr;

  switch (cos->GetDataType())
  {
    case PoDoFo::ePdfDataType_Bool:      type = "<ffi-cos-bool>";      break;
    case PoDoFo::ePdfDataType_Number:    type = "<ffi-cos-number>";    break;
    case PoDoFo::ePdfDataType_Real:      type = "<ffi-cos-real>";      break;
    case PoDoFo::ePdfDataType_String:    type = "<ffi-cos-string>";    break;
    case PoDoFo::ePdfDataType_HexString: type = "<ffi-cos-hexstring>"; break;
    case PoDoFo::ePdfDataType_Name:      type = "<ffi-cos-name>";      break;
    case PoDoFo::ePdfDataType_Array:     type = "<ffi-cos-array>";     break;
    case PoDoFo::ePdfDataType_Null:      type = "<ffi-cos-null>";      break;
    case PoDoFo::ePdfDataType_Reference: type = "<ffi-cos-reference>"; break;
    case PoDoFo::ePdfDataType_RawData:   type = "<ffi-cos-rawdata>";   break;

    case PoDoFo::ePdfDataType_Dictionary:
      type = cos->HasStream() ? "<ffi-cos-stream>"
                              : "<ffi-cos-dictionary>";
      break;

    default:                             type = "<ffi-cos-unknown>";   break;
  }

  return (char *)type;
}
