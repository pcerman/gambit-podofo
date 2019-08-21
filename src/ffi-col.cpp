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

#include <iterator>
#include <podofo/podofo.h>

#include "stream.h"
#include "utils.h"

extern "C"
{
  #include "ffi-pdf.h"
}

// ----- Array ---------------------------------------------------------

int ffi_arr_size(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return 0;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsArray())
    return 0;

  int size = 0;

  try
  {
    PoDoFo::PdfArray & array = cos->GetArray();
    size = array.GetSize();
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    size = 0;
  }

  return size;
}

PDF_COBJ * ffi_arr_value(PDF_COBJ * pdf, int idx)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (idx < 0 || !cos->IsArray())
    return nullptr;

  PoDoFo::PdfObject * val = nullptr;

  try
  {
    PoDoFo::PdfArray & array = cos->GetArray();
    if (idx < array.size())
    {
      val = &array[idx];
    }
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    val = nullptr;
  }

  return new_pdf_ptr(PDF_TYPE_COBJ, val, pdf);
}

PDF_COBJ * ffi_arr_indirect(PDF_COBJ * pdf, int idx)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (idx < 0 || !cos->IsArray())
    return nullptr;

  PoDoFo::PdfObject * val = nullptr;

  try
  {
    PoDoFo::PdfArray & array = cos->GetArray();
    if (idx < array.GetSize())
    {
      PoDoFo::PdfObject & aval = array[idx];
      if (aval.IsReference())
      {
        PoDoFo::PdfVecObjects * owner = find_cos_owner(pdf);
        if (owner != nullptr)
          val = owner->GetObject( aval.GetReference() );
      }
      else
      {
        val = &aval;
      }
    }
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    val = nullptr;
  }

  return new_pdf_ptr(PDF_TYPE_COBJ, val, pdf);
}

int ffi_arr_resize(PDF_COBJ * pdf, int size)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsArray() || size < 0)
    return false;

  bool res = false;

  try
  {
    PoDoFo::PdfArray & array = cos->GetArray();
    array.resize(size, PoDoFo::PdfObject(PoDoFo::PdfVariant()));

    res = true;
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    res = false;
  }

  return res;
}

int ffi_arr_remove(PDF_COBJ * pdf, int idx)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsArray() || idx < 0)
    return false;

  bool res = false;

  try
  {
    PoDoFo::PdfArray & array = cos->GetArray();
    if (idx < array.size())
    {
      array.erase(array.begin() + idx);
      res = true;
    }
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    res = false;
  }

  return res;
}

int ffi_arr_insert(PDF_COBJ * pdf, int idx)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsArray())
    return false;

  bool res = false;

  try
  {
    PoDoFo::PdfArray & array = cos->GetArray();
    if (idx < 0 || idx >= array.size())
    {
      array.push_back(PoDoFo::PdfObject(PoDoFo::PdfVariant()));
      res = true;
    }
    else
    {
      array.insert(array.begin() + idx, PoDoFo::PdfObject(PoDoFo::PdfVariant()));
      res = true;
    }
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    res = false;
  }

  return res;
}

PDF_COBJ * arr_set_value(PDF_COBJ * pdf, int idx, const PoDoFo::PdfVariant & variant, int indirect)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsArray())
    return nullptr;

  const PoDoFo::PdfObject * val = nullptr;

  try
  {
    auto & arr = cos->GetArray();

    if (indirect)
    {
      PoDoFo::PdfVecObjects * owner = find_cos_owner(pdf);

      if (owner != nullptr)
      {
        val = owner->CreateObject(variant);
        if (val != nullptr)
        {
          if (idx < 0 || idx >= arr.size())
            arr.push_back(val->Reference());
          else
            arr[idx] = val->Reference();
        }
      }
    }
    else if (idx < 0 || idx >= arr.size())
    {
      arr.push_back(variant);
      val = &arr.back();
    }
    else
    {
      arr[idx] = variant;
      val = &arr[idx];
    }
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    val = nullptr;
  }

  return new_pdf_ptr(PDF_TYPE_COBJ, val, pdf);
}

PDF_COBJ * ffi_arr_set_bool(PDF_COBJ * pdf, int idx, int value, int indirect)
{
  return arr_set_value(pdf, idx, PoDoFo::PdfVariant(value != 0), indirect);
}

PDF_COBJ * ffi_arr_set_number(PDF_COBJ * pdf, int idx, long long value, int indirect)
{
  return arr_set_value(pdf, idx, PoDoFo::PdfVariant((PoDoFo::pdf_int64)value), indirect);
}

PDF_COBJ * ffi_arr_set_real(PDF_COBJ * pdf, int idx, double value, int indirect)
{
  return arr_set_value(pdf, idx, PoDoFo::PdfVariant(value), indirect);
}

PDF_COBJ * ffi_arr_set_string(PDF_COBJ * pdf, int idx, const char * value, int indirect)
{
  if (value == nullptr)
    return nullptr;

  const PoDoFo::PdfString str((PoDoFo::pdf_utf8 *)value);
  return arr_set_value(pdf, idx, PoDoFo::PdfVariant(str), indirect);
}

PDF_COBJ * ffi_arr_set_hexstring(PDF_COBJ * pdf, int idx, const char * value, int indirect)
{
  if (value == nullptr)
    return nullptr;

  int len = strlen(value);
  if (value[0] == '<')
  {
    value++;
    len--;
  }
  if (value[len - 1] == '>')
  {
    len--;
  }

  if (len < 2)
    return nullptr;

  for (int i=1; i < len; i += 2)
  {
    int d1 = get_hex_digit(value[i-1]);
    int d2 = get_hex_digit(value[i]);
    if (d1 < 0 || d2 < 0)
      return nullptr;

    ((char *)value)[i / 2] = (char)(16 * d1 + d2);
  }

  const PoDoFo::PdfString str(value, len / 2, true);
  return arr_set_value(pdf, idx, PoDoFo::PdfVariant(str), indirect);
}

PDF_COBJ * ffi_arr_set_name(PDF_COBJ * pdf, int idx, const char * value, int indirect)
{
  if (value == nullptr)
    return nullptr;

  if (*value == '/')
    value++;

  const PoDoFo::PdfName val_name(value);
  return arr_set_value(pdf, idx, PoDoFo::PdfVariant(val_name), indirect);
}

PDF_COBJ * ffi_arr_set_null(PDF_COBJ * pdf, int idx)
{
  return arr_set_value(pdf, idx, PoDoFo::PdfVariant(), false);
}

PDF_COBJ * ffi_arr_set_dictionary(PDF_COBJ * pdf, int idx, int indirect)
{
  return arr_set_value(pdf, idx, PoDoFo::PdfVariant(PoDoFo::PdfDictionary()), indirect);
}

PDF_COBJ * ffi_arr_set_array(PDF_COBJ * pdf, int idx, int indirect)
{
  return arr_set_value(pdf, idx, PoDoFo::PdfVariant(PoDoFo::PdfArray()), indirect);
}

int ffi_arr_set_reference(PDF_COBJ * pdf, int idx, PDF_COBJ * pdf_ref)
{
  if (!is_pdf_ptr_valid(pdf_ref))
    return false;

  auto cos_ref = (PoDoFo::PdfObject *)pdf_ref->ptr;

  const PoDoFo::PdfReference * ref = nullptr;
  if (cos_ref->IsReference())
    ref = &cos_ref->GetReference();
  else if (cos_ref->Reference().IsIndirect())
    ref = &cos_ref->Reference();

  if (ref == nullptr)
    return false;
  
  return arr_set_value(pdf, idx, PoDoFo::PdfVariant(*ref), false) ? true : false;
}

// ----- Dictionary ----------------------------------------------------

int ffi_dic_size(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return 0;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsDictionary())
    return 0;

  int size = 0;

  try
  {
    auto & dic = cos->GetDictionary();
    auto & map = dic.GetKeys();
    size = map.size();
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    size = 0;
  }

  return size;
}

char * ffi_dic_key(PDF_COBJ * pdf, int idx)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  if (idx < 0)
    return nullptr;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsDictionary())
    return nullptr;

  char * name = nullptr;

  try
  {
    auto & dic = cos->GetDictionary();
    auto & map = dic.GetKeys();

    if (idx < map.size())
    {
      auto it = map.begin();
      std::advance(it, idx);

      if (it != map.end())
      {
        const PoDoFo::PdfName & pdf_name = it->first;
        name = set_name(pdf_name.GetName().c_str());
      }
    }
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    name = nullptr;
  }

  return name;
}

PDF_COBJ * ffi_dic_value(PDF_COBJ * pdf, const char * name)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  if (name == nullptr)
    return nullptr;

  if (*name == '/')
    name++;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsDictionary())
    return nullptr;

  const PoDoFo::PdfObject * val = nullptr;

  try
  {
    auto & dic = cos->GetDictionary();
    val = dic.GetKey( PoDoFo::PdfName(name) );
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    val = nullptr;
  }

  return new_pdf_ptr(PDF_TYPE_COBJ, val, pdf);
}

PDF_COBJ * ffi_dic_indirect(PDF_COBJ * pdf, const char * name)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  if (name == nullptr)
    return nullptr;

  if (*name == '/')
    name++;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsDictionary())
    return nullptr;

  const PoDoFo::PdfObject * val = nullptr;

  try
  {
    val = cos->GetIndirectKey( PoDoFo::PdfName(name) );
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    val = nullptr;
  }

  return new_pdf_ptr(PDF_TYPE_COBJ, val, pdf);
}

int ffi_dic_remove(PDF_COBJ * pdf, const char * name)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  if (name == nullptr)
    return false;

  if (*name == '/')
    name++;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsDictionary())
    return false;

  bool res = false;

  try
  {
    auto & dic = cos->GetDictionary();
    res = dic.RemoveKey( PoDoFo::PdfName(name) );
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    res = false;
  }

  return res;
}

PDF_COBJ * dic_set_value(PDF_COBJ * pdf, const char * name, const PoDoFo::PdfVariant & variant, int indirect)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  if (name == nullptr)
    return nullptr;

  if (*name == '/')
    name++;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsDictionary())
    return nullptr;

  const PoDoFo::PdfObject * val = nullptr;

  try
  {
    auto & dic = cos->GetDictionary();

    if (indirect)
    {
      PoDoFo::PdfVecObjects * owner = find_cos_owner(pdf);

      if (owner != nullptr)
      {
        val = owner->CreateObject(variant);
        if (val != nullptr)
        {
          dic.AddKey(PoDoFo::PdfName(name), val->Reference());
        }
      }
    }
    else
    {
      dic.AddKey(PoDoFo::PdfName(name), PoDoFo::PdfObject(variant));

      val = dic.GetKey(PoDoFo::PdfName(name));
    }
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    val = nullptr;
  }

  return new_pdf_ptr(PDF_TYPE_COBJ, val, pdf);
}

PDF_COBJ * ffi_dic_set_bool(PDF_COBJ * pdf, const char * name, int value, int indirect)
{
  return dic_set_value(pdf, name, PoDoFo::PdfVariant(value != 0), indirect);
}

PDF_COBJ * ffi_dic_set_number(PDF_COBJ * pdf, const char * name, long long value, int indirect)
{
  return dic_set_value(pdf, name, PoDoFo::PdfVariant((PoDoFo::pdf_int64)value), indirect);
}

PDF_COBJ * ffi_dic_set_real(PDF_COBJ * pdf, const char * name, double value, int indirect)
{
  return dic_set_value(pdf, name, PoDoFo::PdfVariant(value), indirect);
}

PDF_COBJ * ffi_dic_set_string(PDF_COBJ * pdf, const char * name, const char * value, int indirect)
{
  if (value == nullptr)
    return nullptr;

  const PoDoFo::PdfString str((PoDoFo::pdf_utf8 *)value);
  return dic_set_value(pdf, name, PoDoFo::PdfVariant(str), indirect);
}

PDF_COBJ * ffi_dic_set_hexstring(PDF_COBJ * pdf, const char * name, const char * value, int indirect)
{
  if (value == nullptr)
    return nullptr;

  int len = strlen(value);
  if (value[0] == '<')
  {
    value++;
    len--;
  }
  if (value[len - 1] == '>')
  {
    len--;
  }

  if (len < 2)
    return nullptr;

  for (int i=1; i < len; i += 2)
  {
    int d1 = get_hex_digit(value[i-1]);
    int d2 = get_hex_digit(value[i]);
    if (d1 < 0 || d2 < 0)
      return nullptr;

    ((char *)value)[i / 2] = (char)(16 * d1 + d2);
  }

  const PoDoFo::PdfString str(value, len / 2, true);
  return dic_set_value(pdf, name, PoDoFo::PdfVariant(str), indirect);
}

PDF_COBJ * ffi_dic_set_name(PDF_COBJ * pdf, const char * name, const char * value, int indirect)
{
  if (value == nullptr)
    return nullptr;

  if (*value == '/')
    value++;

  const PoDoFo::PdfName val_name(value);
  return dic_set_value(pdf, name, PoDoFo::PdfVariant(val_name), indirect);
}

PDF_COBJ * ffi_dic_set_null(PDF_COBJ * pdf, const char * name)
{
  return dic_set_value(pdf, name, PoDoFo::PdfVariant(), false);
}

PDF_COBJ * ffi_dic_set_dictionary(PDF_COBJ * pdf, const char * name, int indirect)
{
  return dic_set_value(pdf, name, PoDoFo::PdfVariant(PoDoFo::PdfDictionary()), indirect);
}

PDF_COBJ * ffi_dic_set_array(PDF_COBJ * pdf, const char * name, int indirect)
{
  return dic_set_value(pdf, name, PoDoFo::PdfVariant(PoDoFo::PdfArray()), indirect);
}

int ffi_dic_set_reference(PDF_COBJ * pdf, const char * name, PDF_COBJ * pdf_ref)
{
  if (!is_pdf_ptr_valid(pdf_ref))
    return false;

  auto cos_ref = (PoDoFo::PdfObject *)pdf_ref->ptr;

  const PoDoFo::PdfReference * ref = nullptr;
  if (cos_ref->IsReference())
    ref = &cos_ref->GetReference();
  else if (cos_ref->Reference().IsIndirect())
    ref = &cos_ref->Reference();

  if (ref == nullptr)
    return false;
  
  return dic_set_value(pdf, name, PoDoFo::PdfVariant(*ref), false) ? true : false;
}

PDF_STREAM * ffi_dic_stream(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->HasStream())
    return nullptr;

  char * buffer = nullptr;
  PoDoFo::pdf_long size = 0;

  try
  {
    PoDoFo::PdfStream * stream =  cos->GetStream();
    stream->GetCopy(&buffer, &size);
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    buffer = nullptr;
  }

  if (buffer == nullptr)
    return nullptr;

  return new_pdf_ptr(PDF_TYPE_STREAM, new Stream(buffer, size), nullptr);
}

PDF_STREAM * ffi_dic_filtered_stream(PDF_COBJ * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->HasStream())
    return nullptr;

  char * buffer = nullptr;
  PoDoFo::pdf_long size = 0;

  try
  {
    PoDoFo::PdfStream * stream =  cos->GetStream();
    stream->GetFilteredCopy(&buffer, &size);
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    buffer = nullptr;
  }

  if (buffer == nullptr)
    return nullptr;

  return new_pdf_ptr(PDF_TYPE_STREAM, new Stream(buffer, size), nullptr);
}

int ffi_dic_set_stream(PDF_COBJ * pdf, PDF_STREAM * pdf_stream)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto cos = (PoDoFo::PdfObject *)pdf->ptr;
  if (!cos->IsDictionary())
    return false;

  if (!is_pdf_ptr_valid(pdf_stream))
    return false;

  auto stream = (Stream *)pdf_stream->ptr;
  if (stream->Size() <= 0)
    return false;

  bool res = false;

  try
  {
    PoDoFo::PdfMemoryInputStream input_stream((char *)stream->Data(), stream->Size());

    auto cos_stream = cos->GetStream();
    if (cos_stream != nullptr)
    {
      if (cos_stream->IsAppending())
        cos_stream->EndAppend();

      cos_stream->SetRawData(&input_stream);
      res = true;
    }
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    res = false;
  }

  return res;
}
