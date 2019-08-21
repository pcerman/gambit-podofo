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

#include "utils.h"

extern "C"
{
  #include "ffi-pdf.h"
}

// ----- Document ------------------------------------------------------

PDF_MDOC * ffi_create_mem_doc()
{
  PoDoFo::PdfMemDocument * document = nullptr;

  try
  {
    document = new PoDoFo::PdfMemDocument();
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    document = nullptr;
  }

  return new_pdf_ptr(PDF_TYPE_MEM_DOC, document, nullptr);
}

char * ffi_doc_version(PDF_MDOC * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  int version = -1;

  try
  {
    auto mdoc = (PoDoFo::PdfMemDocument *)pdf->ptr;
    version = mdoc->GetPdfVersion();
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    version = -1;
  }

  switch (version)
  {
    case PoDoFo::ePdfVersion_1_0: return set_string("1.0");
    case PoDoFo::ePdfVersion_1_1: return set_string("1.1");
    case PoDoFo::ePdfVersion_1_2: return set_string("1.2");
    case PoDoFo::ePdfVersion_1_3: return set_string("1.3");
    case PoDoFo::ePdfVersion_1_4: return set_string("1.4");
    case PoDoFo::ePdfVersion_1_5: return set_string("1.5");
    case PoDoFo::ePdfVersion_1_6: return set_string("1.6");
    case PoDoFo::ePdfVersion_1_7: return set_string("1.7");
  }

  return nullptr;
}

int ffi_doc_set_version(PDF_MDOC * pdf, const char * version)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto doc = (PoDoFo::PdfMemDocument *)pdf->ptr;

  PoDoFo::EPdfVersion pdfver;

  if (strcmp(version, "1.0") == 0)
    pdfver = PoDoFo::ePdfVersion_1_0;
  else if (strcmp(version, "1.1") == 0)
    pdfver = PoDoFo::ePdfVersion_1_1;
  else if (strcmp(version, "1.2") == 0)
    pdfver = PoDoFo::ePdfVersion_1_2;
  else if (strcmp(version, "1.3") == 0)
    pdfver = PoDoFo::ePdfVersion_1_3;
  else if (strcmp(version, "1.4") == 0)
    pdfver = PoDoFo::ePdfVersion_1_4;
  else if (strcmp(version, "1.5") == 0)
    pdfver = PoDoFo::ePdfVersion_1_5;
  else if (strcmp(version, "1.6") == 0)
    pdfver = PoDoFo::ePdfVersion_1_6;
  else if (strcmp(version, "1.7") == 0)
    pdfver = PoDoFo::ePdfVersion_1_7;
  else
    return false;

  doc->SetPdfVersion(pdfver);

  return true;
}

int ffi_doc_load(PDF_MDOC * pdf, const char * filename)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto doc = (PoDoFo::PdfMemDocument *)pdf->ptr;

  bool result = true;

  try
  {
    doc->Load(filename);
  }
  catch (PoDoFo::PdfError & err)
  {
    if (err != PoDoFo::ePdfError_FileNotFound)
      err.PrintErrorMsg();
    result = false;
  }

  return result;
}

int ffi_doc_write(PDF_MDOC * pdf, const char * filename)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto doc = (PoDoFo::PdfMemDocument *)pdf->ptr;

  bool result = true;

  try
  {
    doc->Write(filename);
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    result = false;
  }

  return result;
}

PDF_COBJ * ffi_doc_catalog(PDF_MDOC * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  auto doc = (PoDoFo::PdfMemDocument *)pdf->ptr;

  PoDoFo::PdfObject * cos = nullptr;

  try
  {
    cos = doc->GetCatalog();
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    cos = nullptr;
  }

  return new_pdf_ptr(PDF_TYPE_COBJ, cos, pdf);
}

PDF_COBJ * ffi_doc_trailer(PDF_MDOC * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  auto doc = (PoDoFo::PdfMemDocument *)pdf->ptr;

  const PoDoFo::PdfObject * cos = nullptr;

  try
  {
    cos = doc->GetTrailer();
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    cos = nullptr;
  }

  return new_pdf_ptr(PDF_TYPE_COBJ, cos, pdf);
}

PDF_COBJ * ffi_doc_struct_tree_root(PDF_MDOC * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return nullptr;

  auto doc = (PoDoFo::PdfMemDocument *)pdf->ptr;

  PoDoFo::PdfObject * cos = nullptr;

  try
  {
    cos = doc->GetStructTreeRoot();
  }
  catch (PoDoFo::PdfError & err)
  {
    err.PrintErrorMsg();
    cos = nullptr;
  }

  return new_pdf_ptr(PDF_TYPE_COBJ, cos, pdf);
}
