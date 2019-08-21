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

#include <cstring>
#include "stream.h"
#include "utils.h"

extern "C"
{
  #include "ffi-pdf.h"
}

// ----- Stream class --------------------------------------------------

Stream::Stream(void * dataA, int data_sizeA)
  : data { dataA }
  , capacity{ data_sizeA }
  , data_size{ data_sizeA }
  , position{}
{
  if (data_sizeA < 0)
    data = nullptr;

  if (data == nullptr)
  {
    capacity = 0;
    data_size = 0;
  }
}

Stream::~Stream()
{
  if (data != nullptr)
    PoDoFo::podofo_free(data);
}

int Stream::SetCapacity(int capacityA)
{
  if (capacityA <= 0)
  {
    Release();
  }
  else if (capacity != capacityA)
  {
    void * ptr = PoDoFo::podofo_realloc(data, capacityA);

    if (ptr != nullptr)
    {
      data = ptr;
      capacity = capacityA;

      if (data_size > capacity)
        data_size = capacity;

      if (position > data_size)
        position = data_size;
    }
  }

  return capacity;
}

int Stream::SetSize(int size)
{
  if (size <= 0)
  {
    data_size = 0;
    position = 0;
  }
  else if (size < data_size)
  {
    data_size = size;
    if (position > data_size)
      position = data_size;
  }

  return data_size;
}

int Stream::SetPosition(int positionA)
{
  position = positionA;
  if (position < 0)
  {
    position = data_size;
  }
  else if (position > data_size)
  {
    position = data_size;
  }

  return position;
}

int Stream::Read(void * buffer, int size)
{
  if (buffer == nullptr || size <= 0 || eof())
    return 0;

  if (size > Available())
    size = Available();

  memmove(buffer, (BYTE *)data + position, size);

  position += size;

  return size;
}

int Stream::Write(void * buffer, int size)
{
  if (buffer == nullptr || size <= 0)
    return 0;

  if (position + size > capacity)
  {
    SetCapacity(position + size + 1024);
  }

  if (position + size > capacity)
  {
    size = capacity - position;
    if (size <= 0)
      return 0;
  }

  memmove((BYTE *)data + position, buffer, size);

  position += size;
  if (position > data_size)
    data_size = position;

  return size;
}

void Stream::Release()
{
  if (data != nullptr)
  {
    PoDoFo::podofo_free(data);

    data = nullptr;
    capacity = 0;
    data_size = 0;
    position = 0;
  }
}

// ----- Stream --------------------------------------------------------

PDF_STREAM * ffi_create_stream(int size)
{
  if (size < 0)
    return nullptr;

  auto stream = new Stream(nullptr, 0);

  if (size > 0)
  {
    stream->SetCapacity(std::max(size, 128));
  }

  return new_pdf_ptr(PDF_TYPE_STREAM, stream, nullptr);
}

int ffi_stream_capacity(PDF_STREAM * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return 0;

  auto stream = (Stream *)pdf->ptr;

  return stream->Capacity();
}

int ffi_stream_set_capacity(PDF_STREAM * pdf, int capacity)
{
  if (!is_pdf_ptr_valid(pdf))
    return 0;

  auto stream = (Stream *)pdf->ptr;

  return stream->SetCapacity(capacity);
}

int ffi_stream_size(PDF_STREAM * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return 0;

  auto stream = (Stream *)pdf->ptr;

  return stream->Size();
}

int ffi_stream_set_size(PDF_STREAM * pdf, int size)
{
  if (!is_pdf_ptr_valid(pdf))
    return 0;

  auto stream = (Stream *)pdf->ptr;

  return stream->SetSize(size);
}

int ffi_stream_position(PDF_STREAM * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return 0;

  auto stream = (Stream *)pdf->ptr;

  return stream->Position();
}

int ffi_stream_set_position(PDF_STREAM * pdf, int pos)
{
  if (!is_pdf_ptr_valid(pdf))
    return 0;

  auto stream = (Stream *)pdf->ptr;

  return stream->SetPosition(pos);
}

int ffi_stream_available(PDF_STREAM * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return 0;

  auto stream = (Stream *)pdf->ptr;

  return stream->Available();
}

int ffi_stream_is_eof(PDF_STREAM * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto stream = (Stream *)pdf->ptr;

  return stream->eof();
}

int ffi_stream_read(PDF_STREAM * pdf, unsigned char * buffer, int size)
{
  if (!is_pdf_ptr_valid(pdf))
    return 0;

  auto stream = (Stream *)pdf->ptr;

  return stream->Read(buffer, size);
}

int ffi_stream_write(PDF_STREAM * pdf, unsigned char * buffer, int size)
{
  if (!is_pdf_ptr_valid(pdf))
    return 0;

  auto stream = (Stream *)pdf->ptr;

  return stream->Write(buffer, size);
}

int ffi_stream_print(PDF_STREAM * pdf, char * buffer)
{
  if (!is_pdf_ptr_valid(pdf))
    return 0;

  if (buffer == nullptr)
    return 0;

  auto stream = (Stream *)pdf->ptr;

  return stream->Write(buffer, strlen(buffer));
}

int ffi_stream_dispose(PDF_STREAM * pdf)
{
  if (!is_pdf_ptr_valid(pdf))
    return false;

  auto stream = (Stream *)pdf->ptr;

  if (stream->Data() == nullptr)
    return false;

  stream->Release();

  return true;
}
