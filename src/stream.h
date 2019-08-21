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

// ----- Stream --------------------------------------------------------

class Stream
{
public:

  Stream(void * dataA, int data_sizeA);
  ~Stream();

  void * Data() const   { return data; }
  int Capacity() const  { return capacity; }
  int Size() const      { return data_size; }
  int Position() const  { return position; }
  int Available() const { return std::max(data_size - position, 0); }

  bool eof() const      { return position >= data_size; }

  int SetCapacity(int capacityA);
  int SetSize(int size);
  int SetPosition(int positionA);
  int Read(void * buffer, int size);
  int Write(void * buffer, int size);

  void Release();

private:

  void * data = nullptr;
  int capacity = 0;
  int data_size = 0;
  int position  = 0;
};
