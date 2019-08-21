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

#define FFI_MAJOR_VERSION  0
#define FFI_MINOR_VERSION  1
#define FFI_RELEASE        0

enum PDF_TYPE
{
  PDF_TYPE_NONE,
  PDF_TYPE_MEM_DOC,
  PDF_TYPE_COBJ,
  PDF_TYPE_STREAM,
};

typedef struct _PDF_PTR PDF_PTR;

struct _PDF_PTR
{
  int       count : 24;
  int       type  : 8;
  void    * ptr;
  PDF_PTR * owner;
};

typedef struct _PDF_PTR  PDF_MDOC;
typedef struct _PDF_PTR  PDF_COBJ;
typedef struct _PDF_PTR  PDF_STREAM;

typedef unsigned char BYTE;

void ffi_init();

int is_pdf_ptr_valid(PDF_PTR *);
void delete_pdf_ptr(PDF_PTR *);

// ----- ffi -----------------------------------------------------------

char * podofo_version();
char * ffi_version();

void ffi_dispose(PDF_PTR *);
char * ffi_type_of(PDF_PTR *);
char * ffi_cos_type_of(PDF_PTR *);

// ----- Document ------------------------------------------------------

PDF_MDOC * ffi_create_mem_doc();

char * ffi_doc_version(PDF_MDOC *);
int ffi_doc_set_version(PDF_MDOC *, const char *);

int ffi_doc_load(PDF_MDOC *, const char *);
int ffi_doc_write(PDF_MDOC *, const char *);

PDF_COBJ * ffi_doc_catalog(PDF_MDOC *);
PDF_COBJ * ffi_doc_trailer(PDF_MDOC *);
PDF_COBJ * ffi_doc_struct_tree_root(PDF_MDOC *);

// ----- Object --------------------------------------------------------

int ffi_obj_is_bool(PDF_COBJ *);
int ffi_obj_is_number(PDF_COBJ *);
int ffi_obj_is_real(PDF_COBJ *);
int ffi_obj_is_string(PDF_COBJ *);
int ffi_obj_is_hexstring(PDF_COBJ *);
int ffi_obj_is_name(PDF_COBJ *);
int ffi_obj_is_array(PDF_COBJ *);
int ffi_obj_is_dictionary(PDF_COBJ *);
int ffi_obj_is_rawdata(PDF_COBJ *);
int ffi_obj_is_null(PDF_COBJ *);
int ffi_obj_is_reference(PDF_COBJ *);
int ffi_obj_has_stream(PDF_COBJ *);

int ffi_obj_bool(PDF_COBJ *);
long long ffi_obj_number(PDF_COBJ *);
double ffi_obj_real(PDF_COBJ *);
char * ffi_obj_string(PDF_COBJ *);
char * ffi_obj_hexstring(PDF_COBJ *);
char * ffi_obj_name(PDF_COBJ *);
char * ffi_ref_id(PDF_COBJ *);
PDF_COBJ * ffi_ref_indirect(PDF_COBJ *);

// ----- Array ---------------------------------------------------------

int ffi_arr_size(PDF_COBJ *);
PDF_COBJ * ffi_arr_value(PDF_COBJ *, int);
PDF_COBJ * ffi_arr_indirect(PDF_COBJ *, int);

int ffi_arr_resize(PDF_COBJ *, int);
int ffi_arr_remove(PDF_COBJ *, int);
int ffi_arr_insert(PDF_COBJ *, int);
PDF_COBJ * ffi_arr_set_bool(PDF_COBJ *, int, int, int);
PDF_COBJ * ffi_arr_set_number(PDF_COBJ *, int, long long, int);
PDF_COBJ * ffi_arr_set_real(PDF_COBJ *, int, double, int);
PDF_COBJ * ffi_arr_set_string(PDF_COBJ *, int, const char *, int);
PDF_COBJ * ffi_arr_set_hexstring(PDF_COBJ *, int, const char *, int);
PDF_COBJ * ffi_arr_set_name(PDF_COBJ *, int, const char *, int);
PDF_COBJ * ffi_arr_set_null(PDF_COBJ *, int);
PDF_COBJ * ffi_arr_set_dictionary(PDF_COBJ *, int, int);
PDF_COBJ * ffi_arr_set_array(PDF_COBJ *, int, int);
int ffi_arr_set_reference(PDF_COBJ *, int, PDF_COBJ *);

// ----- Dictionary ----------------------------------------------------

int ffi_dic_size(PDF_COBJ *);
char * ffi_dic_key(PDF_COBJ *, int);
PDF_COBJ * ffi_dic_value(PDF_COBJ *, const char *);
PDF_COBJ * ffi_dic_indirect(PDF_COBJ *, const char *);
int ffi_dic_remove(PDF_COBJ *, const char *);
PDF_COBJ * ffi_dic_set_bool(PDF_COBJ *, const char *, int, int);
PDF_COBJ * ffi_dic_set_number(PDF_COBJ *, const char *, long long, int);
PDF_COBJ * ffi_dic_set_real(PDF_COBJ *, const char *, double, int);
PDF_COBJ * ffi_dic_set_string(PDF_COBJ *, const char *, const char *, int);
PDF_COBJ * ffi_dic_set_hexstring(PDF_COBJ *, const char *, const char *, int);
PDF_COBJ * ffi_dic_set_name(PDF_COBJ *, const char *, const char *, int);
PDF_COBJ * ffi_dic_set_null(PDF_COBJ *, const char *);
PDF_COBJ * ffi_dic_set_dictionary(PDF_COBJ *, const char *, int);
PDF_COBJ * ffi_dic_set_array(PDF_COBJ *, const char *, int);
int ffi_dic_set_reference(PDF_COBJ *, const char *, PDF_COBJ *);

PDF_STREAM * ffi_dic_stream(PDF_COBJ *);
PDF_STREAM * ffi_dic_filtered_stream(PDF_COBJ *);
int ffi_dic_set_stream(PDF_COBJ *, PDF_STREAM *);

// ----- Stream --------------------------------------------------------

PDF_STREAM * ffi_create_stream(int);
int ffi_stream_capacity(PDF_STREAM *);
int ffi_stream_set_capacity(PDF_STREAM *, int);
int ffi_stream_size(PDF_STREAM *);
int ffi_stream_set_size(PDF_STREAM *, int);
int ffi_stream_position(PDF_STREAM *);
int ffi_stream_set_position(PDF_STREAM *, int);
int ffi_stream_available(PDF_STREAM *);
int ffi_stream_is_eof(PDF_STREAM *);
int ffi_stream_read(PDF_STREAM *, unsigned char *, int);
int ffi_stream_write(PDF_STREAM *, unsigned char *, int);
int ffi_stream_print(PDF_STREAM *, char *);
int ffi_stream_dispose(PDF_STREAM *);
