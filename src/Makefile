sources = ffi-pdf.cpp ffi-doc.cpp ffi-obj.cpp ffi-col.cpp ffi-stream.cpp

objects = $(sources:.cpp=.o)

CXXFLAGS = -Os -s -DNDEBUG

ffi-podofo.o1 : ffi-podofo.scm ffi-podofo\#.scm ffi-release.h $(objects)
	rm -f ffi-podofo.o[0-9]
	gsc -ld-options "$(objects) -lpodofo -lstdc++" -dynamic ffi-podofo.scm

ffi-pdf.o : stream.h
ffi-col.o : stream.h
ffi-stream.o : stream.h

$(objects) : ffi-pdf.h utils.h

$(objects) : %.o : %.cpp
	g++ -fPIC -c $(CXXFLAGS) $<

.PHONY : install
install : ffi-podofo.o1
	strip -g ffi-podofo.o1
	$(eval dest = $(shell gsi -e '(print (path-expand "~~/lib/pdf"))'))
	$(and $(dest), install -D -m 0644 -t $(dest) podofo.scm ffi-podofo#.scm ffi-podofo.o1)

.PHONY : clean
clean :
	rm -f *.o[0-9]
	rm -f *.o
