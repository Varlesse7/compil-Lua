#include "fichierToStruct.h"
#include "structure.h"
#include "liste.h"

#ifndef SRC_CHUNK_H
#define SRC_CHUNK_H

chunk createChunk();
void libererChunk(chunk c);
chunk decode_chunk (codeLua f);

#endif //SRC_CHUNK_H
