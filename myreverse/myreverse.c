#define BUFF_SIZE 10
#include <stdlib.h>

typedef struct
{
  int size;
  char* chars;
} Buffer;

Buffer* newBuff()
{
  Buffer* buff = malloc(sizeof(Buffer));
  buff->chars = malloc(BUFF_SIZE);
  buff->size = 0;
  
  return buff;
}

int readBuff(Buffer* buff)
{
  int len = read(0, buff->chars + buff->size, BUFF_SIZE - buff->size);
  buff->size += len;
  return 0;
}

int process(Buffer* buff)
{
  do
  {
    
  } while (1);
  return 0;
}

int main()
{
  Buffer* buff = newBuff();
  process(buff);
  
  return 0;
}