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
  int beg = 0, end = 0, skip = 0;
  while (1)
  {
    int endOfWord = 0;
    while (end < buff->size)
    {
      if (buff->chars[end] != '\n')
      {
	++end;
      } else {
	endOfWord = 1;
	break;
      }
    }
    
    if (endOfWord)
    {
      // TODO
    }
  }
  return 0;
}

int main()
{
  Buffer* buff = newBuff();
  process(buff);
  
  return 0;
}