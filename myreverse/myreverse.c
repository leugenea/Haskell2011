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
  return len;
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
      /*
      if (skip)		// word is tooooo looooong
      {
	skip = 0;
	beg = ++end;
	continue;
      }*/
      int i;
      for (i = 0; i < (end-beg+1)/2; ++i)
      {
	char t = buff->chars[beg+i];
	buff->chars[beg+i] = buff->chars[end-i];
	buff->chars[end-i] = t;
      }
      if (write(1, buff->chars+beg, end-beg+1))
      {
	return -1;
      }
      beg = ++end;
    } else {
      if ((beg == 0) && (end == BUFF_SIZE))
      {
	//skip = 1;
	beg = 0;
	end = 0;
	buff->size = 0;
	continue;
      }
      if (memmove(buff->chars, buff->chars+beg, beg-end) < 0)
      {
	return -2;
      }
      buff->size -= beg;
      end = buff->size - 1;
      beg = 0;
      
      if (readBuff(buff) < 0)
      {
	return -3;
      }
    }
  }
  return 0;
}

int main()
{
  Buffer* buff = newBuff();
  //return process(buff);
  
  return 0;
}