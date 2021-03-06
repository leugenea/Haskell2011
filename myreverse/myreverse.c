#define BUFF_SIZE 10
#include <stdlib.h>
#include <stdio.h>

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
  int beg = 0, end = -1, skip = 0;
  while (1)
  {
    int endOfWord = 0;
    while (end+1 < buff->size)
    {
      if (buff->chars[end+1] != '\n')
      {
	++end;
      } else {
	endOfWord = 1;
	break;
      }
    }
    
    if (endOfWord)
    {
      if (skip)
      {
	skip = 0;
	beg = end + 1;
	continue;
      }
      int i;
      for (i = 0; i < (end-beg+1)/2; ++i)
      {
	char t = buff->chars[beg+i];
	buff->chars[beg+i] = buff->chars[end-i];
	buff->chars[end-i] = t;
      }
      if (end-beg != -1)
      {
	if (write(1, buff->chars+beg, end-beg+2) < 0)
	{
	  return -1;
	}
      }
      beg = ++end + 1;
    } else {
      if ((beg == 0) && (end == BUFF_SIZE-1))
      {
	beg = 0;
	end = -1;
	buff->size = 0;
	skip = 1;
	continue;
      }
      if (memmove(buff->chars, buff->chars+beg, beg-end+1) < 0)
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
  return process(buff);
}