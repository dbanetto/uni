#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

/* Program developed for NWEN243, Victoria University of Wellington
Author: Kris Bubendorfer (c) 2014-15.
Uses a modified caeser I & II (in 2015)

Compile:  gcc -o decode decode.c

See encode for  examples on using it
*/

char upcase(char ch)
{
    if(islower(ch))
        ch -= 'a' - 'A';
    return ch;
}

char* fixkey(char* s)
{
    int i, j;
    char plain[26]; // assume key < length of alphabet, local array on stack, will go away!

    for (i = 0, j = 0; i < strlen(s); i++) {
        if(isalpha(s[i])) {
            plain[j++] = upcase(s[i]);
        }
    }
    plain[j] = '\0';
    return strcpy(s, plain);
}


int in(char c, char* s, int pos) {
    // Assume everything is already in the same case
    int i;

    for(i = 0; i < pos; i++)
        if(c == s[i]) return 1;

    return 0;
}


int char_used(char* set, char c)
{
    return set[c - 'A'];
}

void buildtable(char* key, char* decode)
{

    // This function needs to build an array of mappings in the 'encode' array from plaintext characters
    // to encypered characters.  The encode array will be indexed by the plaintext char.  To
    // make this a useful 0-26 index for the array, 'A' will be stubtracted from it (yes you
    // can do this in C).  You can see this in the main(){} below.  The values in the array
    // will be the cipher value, in the example at the top A -> H, B -> J, etc.

    // You are implementing a Caesar 1 & 2 combo Cypher as given in handout.
    // Your code here:

    // probably need to declare some stuff here!

    char *set = (char*)calloc(26, sizeof(char));
    int index = (strlen(key) - 1) % 26;
    /*printf("Offset: %i\n", index);*/
    fixkey(key); // fix the key, i.e., uppercase and remove whitespace and punctuation

    char *itr = 0, last;
    for (itr = key; *itr; itr++) {
        if (!char_used(decode, *itr)) {
            set[index] = *itr;
            decode[*itr - 'A'] = index + 'A';
            index = (index + 1) % 26;
            last = *itr;
        }
    }

    int count;
    last = (((last - 'A') + 1) % 26) + 'A';
    for (count = 0; count < 26; count++) {
        if (!char_used(decode, last)) {
            set[index] = last;
            decode[last - 'A'] = index + 'A';
            index = (index + 1) % 26;
        }
        last = (((last - 'A') + 1) % 26) + 'A';
    }

    // Do some stuff here to make a translation between plain and cypher maps.

    free(set);
}

int main(int argc, char **argv) {
    // format will be: 'program' key {encode|decode}
    // We'll be using stdin and stdout for files to encode and decode.

    // first allocate some space for our translation table.

    char* decode = (char*)malloc(sizeof(char)*26); // this changed from encode
    char ch;

    if(argc != 2) {
        fprintf(stderr,"format is: '%s' key", argv[0]);
        exit(1);
    }

    // Build translation tables, and ensure key is upcased and alpha chars only.

    buildtable(argv[1], decode); // this changed from encode

    // write the key to stderr (so it doesn't break our pipes)

    fprintf(stderr,"key: %s - %d\n", decode, strlen(decode));


    // the following code does the translations.  Characters are read
    // one-by-one from stdin, translated and written to stdout.

    ch = fgetc(stdin);
    while (!feof(stdin)) {
        if(isalpha(ch))          // only decrypt alpha chars
            fputc(decode[ch-'A'], stdout);
        else
            fputc(ch, stdout);
        ch = fgetc(stdin);      // get next char from stdin
    }

    free(decode);
}
