*chunk def*
a CHUNK is an internal unit between the master node and worker node specifying the size of data over which the user-inputted functions should be applied
- default number of chunks is 1-- the entire input file
- the number of chunks (or size of chunks?) can be user specified using the built-in(?) chunking function(s?), but the user cannot directly access a chunk

*chunk functs*
default = 1 << not really a function
	- takes in file 
	- returns pointer to SOF

chunk_quant
	- takes in int numOfChunks
	- takes in int smallestUnit** 
	- splits file into specified number of chunks, roughly equal sizes 
	- returns array begPointers and int avgChunkSize

chunk_size 
	- takes in int sizeOfChunks
	- takes in int smallestUnit** 
	- splits file into chunks of specified sizes
	- returns array of begPointers and int chunkQuant

** need to be careful of smallest unit... 
ex. if a user is searching for frequency of a word, we need to make sure that words dont get split in half when chunking; in this case the smallest unit is a word
ex.2 if a user is searching for repetition of a paragraph, we need to make sure that paragraphs don't get split when chunking 

all of thes are still strings though, just different lengths of strings
- string sizes: letters, words, sentences, paragraphs.. anything else?

so either we can make the user input a smallestUnit or we have to take a look at their map function and figure out the smallest unit before we chunk 

*chunk files*
- scanner: keyword chunk
- parser: