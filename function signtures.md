FUNCTION SIGNATURES:

Splitter: function that splits text into little files
//built-in splitter functions 
	split_by_quant(File* inputFile, int quant); //input: number of chunks
	split_by_size(File* inputFile, int size); //input: size of chunks

//user-defined splitter function) 
	split_by_reg(File* inputFile, string regex); //input: regex

	return array[lilFilePtrs]; 
	//an array name = a pointer
	//the littleFilePtrs point to the little files

Mapper: user defined function that gets applied to each little file
	map(File* lilFilePtr); //lilFilePtr is one of the elements from the array returned by the splitter function
	return void* result; 

Reducer: user defined function that aggregates all the return values after map function is applied
	reduce(void* result);
	return void* total

Minimap: built-in function that orchestrates the splitting and mapreducing
	minimap(File* inputFile, void* splitter, void* mapper, void* reducer);
	return int flag; //0 = success, !0 = fail



	

