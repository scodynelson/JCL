package jcl.readtables.reader.impl.macrofunctions;

import jcl.LispStruct;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.readtables.reader.LispReader;
import jcl.readtables.reader.syntax.ReadExtendedToken;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

public interface MacroFunctionReader extends LispReader {

	Map<Integer, LispStruct> SHARP_EQUAL_FINAL_TABLE = new ConcurrentHashMap<>();
	Map<Integer, UUID> SHARP_EQUAL_TEMP_TABLE = new ConcurrentHashMap<>();
	Map<UUID, LispStruct> SHARP_EQUAL_REPL_TABLE = new ConcurrentHashMap<>();

	//*************************//
	//** READ-EXTENDED-TOKEN **//
	//*************************//

	ReadExtendedToken readExtendedToken();

	String readExtendedTokenEscaped();

	//************************//
	//** #R, #B, #O, and #X **//
	//************************//

	IntegerStruct readIntegerToken(Integer radix);

	//***************//
	//** READ-LIST **//
	//***************//

	ListStruct readList();

	//***********************//
	//** READ-UNICODE-CHAR **//
	//***********************//

	int readUnicodeChar();

	//***************//
	//** #+ and #- **//
	//***************//

	void readFeatures(boolean shouldHideFeatures);
}
