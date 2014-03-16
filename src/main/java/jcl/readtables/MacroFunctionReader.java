package jcl.readtables;

import jcl.LispStruct;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.syntax.reader.ReadExtendedToken;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public interface MacroFunctionReader extends LispReader {

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

	//***************//
	//** #= and ## **//
	//***************//

	Map<Integer, LispStruct> SHARP_EQUAL_FINAL_TABLE = new ConcurrentHashMap<>();
	Map<Integer, Object> SHARP_EQUAL_TEMP_TABLE = new ConcurrentHashMap<>();
	Map<Integer, LispStruct> SHARP_EQUAL_REPL_TABLE = new ConcurrentHashMap<>();
	Map<Integer, LispStruct> SHARP_EQUAL_CIRCLE_TABLE = new ConcurrentHashMap<>();

	void circleSubst(Map<Long, LispStruct> replTable, LispStruct tree);
}
