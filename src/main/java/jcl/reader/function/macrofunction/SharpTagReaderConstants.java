package jcl.reader.function.macrofunction;

import jcl.LispStruct;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

public interface SharpTagReaderConstants {

	Map<Integer, LispStruct> SHARP_EQUAL_FINAL_TABLE = new ConcurrentHashMap<>();
	Map<Integer, UUID> SHARP_EQUAL_TEMP_TABLE = new ConcurrentHashMap<>();
	Map<UUID, LispStruct> SHARP_EQUAL_REPL_TABLE = new ConcurrentHashMap<>();
}
