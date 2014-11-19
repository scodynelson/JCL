package jcl.reader.macrofunction;

import jcl.LispStruct;

import java.math.BigInteger;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

public interface SharpTagReaderConstants {

	Map<BigInteger, LispStruct> SHARP_EQUAL_FINAL_TABLE = new ConcurrentHashMap<>();
	Map<BigInteger, UUID> SHARP_EQUAL_TEMP_TABLE = new ConcurrentHashMap<>();
	Map<UUID, LispStruct> SHARP_EQUAL_REPL_TABLE = new ConcurrentHashMap<>();
}
