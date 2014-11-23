/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;

import java.math.BigInteger;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Interface for defining global constants for the #= and ## tag readers.
 */
public interface SharpTagReaderConstants {

	/**
	 * Map containing the number argument to #= to parsed {@link LispStruct}s produced by the #= reader macro function.
	 */
	Map<BigInteger, LispStruct> SHARP_EQUAL_FINAL_TABLE = new ConcurrentHashMap<>();

	/**
	 * Map containing the number argument of #= to a temporary {@link UUID} tag value to handle {@link LispStruct}s not
	 * yet parsed by the reader.
	 */
	Map<BigInteger, UUID> SHARP_EQUAL_TEMP_TABLE = new ConcurrentHashMap<>();

	/**
	 * Map containing the temporary {@link UUID} tag value to a {@link LispStruct} that has been parsed by the reader,
	 * but may have yet to return to the top level of the #= parse.
	 */
	Map<UUID, LispStruct> SHARP_EQUAL_REPL_TABLE = new ConcurrentHashMap<>();
}
