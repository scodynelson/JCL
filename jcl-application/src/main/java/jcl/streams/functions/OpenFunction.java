/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.LispStruct;
import jcl.LispType;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntegerStruct;
import jcl.pathnames.PathnameStruct;
import jcl.streams.DirectionType;
import jcl.streams.ExternalFormat;
import jcl.streams.FileStreamStructs;
import jcl.streams.IfDoesNotExistType;
import jcl.streams.IfExistsType;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.springframework.stereotype.Component;

@Component
public final class OpenFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "OPEN";
	private static final String FILESPEC_ARGUMENT = "FILESPEC";

	public OpenFunction() {
		super("Writes the characters of the sub-sequence of string bounded by start and end to output-stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FILESPEC_ARGUMENT)
		                .keyParameter(CommonLispSymbols.DIRECTION_KEYWORD).withInitialValue(IntegerStruct.ZERO)
		                .keyParameter(CommonLispSymbols.ELEMENT_TYPE_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.IF_EXISTS_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.IF_DOES_NOT_EXIST_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final PathnameStruct pathname = arguments.getRequiredArgument(FILESPEC_ARGUMENT).asPathname().get();

		final SymbolStruct directionSymbol = arguments.getKeyArgument(CommonLispSymbols.DIRECTION_KEYWORD, SymbolStruct.class);
		final DirectionType directionType = DirectionType.fromValue(directionSymbol);

		final LispType elementType = arguments.getKeyArgument(CommonLispSymbols.ELEMENT_TYPE_KEYWORD, LispType.class);

		final SymbolStruct ifExistsSymbol = arguments.getKeyArgument(CommonLispSymbols.IF_EXISTS_KEYWORD, SymbolStruct.class);
		final IfExistsType ifExistsType = IfExistsType.fromValue(ifExistsSymbol);

		final SymbolStruct ifDoesNotExistSymbol = arguments.getKeyArgument(CommonLispSymbols.IF_DOES_NOT_EXIST_KEYWORD, SymbolStruct.class);
		final IfDoesNotExistType ifDoesNotExistType = IfDoesNotExistType.fromValue(ifDoesNotExistSymbol);

		final ExternalFormat externalFormat = arguments.getKeyArgument(CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD, ExternalFormat.class);

		return FileStreamStructs.open(pathname, directionType, elementType, ifExistsType, ifDoesNotExistType, externalFormat);
	}
}
