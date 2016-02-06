/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.streams.InputStream;
import jcl.streams.PeekType;
import jcl.streams.ReadPeekResult;
import jcl.streams.StreamVariables;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import jcl.types.BooleanType;
import jcl.types.CharacterType;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PeekCharFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public PeekCharFunction() {
		super("Obtains the next character in input-stream without actually reading it.");
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		final List<OptionalParameter> optionalParameters = new ArrayList<>(5);

		final OptionalParameter peekType =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "PEEK-TYPE")
				                 .initForm(NILStruct.INSTANCE)
				                 .suppliedPBinding()
				                 .build();
		optionalParameters.add(peekType);
		final OptionalParameter inputStream =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "INPUT-STREAM")
				                 .suppliedPBinding()
				                 .initForm(StreamVariables.STANDARD_INPUT.getVariableValue())
				                 .build();
		optionalParameters.add(inputStream);
		final OptionalParameter eofErrorP =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "EOF-ERROR-P")
				                 .suppliedPBinding()
				                 .initForm(TStruct.INSTANCE)
				                 .build();
		optionalParameters.add(eofErrorP);
		final OptionalParameter eofValue =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "EOF-VALUE")
				                 .suppliedPBinding()
				                 .initForm(NILStruct.INSTANCE)
				                 .build();
		optionalParameters.add(eofValue);
		final OptionalParameter recursiveP =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "RECURSIVE-P")
				                 .suppliedPBinding()
				                 .initForm(NILStruct.INSTANCE)
				                 .build();
		optionalParameters.add(recursiveP);

		return optionalParameters;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final int length = lispStructs.length;

		PeekType peekType = PeekType.NIL_PEEK_TYPE;
		if (length > 0) {
			final LispStruct lispStruct = lispStructs[0];
			validator.validateTypes(lispStruct, functionName(), "Peek Type", BooleanType.INSTANCE, CharacterType.INSTANCE);
			if (TStruct.INSTANCE.equals(lispStruct)) {
				peekType = PeekType.T_PEEK_TYPE;
			} else if (NILStruct.INSTANCE.equals(lispStruct) || NullStruct.INSTANCE.equals(lispStruct)) {
				peekType = PeekType.NIL_PEEK_TYPE;
			} else if (lispStruct instanceof CharacterStruct) {
				final CharacterStruct character = (CharacterStruct) lispStruct;
				peekType = PeekType.getCharacterPeekType(character.getCodePoint());
			} else {
				throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
			}
		}

		InputStream inputStream = StreamVariables.STANDARD_INPUT.getVariableValue();
		if (length > 1) {
			final LispStruct lispStruct = lispStructs[1];
			validator.validateTypes(lispStruct, functionName(), "Input Stream", StreamType.INSTANCE);

			if (lispStruct instanceof InputStream) {
				inputStream = (InputStream) lispStruct;
			} else {
				final String printedObject = printer.print(lispStruct);
				throw new TypeErrorException("The value " + printedObject + " is not an Input Stream.");
			}
		}

		BooleanStruct eofErrorP = TStruct.INSTANCE;
		if (length > 2) {
			final LispStruct lispStruct = lispStructs[2];
			validator.validateTypes(lispStruct, functionName(), "EOF Error Predicate", BooleanType.INSTANCE);
			eofErrorP = (BooleanStruct) lispStruct;
		}

		LispStruct eofValue = NILStruct.INSTANCE;
		if (length > 3) {
			eofValue = lispStructs[3];
		}

		BooleanStruct recursiveP = NILStruct.INSTANCE;
		if (length > 4) {
			final LispStruct lispStruct = lispStructs[4];
			validator.validateTypes(lispStruct, functionName(), "Recursive Predicate", BooleanType.INSTANCE);
			recursiveP = (BooleanStruct) lispStruct;
		}

		final ReadPeekResult readPeekResult = inputStream.peekChar(peekType, eofErrorP.booleanValue(), eofValue, recursiveP.booleanValue());
		return readPeekResult.isEof() ? eofValue : CharacterStruct.valueOf(readPeekResult.getResult());
	}

	@Override
	protected String functionName() {
		return "PEEK-CHAR";
	}
}
