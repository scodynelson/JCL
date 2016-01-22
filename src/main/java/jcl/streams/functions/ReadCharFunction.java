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
import jcl.reader.Reader;
import jcl.streams.InputStream;
import jcl.streams.ReadPeekResult;
import jcl.streams.StreamVariables;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
public final class ReadCharFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = 8697359997833339004L;

	@Autowired
	private ApplicationContext context;

	@Autowired
	private Printer printer;

	public ReadCharFunction() {
		super("Returns the next character from input-stream.");
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {

		final List<OptionalParameter> optionalBindings = new ArrayList<>(4);

		final OptionalParameter inputStreamOptionalBinding =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "INPUT-STREAM")
				                 .suppliedPBinding()
				                 .build();
		optionalBindings.add(inputStreamOptionalBinding);

		final OptionalParameter eofErrorPOptionalBinding =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "EOF-ERROR")
				                 .suppliedPBinding()
				                 .build();
		optionalBindings.add(eofErrorPOptionalBinding);

		final OptionalParameter eofValueOptionalBinding =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "EOF-VALUE")
				                 .suppliedPBinding()
				                 .build();
		optionalBindings.add(eofValueOptionalBinding);

		final OptionalParameter recursivePOptionalBinding =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "RECURSIVE-P")
				                 .suppliedPBinding()
				                 .build();
		optionalBindings.add(recursivePOptionalBinding);

		return optionalBindings;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final int length = lispStructs.length;

		InputStream inputStream = StreamVariables.STANDARD_INPUT.getVariableValue();
		if (length > 0) {
			final LispStruct inputStreamArg = lispStructs[0];

			if (TStruct.INSTANCE.equals(inputStreamArg)) {
				inputStream = StreamVariables.TERMINAL_IO.getVariableValue();
			} else if (NILStruct.INSTANCE.equals(inputStreamArg) || NullStruct.INSTANCE.equals(inputStreamArg)) {
				inputStream = StreamVariables.STANDARD_INPUT.getVariableValue();
			} else if (inputStreamArg instanceof InputStream) {
				inputStream = (InputStream) inputStreamArg;
			} else {
				final String printedObject = printer.print(inputStreamArg);
				throw new TypeErrorException("The value " + printedObject + " is not either T, NIL, or a STREAM.");
			}
		}

		BooleanStruct eofErrorP = TStruct.INSTANCE;
		if (length > 1) {
			final LispStruct eofErrorPArg = lispStructs[1];
			if (NILStruct.INSTANCE.equals(eofErrorPArg) || NullStruct.INSTANCE.equals(eofErrorPArg)) {
				eofErrorP = NILStruct.INSTANCE;
			} else {
				eofErrorP = TStruct.INSTANCE;
			}
		}

		LispStruct eofValue = NILStruct.INSTANCE;
		if (length > 2) {
			eofValue = lispStructs[1];
		}

		BooleanStruct recursiveP = NILStruct.INSTANCE;
		if (length >= 3) {
			final LispStruct recursivePArg = lispStructs[1];
			if (NILStruct.INSTANCE.equals(recursivePArg) || NullStruct.INSTANCE.equals(recursivePArg)) {
				recursiveP = NILStruct.INSTANCE;
			} else {
				recursiveP = TStruct.INSTANCE;
			}
		}

		final Reader reader = context.getBean(Reader.class, inputStream);
		final ReadPeekResult readPeekResult = reader.readChar(eofErrorP.booleanValue(), eofValue, recursiveP.booleanValue());
		if (readPeekResult.isEof()) {
			return readPeekResult.getEofValue();
		} else {
			final int codePoint = readPeekResult.getResult();
			return CharacterStruct.valueOf(codePoint);
		}
	}

	@Override
	protected String functionName() {
		return "READ-CHAR";
	}
}
