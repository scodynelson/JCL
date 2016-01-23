/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.functions;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.streams.InputStream;
import jcl.streams.StreamVariables;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
public final class ReadFunction extends FunctionStruct {

	public static final SymbolStruct READ = GlobalPackageStruct.COMMON_LISP.intern("READ").getSymbol();

	@Autowired
	private ApplicationContext context;

	@Autowired
	private Printer printer;

	private ReadFunction() {
		super("Parses the printed representation of an object from input-stream and builds such an object.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		READ.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(READ);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final List<OptionalParameter> optionalBindings = new ArrayList<>(4);

		final SymbolStruct inputStreamArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("INPUT-STREAM").getSymbol();

		final SymbolStruct inputStreamSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("INPUT-STREAM-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter inputStreamSuppliedPBinding = new SuppliedPParameter(inputStreamSuppliedP);

		final OptionalParameter inputStreamOptionalBinding = new OptionalParameter(inputStreamArgSymbol, NullStruct.INSTANCE, inputStreamSuppliedPBinding);
		optionalBindings.add(inputStreamOptionalBinding);

		final SymbolStruct eofErrorPArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("EOF-ERROR").getSymbol();

		final SymbolStruct eofErrorPSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("EOF-ERROR-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter eofErrorPSuppliedPBinding = new SuppliedPParameter(eofErrorPSuppliedP);

		final OptionalParameter eofErrorPOptionalBinding = new OptionalParameter(eofErrorPArgSymbol, NullStruct.INSTANCE, eofErrorPSuppliedPBinding);
		optionalBindings.add(eofErrorPOptionalBinding);

		final SymbolStruct eofValueArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("EOF-VALUE").getSymbol();

		final SymbolStruct eofValueSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("EOF-VALUE-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter eofValueSuppliedPBinding = new SuppliedPParameter(eofValueSuppliedP);

		final OptionalParameter eofValueOptionalBinding = new OptionalParameter(eofValueArgSymbol, NullStruct.INSTANCE, eofValueSuppliedPBinding);
		optionalBindings.add(eofValueOptionalBinding);

		final SymbolStruct recursivePArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("RECURSIVE-P").getSymbol();

		final SymbolStruct recursivePSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("RECURSIVE-P-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter recursivePSuppliedPBinding = new SuppliedPParameter(recursivePSuppliedP);

		final OptionalParameter recursivePOptionalBinding = new OptionalParameter(recursivePArgSymbol, NullStruct.INSTANCE, recursivePSuppliedPBinding);
		optionalBindings.add(recursivePOptionalBinding);

		return OrdinaryLambdaList.builder()
		                         .optionalBindings(optionalBindings)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

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

		LispStruct eofValue = NullStruct.INSTANCE;
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

		return read(inputStream, eofErrorP, eofValue, recursiveP);
	}

	public LispStruct read(final InputStream inputStream, final BooleanStruct eofErrorP, final LispStruct eofValue,
	                       final BooleanStruct recursiveP) {

		return read(inputStream, eofErrorP.booleanValue(), eofValue, recursiveP.booleanValue());
	}

	public LispStruct read(final InputStream inputStream, final boolean eofErrorP, final LispStruct eofValue,
	                       final boolean recursiveP) {

		final Reader reader = context.getBean(Reader.class, inputStream);
		return reader.read(eofErrorP, eofValue, recursiveP);
	}

	public LispStruct read(final Reader reader, final boolean eofErrorP, final LispStruct eofValue,
	                       final boolean recursiveP) {

		return reader.read(eofErrorP, eofValue, recursiveP);
	}
}
