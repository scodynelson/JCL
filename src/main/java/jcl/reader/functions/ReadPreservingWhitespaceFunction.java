/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.functions;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
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
public final class ReadPreservingWhitespaceFunction extends FunctionStruct {

	public static final SymbolStruct<?> READ_PRESERVING_WHITESPACE = new SymbolStruct<>("READ-PRESERVING-WHITESPACE", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = 907539293814708746L;

	@Autowired
	private ApplicationContext context;

	@Autowired
	private Printer printer;

	private ReadPreservingWhitespaceFunction() {
		super("Parses the printed representation of an object from input-stream and builds such an object preserving any whitespace character that delimits the printed representation of the object.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		READ_PRESERVING_WHITESPACE.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final List<RequiredBinding> requiredBindings = Collections.emptyList();

		final List<OptionalBinding> optionalBindings = new ArrayList<>();

		final SymbolStruct<?> inputStreamArgSymbol = new SymbolStruct<>("INPUT-STREAM", GlobalPackageStruct.COMMON_LISP);

		final SymbolStruct<?> inputStreamSuppliedP = new SymbolStruct<>("INPUT-STREAM-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final SuppliedPBinding inputStreamSuppliedPBinding = new SuppliedPBinding(inputStreamSuppliedP);

		final OptionalBinding inputStreamOptionalBinding = new OptionalBinding(inputStreamArgSymbol, NullStruct.INSTANCE, inputStreamSuppliedPBinding);
		optionalBindings.add(inputStreamOptionalBinding);

		final SymbolStruct<?> eofErrorPArgSymbol = new SymbolStruct<>("EOF-ERROR", GlobalPackageStruct.COMMON_LISP);

		final SymbolStruct<?> eofErrorPSuppliedP = new SymbolStruct<>("EOF-ERROR-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final SuppliedPBinding eofErrorPSuppliedPBinding = new SuppliedPBinding(eofErrorPSuppliedP);

		final OptionalBinding eofErrorPOptionalBinding = new OptionalBinding(eofErrorPArgSymbol, NullStruct.INSTANCE, eofErrorPSuppliedPBinding);
		optionalBindings.add(eofErrorPOptionalBinding);

		final SymbolStruct<?> eofValueArgSymbol = new SymbolStruct<>("EOF-VALUE", GlobalPackageStruct.COMMON_LISP);

		final SymbolStruct<?> eofValueSuppliedP = new SymbolStruct<>("EOF-VALUE-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final SuppliedPBinding eofValueSuppliedPBinding = new SuppliedPBinding(eofValueSuppliedP);

		final OptionalBinding eofValueOptionalBinding = new OptionalBinding(eofValueArgSymbol, NullStruct.INSTANCE, eofValueSuppliedPBinding);
		optionalBindings.add(eofValueOptionalBinding);

		final SymbolStruct<?> recursivePArgSymbol = new SymbolStruct<>("RECURSIVE-P", GlobalPackageStruct.COMMON_LISP);

		final SymbolStruct<?> recursivePSuppliedP = new SymbolStruct<>("RECURSIVE-P-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final SuppliedPBinding recursivePSuppliedPBinding = new SuppliedPBinding(recursivePSuppliedP);

		final OptionalBinding recursivePOptionalBinding = new OptionalBinding(recursivePArgSymbol, NullStruct.INSTANCE, recursivePSuppliedPBinding);
		optionalBindings.add(recursivePOptionalBinding);

		final RestBinding restBinding = null;

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final int length = lispStructs.length;

		InputStream inputStream = StreamVariables.STANDARD_INPUT.getValue();
		if (length > 0) {
			final LispStruct inputStreamArg = lispStructs[0];

			if (TStruct.INSTANCE.equals(inputStreamArg)) {
				inputStream = StreamVariables.TERMINAL_IO.getValue();
			} else if (NILStruct.INSTANCE.equals(inputStreamArg) || NullStruct.INSTANCE.equals(inputStreamArg)) {
				inputStream = StreamVariables.STANDARD_INPUT.getValue();
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

		return readPreservingWhitespace(inputStream, eofErrorP, eofValue, recursiveP);
	}

	public LispStruct readPreservingWhitespace(final InputStream inputStream, final BooleanStruct eofErrorP, final LispStruct eofValue,
	                                           final BooleanStruct recursiveP) {

		return readPreservingWhitespace(inputStream, eofErrorP.booleanValue(), eofValue, recursiveP.booleanValue());
	}

	public LispStruct readPreservingWhitespace(final InputStream inputStream, final boolean eofErrorP, final LispStruct eofValue,
	                                           final boolean recursiveP) {

		final Reader reader = context.getBean(Reader.class, inputStream);
		return reader.readPreservingWhitespace(eofErrorP, eofValue, recursiveP);
	}

	public LispStruct readPreservingWhitespace(final Reader reader, final boolean eofErrorP, final LispStruct eofValue,
	                                           final boolean recursiveP) {

		return reader.readPreservingWhitespace(eofErrorP, eofValue, recursiveP);
	}
}
