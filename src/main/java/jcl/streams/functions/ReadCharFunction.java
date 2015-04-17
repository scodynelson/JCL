/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
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
import jcl.streams.ReadPeekResult;
import jcl.streams.StreamVariables;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
public final class ReadCharFunction extends FunctionStruct {

	public static final SymbolStruct<?> READ_CHAR = new SymbolStruct<>("READ-CHAR", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = -4477847470997236613L;

	@Autowired
	private ApplicationContext context;

	@Autowired
	private Printer printer;

	private ReadCharFunction() {
		super("Returns the next character from input-stream.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		READ_CHAR.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final List<RequiredBinding> requiredBindings = Collections.emptyList();

		final List<OptionalBinding> optionalBindings = new ArrayList<>();

		final SymbolStruct<?> inputStreamArgSymbol = new SymbolStruct<>("INPUT-STREAM", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation inputStreamArgAllocation = new ParameterAllocation(0);

		final SymbolStruct<?> inputStreamSuppliedP = new SymbolStruct<>("INPUT-STREAM-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final ParameterAllocation inputStreamSuppliedPAllocation = new ParameterAllocation(1);
		final SuppliedPBinding inputStreamSuppliedPBinding = new SuppliedPBinding(inputStreamSuppliedP, inputStreamSuppliedPAllocation);

		final OptionalBinding inputStreamOptionalBinding = new OptionalBinding(inputStreamArgSymbol, inputStreamArgAllocation, NullStruct.INSTANCE, inputStreamSuppliedPBinding);
		optionalBindings.add(inputStreamOptionalBinding);

		final SymbolStruct<?> eofErrorPArgSymbol = new SymbolStruct<>("EOF-ERROR", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation eofErrorPArgAllocation = new ParameterAllocation(2);

		final SymbolStruct<?> eofErrorPSuppliedP = new SymbolStruct<>("EOF-ERROR-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final ParameterAllocation eofErrorPSuppliedPAllocation = new ParameterAllocation(3);
		final SuppliedPBinding eofErrorPSuppliedPBinding = new SuppliedPBinding(eofErrorPSuppliedP, eofErrorPSuppliedPAllocation);

		final OptionalBinding eofErrorPOptionalBinding = new OptionalBinding(eofErrorPArgSymbol, eofErrorPArgAllocation, NullStruct.INSTANCE, eofErrorPSuppliedPBinding);
		optionalBindings.add(eofErrorPOptionalBinding);

		final SymbolStruct<?> eofValueArgSymbol = new SymbolStruct<>("EOF-VALUE", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation eofValueArgAllocation = new ParameterAllocation(4);

		final SymbolStruct<?> eofValueSuppliedP = new SymbolStruct<>("EOF-VALUE-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final ParameterAllocation eofValueSuppliedPAllocation = new ParameterAllocation(5);
		final SuppliedPBinding eofValueSuppliedPBinding = new SuppliedPBinding(eofValueSuppliedP, eofValueSuppliedPAllocation);

		final OptionalBinding eofValueOptionalBinding = new OptionalBinding(eofValueArgSymbol, eofValueArgAllocation, NullStruct.INSTANCE, eofValueSuppliedPBinding);
		optionalBindings.add(eofValueOptionalBinding);

		final SymbolStruct<?> recursivePArgSymbol = new SymbolStruct<>("RECURSIVE-P", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation recursivePArgAllocation = new ParameterAllocation(6);

		final SymbolStruct<?> recursivePSuppliedP = new SymbolStruct<>("RECURSIVE-P-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final ParameterAllocation recursivePSuppliedPAllocation = new ParameterAllocation(7);
		final SuppliedPBinding recursivePSuppliedPBinding = new SuppliedPBinding(recursivePSuppliedP, recursivePSuppliedPAllocation);

		final OptionalBinding recursivePOptionalBinding = new OptionalBinding(recursivePArgSymbol, recursivePArgAllocation, NullStruct.INSTANCE, recursivePSuppliedPBinding);
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

		final ReadPeekResult readPeekResult = readChar(inputStream, eofErrorP, eofValue, recursiveP);
		if (readPeekResult.isEof()) {
			return readPeekResult.getEofValue();
		} else {
			final int codePoint = readPeekResult.getResult();
			return new CharacterStruct(codePoint);
		}
	}

	public ReadPeekResult readChar(final InputStream inputStream, final BooleanStruct eofErrorP, final LispStruct eofValue,
	                               final BooleanStruct recursiveP) {

		return readChar(inputStream, eofErrorP.booleanValue(), eofValue, recursiveP.booleanValue());
	}

	public ReadPeekResult readChar(final InputStream inputStream, final boolean eofErrorP, final LispStruct eofValue,
	                               final boolean recursiveP) {

		final Reader reader = context.getBean(Reader.class, inputStream);
		return reader.readChar(eofErrorP, eofValue, recursiveP);
	}

	public ReadPeekResult readChar(final Reader reader, final boolean eofErrorP, final LispStruct eofValue,
	                               final boolean recursiveP) {

		return reader.readChar(eofErrorP, eofValue, recursiveP);
	}
}
