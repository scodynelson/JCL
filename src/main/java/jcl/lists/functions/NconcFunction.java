/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

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
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class NconcFunction extends FunctionStruct {

	public static final SymbolStruct<?> NCONC = new SymbolStruct<>("NCONC", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = 2304204729441852930L;

	@Autowired
	private NullFunction nullFunction;

	@Autowired
	private Printer printer;

	private NconcFunction() {
		super("Returns a list that is the concatenation of lists.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		NCONC.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final List<RequiredBinding> requiredBindings = Collections.emptyList();
		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final SymbolStruct<?> listRestArgSymbol = new SymbolStruct<>("LISTS", GlobalPackageStruct.COMMON_LISP);
		final RestBinding restBinding = new RestBinding(listRestArgSymbol);

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return nconc(lispStructs);
	}

	public LispStruct nconc(final LispStruct... lispStructs) {

		if (lispStructs.length == 0) {
			return NullStruct.INSTANCE;
		}

		// NOTE: The (length - 1) below. This is so we skip the last argument.
		for (int i = 0; i < (lispStructs.length - 1); i++) {
			final LispStruct lispStruct = lispStructs[0];
			if (!(lispStruct instanceof ListStruct)) {
				final String printedObject = printer.print(lispStruct);
				throw new ErrorException("The value " + printedObject + " is not a list.");
			}
		}

		LispStruct top = ListStruct.buildProperList(lispStructs);

		while (!nullFunction.nullFnJavaBoolean(top)) {

			final ConsStruct topConsStruct = (ConsStruct) top;
			final LispStruct topOfTop = topConsStruct.getCar();

			if (topOfTop instanceof ConsStruct) {

				final ConsStruct result = (ConsStruct) topOfTop;
				ConsStruct splice = result;

				LispStruct elements = topConsStruct.getCdr();

				while (!NullStruct.INSTANCE.equals(elements) && !NILStruct.INSTANCE.equals(elements)) {

					// NOTE: The cast below is safe because of the check we do ealier to verify all the 'lispStructs' are actually lists.
					final ConsStruct elementsConsStruct = (ConsStruct) elements;
					final LispStruct ele = elementsConsStruct.getCar();

					final ListStruct spliceLast = splice.getLast();
					final ConsStruct spliceLastConsStruct = (ConsStruct) spliceLast;

					if (ele instanceof ConsStruct) {
						spliceLastConsStruct.setCdr(ele);
						splice = (ConsStruct) ele;
					} else if (nullFunction.nullFnJavaBoolean(ele)) {
						spliceLastConsStruct.setCdr(NullStruct.INSTANCE);
					} else {
						final LispStruct cdrElements = elementsConsStruct.getCdr();
						if (nullFunction.nullFnJavaBoolean(cdrElements)) {
							spliceLastConsStruct.setCdr(ele);
						} else {
							final String printedObject = printer.print(ele);
							throw new ErrorException("Argument is not a list -- " + printedObject);
						}
					}

					elements = elementsConsStruct.getCdr();
				}

				return result;
			} else if (nullFunction.nullFnJavaBoolean(top)) {
				top = topConsStruct.getCdr();
			} else {
				final LispStruct cdrTop = topConsStruct.getCdr();
				if (nullFunction.nullFnJavaBoolean(cdrTop)) {
					return topOfTop;
				} else {
					final String printedObject = printer.print(topOfTop);
					throw new ErrorException("Argument is not a list -- " + printedObject);
				}
			}
		}

		return NullStruct.INSTANCE;
	}
}
