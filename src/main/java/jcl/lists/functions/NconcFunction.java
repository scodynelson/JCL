/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
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

	public static final SymbolStruct NCONC = GlobalPackageStruct.COMMON_LISP.intern("NCONC").getSymbol();

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
		GlobalPackageStruct.COMMON_LISP.export(NCONC);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct listRestArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("LISTS").getSymbol();
		final RestParameter restBinding = new RestParameter(listRestArgSymbol);

		return OrdinaryLambdaList.builder()
		                         .restBinding(restBinding)
		                         .build();
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
