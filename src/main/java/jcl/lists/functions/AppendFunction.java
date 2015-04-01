/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.conditions.exceptions.ErrorException;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class AppendFunction extends FunctionStruct {

	public static final SymbolStruct<?> APPEND = new SymbolStruct<>("APPEND", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = -6347931755033347733L;

	@Autowired
	private NullFunction nullFunction;

	@Autowired
	private Printer printer;

	private AppendFunction() {
		super("Returns a new list that is the concatenation of the copies.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		APPEND.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final List<RequiredBinding> requiredBindings = Collections.emptyList();
		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final SymbolStruct<?> listRestArgSymbol = new SymbolStruct<>("LISTS", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation listRestArgArgAllocation = new ParameterAllocation(0);
		final RestBinding restBinding = new RestBinding(listRestArgSymbol, listRestArgArgAllocation);

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return append(lispStructs);
	}

	public LispStruct append(final LispStruct... lispStructs) {

		LispStruct top = ListStruct.buildProperList(lispStructs);

		while (top instanceof ConsStruct) {

			final ConsStruct topConsStruct = (ConsStruct) top;
			final LispStruct carOfTop = topConsStruct.getCar();

			if (nullFunction.nullFnJavaBoolean(carOfTop)) {
				top = topConsStruct.getCdr();
			} else if (!(carOfTop instanceof ConsStruct)) {
				final LispStruct cdrTop = topConsStruct.getCdr();
				if (nullFunction.nullFnJavaBoolean(cdrTop)) {
					return carOfTop;
				} else {
					final String printedObject = printer.print(carOfTop);
					throw new ErrorException("Argument is not a list -- " + printedObject);
				}
			} else {
				final LispStruct cdrTop = topConsStruct.getCdr();
				if (!(cdrTop instanceof ConsStruct)) {
					return carOfTop;
				}

				final ConsStruct carOfTopConsStruct = (ConsStruct) carOfTop;

				final ConsStruct result = new ConsStruct(carOfTopConsStruct.getCar());
				ConsStruct splice = result;

				LispStruct x = carOfTopConsStruct.getCdr();
				while (x instanceof ConsStruct) {
					final ConsStruct xAsConsStruct = (ConsStruct) x;

					final LispStruct carOfX = xAsConsStruct.getCar();
					final ConsStruct newSpliceCdr = new ConsStruct(carOfX);
					splice.setCdr(newSpliceCdr);
					splice = newSpliceCdr;

					x = xAsConsStruct.getCdr();
				}
				if (!nullFunction.nullFnJavaBoolean(x)) {
					final String printedObject = printer.print(carOfTop);
					throw new TypeErrorException("Argument is not a proper list -- " + printedObject);
				}

				ConsStruct y = (ConsStruct) topConsStruct.getCdr();

				while (y.getCdr() instanceof ConsStruct) {
					final LispStruct carOfY = y.getCar();

					if (carOfY instanceof ListStruct) {

						LispStruct innerX = y.getCar();
						while (innerX instanceof ConsStruct) {
							final ConsStruct innerXAsConsStruct = (ConsStruct) innerX;

							final LispStruct carOfInnerX = innerXAsConsStruct.getCar();
							final ConsStruct newSpliceCdr = new ConsStruct(carOfInnerX);
							splice.setCdr(newSpliceCdr);
							splice = newSpliceCdr;

							innerX = innerXAsConsStruct.getCdr();
						}

						if (!nullFunction.nullFnJavaBoolean(innerX)) {
							final String printedObject = printer.print(y.getCar());
							throw new TypeErrorException("Argument is not a proper list -- " + printedObject);
						}

						y = (ConsStruct) y.getCdr();
					} else {
						final String printedObject = printer.print(carOfY);
						throw new ErrorException("Argument is not a list -- " + printedObject);
					}
				}

				splice.setCdr(y.getCar());

				return result;
			}
		}

		return NullStruct.INSTANCE;
	}
}
