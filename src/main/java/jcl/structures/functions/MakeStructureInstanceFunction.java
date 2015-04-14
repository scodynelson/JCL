/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.structures.functions;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.classes.StructureClassStruct;
import jcl.classes.StructureObjectStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class MakeStructureInstanceFunction extends FunctionStruct {

	public static final SymbolStruct<?> MAKE_STRUCTURE_INSTANCE = new SymbolStruct<>("MAKE-STRUCTURE-INSTANCE", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = -3732127004637874803L;

	private MakeStructureInstanceFunction() {
		super("Makes a new structure-object instance of the structure-class assigned to the provided symbol with the provided arguments as slot values.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		MAKE_STRUCTURE_INSTANCE.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> symbolArgSymbol = new SymbolStruct<>("SYM", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation symbolArgAllocation = new ParameterAllocation(0);
		final RequiredBinding symbolArgRequiredBinding = new RequiredBinding(symbolArgSymbol, symbolArgAllocation);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(symbolArgRequiredBinding);

		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final SymbolStruct<?> argsRestArgSymbol = new SymbolStruct<>("ARGS", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation argsRestArgArgAllocation = new ParameterAllocation(0);
		final RestBinding restBinding = new RestBinding(argsRestArgSymbol, argsRestArgArgAllocation);

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final SymbolStruct<?> symbol = (SymbolStruct) lispStructs[0];
		final LispStruct[] arguments = Arrays.copyOfRange(lispStructs, 1, lispStructs.length);
		return makeStructureInstance(symbol, arguments);
	}

	public StructureObjectStruct makeStructureInstance(final SymbolStruct<?> symbol, final LispStruct... args) {

		final StructureClassStruct structureClass = symbol.getStructureClass();
		if (structureClass == null) {
			throw new ProgramErrorException("Provided symbol '" + symbol + "' does not have a defined structure-class.");
		}

		// TODO
		return structureClass.newInstance(null);
	}
}
