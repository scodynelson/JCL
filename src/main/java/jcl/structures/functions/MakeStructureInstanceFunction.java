/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.structures.functions;

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
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.structures.StructureClassStruct;
import jcl.structures.StructureObjectStruct;
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

		final SymbolStruct<?> structSymArgSymbol = new SymbolStruct<>("STRUCT-SYM", GlobalPackageStruct.COMMON_LISP);
		final RequiredBinding structSymArgRequiredBinding = new RequiredBinding(structSymArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(structSymArgRequiredBinding);

		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final RestBinding restBinding = null;

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final SymbolStruct<?> structSymbol = (SymbolStruct) lispStructs[0];
		return makeStructureInstance(structSymbol);
	}

	public StructureObjectStruct makeStructureInstance(final SymbolStruct<?> structSymbol) {

		final StructureClassStruct structureClass = structSymbol.getStructureClass();
		if (structureClass == null) {
			throw new ProgramErrorException("Provided symbol '" + structSymbol + "' does not have a defined structure-class.");
		}

		return structureClass.newInstance();
	}
}
