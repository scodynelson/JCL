/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.structures.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.structures.StructureClassStruct;
import jcl.structures.StructureObjectStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class MakeStructureInstanceFunction extends FunctionStruct {

	public static final SymbolStruct<?> MAKE_STRUCTURE_INSTANCE = GlobalPackageStruct.SYSTEM.intern("MAKE-STRUCTURE-INSTANCE").getSymbol();

	private static final long serialVersionUID = -3732127004637874803L;

	private MakeStructureInstanceFunction() {
		super("Makes a new structure-object instance of the structure-class assigned to the provided symbol with the provided arguments as slot values.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		MAKE_STRUCTURE_INSTANCE.setFunction(this);
		GlobalPackageStruct.SYSTEM.export(MAKE_STRUCTURE_INSTANCE);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> structSymArgSymbol = GlobalPackageStruct.SYSTEM.intern("STRUCT-SYM").getSymbol();
		final RequiredParameter structSymArgRequiredBinding = new RequiredParameter(structSymArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(structSymArgRequiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .build();
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
