/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import java.util.List;

import jcl.compiler.real.environment.binding.Binding;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;

public final class Environments {

	private Environments() {
	}

	public static void addDynamicVariableBinding(final SpecialDeclarationStruct specialDeclaration,
	                                             final Environment environment) {
		final SymbolStruct<?> var = specialDeclaration.getVar();

		final Binding binding = new Binding(var, TType.INSTANCE);
		environment.addDynamicBinding(binding);
	}

	public static LambdaEnvironment getEnclosingLambda(final Environment environment) {

		Environment currentEnvironment = environment;

		while (!(currentEnvironment instanceof LambdaEnvironment)) {
			currentEnvironment = currentEnvironment.getParent();
		}

		// NOTE: This will never be an improper cast, since the Null Environment is a LambdaEnvironment
		return (LambdaEnvironment) currentEnvironment;
	}

	public static boolean isSpecial(final DeclareStruct declare, final SymbolStruct<?> var) {
		boolean isSpecial = false;

		final List<SpecialDeclarationStruct> specialDeclarations = declare.getSpecialDeclarations();
		for (final SpecialDeclarationStruct specialDeclaration : specialDeclarations) {
			final SymbolStruct<?> specialVar = specialDeclaration.getVar();
			if (var.equals(specialVar)) {
				isSpecial = true;
				break;
			}
		}

		return isSpecial;
	}
}
